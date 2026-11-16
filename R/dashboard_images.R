# Parallel download of individual PNG images from an IFCB Dashboard

# Images requested between two progress reports (and two checks for samples
# that are not on the dashboard)
DASHBOARD_IMAGE_CHUNK <- 100L

# Seconds one image request may take, as in download_dashboard_image_single()
DASHBOARD_IMAGE_TIMEOUT <- 15

# Sample names the /data/ endpoint is asked for
DASHBOARD_SAMPLE_PATTERN <- "^D\\d{8}T\\d{6}_IFCB\\d+$"

#' Sample name and ROI number of PNG file names
#'
#' Parses all names in one call to the iRfcb parser. One malformed name makes
#' that call fail, so the names are then parsed one by one and the malformed
#' ones dropped.
#'
#' @param file_names Character vector of PNG file names.
#' @return Data frame with columns \code{file_name}, \code{sample_name} and
#'   \code{roi_number}, one row per name that could be parsed.
#' @keywords internal
parse_dashboard_image_names <- function(file_names) {
  parse <- function(names) {
    parsed <- tryCatch(iRfcb::ifcb_convert_filenames(names),
                       error = function(e) NULL)
    if (is.null(parsed) || nrow(parsed) != length(names) ||
        !"roi" %in% names(parsed)) {
      return(NULL)
    }
    data.frame(file_name = names,
               sample_name = as.character(parsed$sample),
               roi_number = suppressWarnings(as.integer(parsed$roi)),
               stringsAsFactors = FALSE)
  }
  empty <- data.frame(file_name = character(), sample_name = character(),
                      roi_number = integer(), stringsAsFactors = FALSE)
  if (length(file_names) == 0) return(empty)

  all_at_once <- parse(file_names)
  if (!is.null(all_at_once)) return(all_at_once)
  do.call(rbind, c(list(empty), lapply(file_names, parse)))
}

#' Requests needed to download a set of dashboard images
#'
#' @inheritParams download_dashboard_images_individual
#' @return Data frame with one row per image that can be requested:
#'   \code{file_name} (as given), \code{sample_name}, \code{url} and
#'   \code{dest_path}. Names that cannot be parsed, or whose sample name is
#'   not in the \code{DYYYYMMDDTHHMMSS_IFCBnnn} form, are left out.
#' @keywords internal
dashboard_image_requests <- function(base_url, file_names, dest_dir) {
  parsed <- parse_dashboard_image_names(file_names)
  usable <- grepl(DASHBOARD_SAMPLE_PATTERN, parsed$sample_name) &
    !is.na(parsed$roi_number) & parsed$roi_number >= 0L
  parsed <- parsed[usable, , drop = FALSE]

  png_name <- sprintf("%s_%05d.png", parsed$sample_name, parsed$roi_number)
  data.frame(
    file_name = parsed$file_name,
    sample_name = parsed$sample_name,
    url = paste0(sub("/+$", "", base_url), "/data/", png_name,
                 recycle0 = TRUE),
    dest_path = file.path(dest_dir, parsed$sample_name, png_name),
    stringsAsFactors = FALSE
  )
}

#' Request a batch of images at the same time
#'
#' Sends every request through one connection pool that is limited to
#' \code{parallel_downloads} connections, and writes each response straight
#' to its file. A response that is not an image (an error page) is written
#' too: the caller decides from the status what to keep.
#'
#' @param urls Character vector of image URLs.
#' @param dest_paths Character vector of file paths, one per URL.
#' @param parallel_downloads Integer. Number of simultaneous connections.
#' @param multi_timeout Numeric. Seconds to wait for the whole batch;
#'   requests still running after that are cancelled.
#' @return Integer vector with the HTTP status of each request, \code{NA}
#'   for a request that got no response.
#' @keywords internal
fetch_dashboard_image_batch <- function(urls, dest_paths,
                                        parallel_downloads = 5,
                                        multi_timeout = 120) {
  connections <- max(1L, as.integer(parallel_downloads))
  # Without multiplexing, so that the number of connections is the number of
  # requests the dashboard handles at once
  pool <- curl::new_pool(total_con = connections, host_con = connections,
                         multiplex = FALSE)
  responses <- new.env(parent = emptyenv())
  record <- function(i) {
    force(i)
    function(response) assign(as.character(i), response$status_code, responses)
  }

  for (i in seq_along(urls)) {
    handle <- curl::new_handle(url = urls[i], connecttimeout = 10,
                               timeout = DASHBOARD_IMAGE_TIMEOUT)
    curl::multi_add(handle, done = record(i), fail = function(message) NULL,
                    data = dest_paths[i], pool = pool)
  }
  tryCatch(curl::multi_run(timeout = multi_timeout, pool = pool),
           error = function(e) NULL)
  lapply(curl::multi_list(pool), curl::multi_cancel)

  vapply(seq_along(urls), function(i) {
    status <- responses[[as.character(i)]]
    if (is.null(status)) NA_integer_ else as.integer(status)
  }, integer(1))
}

#' Download a batch of images, trying failed requests again
#'
#' An image counts as downloaded when the dashboard answered 200 and the file
#' is not empty. Anything else is deleted. A 404 or 410 is final; other
#' failures are requested again, up to \code{max_retries} attempts in all.
#'
#' @inheritParams fetch_dashboard_image_batch
#' @param max_retries Integer. Maximum number of attempts per image.
#' @return Logical vector, \code{TRUE} for each image that was downloaded.
#' @keywords internal
fetch_dashboard_images_with_retry <- function(urls, dest_paths, max_retries,
                                              parallel_downloads,
                                              multi_timeout) {
  downloaded <- rep(FALSE, length(urls))
  pending <- seq_along(urls)
  attempts <- max(1L, as.integer(max_retries))

  for (attempt in seq_len(attempts)) {
    if (length(pending) == 0) break
    paths <- dest_paths[pending]
    status <- fetch_dashboard_image_batch(urls[pending], paths,
                                          parallel_downloads, multi_timeout)
    size <- file.size(paths)
    good <- !is.na(status) & status == 200L & !is.na(size) & size > 0
    unlink(paths[!good])

    downloaded[pending[good]] <- TRUE
    pending <- pending[!good & !status %in% c(404L, 410L)]
    if (length(pending) > 0 && attempt < attempts) Sys.sleep(0.5)
  }
  downloaded
}

#' Samples that are not on the dashboard
#'
#' @param sample_names Sample name of each image requested so far.
#' @param downloaded Logical, whether each of those images was downloaded.
#' @param sample_fail_threshold Number of failures from which a sample
#'   without a single image is given up.
#' @return Character vector of sample names.
#' @keywords internal
unavailable_dashboard_samples <- function(sample_names, downloaded,
                                          sample_fail_threshold) {
  failures <- table(sample_names[!downloaded])
  failing <- names(failures)[failures >= sample_fail_threshold]
  setdiff(failing, sample_names[downloaded])
}

#' Download dashboard images several at a time
#'
#' The parallel path of \code{\link{download_dashboard_images_individual}}.
#'
#' @inheritParams download_dashboard_images_individual
#' @return Character vector of successfully downloaded file names.
#' @keywords internal
download_dashboard_images_parallel <- function(base_url, file_names, dest_dir,
                                               max_retries, sample_fail_threshold,
                                               parallel_downloads, multi_timeout,
                                               progress) {
  requests <- dashboard_image_requests(base_url, file_names, dest_dir)
  on_disk <- file.exists(requests$dest_path)
  total <- length(file_names)

  # What has been tried so far: the files found on disk count as downloaded
  tried <- data.frame(file_name = requests$file_name[on_disk],
                      sample_name = requests$sample_name[on_disk],
                      downloaded = rep(TRUE, sum(on_disk)),
                      stringsAsFactors = FALSE)
  queue <- requests[!on_disk, , drop = FALSE]
  given_up <- character()
  for (folder in unique(dirname(queue$dest_path))) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  report_bulk_progress(progress, total - nrow(queue), total)

  while (nrow(queue) > 0) {
    in_chunk <- seq_len(nrow(queue)) <= DASHBOARD_IMAGE_CHUNK
    chunk <- queue[in_chunk, , drop = FALSE]
    downloaded <- fetch_dashboard_images_with_retry(
      chunk$url, chunk$dest_path, max_retries, parallel_downloads, multi_timeout
    )
    tried <- rbind(tried, data.frame(file_name = chunk$file_name,
                                     sample_name = chunk$sample_name,
                                     downloaded = downloaded,
                                     stringsAsFactors = FALSE))

    unavailable <- setdiff(
      unavailable_dashboard_samples(tried$sample_name, tried$downloaded,
                                    sample_fail_threshold),
      given_up
    )
    for (sample_name in unavailable) {
      warning("Skipping remaining images from ", sample_name, " (",
              sum(tried$sample_name == sample_name),
              " failures, no image found)", call. = FALSE)
    }
    given_up <- c(given_up, unavailable)
    queue <- queue[!in_chunk & !queue$sample_name %in% given_up, , drop = FALSE]
    report_bulk_progress(progress, total - nrow(queue), total)
  }

  file_names[file_names %in% tried$file_name[tried$downloaded]]
}
