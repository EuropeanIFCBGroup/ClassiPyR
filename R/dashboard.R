# Dashboard functions for ClassiPyR
#
# Functions for fetching sample lists and images from remote IFCB Dashboard
# instances (e.g., https://habon-ifcb.whoi.edu/).

#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory curl_fetch_disk new_handle
#' @importFrom iRfcb ifcb_download_dashboard_data ifcb_download_dashboard_metadata
NULL

#' Get persistent cache directory for dashboard downloads
#'
#' Returns the path to the dashboard cache directory. During R CMD check,
#' uses a temporary directory.
#'
#' @return Path to the dashboard cache directory
#' @export
#' @examples
#' cache_dir <- get_dashboard_cache_dir()
#' print(cache_dir)
get_dashboard_cache_dir <- function() {
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))) {
    return(file.path(tempdir(), "ClassiPyR", "dashboard"))
  }
  file.path(tools::R_user_dir("ClassiPyR", "cache"), "dashboard")
}

#' Parse an IFCB Dashboard URL
#'
#' Extracts the base URL and optional dataset name from a Dashboard URL.
#'
#' @param url Character. A Dashboard URL, e.g.
#'   \code{"https://habon-ifcb.whoi.edu/"} or
#'   \code{"https://habon-ifcb.whoi.edu/timeline?dataset=tangosund"}.
#' @return A list with \code{base_url} (without trailing slash) and
#'   \code{dataset_name} (character or NULL).
#' @export
#' @examples
#' parse_dashboard_url("https://habon-ifcb.whoi.edu/")
#' parse_dashboard_url("https://habon-ifcb.whoi.edu/timeline?dataset=tangosund")
parse_dashboard_url <- function(url) {
  if (is.null(url) || !is.character(url) || length(url) != 1 || !nzchar(url)) {
    stop("url must be a non-empty character string")
  }
  if (!grepl("^https?://", url)) {
    stop("url must start with http:// or https://")
  }

  # Extract dataset from query parameter ?dataset=xxx
  dataset_name <- NULL
  query_match <- regmatches(url, regexpr("[?&]dataset=([^&#]+)", url))
  if (length(query_match) == 1 && nzchar(query_match)) {
    dataset_name <- sub("^[?&]dataset=", "", query_match)
  }

  # Strip query string and path components (timeline, etc.) to get base URL
  base_url <- sub("[?].*$", "", url)
  base_url <- sub("/timeline/?$", "", base_url)
  base_url <- sub("/+$", "", base_url)

  list(base_url = base_url, dataset_name = dataset_name)
}

#' List bins from an IFCB Dashboard
#'
#' Fetches the list of bin (sample) names for a dashboard dataset. Delegates to
#' \code{\link[iRfcb]{ifcb_download_dashboard_metadata}}, which retrieves
#' per-bin metadata from the \code{api/export_metadata} endpoint, and returns
#' the \code{pid} column.
#'
#' The previous implementation used the \code{api/list_bins} endpoint, which was
#' removed from the upstream IFCB Dashboard (2026-03-08) and no longer works.
#'
#' @param base_url Character. Base URL (e.g. \code{"https://habon-ifcb.whoi.edu"}).
#' @param dataset_name Optional character. Dataset slug (e.g. \code{"tangosund"}).
#' @return Character vector of bin (sample) names.
#' @export
#' @examples
#' \dontrun{
#'   bins <- list_dashboard_bins("https://ifcb-data.whoi.edu", "mvco")
#' }
list_dashboard_bins <- function(base_url, dataset_name = NULL) {
  metadata <- ifcb_download_dashboard_metadata(
    base_url, dataset_name = dataset_name, quiet = TRUE
  )

  if (is.null(metadata) || !"pid" %in% names(metadata)) {
    return(character(0))
  }

  as.character(metadata$pid)
}

#' Resolve the dataset name for a sample from the Dashboard API
#'
#' Queries the \code{/api/bin/<sample>} endpoint to retrieve the
#' \code{primary_dataset} field. Useful when the user did not provide a
#' \code{?dataset=} query parameter in the dashboard URL.
#'
#' @param base_url Character. Dashboard base URL (no trailing slash).
#' @param sample_name Character. Sample name (bin PID).
#' @return Character dataset name, or NULL if it could not be resolved.
#' @export
resolve_sample_dataset <- function(base_url, sample_name) {
  api_url <- paste0(sub("/+$", "", base_url), "/api/bin/", sample_name)

  tryCatch({
    response <- curl::curl_fetch_memory(api_url,
      handle = curl::new_handle(httpheader = c(Accept = "application/json")))

    if (response$status_code != 200) return(NULL)

    json_content <- rawToChar(response$content)
    Encoding(json_content) <- "UTF-8"
    parsed <- jsonlite::fromJSON(json_content, flatten = TRUE)

    ds <- parsed[["primary_dataset"]]
    if (!is.null(ds) && is.character(ds) && nzchar(ds)) ds else NULL
  }, error = function(e) {
    NULL
  })
}

#' Download and extract PNG images from the Dashboard
#'
#' Downloads a zip file of PNG images for a sample from the Dashboard.
#' Extracts into the cache directory. Skips re-download if PNGs already exist.
#'
#' @param base_url Character. Dashboard base URL.
#' @param sample_name Character. Sample name (bin PID).
#' @param cache_dir Character. Cache directory. Defaults to
#'   \code{\link{get_dashboard_cache_dir}()}.
#' @param parallel_downloads Integer. Number of parallel downloads.
#' @param sleep_time Numeric. Seconds to sleep between download batches.
#' @param multi_timeout Numeric. Timeout in seconds for multi-file downloads.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @return Path to the folder containing extracted PNGs, or NULL on failure.
#' @export
download_dashboard_images <- function(base_url, sample_name,
                                      cache_dir = get_dashboard_cache_dir(),
                                      parallel_downloads = 5, sleep_time = 2,
                                      multi_timeout = 120, max_retries = 3) {
  # Expected path structure: cache_dir/sample_name/sample_name/*.png
  png_folder <- file.path(cache_dir, sample_name)
  png_subfolder <- file.path(png_folder, sample_name)

  # Check if PNGs already exist in cache
 if (dir.exists(png_subfolder)) {
    existing_pngs <- list.files(png_subfolder, pattern = "\\.png$")
    if (length(existing_pngs) > 0) {
      return(png_folder)
    }
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Build the dashboard URL for download
  # ifcb_download_dashboard_data expects a URL with a path component
  dashboard_url <- paste0(sub("/+$", "", base_url), "/")

  tryCatch({
    ifcb_download_dashboard_data(
      dashboard_url = dashboard_url,
      samples = sample_name,
      file_types = "zip",
      dest_dir = cache_dir,
      parallel_downloads = parallel_downloads,
      sleep_time = sleep_time,
      multi_timeout = multi_timeout,
      max_retries = max_retries,
      quiet = TRUE
    )

    # The download saves to cache_dir/DYYYYMMDD/sample_name.zip
    # Find the zip file
    date_part <- substr(sample_name, 1, 9)
    zip_path <- file.path(cache_dir, date_part, paste0(sample_name, ".zip"))

    if (!file.exists(zip_path)) {
      # Try alternate location (directly in cache_dir)
      zip_path <- file.path(cache_dir, paste0(sample_name, ".zip"))
    }

    if (!file.exists(zip_path)) {
      warning("Zip file not found after download for: ", sample_name)
      return(NULL)
    }

    # Extract to the expected folder structure
    dir.create(png_subfolder, recursive = TRUE, showWarnings = FALSE)

    # ZIP Slip prevention: reject entries with ".." path components
    zip_contents <- utils::unzip(zip_path, list = TRUE)$Name
    bad <- grep("\\.\\.", zip_contents, value = TRUE)
    if (length(bad) > 0) {
      unlink(png_subfolder, recursive = TRUE)
      stop("ZIP archive contains path traversal entry: ", bad[1])
    }
    # Only extract PNG images (dashboard ZIPs also contain metadata/csv/json)
    png_entries <- grep("\\.png$", zip_contents, value = TRUE, ignore.case = TRUE)
    utils::unzip(zip_path, files = png_entries, exdir = png_subfolder)

    # Clean up zip file
    unlink(zip_path)
    # Also clean up the date folder if empty
    date_folder <- file.path(cache_dir, date_part)
    if (dir.exists(date_folder) && length(list.files(date_folder)) == 0) {
      unlink(date_folder, recursive = TRUE)
    }

    png_folder
  }, error = function(e) {
    warning("Failed to download images for ", sample_name, ": ", e$message)
    NULL
  })
}

#' Download ADC file from the Dashboard
#'
#' Downloads the ADC file for a sample from the Dashboard on demand.
#'
#' @param base_url Character. Dashboard base URL.
#' @param sample_name Character. Sample name.
#' @param cache_dir Character. Cache directory.
#' @param parallel_downloads Integer. Number of parallel downloads.
#' @param sleep_time Numeric. Seconds to sleep between download batches.
#' @param multi_timeout Numeric. Timeout in seconds for multi-file downloads.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @return Path to the downloaded ADC file, or NULL on failure.
#' @export
download_dashboard_adc <- function(base_url, sample_name,
                                   cache_dir = get_dashboard_cache_dir(),
                                   parallel_downloads = 5, sleep_time = 2,
                                   multi_timeout = 120, max_retries = 3) {
  date_part <- substr(sample_name, 1, 9)
  adc_path <- file.path(cache_dir, date_part, paste0(sample_name, ".adc"))

  if (file.exists(adc_path)) {
    return(adc_path)
  }

  dashboard_url <- paste0(sub("/+$", "", base_url), "/")

  tryCatch({
    ifcb_download_dashboard_data(
      dashboard_url = dashboard_url,
      samples = sample_name,
      file_types = "adc",
      dest_dir = cache_dir,
      parallel_downloads = parallel_downloads,
      sleep_time = sleep_time,
      multi_timeout = multi_timeout,
      max_retries = max_retries,
      quiet = TRUE
    )

    if (file.exists(adc_path)) {
      return(adc_path)
    }

    # Try alternate location
    alt_path <- file.path(cache_dir, paste0(sample_name, ".adc"))
    if (file.exists(alt_path)) {
      return(alt_path)
    }

    NULL
  }, error = function(e) {
    warning("Failed to download ADC for ", sample_name, ": ", e$message)
    NULL
  })
}

#' Download and parse autoclass scores from the Dashboard
#'
#' Downloads \code{_class_scores.csv} for a sample and extracts the winning
#' class (column with max score) per ROI.
#'
#' @param base_url Character. Dashboard base URL.
#' @param sample_name Character. Sample name.
#' @param cache_dir Character. Cache directory.
#' @param dataset_name Optional character. Dataset slug (e.g. \code{"mvco"}).
#'   If NULL, resolved automatically via \code{\link{resolve_sample_dataset}}.
#' @param parallel_downloads Integer. Number of parallel downloads.
#' @param sleep_time Numeric. Seconds to sleep between download batches.
#' @param multi_timeout Numeric. Timeout in seconds for multi-file downloads.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @return Data frame with columns \code{file_name}, \code{class_name},
#'   \code{score}, or NULL on failure.
#' @export
download_dashboard_autoclass <- function(base_url, sample_name,
                                         cache_dir = get_dashboard_cache_dir(),
                                         dataset_name = NULL,
                                         parallel_downloads = 5, sleep_time = 2,
                                         multi_timeout = 120, max_retries = 3) {
  # Class scores are served from the dataset path (e.g., /mvco/), not /data/
  # If dataset_name is not provided, resolve it from the bin API
  if (is.null(dataset_name) || !nzchar(dataset_name)) {
    dataset_name <- resolve_sample_dataset(base_url, sample_name)
  }

  if (!is.null(dataset_name) && nzchar(dataset_name)) {
    dashboard_url <- paste0(sub("/+$", "", base_url), "/", dataset_name, "/")
  } else {
    dashboard_url <- paste0(sub("/+$", "", base_url), "/")
  }

  tryCatch({
    ifcb_download_dashboard_data(
      dashboard_url = dashboard_url,
      samples = sample_name,
      file_types = "autoclass",
      dest_dir = cache_dir,
      parallel_downloads = parallel_downloads,
      sleep_time = sleep_time,
      multi_timeout = multi_timeout,
      max_retries = max_retries,
      quiet = TRUE
    )

    # Find the downloaded CSV file - may have a version suffix
    csv_pattern <- paste0("^", sample_name, "_class.*\\.csv$")
    csv_files <- list.files(cache_dir, pattern = csv_pattern, recursive = TRUE,
                            full.names = TRUE)

    if (length(csv_files) == 0) {
      return(NULL)
    }

    csv_path <- csv_files[1]

    # Parse the score matrix CSV
    # Rows = ROIs, columns = class names, values = scores
    scores <- utils::read.csv(csv_path, check.names = FALSE)

    if (nrow(scores) == 0 || ncol(scores) < 2) {
      return(NULL)
    }

    # The first column is typically the ROI identifier
    # Check if the first column looks like a ROI ID (numeric or sample_NNNNN)
    first_col <- scores[[1]]
    class_cols <- if (is.numeric(first_col) || all(grepl("^\\d+$|_\\d+$", as.character(first_col)))) {
      # First column is ROI identifier, class scores start at column 2
      names(scores)[-1]
    } else {
      names(scores)
    }

    score_matrix <- as.matrix(scores[, class_cols, drop = FALSE])

    # Extract winning class per ROI
    max_idx <- apply(score_matrix, 1, which.max)
    max_scores <- apply(score_matrix, 1, max)
    winning_classes <- class_cols[max_idx]

    # Extract file names from pid column (e.g., "D20190402T200352_IFCB010_00001")
    # or fall back to sequential ROI numbers if pid is not available
    if (is.character(first_col) && all(grepl("_\\d+$", first_col))) {
      # pid column contains full identifiers — use them directly
      file_names <- paste0(first_col, ".png")
    } else if (is.numeric(first_col) || all(grepl("^\\d+$", as.character(first_col)))) {
      # First column is numeric ROI numbers
      roi_numbers <- as.integer(first_col)
      file_names <- sprintf("%s_%05d.png", sample_name, roi_numbers)
    } else {
      # Fallback: sequential numbering
      file_names <- sprintf("%s_%05d.png", sample_name, seq_len(nrow(scores)))
    }

    data.frame(
      file_name = file_names,
      class_name = winning_classes,
      score = max_scores,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning("Failed to download autoclass for ", sample_name, ": ", e$message)
    NULL
  })
}

#' Bulk download zip archives for multiple samples from the Dashboard
#'
#' Downloads zip files for all specified samples in a single batched call
#' to \code{\link[iRfcb]{ifcb_download_dashboard_data}}, leveraging its
#' built-in parallel download support. Samples already cached are skipped.
#' After download, zips are extracted and cleaned up.
#'
#' @param base_url Character. Dashboard base URL.
#' @param sample_names Character vector. Sample names to download.
#' @param cache_dir Character. Cache directory.
#' @param parallel_downloads Integer. Number of parallel downloads.
#' @param sleep_time Numeric. Seconds to sleep between download batches.
#' @param multi_timeout Numeric. Timeout in seconds for multi-file downloads.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @return Character vector of sample names that were successfully downloaded
#'   or already cached.
#' @export
download_dashboard_images_bulk <- function(base_url, sample_names,
                                           cache_dir = get_dashboard_cache_dir(),
                                           parallel_downloads = 5, sleep_time = 2,
                                           multi_timeout = 120, max_retries = 3) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dashboard_url <- paste0(sub("/+$", "", base_url), "/")

  # Determine which samples need downloading (not already cached)
  needs_download <- vapply(sample_names, function(sn) {
    png_subfolder <- file.path(cache_dir, sn, sn)
    !(dir.exists(png_subfolder) &&
        length(list.files(png_subfolder, pattern = "\\.png$")) > 0)
  }, logical(1))

  to_download <- sample_names[needs_download]

  if (length(to_download) > 0) {
    tryCatch({
      ifcb_download_dashboard_data(
        dashboard_url = dashboard_url,
        samples = to_download,
        file_types = "zip",
        dest_dir = cache_dir,
        parallel_downloads = parallel_downloads,
        sleep_time = sleep_time,
        multi_timeout = multi_timeout,
        max_retries = max_retries,
        quiet = TRUE
      )
    }, error = function(e) {
      warning("Bulk zip download failed: ", e$message)
    })

    # Extract each downloaded zip into the expected folder structure
    for (sn in to_download) {
      date_part <- substr(sn, 1, 9)
      zip_path <- file.path(cache_dir, date_part, paste0(sn, ".zip"))

      if (!file.exists(zip_path)) {
        zip_path <- file.path(cache_dir, paste0(sn, ".zip"))
      }
      if (!file.exists(zip_path)) next

      png_subfolder <- file.path(cache_dir, sn, sn)
      dir.create(png_subfolder, recursive = TRUE, showWarnings = FALSE)
      tryCatch({
        # ZIP Slip prevention: reject entries with ".." path components
        zip_contents <- utils::unzip(zip_path, list = TRUE)$Name
        bad <- grep("\\.\\.", zip_contents, value = TRUE)
        if (length(bad) > 0) {
          unlink(png_subfolder, recursive = TRUE)
          stop("ZIP archive contains path traversal entry: ", bad[1])
        }
        # Only extract PNG images (dashboard ZIPs also contain metadata/csv/json)
        png_entries <- grep("\\.png$", zip_contents, value = TRUE, ignore.case = TRUE)
        utils::unzip(zip_path, files = png_entries, exdir = png_subfolder)
      }, error = function(e) NULL)
      unlink(zip_path)

      # Clean up empty date folder
      date_folder <- file.path(cache_dir, date_part)
      if (dir.exists(date_folder) && length(list.files(date_folder)) == 0) {
        unlink(date_folder, recursive = TRUE)
      }
    }
  }

  # Return all samples that are now cached
  cached_ok <- vapply(sample_names, function(sn) {
    png_subfolder <- file.path(cache_dir, sn, sn)
    dir.exists(png_subfolder) &&
      length(list.files(png_subfolder, pattern = "\\.png$")) > 0
  }, logical(1))

  sample_names[cached_ok]
}

#' Download a single PNG image from the Dashboard
#'
#' Downloads one PNG from the Dashboard's \code{/data/} endpoint.
#' The image is saved to \code{dest_dir/sample_name/file_name}.
#'
#' @param base_url Character. Dashboard base URL.
#' @param sample_name Character. Sample name (bin PID).
#' @param roi_number Integer. ROI number to download.
#' @param dest_dir Character. Destination directory.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @param timeout Numeric. Request timeout in seconds.
#' @return File path to the downloaded PNG, or NULL on failure.
#' @export
download_dashboard_image_single <- function(base_url, sample_name, roi_number,
                                            dest_dir, max_retries = 3,
                                            timeout = 15) {
  if (!grepl("^D\\d{8}T\\d{6}_IFCB\\d+$", sample_name)) {
    stop("Invalid sample_name format: ", sample_name)
  }
  roi_number <- as.integer(roi_number)
  if (is.na(roi_number) || roi_number < 0L) {
    stop("roi_number must be a non-negative integer")
  }
  file_name <- sprintf("%s_%05d.png", sample_name, roi_number)
  dest_folder <- file.path(dest_dir, sample_name)
  dest_path <- file.path(dest_folder, file_name)

  if (file.exists(dest_path)) {
    return(dest_path)
  }

  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)

  img_url <- paste0(sub("/+$", "", base_url), "/data/", file_name)

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 10, timeout = timeout)
      response <- curl::curl_fetch_disk(img_url, dest_path, handle = h)
      if (response$status_code == 200 && file.exists(dest_path) &&
          file.info(dest_path)$size > 0) {
        return(dest_path)
      }
      # Non-200 status or empty file — no point retrying a 404
      if (file.exists(dest_path)) unlink(dest_path)
      if (response$status_code %in% c(404L, 410L)) return(NULL)
      NULL
    }, error = function(e) {
      if (file.exists(dest_path)) unlink(dest_path)
      NULL
    })

    if (!is.null(result)) return(result)
    if (attempt < max_retries) Sys.sleep(0.5)
  }

  NULL
}

#' Download individual PNG images from the Dashboard
#'
#' Downloads specific PNG files from the Dashboard's \code{/data/} endpoint,
#' one at a time. This is much faster than downloading entire zip archives
#' when only a subset of ROIs are needed (e.g., class review mode).
#'
#' Samples that fail repeatedly are automatically skipped to avoid long
#' waits when annotations reference samples not available on the dashboard.
#'
#' @param base_url Character. Dashboard base URL.
#' @param file_names Character vector. PNG file names
#'   (e.g., \code{"D20240716T000431_IFCB134_00108.png"}).
#' @param dest_dir Character. Destination directory.
#' @param max_retries Integer. Maximum number of retry attempts per image.
#' @param sample_fail_threshold Integer. After this many consecutive failures
#'   from the same sample, skip all remaining images from that sample.
#' @return Character vector of successfully downloaded file names.
#' @export
download_dashboard_images_individual <- function(base_url, file_names, dest_dir,
                                                 max_retries = 3,
                                                 sample_fail_threshold = 2) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  succeeded <- character()
  # Track consecutive failures per sample to skip unavailable samples early
  sample_failures <- list()
  skipped_samples <- character()

  for (fname in file_names) {
    # Parse sample_name and roi_number via iRfcb parser (handles legacy names).
    parsed <- tryCatch(
      iRfcb::ifcb_convert_filenames(fname),
      error = function(e) NULL
    )
    if (is.null(parsed) || nrow(parsed) == 0) next

    sample_name <- as.character(parsed$sample[1])
    roi_number <- if ("roi" %in% names(parsed)) {
      suppressWarnings(as.integer(parsed$roi[1]))
    } else {
      NA_integer_
    }
    if (is.na(roi_number)) next

    # Skip samples that have already been marked as unavailable
    if (sample_name %in% skipped_samples) next

    result <- download_dashboard_image_single(
      base_url = base_url,
      sample_name = sample_name,
      roi_number = roi_number,
      dest_dir = dest_dir,
      max_retries = max_retries
    )

    if (!is.null(result)) {
      succeeded <- c(succeeded, fname)
      # Reset failure counter on success
      sample_failures[[sample_name]] <- 0L
    } else {
      prev <- sample_failures[[sample_name]]
      count <- (if (is.null(prev)) 0L else prev) + 1L
      sample_failures[[sample_name]] <- count
      if (count >= sample_fail_threshold) {
        skipped_samples <- c(skipped_samples, sample_name)
        warning("Skipping remaining images from ", sample_name,
                " (", count, " consecutive failures)", call. = FALSE)
      }
    }
  }

  succeeded
}
