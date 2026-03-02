# Dashboard functions for ClassiPyR
#
# Functions for fetching sample lists and images from remote IFCB Dashboard
# instances (e.g., https://habon-ifcb.whoi.edu/).

#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory new_handle
#' @importFrom iRfcb ifcb_download_dashboard_data
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
#' Fetches the bin list from the Dashboard API. This is a vendored copy of
#' \code{iRfcb::ifcb_list_dashboard_bins()} from the development version that
#' supports the \code{dataset_name} parameter.
#'
#' @param base_url Character. Base URL (e.g. \code{"https://habon-ifcb.whoi.edu"}).
#' @param dataset_name Optional character. Dataset slug (e.g. \code{"tangosund"}).
#' @return Character vector of bin (sample) names.
#' @export
#' @examples
#' \donttest{
#'   bins <- list_dashboard_bins("https://ifcb-data.whoi.edu", "mvco")
#' }
# TODO: Replace with iRfcb::ifcb_list_dashboard_bins() once iRfcb >= 0.9.0
# ships dataset_name support.
list_dashboard_bins <- function(base_url, dataset_name = NULL) {
  base_url <- sub("/+$", "", base_url)

  api_url <- paste0(base_url, "/api/list_bins")

  if (!is.null(dataset_name) && nzchar(dataset_name)) {
    dataset_name <- utils::URLencode(dataset_name, reserved = TRUE)
    api_url <- paste0(api_url, "?dataset=", dataset_name)
  }

  response <- tryCatch(
    curl::curl_fetch_memory(api_url,
      handle = curl::new_handle(httpheader = c(Accept = "application/json"))),
    error = function(e) stop("Failed to connect to IFCB Dashboard API: ", e$message)
  )

  if (response$status_code != 200) {
    stop("API request failed [", response$status_code, "]: ", api_url)
  }

  json_content <- rawToChar(response$content)
  Encoding(json_content) <- "UTF-8"

  parsed <- tryCatch(
    jsonlite::fromJSON(json_content, flatten = TRUE),
    error = function(e) stop("Failed to parse JSON content: ", e$message)
  )

  # The API returns a list with one element containing a data frame with a
  # "pid" column (or similar). Extract the sample names.
  if (is.data.frame(parsed)) {
    bins <- parsed[[1]]
  } else if (is.list(parsed) && length(parsed) > 0) {
    first <- parsed[[1]]
    if (is.data.frame(first)) {
      bins <- first[[1]]
    } else {
      bins <- as.character(first)
    }
  } else {
    bins <- as.character(parsed)
  }

  as.character(bins)
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
#' @return Path to the folder containing extracted PNGs, or NULL on failure.
#' @export
download_dashboard_images <- function(base_url, sample_name, cache_dir = get_dashboard_cache_dir()) {
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
      parallel_downloads = 2,
      sleep_time = 5,
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
    utils::unzip(zip_path, exdir = png_subfolder)

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
#' @return Path to the downloaded ADC file, or NULL on failure.
#' @export
download_dashboard_adc <- function(base_url, sample_name, cache_dir = get_dashboard_cache_dir()) {
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
      parallel_downloads = 2,
      sleep_time = 5,
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
#' @return Data frame with columns \code{file_name}, \code{class_name},
#'   \code{score}, or NULL on failure.
#' @export
download_dashboard_autoclass <- function(base_url, sample_name, cache_dir = get_dashboard_cache_dir()) {
  # The dashboard URL needs to include the dataset path for autoclass
  dashboard_url <- paste0(sub("/+$", "", base_url), "/")

  tryCatch({
    ifcb_download_dashboard_data(
      dashboard_url = dashboard_url,
      samples = sample_name,
      file_types = "autoclass",
      dest_dir = cache_dir,
      parallel_downloads = 2,
      sleep_time = 5,
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

    # Build ROI numbers
    roi_numbers <- seq_len(nrow(scores))

    data.frame(
      file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
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
#' @return Character vector of sample names that were successfully downloaded
#'   or already cached.
#' @export
download_dashboard_images_bulk <- function(base_url, sample_names,
                                           cache_dir = get_dashboard_cache_dir()) {
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
        parallel_downloads = 2,
        sleep_time = 5,
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
      tryCatch(utils::unzip(zip_path, exdir = png_subfolder), error = function(e) NULL)
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
