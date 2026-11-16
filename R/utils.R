# Utility functions for ClassiPyR
#
# Note: The following imports are used by the Shiny app (server.R, ui.R, global.R)
# which are not part of the package build but are needed at runtime.
#' @importFrom iRfcb ifcb_get_mat_variable
#' @importFrom shiny shinyApp
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton
#' @importFrom bslib bs_theme
#' @importFrom DT renderDT
#' @importFrom jsonlite fromJSON
#' @importFrom lifecycle deprecated
#' @importFrom dplyr filter
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbWriteTable dbExecute
#' @importFrom RSQLite SQLite
NULL

#' Get ClassiPyR configuration directory
#'
#' Returns the path to the configuration directory for storing settings.
#' Uses tools::R_user_dir() for CRAN compliance. During R CMD check,
#' uses a temporary directory to avoid writing to user directories.
#'
#' @return Path to the configuration directory
#' @export
#' @examples
#' # Get the configuration directory path
#' config_dir <- get_config_dir()
#' print(config_dir)
get_config_dir <- function() {
  # Check if running under R CMD check
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))) {
    return(file.path(tempdir(), "ClassiPyR"))
  }
  tools::R_user_dir("ClassiPyR", "config")
}

#' Get default database directory
#'
#' Returns the default path for the SQLite annotations database. This is a
#' persistent, local, user-level directory that survives package reinstalls.
#' The database should be stored on a local filesystem, not on a network
#' drive, because SQLite file locking is unreliable over network filesystems.
#'
#' @return Path to the default database directory
#' @export
#' @seealso \code{\link{get_db_path}} for the full database file path
#' @examples
#' # Get the default database directory
#' db_dir <- get_default_db_dir()
#' print(db_dir)
get_default_db_dir <- function() {
  # Check if running under R CMD check
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))) {
    return(file.path(tempdir(), "ClassiPyR", "db"))
  }
  tools::R_user_dir("ClassiPyR", "data")
}

#' Get path to settings file
#'
#' Returns the path to the settings JSON file, creating the configuration
#' directory if it doesn't exist.
#'
#' @return Path to the settings JSON file
#' @export
#' @examples
#' # Get the settings file path
#' settings_path <- get_settings_path()
#' print(settings_path)
get_settings_path <- function() {
  config_dir <- get_config_dir()
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(config_dir, "settings.json")
}

#' Update settings file by merging new values
#'
#' Reads the existing settings JSON file (if any), merges the given settings
#' into it, and writes the result back. Callers that only know a subset of
#' settings keys can therefore update those keys without erasing the rest of
#' the file. A key with a \code{NULL} value is removed from the file.
#'
#' @param settings Named list of settings to update.
#' @param settings_file Path to the settings JSON file. Defaults to
#'   \code{\link{get_settings_path}}.
#' @return Invisibly, the full merged settings list.
#' @export
update_settings_file <- function(settings, settings_file = get_settings_path()) {
  existing <- list()
  if (file.exists(settings_file)) {
    existing <- tryCatch(
      jsonlite::fromJSON(settings_file),
      error = function(e) list()
    )
    if (!is.list(existing)) existing <- list()
  }
  merged <- utils::modifyList(existing, settings)
  jsonlite::write_json(merged, settings_file, auto_unbox = TRUE, pretty = TRUE)
  invisible(merged)
}

#' Get path to file index cache
#'
#' Returns the path to the file index JSON cache file. The file index
#' stores scanned folder results to avoid expensive recursive directory
#' scans on startup.
#'
#' @return Path to the file index JSON file
#' @export
get_file_index_path <- function() {
  file.path(get_config_dir(), "file_index.json")
}

#' Save file index to disk cache
#'
#' Writes the file index data to a JSON cache file for fast startup.
#'
#' @param data List containing scan results (sample names, path maps, etc.)
#' @return NULL (called for side effects)
#' @export
save_file_index <- function(data) {
  tryCatch({
    path <- get_file_index_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE)
  }, error = function(e) {
    message("Could not save file index: ", e$message)
  })
}

#' Load file index from disk cache
#'
#' Reads the cached file index if it exists and is valid JSON.
#'
#' @return List with cached data, or NULL if no cache exists or it is invalid
#' @export
load_file_index <- function() {
  path <- get_file_index_path()
  if (file.exists(path)) {
    tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(e) {
        message("Could not load file index: ", e$message)
        NULL
      }
    )
  } else {
    NULL
  }
}

#' Rescan folders and rebuild the file index cache
#'
#' Scans the configured (or specified) ROI, classification, and output folders
#' for IFCB sample files and saves the results to the file index cache.
#' This can be called outside the Shiny app, e.g. from a cron job, to keep
#' the cache up to date without manually clicking the rescan button.
#'
#' If folder paths are not provided, they are read from saved settings.
#'
#' @param roi_folder Path to ROI/PNG data folder. If NULL, read from saved settings.
#' @param csv_folder Path to classification folder (CSV/H5/MAT). If NULL, read from saved settings.
#' @param output_folder Path to output folder for MAT annotations. If NULL, read from saved settings.
#' @param verbose If TRUE, print progress messages. Default TRUE.
#' @param db_folder Path to the database folder for SQLite annotations. If NULL,
#'   read from saved settings; if not found in settings, defaults to
#'   \code{\link{get_default_db_dir}()}.
#' @param data_source Either \code{"local"} (default) for local folder scanning,
#'   or \code{"dashboard"} to fetch the sample list from a remote IFCB Dashboard.
#' @param dashboard_url When \code{data_source = "dashboard"}, the full Dashboard
#'   URL (e.g. \code{"https://habon-ifcb.whoi.edu/timeline?dataset=tangosund"}).
#' @param dashboard_autoclass Logical. When \code{TRUE} and
#'   \code{data_source = "dashboard"}, skip scanning the local
#'   \code{csv_folder} for classifier output files (the dashboard's own
#'   auto-classifications are used instead). Defaults to \code{FALSE}.
#' @param progress Optional callback function used to report scan progress.
#'   The callback receives named arguments \code{value} (numeric in [0, 1])
#'   and \code{detail} (character message). Mainly intended for Shiny
#'   progress bars.
#' @return Invisibly returns the file index list, or NULL if roi_folder is invalid.
#' @export
#' @examples
#' \dontrun{
#' # Rescan using saved settings
#' rescan_file_index()
#'
#' # Rescan with explicit paths
#' rescan_file_index(
#'   roi_folder = "/data/ifcb/raw",
#'   csv_folder = "/data/ifcb/classified",
#'   output_folder = "/data/ifcb/manual"
#' )
#'
#' # Scan from a remote Dashboard
#' rescan_file_index(data_source = "dashboard",
#'                   dashboard_url = "https://habon-ifcb.whoi.edu/timeline?dataset=tangosund")
#'
#' # Use in a cron job:
#' # Rscript -e 'ClassiPyR::rescan_file_index()'
#' }
rescan_file_index <- function(roi_folder = NULL, csv_folder = NULL,
                              output_folder = NULL, verbose = TRUE,
                              db_folder = NULL, data_source = "local",
                              dashboard_url = NULL,
                              dashboard_autoclass = FALSE,
                              progress = NULL) {
  report_progress <- function(value = NULL, detail = NULL) {
    if (is.function(progress)) {
      tryCatch(progress(value = value, detail = detail), error = function(e) NULL)
    }
  }

  report_progress(0, "Preparing scan...")
  # Read from saved settings if not provided
  if (is.null(roi_folder) || is.null(csv_folder) || is.null(output_folder) ||
      is.null(db_folder)) {
    settings_path <- get_settings_path()
    if (file.exists(settings_path)) {
      saved <- tryCatch(
        jsonlite::fromJSON(settings_path),
        error = function(e) list()
      )
      if (is.null(roi_folder)) roi_folder <- saved$roi_folder
      if (is.null(csv_folder)) csv_folder <- saved$csv_folder
      if (is.null(output_folder)) output_folder <- saved$output_folder
      if (is.null(db_folder)) db_folder <- saved$db_folder
    }
  }

  # Fall back to default db folder if still NULL
  if (is.null(db_folder)) {
    db_folder <- get_default_db_dir()
  }

  # Dashboard mode: fetch sample list from remote API
  if (identical(data_source, "dashboard")) {
    if (is.null(dashboard_url) || !nzchar(dashboard_url)) {
      if (verbose) message("Dashboard URL not set")
      return(invisible(NULL))
    }

    parsed <- parse_dashboard_url(dashboard_url)
    report_progress(0.15, "Querying dashboard bins...")
    if (verbose) message("Fetching bin list from: ", parsed$base_url)

    bins <- tryCatch(
      list_dashboard_bins(parsed$base_url, parsed$dataset_name),
      error = function(e) {
        if (verbose) message("Failed to list dashboard bins: ", e$message)
        character()
      }
    )

    sample_names <- as.character(bins)
    report_progress(0.65, paste0("Found ", length(sample_names), " samples"))
    if (verbose) message("  Found ", length(sample_names), " samples")

    if (length(sample_names) == 0) {
      if (verbose) message("No samples found on dashboard.")
      return(invisible(NULL))
    }

    # Check DB for existing annotations
    db_path <- get_db_path(db_folder)
    annotated_db <- list_annotated_samples_db(db_path)
    annotated <- annotated_db[annotated_db %in% sample_names]
    report_progress(0.75, "Merging annotation status...")

    # When not using dashboard auto-classifications, scan local csv_folder
    # for classifier output (CSV, H5, MAT) to populate classified status
    classified <- character()
    csv_map <- list()
    mat_file_map <- list()
    h5_file_map <- list()

    csv_valid <- !isTRUE(dashboard_autoclass) &&
      !is.null(csv_folder) && length(csv_folder) == 1 &&
      !isTRUE(is.na(csv_folder)) && nzchar(csv_folder) && dir.exists(csv_folder)

    if (csv_valid) {
      report_progress(0.78, "Scanning local classification files...")
      if (verbose) message("Scanning classification files in: ", csv_folder)

      scan <- scan_classification_files(csv_folder, sample_names)
      csv_map <- scan$csv_map
      mat_file_map <- scan$mat_map
      h5_file_map <- scan$h5_map
      classified <- scan$classified
      if (verbose) message("  Found ", length(classified), " classified samples")
    }
    report_progress(0.85, "Building index...")

    index_data <- list(
      data_source = "dashboard",
      dashboard_url = dashboard_url,
      dashboard_base_url = parsed$base_url,
      dashboard_dataset = parsed$dataset_name,
      csv_folder = csv_folder,
      db_folder = db_folder,
      dashboard_autoclass = dashboard_autoclass,
      sample_names = sample_names,
      classified_samples = classified,
      annotated_samples = annotated,
      roi_path_map = list(),
      csv_path_map = csv_map,
      classifier_mat_files = mat_file_map,
      classifier_h5_files = h5_file_map,
      timestamp = as.character(Sys.time())
    )

    save_file_index(index_data)
    report_progress(1, "Dashboard sync complete")
    if (verbose) message("File index saved to: ", get_file_index_path())

    return(invisible(index_data))
  }

  # Validate source folder (ROI and/or extracted PNG tree)
  roi_valid <- !is.null(roi_folder) && length(roi_folder) == 1 &&
    !isTRUE(is.na(roi_folder)) && nzchar(roi_folder) && dir.exists(roi_folder)
  csv_valid <- !is.null(csv_folder) && length(csv_folder) == 1 &&
    !isTRUE(is.na(csv_folder)) && nzchar(csv_folder) && dir.exists(csv_folder)
  output_valid <- !is.null(output_folder) && length(output_folder) == 1 &&
    !isTRUE(is.na(output_folder)) && nzchar(output_folder) && dir.exists(output_folder)

  if (!roi_valid) {
    if (verbose) message("ROI/PNG source folder not set or does not exist: ", roi_folder)
    return(invisible(NULL))
  }
  report_progress(0.08, "Scanning ROI files...")

  # Scan ROI files
  if (verbose) message("Scanning ROI files in: ", roi_folder)
  roi_files <- list.files(roi_folder, pattern = "\\.roi$",
                          recursive = TRUE, full.names = TRUE)
  sample_names_raw <- tools::file_path_sans_ext(basename(roi_files))

  # Build ROI path map (handle duplicates: keep first occurrence)
  roi_map <- first_path_by_name(sample_names_raw, roi_files)
  sample_names <- unique(sample_names_raw)
  report_progress(0.22, paste0("Found ", length(sample_names), " ROI samples"))

  # Scan extracted PNG sample folders by directory name.
  # This avoids recursively listing every PNG file (can be millions).
  if (verbose) message("Scanning extracted PNG files in: ", roi_folder)
  png_dirs <- list.dirs(roi_folder, recursive = TRUE, full.names = TRUE)
  # Only folders named like a sample can hold that sample's PNGs
  png_dirs <- png_dirs[grepl(VALID_SAMPLE_NAME_PATTERN, basename(png_dirs))]
  n_png_dirs <- length(png_dirs)
  has_png <- logical(n_png_dirs)

  for (i in seq_len(n_png_dirs)) {
    sn <- basename(png_dirs[[i]])

    # Keep directory only when it actually contains sample PNGs
    has_png[i] <- length(list.files(
      png_dirs[[i]],
      pattern = paste0("^", sn, "_\\d+\\.png$"),
      recursive = FALSE,
      full.names = FALSE
    )) > 0

    # Progress through deep directory trees without listing every PNG globally.
    if (i %% 500 == 0 || i == n_png_dirs) {
      report_progress(0.22 + 0.28 * i / n_png_dirs,
                      paste0("Checking PNG sample folders (", i, "/", n_png_dirs, ")"))
    }
  }

  png_samples <- basename(png_dirs)[has_png]
  png_map <- first_path_by_name(png_samples, png_dirs[has_png])

  png_samples <- unique(png_samples)
  sample_names <- unique(c(sample_names, png_samples))
  if (verbose) {
    message("  Found ", length(unique(sample_names_raw)), " samples from ROI")
    message("  Found ", length(png_samples), " samples from PNG folders")
    message("  Found ", length(sample_names), " total samples")
  }
  report_progress(0.52, paste0("Found ", length(sample_names), " total samples"))

  if (length(sample_names) == 0) {
    if (verbose) message("No samples found in ROI or PNG folders.")
    return(invisible(NULL))
  }

  # Scan classification files
  classified <- character()
  mat_file_map <- list()
  h5_file_map <- list()
  csv_map <- list()

  if (csv_valid) {
    report_progress(0.56, "Scanning classification files...")
    if (verbose) message("Scanning classification files in: ", csv_folder)

    scan <- scan_classification_files(csv_folder, sample_names)
    csv_map <- scan$csv_map
    mat_file_map <- scan$mat_map
    h5_file_map <- scan$h5_map
    classified <- scan$classified
    if (verbose) message("  Found ", length(classified), " classified samples")
  }
  report_progress(0.78, "Classification scan complete")

  # Scan for manual annotations: .mat files in the output folder and the
  # SQLite database. The database is read even when the output folder is
  # missing, since SQLite storage does not need an output folder at all.
  report_progress(0.82, "Scanning annotation outputs...")
  if (verbose && output_valid) message("Scanning output folder: ", output_folder)
  annotated <- scan_annotated_samples(
    if (output_valid) output_folder else NULL, db_folder, sample_names
  )
  if (verbose) message("  Found ", length(annotated), " annotated samples")
  report_progress(0.92, "Saving file index cache...")

  # Save to cache
  index_data <- list(
    roi_folder = roi_folder,
    csv_folder = csv_folder,
    output_folder = output_folder,
    db_folder = db_folder,
    sample_names = sample_names,
    classified_samples = classified,
    annotated_samples = annotated,
    roi_path_map = roi_map,
    png_sample_path_map = png_map,
    csv_path_map = csv_map,
    classifier_mat_files = mat_file_map,
    classifier_h5_files = h5_file_map,
    timestamp = as.character(Sys.time())
  )

  save_file_index(index_data)
  report_progress(1, "Sync complete")
  if (verbose) message("File index saved to: ", get_file_index_path())

  invisible(index_data)
}

# Named list mapping each name to one of its paths
#
# Vectorised replacement for growing a list one `[[name]] <-` at a time, which
# is quadratic in the number of samples and dominated the folder scan for
# large archives.
#
# @param names Character vector of names (e.g. sample names).
# @param paths Character vector of paths, parallel to `names`.
# @param keep "first" or "last": which path wins when a name occurs twice.
# @return Named list of single paths.
# @noRd
first_path_by_name <- function(names, paths, keep = c("first", "last")) {
  keep <- match.arg(keep)
  unique_rows <- !duplicated(names, fromLast = identical(keep, "last"))
  as.list(stats::setNames(paths[unique_rows], names[unique_rows]))
}

# Map samples to their classification CSV file
#
# A file is named `{sample}.csv`, or, as iRfcb also accepts,
# `{sample}_class.csv` or `{sample}_class_vN.csv`. `{sample}.csv` wins when a
# sample has both; of several `_class` files the last one by name is kept, as
# for `.mat` and `.h5` classifier output. The content is not read here:
# `load_from_csv()` tells a label file from a class score matrix.
#
# @param csv_files Paths of the `.csv` files found.
# @param sample_names Samples to keep files for.
# @return Named list of single paths.
# @noRd
map_classification_csv <- function(csv_files, sample_names) {
  file_names <- basename(csv_files)

  plain_samples <- sub("\\.csv$", "", file_names)
  is_plain <- plain_samples %in% sample_names
  plain_map <- first_path_by_name(plain_samples[is_plain], csv_files[is_plain])

  class_samples <- sub(CLASS_CSV_SUFFIX, "", file_names)
  is_class <- grepl(CLASS_CSV_SUFFIX, file_names) &
    class_samples %in% sample_names & !class_samples %in% names(plain_map)
  class_map <- first_path_by_name(class_samples[is_class], csv_files[is_class],
                                  keep = "last")

  c(plain_map, class_map)
}

# The `_class` (and optional version) ending of a classification CSV file,
# the same rule iRfcb uses to resolve `{sample}_class.csv` to its sample
CLASS_CSV_SUFFIX <- "_class(_v\\d+)?\\.csv$"

# Find the classification files of a set of samples
#
# @param csv_folder Classification folder (searched recursively).
# @param sample_names Samples to keep files for.
# @return List with `csv_map`, `mat_map`, `h5_map` (named lists of paths) and
#   `classified` (samples with at least one classification file).
# @noRd
scan_classification_files <- function(csv_folder, sample_names) {
  csv_files <- list.files(csv_folder, pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)
  csv_map <- map_classification_csv(csv_files, sample_names)

  # A sample with several classifier versions keeps the last one by name
  mat_files <- list.files(csv_folder, pattern = "_class.*\\.mat$",
                          recursive = TRUE, full.names = TRUE)
  mat_samples <- sub("_class.*\\.mat$", "", basename(mat_files))
  mat_keep <- mat_samples %in% sample_names
  mat_map <- first_path_by_name(mat_samples[mat_keep], mat_files[mat_keep],
                                keep = "last")

  h5_files <- list.files(csv_folder, pattern = "_class.*\\.h5$",
                         recursive = TRUE, full.names = TRUE)
  h5_samples <- sub("_class.*\\.h5$", "", basename(h5_files))
  h5_keep <- h5_samples %in% sample_names
  h5_map <- first_path_by_name(h5_samples[h5_keep], h5_files[h5_keep],
                               keep = "last")

  list(
    csv_map = csv_map,
    mat_map = mat_map,
    h5_map = h5_map,
    classified = unique(c(names(csv_map), names(h5_map), names(mat_map)))
  )
}

# Samples that have manual annotations
#
# @param output_folder Folder with manual `.mat` files, or NULL to skip it.
# @param db_folder Folder holding the annotations database, or NULL to skip it.
# @param sample_names Known samples; others are dropped from the result.
# @return Character vector of annotated sample names.
# @noRd
scan_annotated_samples <- function(output_folder, db_folder, sample_names) {
  annotated_mat <- character()
  if (!is.null(output_folder) && dir.exists(output_folder)) {
    mat_files <- list.files(output_folder, pattern = "\\.mat$",
                            full.names = FALSE)
    mat_files <- mat_files[!grepl("_class", mat_files)]
    annotated_mat <- tools::file_path_sans_ext(mat_files)
  }

  annotated_db <- character()
  if (!is.null(db_folder)) {
    annotated_db <- tryCatch(
      list_annotated_samples_db(get_db_path(db_folder)),
      error = function(e) character()
    )
  }

  annotated <- unique(c(annotated_mat, annotated_db))
  annotated[annotated %in% sample_names]
}

# Constants
VALID_SAMPLE_NAME_PATTERN <- "^D\\d{8}T\\d{6}_IFCB\\d+$"

# Characters unsafe for class names (used in folder names and HTML display):
# - / and \ are path separators -> replaced with "_" to preserve meaning
#   (common in taxonomy for ambiguous IDs: "Snowella/Woronichinia" -> "Snowella_Woronichinia")
# - < > " ' & are HTML/XSS risks -> removed
# - : * ? | are Windows filesystem unsafe -> removed
# - .. is path traversal risk -> removed
PATH_SEPARATOR_CHARS <- "[/\\\\]"
UNSAFE_CLASS_CHARS <- "[<>\"'&:*?|]"

#' Validate IFCB sample name format
#'
#' Checks if a sample name matches the expected IFCB naming convention:
#' DYYYYMMDDTHHMMSS_IFCBNNN (e.g., D20230101T120000_IFCB134).
#'
#' @param sample_name Sample name to validate
#' @return TRUE if valid, FALSE otherwise
#' @export
#' @examples
#' # Valid sample names
#' is_valid_sample_name("D20230101T120000_IFCB134")
#' is_valid_sample_name("D20220522T000439_IFCB1")
#'
#' # Invalid sample names
#' is_valid_sample_name("invalid_name")
#' is_valid_sample_name("20230101T120000_IFCB134")  # Missing 'D' prefix
#' is_valid_sample_name(NULL)
is_valid_sample_name <- function(sample_name) {
  if (is.null(sample_name) || length(sample_name) != 1 || !is.character(sample_name)) {
    return(FALSE)
  }
  grepl(VALID_SAMPLE_NAME_PATTERN, sample_name)
}

#' Sanitize string for safe use in HTML/file paths
#'
#' Removes or replaces characters that could be dangerous in HTML contexts
#' or file paths, including XSS attack vectors and path traversal attempts.
#'
#' @param x String to sanitize
#' @return Sanitized string
#' @export
#' @examples
#' # Remove HTML special characters
#' sanitize_string("<script>alert('xss')</script>")
#'
#' # Remove path traversal attempts
#' sanitize_string("../../../etc/passwd")
#'
#' # Normal strings pass through
#' sanitize_string("Diatom_chain")
sanitize_string <- function(x) {
  # Remove or replace potentially dangerous characters
  x <- gsub("[<>\"'&]", "", x)
  # Prevent path traversal
  x <- gsub("\\.\\.", "", x)
  x <- gsub("[/\\\\]", "", x)
  x
}

#' Load class list from MAT or TXT file
#'
#' Reads a class list from either a MATLAB .mat file (class2use format)
#' or a plain text file with one class per line. Class names are sanitized
#' for safe use in file paths and HTML.
#'
#' @param file_path Path to class2use file (.mat or .txt)
#' @return Character vector of class names
#' @export
#' @examples
#' \dontrun{
#' # Load from MATLAB file
#' classes <- load_class_list("/path/to/class2use.mat")
#'
#' # Load from text file
#' classes <- load_class_list("/path/to/class2use.txt")
#' }
#'
#' # Create a temporary text file for demonstration
#' tmp_file <- tempfile(fileext = ".txt")
#' writeLines(c("Diatom", "Ciliate", "Dinoflagellate"), tmp_file)
#' classes <- load_class_list(tmp_file)
#' print(classes)
#' unlink(tmp_file)
load_class_list <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  if (ext == "mat") {
    # Load from MAT file
    classes <- ifcb_get_mat_variable(file_path)
  } else if (ext == "txt") {
    # Load from text file (one class per line)
    classes <- readLines(file_path, warn = FALSE)
    classes <- trimws(classes)
    classes <- classes[classes != ""]
  } else {
    stop("Unsupported file format. Use .mat or .txt")
  }


  sanitize_class_names(classes)
}

# Make class names safe for use as folder names and in HTML
#
# Path separators become underscores (so "Snowella/Woronichinia" keeps its
# meaning), other unsafe characters are removed. Used for class lists read
# from a file and for names typed into the class list editor, so both end up
# with the same folder names in the PNG output.
#
# @param classes Character vector of class names.
# @return Sanitized character vector, same length.
# @noRd
sanitize_class_names <- function(classes) {
  if (length(classes) == 0) {
    return(classes)
  }

  has_path_sep <- grepl(PATH_SEPARATOR_CHARS, classes)
  has_unsafe <- grepl(UNSAFE_CLASS_CHARS, classes) | grepl("\\.\\.", classes)

  if (any(has_path_sep)) {
    message("Note: Forward slashes in class names replaced with underscores for filesystem compatibility")
    classes <- gsub(PATH_SEPARATOR_CHARS, "_", classes)
  }

  if (any(has_unsafe)) {
    warning("Some class names contain unsafe characters (< > \" ' & : * ? | ..) and were sanitized")
    classes <- gsub(UNSAFE_CLASS_CHARS, "", classes)
    classes <- gsub("\\.\\.", "", classes)
  }

  classes
}

#' Get sample paths from sample name
#'
#' Constructs file paths to IFCB data files (ROI, ADC) based on the standard
#' IFCB folder structure: roi_folder/YYYY/DYYYYMMDD/sample_name.ext
#'
#' @param sample_name Sample name (e.g., "D20220522T000439_IFCB134")
#' @param roi_folder Base ROI folder path
#' @return List with components: year, date_part, roi_path, adc_path, adc_folder
#' @export
#' @examples
#' # Get paths for a sample
#' paths <- get_sample_paths("D20230101T120000_IFCB134", "/data/ifcb/raw")
#' print(paths$year)       # "2023"
#' print(paths$date_part)  # "D20230101"
#' print(paths$roi_path)   # "/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.roi"
get_sample_paths <- function(sample_name, roi_folder) {
  # Validate sample name format to prevent path traversal

  if (!is_valid_sample_name(sample_name)) {
    stop("Invalid sample name format. Expected pattern: DYYYYMMDDTHHMMSS_IFCBNNN")
  }

  year <- substr(sample_name, 2, 5)
  date_part <- substr(sample_name, 1, 9)

  list(
    year = year,
    date_part = date_part,
    roi_path = file.path(roi_folder, year, date_part, paste0(sample_name, ".roi")),
    adc_path = file.path(roi_folder, year, date_part, paste0(sample_name, ".adc")),
    adc_folder = file.path(roi_folder, year, date_part)
  )
}

#' Read ROI dimensions from ADC file
#'
#' Reads an IFCB ADC file and extracts ROI dimensions (width, height, area)
#' for each ROI in the sample.
#'
#' @param adc_path Path to ADC file
#' @return Data frame with columns: roi_number, width, height, area
#' @export
#' @examples
#' \dontrun{
#' # Read dimensions from an ADC file
#' dims <- read_roi_dimensions("/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
#' head(dims)
#' }
read_roi_dimensions <- function(adc_path) {
  tryCatch({
    if (!file.exists(adc_path)) {
      stop("ADC file not found: ", adc_path)
    }
    
    if (file.info(adc_path)$size == 0) {
      return(data.frame(
        roi_number = integer(),
        width = numeric(),
        height = numeric(),
        area = numeric()
      ))
    }

    adc_data <- utils::read.csv(adc_path, header = FALSE)

    n_rois <- nrow(adc_data)

    # IFCB ADC columns: V14=ROIx, V15=ROIy, V16=ROIwidth, V17=ROIheight
    if (ncol(adc_data) >= 17) {
      roi_width <- adc_data$V16
      roi_height <- adc_data$V17
    } else {
      warning("ADC file has fewer than 17 columns, using default dimensions")
      roi_width <- rep(1, n_rois)
      roi_height <- rep(1, n_rois)
    }

    data.frame(
      roi_number = seq_len(n_rois),
      width = roi_width,
      height = roi_height,
      area = roi_width * roi_height
    )
  }, error = function(e) {
    stop("Failed to read ADC file: ", e$message)
  })
}

#' Read PNG dimensions from image header
#'
#' Reads width and height from the PNG IHDR chunk without decoding full image
#' data. Returns `NA` values when the file is missing or not a valid PNG.
#'
#' @param png_path Path to PNG file
#' @return Named list with `width` and `height` (numeric)
#' @keywords internal
read_png_dimensions <- function(png_path) {
  if (!file.exists(png_path)) {
    return(list(width = NA_real_, height = NA_real_))
  }

  con <- file(png_path, "rb")
  on.exit(close(con), add = TRUE)

  sig <- readBin(con, what = "raw", n = 8)
  png_sig <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  if (!identical(sig, png_sig)) {
    return(list(width = NA_real_, height = NA_real_))
  }

  raw_to_uint32 <- function(x) {
    if (length(x) != 4) return(NA_real_)
    b <- as.numeric(x)
    (((b[1] * 256 + b[2]) * 256 + b[3]) * 256 + b[4])
  }

  ihdr_len_raw <- readBin(con, what = "raw", n = 4)
  ihdr_len <- raw_to_uint32(ihdr_len_raw)
  ihdr_type_raw <- readBin(con, what = "raw", n = 4)
  ihdr_type <- rawToChar(ihdr_type_raw)

  if (is.na(ihdr_len) || ihdr_len < 8 || !identical(ihdr_type, "IHDR")) {
    return(list(width = NA_real_, height = NA_real_))
  }

  width <- raw_to_uint32(readBin(con, what = "raw", n = 4))
  height <- raw_to_uint32(readBin(con, what = "raw", n = 4))

  list(width = width, height = height)
}

# Look up ROI dimensions for a vector of ROI numbers
#
# @param roi_numbers ROI numbers to look up.
# @param roi_dimensions Data frame from read_roi_dimensions(), or NULL.
# @return Data frame with `width`, `height` and `roi_area`, one row per ROI
#   number; NA where the ROI is not in `roi_dimensions`.
# @noRd
match_roi_dims <- function(roi_numbers, roi_dimensions) {
  idx <- if (is.null(roi_dimensions)) {
    rep(NA_integer_, length(roi_numbers))
  } else {
    match(roi_numbers, roi_dimensions$roi_number)
  }
  pick <- function(column) {
    if (is.null(column)) rep(NA_real_, length(idx)) else as.numeric(column[idx])
  }
  data.frame(
    width = pick(roi_dimensions$width),
    height = pick(roi_dimensions$height),
    roi_area = pick(roi_dimensions$area)
  )
}

# ROI number encoded in an IFCB PNG file name
#
# @param file_name Character vector of PNG file names.
# @return Integer vector of ROI numbers (NA when the name has none).
# @noRd
roi_number_from_file_name <- function(file_name) {
  has_roi <- grepl("_\\d+\\.png$", file_name)
  out <- rep(NA_integer_, length(file_name))
  out[has_roi] <- as.integer(sub(".*_(\\d+)\\.png$", "\\1", file_name[has_roi]))
  out
}

# ROI dimensions of a sample that only exists as extracted PNG files
#
# @param sample_name Sample name.
# @param sample_png_dir Folder holding the sample's PNG files.
# @return Data frame like read_roi_dimensions(), or NULL when the folder has
#   no PNG files of the sample.
# @noRd
infer_roi_dims_from_png <- function(sample_name, sample_png_dir) {
  if (is.null(sample_png_dir) || !dir.exists(sample_png_dir)) {
    return(NULL)
  }
  png_files <- list.files(sample_png_dir,
                          pattern = paste0("^", sample_name, "_\\d+\\.png$"),
                          full.names = FALSE)
  roi_number <- roi_number_from_file_name(png_files)
  keep <- !is.na(roi_number) & !duplicated(roi_number)
  if (!any(keep)) {
    return(NULL)
  }
  png_files <- png_files[keep]
  roi_number <- roi_number[keep]

  dims <- lapply(file.path(sample_png_dir, png_files), read_png_dimensions)
  width <- vapply(dims, `[[`, numeric(1), "width")
  height <- vapply(dims, `[[`, numeric(1), "height")

  out <- data.frame(roi_number = roi_number, width = width, height = height,
                    area = width * height)
  out <- out[order(out$roi_number), ]
  rownames(out) <- NULL
  out
}

#' Create empty changes log data frame
#'
#' Creates an empty data frame with the correct structure for tracking
#' annotation changes during a session.
#'
#' @return Empty data frame with columns: image, original_class, new_class
#' @export
#' @examples
#' # Create an empty changes log
#' changes <- create_empty_changes_log()
#' print(names(changes))
#' print(nrow(changes))
create_empty_changes_log <- function() {
  data.frame(
    image = character(),
    original_class = character(),
    new_class = character(),
    stringsAsFactors = FALSE
  )
}

#' Initialize Python environment for iRfcb
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `init_python_env()` was deprecated in ClassiPyR 0.3.0 and is now a no-op.
#' As of iRfcb 0.10.0, MATLAB .mat files are read and written with a native R
#' implementation, so ClassiPyR no longer requires Python.
#'
#' If you need a Python environment for other iRfcb features (e.g. feature
#' extraction), set one up with [iRfcb::ifcb_py_install()].
#'
#' @param venv_path Ignored.
#' @return FALSE, invisibly
#' @export
#' @keywords internal
#' @md
init_python_env <- function(venv_path = NULL) {
  lifecycle::deprecate_warn(
    "0.3.0", "init_python_env()",
    details = c(
      "ClassiPyR no longer requires Python: iRfcb >= 0.10.0 reads and writes .mat files natively in R.",
      "For other iRfcb features that use Python, see `iRfcb::ifcb_py_install()`."
    )
  )
  invisible(FALSE)
}
