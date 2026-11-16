# Sample loading functions for ClassiPyR

#' @importFrom iRfcb ifcb_get_mat_variable
NULL

#' Load classifications from CSV file (validation mode)
#'
#' Reads a classification CSV file and returns a data frame with classifications.
#' Class names are processed to truncate trailing numbers (matching iRfcb behavior).
#'
#' The CSV file must contain the following columns:
#' \describe{
#'   \item{file_name}{Image filename including the `.png` extension
#'     (e.g., `D20230101T120000_IFCB134_00001.png`).}
#'   \item{class_name}{Predicted class name (e.g., `Diatom`).}
#' }
#'
#' Optional columns may also be included:
#' \describe{
#'   \item{score}{Classification confidence value between 0 and 1.}
#'   \item{class_name_auto}{Raw (unthresholded) class prediction. When
#'     \code{use_threshold = FALSE} and this column exists, its values are
#'     used as \code{class_name}.}
#' }
#'
#' The CSV file must be named after the sample it describes
#' (e.g., `D20230101T120000_IFCB134.csv`) and placed inside the Classification
#' Folder configured in the app (subfolders are searched recursively). As in
#' iRfcb, `D20230101T120000_IFCB134_class.csv` and a versioned
#' `D20230101T120000_IFCB134_class_v1.csv` are accepted too; a file without the
#' `_class` ending is preferred when a sample has both.
#'
#' A class score matrix, such as the `class_scores` export of an IFCB
#' Dashboard (a ROI identifier column followed by one score column per class),
#' is also read: each ROI gets the class with the highest score. Such a file
#' has no thresholded class, so \code{use_threshold} has no effect on it. Any
#' other CSV file is an error.
#'
#' @param csv_path Path to classification CSV file
#' @param use_threshold Logical, whether to use the threshold-filtered
#'   \code{class_name} column (default \code{TRUE}) or the raw
#'   \code{class_name_auto} column when available.
#' @return Data frame with classifications. Expected columns: `file_name`,
#'   `class_name`, and optionally `score`.
#' @export
#' @examples
#' \dontrun{
#' # Load classifications from a CSV file
#' classifications <- load_from_csv("/path/to/D20230101T120000_IFCB134.csv")
#' head(classifications)
#' }
load_from_csv <- function(csv_path, use_threshold = TRUE) {
  classifications <- read_classification_csv(csv_path)

  # When threshold is off and class_name_auto exists, use raw predictions

  if (!use_threshold && "class_name_auto" %in% names(classifications)) {
    classifications$class_name <- classifications$class_name_auto
  }

  # Strip trailing 3-digit suffix from class names (e.g., "Diatom_001" -> "Diatom")
  # This matches iRfcb behavior where class folders may include numeric suffixes
  classifications$class_name <- sub("_\\d{3}$", "", classifications$class_name)

  # Add placeholder dimensions if not present (will be populated later if needed)
  if (!"width" %in% names(classifications)) {
    classifications$width <- NA_real_
  }
  if (!"height" %in% names(classifications)) {
    classifications$height <- NA_real_
  }
  if (!"roi_area" %in% names(classifications)) {
    classifications$roi_area <- NA_real_
  }
  if (!"score" %in% names(classifications)) {
    classifications$score <- NA_real_
  }

  classifications
}

# Read a classification CSV: a label file or a class score matrix
#
# @param csv_path Path to the CSV file.
# @return Data frame with at least `file_name` and `class_name`. Stops with a
#   message naming the file when it is neither kind of classification file.
# @noRd
read_classification_csv <- function(csv_path) {
  columns <- tryCatch(
    names(utils::read.csv(csv_path, nrows = 1, check.names = FALSE)),
    error = function(e) character()
  )
  label_columns <- c("file_name", "class_name")
  if (all(label_columns %in% columns)) {
    return(utils::read.csv(csv_path, stringsAsFactors = FALSE))
  }

  scores <- if (length(columns) >= 2 && !any(label_columns %in% columns)) {
    utils::read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (is_class_score_matrix(scores)) {
    sample_name <- sub("(_class(_v\\d+)?)?\\.csv$", "", basename(csv_path))
    return(winning_class_from_scores(scores, sample_name))
  }

  stop(
    basename(csv_path), " is not a classification file. It needs the columns ",
    "file_name and class_name (missing: ",
    paste(setdiff(label_columns, columns), collapse = ", "),
    "), or a ROI identifier column followed by one score column per class.",
    call. = FALSE
  )
}

# TRUE for a data frame that looks like a class score matrix: a ROI
# identifier column, then only numeric columns
#
# @param scores Data frame, or NULL.
# @noRd
is_class_score_matrix <- function(scores) {
  if (is.null(scores) || nrow(scores) == 0 || ncol(scores) < 2) {
    return(FALSE)
  }
  is_score <- vapply(scores[-1], function(column) {
    is.numeric(column) || all(is.na(column))
  }, logical(1))
  is_roi_id_column(scores[[1]]) && all(is_score)
}

#' Load classifications from HDF5 classifier output file
#'
#' Reads an HDF5 classifier output file (from iRfcb 0.8.0+) and extracts
#' class predictions. Requires the \pkg{hdf5r} package.
#'
#' @param h5_path Path to classifier H5 file (matching pattern *_class*.h5)
#' @param sample_name Sample name (e.g., "D20220522T000439_IFCB134")
#' @param roi_dimensions Data frame from \code{\link{read_roi_dimensions}}
#' @param use_threshold Logical, whether to use the threshold-filtered
#'   \code{class_name} dataset (default \code{TRUE}) or the raw
#'   \code{class_name_auto} dataset.
#' @return Data frame with columns: file_name, class_name, score, width, height,
#'   roi_area
#' @export
#' @examples
#' \dontrun{
#' dims <- read_roi_dimensions("/data/raw/2022/D20220522/D20220522T000439_IFCB134.adc")
#' classifications <- load_from_h5(
#'   h5_path = "/data/classified/D20220522T000439_IFCB134_class.h5",
#'   sample_name = "D20220522T000439_IFCB134",
#'   roi_dimensions = dims,
#'   use_threshold = TRUE
#' )
#' head(classifications)
#' }
load_from_h5 <- function(h5_path, sample_name, roi_dimensions, use_threshold = TRUE) {
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to read H5 classification files. ",
         "Install it with: install.packages('hdf5r')")
  }

  h5 <- hdf5r::H5File$new(h5_path, "r")
  # Every dataset is closed as soon as it is read, so close() releases the
  # file. close_all() starts with a full garbage collection, which is most of
  # the time spent reading a file; it stays as the fallback for anything
  # still open.
  on.exit(
    if (length(h5$get_obj_ids()) > 1) h5$close_all() else h5$close(),
    add = TRUE
  )
  read_dataset <- function(name, ...) {
    dataset <- h5[[name]]
    on.exit(dataset$close(), add = TRUE)
    dataset$read(...)
  }

  roi_numbers <- as.vector(read_dataset("roi_numbers"))

  class_names <- as.character(as.vector(read_dataset(
    if (use_threshold) "class_name" else "class_name_auto"
  )))
  class_names[is.na(class_names)] <- "unclassified"

  # Per-ROI max score from the output_scores matrix (num_classes x num_rois).
  # drop = FALSE: hdf5r otherwise returns the scores of a single-ROI (or
  # single-class) file as a plain vector, which has no columns to reduce.
  output_scores <- read_dataset("output_scores", drop = FALSE)
  if (length(dim(output_scores)) != 2) {
    output_scores <- matrix(output_scores, ncol = length(roi_numbers))
  }
  scores <- if (ncol(output_scores) > 0 && nrow(output_scores) > 0) {
    apply(output_scores, 2, max)
  } else {
    rep(NA_real_, length(roi_numbers))
  }

  dims <- match_roi_dims(roi_numbers, roi_dimensions)

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
    class_name = class_names,
    score = scores,
    width = dims$width,
    height = dims$height,
    roi_area = dims$roi_area,
    stringsAsFactors = FALSE
  )

  # Sort by area (descending)
  classifications[order(-classifications$roi_area), ]
}

#' Load classifications from SQLite database
#'
#' Reads annotations for a sample from the SQLite database and returns a data
#' frame in the same format as \code{\link{load_from_mat}}.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param roi_dimensions Data frame from \code{\link{read_roi_dimensions}}
#' @return Data frame with columns: file_name, class_name, score, width, height,
#'   roi_area. Returns NULL if the sample has no annotations in the database.
#' @export
#' @examples
#' \dontrun{
#' dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
#' db_path <- get_db_path("/data/manual")
#' classifications <- load_from_db(db_path, "D20230101T120000_IFCB134", dims)
#' }
load_from_db <- function(db_path, sample_name, roi_dimensions) {
  load_annotations_db(db_path, sample_name, roi_dimensions)
}

#' Load classifications from existing MAT annotation file
#'
#' Reads a MATLAB annotation file (created by ClassiPyR or ifcb-analysis)
#' and converts class indices to class names using the provided class list.
#'
#' @param mat_path Path to MAT file
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param class2use Character vector of class names (from class2use file)
#' @param roi_dimensions Data frame from \code{\link{read_roi_dimensions}}
#' @return Data frame with columns: file_name, class_name, score, width, height, roi_area
#' @export
#' @examples
#' \dontrun{
#' # Load existing annotations
#' dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
#' class2use <- load_class_list("/data/class2use.mat")
#' classifications <- load_from_mat(
#'   mat_path = "/data/manual/D20230101T120000_IFCB134.mat",
#'   sample_name = "D20230101T120000_IFCB134",
#'   class2use = class2use,
#'   roi_dimensions = dims
#' )
#' head(classifications)
#' }
load_from_mat <- function(mat_path, sample_name, class2use, roi_dimensions) {
  # Read classlist from MAT file (column 2 contains class indices)
  classlist <- ifcb_get_mat_variable(mat_path, variable_name = "classlist")

  # Prefer the class list embedded in the .mat file for accurate index mapping
  mat_class2use <- tryCatch(
    as.character(ifcb_get_mat_variable(mat_path,
                                       variable_name = "class2use_manual")),
    error = function(e) NULL
  )
  if (!is.null(mat_class2use) && length(mat_class2use) > 0) {
    class2use <- mat_class2use
  }

  # Map class indices to class names
  roi_numbers <- classlist[, 1]
  class_indices <- classlist[, 2]

  # Get class names from indices (handle NaN, 0, or NA as "unclassified")
  class_names <- class_names_from_indices(class_indices, class2use)

  dims <- match_roi_dims(roi_numbers, roi_dimensions)

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
    class_name = class_names,
    score = NA_real_,
    width = dims$width,
    height = dims$height,
    roi_area = dims$roi_area,
    stringsAsFactors = FALSE
  )

  # Sort by area (descending)
  classifications[order(-classifications$roi_area), ]
}

#' Load classifications from MATLAB classifier output file
#'
#' Reads a MATLAB classifier output file (from ifcb-analysis random forest
#' classifier) and extracts class predictions.
#'
#' @param mat_path Path to classifier MAT file (matching pattern *_class*.mat)
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param class2use Character vector of class names (unused, kept for API consistency)
#' @param roi_dimensions Data frame from \code{\link{read_roi_dimensions}}
#' @param use_threshold Logical, whether to use threshold-based classification
#'   (TBclass_above_threshold) or raw predictions (TBclass)
#' @return Data frame with columns: file_name, class_name, score, width, height, roi_area
#' @export
#' @examples
#' \dontrun{
#' # Load classifier predictions
#' dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
#' classifications <- load_from_classifier_mat(
#'   mat_path = "/data/classified/D20230101T120000_IFCB134_class_v1.mat",
#'   sample_name = "D20230101T120000_IFCB134",
#'   class2use = NULL,
#'   roi_dimensions = dims,
#'   use_threshold = TRUE
#' )
#' head(classifications)
#' }
load_from_classifier_mat <- function(mat_path, sample_name, class2use, roi_dimensions, use_threshold = TRUE) {
  # Read ROI numbers
  roi_numbers <- as.vector(ifcb_get_mat_variable(mat_path, variable_name = "roinum"))

  # Read class names (already as strings from ifcb_get_mat_variable)
  if (use_threshold) {
    class_names <- as.vector(ifcb_get_mat_variable(mat_path, variable_name = "TBclass_above_threshold"))
  } else {
    class_names <- as.vector(ifcb_get_mat_variable(mat_path, variable_name = "TBclass"))
  }

  # Handle any NA values
  class_names[is.na(class_names)] <- "unclassified"

  dims <- match_roi_dims(roi_numbers, roi_dimensions)

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
    class_name = class_names,
    score = NA_real_,
    width = dims$width,
    height = dims$height,
    roi_area = dims$roi_area,
    stringsAsFactors = FALSE
  )

  # Sort by area (descending)
  classifications[order(-classifications$roi_area), ]
}

#' Create new classifications for annotation mode
#'
#' Creates a classifications data frame with all ROIs set to "unclassified",
#' for use when annotating a sample from scratch.
#'
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param roi_dimensions Data frame from \code{\link{read_roi_dimensions}}
#' @return Data frame with columns: file_name, class_name, score, width, height, roi_area
#' @export
#' @examples
#' # Create mock ROI dimensions
#' roi_dims <- data.frame(
#'   roi_number = 1:5,
#'   width = c(100, 150, 80, 200, 120),
#'   height = c(80, 100, 60, 150, 90),
#'   area = c(8000, 15000, 4800, 30000, 10800)
#' )
#'
#' # Create new classifications (all unclassified)
#' classifications <- create_new_classifications(
#'   sample_name = "D20230101T120000_IFCB134",
#'   roi_dimensions = roi_dims
#' )
#' print(classifications)
create_new_classifications <- function(sample_name, roi_dimensions) {
  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, roi_dimensions$roi_number),
    class_name = "unclassified",
    score = NA_real_,
    width = roi_dimensions$width,
    height = roi_dimensions$height,
    roi_area = roi_dimensions$area,
    stringsAsFactors = FALSE
  )

  # Sort by area (descending) so larger/similar organisms group together
  classifications[order(-classifications$roi_area), ]
}

#' Scan a PNG folder with class subfolders
#'
#' Scans a directory containing PNG images organized into class-name
#' subfolders (e.g. as exported by \code{\link{export_db_to_png}} or other
#' tools). Folder names follow the iRfcb convention where a trailing 3-digit
#' suffix is stripped (e.g. \code{Diatom_001} becomes \code{Diatom}).
#'
#' @param png_folder Path to the top-level folder containing class subfolders
#' @return A list with components:
#'   \describe{
#'     \item{annotations}{Data frame with columns \code{sample_name},
#'       \code{roi_number}, \code{file_name}, and \code{class_name}}
#'     \item{classes_found}{Character vector of unique class names found}
#'     \item{sample_names}{Character vector of unique sample names found}
#'   }
#' @export
#' @examples
#' \dontrun{
#' result <- scan_png_class_folder("/data/png_export")
#' head(result$annotations)
#' result$classes_found
#' result$sample_names
#' }
scan_png_class_folder <- function(png_folder) {
  if (!dir.exists(png_folder)) {
    stop("PNG folder does not exist: ", png_folder)
  }

  subdirs <- list.dirs(png_folder, recursive = FALSE, full.names = TRUE)

  if (length(subdirs) == 0) {
    return(list(
      annotations = data.frame(
        sample_name = character(),
        roi_number = integer(),
        file_name = character(),
        class_name = character(),
        stringsAsFactors = FALSE
      ),
      classes_found = character(),
      sample_names = character()
    ))
  }

  # One listing per class folder, then everything is parsed in one go: a
  # data frame per file was the bulk of the time for large folders
  files_by_dir <- lapply(subdirs, list.files, pattern = "\\.png$",
                         full.names = FALSE)
  file_name <- unlist(files_by_dir, use.names = FALSE)
  class_name <- rep(sub("_\\d{3}$", "", basename(subdirs)),
                    lengths(files_by_dir))

  # Expected format: SampleName_NNNNN.png (5-digit ROI number)
  well_formed <- grepl("^.+_\\d{5}\\.png$", file_name)
  for (fn in file_name[!well_formed]) {
    warning("Skipping file with unexpected name format: ", fn)
  }
  file_name <- file_name[well_formed]
  class_name <- class_name[well_formed]

  sample_name <- sub("^(.+)_\\d{5}\\.png$", "\\1", file_name)
  roi_number <- as.integer(sub("^.+_(\\d{5})\\.png$", "\\1", file_name))

  # The same ROI in two class folders: the first occurrence wins
  roi_key <- paste0(sample_name, "_", roi_number)
  duplicate <- duplicated(roi_key)
  for (i in which(duplicate)) {
    warning(sprintf("Duplicate ROI %s found in class '%s' (already in '%s'), using first occurrence",
                    roi_key[i], class_name[i],
                    class_name[match(roi_key[i], roi_key)]))
  }

  annotations <- data.frame(
    sample_name = sample_name[!duplicate],
    roi_number = roi_number[!duplicate],
    file_name = file_name[!duplicate],
    class_name = class_name[!duplicate],
    stringsAsFactors = FALSE
  )

  if (nrow(annotations) == 0) {
    return(list(
      annotations = annotations,
      classes_found = character(),
      sample_names = character()
    ))
  }

  list(
    annotations = annotations,
    classes_found = sort(unique(annotations$class_name)),
    sample_names = sort(unique(annotations$sample_name))
  )
}

#' Filter classifications to only include extracted images
#'
#' Filters a classifications data frame to only include ROIs that have
#' corresponding PNG files in the extracted folder.
#'
#' @param classifications Data frame of classifications (must have file_name column)
#' @param extracted_folder Path to folder with extracted PNG images
#' @return Filtered classifications data frame
#' @export
#' @examples
#' \dontrun{
#' # Filter to only images that were successfully extracted
#' classifications <- filter_to_extracted(
#'   classifications = classifications,
#'   extracted_folder = "/tmp/png/D20230101T120000_IFCB134"
#' )
#' }
filter_to_extracted <- function(classifications, extracted_folder) {
  if (!dir.exists(extracted_folder)) {
    return(classifications)
  }

  extracted_files <- list.files(extracted_folder, pattern = "\\.png$")
  classifications[classifications$file_name %in% extracted_files, ]
}

# Class names for a vector of class2use indices
#
# @param class_indices Numeric class indices as stored in a manual .mat file.
# @param class2use Character vector of class names.
# @return Character vector; "unclassified" for NaN, NA and out-of-range
#   indices.
# @noRd
class_names_from_indices <- function(class_indices, class2use) {
  class_indices <- as.numeric(class_indices)
  valid <- !is.na(class_indices) & class_indices >= 1 &
    class_indices <= length(class2use)
  class_names <- rep("unclassified", length(class_indices))
  class_names[valid] <- as.character(class2use[class_indices[valid]])
  class_names
}
