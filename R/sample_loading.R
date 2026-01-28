# Sample loading functions for ClassiPyR

#' @importFrom iRfcb ifcb_get_mat_variable
NULL

#' Load classifications from CSV file (validation mode)
#'
#' Reads a classification CSV file and returns a data frame with classifications.
#' Class names are processed to truncate trailing numbers (matching iRfcb behavior).
#'
#' @param csv_path Path to classification CSV file
#' @return Data frame with classifications (columns depend on CSV content)
#' @export
#' @examples
#' \dontrun{
#' # Load classifications from a CSV file
#' classifications <- load_from_csv("/path/to/classifications.csv")
#' head(classifications)
#' }
load_from_csv <- function(csv_path) {
  classifications <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

  # Truncate trailing numbers from class names
  classifications$class_name <- sapply(
    classifications$class_name,
    iRfcb:::truncate_folder_name
  )

  classifications
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
#' @return Data frame with columns: file_name, class_name, score, roi_area
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

  # Map class indices to class names
  roi_numbers <- classlist[, 1]
  class_indices <- classlist[, 2]

  # Get class names from indices (handle 0 or NA as "unclassified")
  class_names <- sapply(class_indices, function(idx) {
    if (is.na(idx) || idx < 1 || idx > length(class2use)) {
      return("unclassified")
    }
    return(class2use[idx])
  })

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
    class_name = class_names,
    score = NA_real_,
    roi_area = roi_dimensions$area[roi_numbers],
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
#' @return Data frame with columns: file_name, class_name, score, roi_area
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

  # Match ROI dimensions
  roi_areas <- sapply(roi_numbers, function(rn) {
    idx <- which(roi_dimensions$roi_number == rn)
    if (length(idx) > 0) roi_dimensions$area[idx] else 1
  })

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
    class_name = class_names,
    score = NA_real_,
    roi_area = roi_areas,
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
#' @return Data frame with columns: file_name, class_name, score, roi_area
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
    roi_area = roi_dimensions$area,
    stringsAsFactors = FALSE
  )

  # Sort by area (descending) so larger/similar organisms group together
  classifications[order(-classifications$roi_area), ]
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
