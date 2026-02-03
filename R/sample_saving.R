# Sample saving functions for ClassiPyR

#' @importFrom iRfcb ifcb_annotate_samples
NULL

#' Save sample annotations to MAT and statistics files
#'
#' Saves the current annotations for a sample, including:
#' - MAT file compatible with ifcb-analysis (requires Python)
#' - Validation statistics CSV files
#' - PNG images organized by class
#'
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param classifications Current classifications data frame
#' @param original_classifications Original classifications data frame (for comparison)
#' @param changes_log Changes log data frame from \code{\link{create_empty_changes_log}}
#' @param temp_png_folder Path to temporary folder with extracted PNG images
#' @param output_folder Output folder path for MAT files
#' @param png_output_folder PNG output folder path (organized by class)
#' @param roi_folder ROI folder path (for ADC file location, used as fallback)
#' @param class2use_path Path to class2use file
#' @param annotator Annotator name for statistics
#' @param adc_folder Direct path to the ADC folder. When provided, this is used
#'   instead of constructing the path via \code{\link{get_sample_paths}}.
#'   This supports non-standard folder structures.
#' @return TRUE on success, FALSE on failure
#' @export
#' @examples
#' \dontrun{
#' # Save annotations for a sample
#' success <- save_sample_annotations(
#'   sample_name = "D20230101T120000_IFCB134",
#'   classifications = current_classifications,
#'   original_classifications = original_classifications,
#'   changes_log = changes_log,
#'   temp_png_folder = "/tmp/png",
#'   output_folder = "/data/manual",
#'   png_output_folder = "/data/png_output",
#'   roi_folder = "/data/raw",
#'   class2use_path = "/data/class2use.mat",
#'   annotator = "John Doe"
#' )
#' }
save_sample_annotations <- function(sample_name,
                                     classifications,
                                     original_classifications,
                                     changes_log,
                                     temp_png_folder,
                                     output_folder,
                                     png_output_folder,
                                     roi_folder,
                                     class2use_path,
                                     annotator = "Unknown",
                                     adc_folder = NULL) {

  if (is.null(sample_name) || is.null(classifications) || is.null(class2use_path)) {
    return(FALSE)
  }

  # Only save if there are changes
  if (nrow(changes_log) == 0) {
    return(FALSE)
  }

  tryCatch({
    # Create output folders if needed
    stats_folder <- file.path(output_folder, "validation_statistics")

    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    if (!dir.exists(stats_folder)) {
      dir.create(stats_folder, recursive = TRUE)
    }
    if (!dir.exists(png_output_folder)) {
      dir.create(png_output_folder, recursive = TRUE)
    }

    # Create temporary PNG folder structure for ifcb_annotate_samples
    temp_annotate_folder <- tempfile(pattern = "ifcb_annotate_")
    dir.create(temp_annotate_folder, recursive = TRUE)

    # Copy images to class subfolders
    copy_images_to_class_folders(
      classifications = classifications,
      src_folder = file.path(temp_png_folder, sample_name),
      temp_folder = temp_annotate_folder,
      output_folder = png_output_folder
    )

    # Find ADC folder: use provided path, or fall back to get_sample_paths()
    if (is.null(adc_folder)) {
      paths <- get_sample_paths(sample_name, roi_folder)
      adc_folder <- paths$adc_folder
    }

    # Run annotation - save MAT to output folder directly
    ifcb_annotate_samples(
      png_folder = temp_annotate_folder,
      adc_folder = adc_folder,
      class2use_file = class2use_path,
      output_folder = output_folder,
      sample_names = sample_name,
      remove_trailing_numbers = FALSE
    )

    # Save statistics
    save_validation_statistics(
      sample_name = sample_name,
      classifications = classifications,
      original_classifications = original_classifications,
      stats_folder = stats_folder,
      annotator = annotator
    )

    # Clean up temp folder
    unlink(temp_annotate_folder, recursive = TRUE)

    return(TRUE)

  }, error = function(e) {
    warning("Save failed for ", sample_name, ": ", e$message)
    return(FALSE)
  })
}

#' Copy images to class-organized folders
#'
#' Copies PNG images from a flat source folder into class-organized subfolders,
#' both for temporary use by ifcb_annotate_samples and for permanent storage.
#'
#' @param classifications Classifications data frame with file_name and class_name columns
#' @param src_folder Source folder containing PNG images
#' @param temp_folder Temporary folder for ifcb_annotate_samples
#' @param output_folder Permanent output folder for class-organized images
#' @return NULL (called for side effects)
#' @export
#' @examples
#' \dontrun{
#' # Copy images to class folders
#' copy_images_to_class_folders(
#'   classifications = data.frame(
#'     file_name = c("sample_00001.png", "sample_00002.png"),
#'     class_name = c("Diatom", "Ciliate")
#'   ),
#'   src_folder = "/tmp/png/sample",
#'   temp_folder = "/tmp/annotate",
#'   output_folder = "/data/png_output"
#' )
#' }
copy_images_to_class_folders <- function(classifications, src_folder, temp_folder, output_folder) {
  for (i in seq_len(nrow(classifications))) {
    img_file <- classifications$file_name[i]
    class_name <- classifications$class_name[i]

    src_path <- file.path(src_folder, img_file)

    if (file.exists(src_path)) {
      # Temp folder for ifcb_annotate_samples
      temp_class_folder <- file.path(temp_folder, class_name)
      if (!dir.exists(temp_class_folder)) {
        dir.create(temp_class_folder, recursive = TRUE)
      }
      file.copy(src_path, file.path(temp_class_folder, img_file), overwrite = TRUE)

      # Permanent output folder organized by class
      output_class_folder <- file.path(output_folder, class_name)
      if (!dir.exists(output_class_folder)) {
        dir.create(output_class_folder, recursive = TRUE)
      }
      file.copy(src_path, file.path(output_class_folder, img_file), overwrite = TRUE)
    }
  }
}

#' Save validation statistics to CSV files
#'
#' Compares current classifications to original classifications and saves
#' summary and detailed statistics to CSV files.
#'
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param classifications Current classifications data frame
#' @param original_classifications Original classifications data frame
#' @param stats_folder Statistics output folder path
#' @param annotator Annotator name
#' @return NULL (called for side effects)
#' @export
#' @examples
#' \dontrun{
#' # Save validation statistics
#' save_validation_statistics(
#'   sample_name = "D20230101T120000_IFCB134",
#'   classifications = current_classifications,
#'   original_classifications = original_classifications,
#'   stats_folder = "/data/manual/validation_statistics",
#'   annotator = "John Doe"
#' )
#' }
save_validation_statistics <- function(sample_name,
                                        classifications,
                                        original_classifications,
                                        stats_folder,
                                        annotator) {

  # Create comparison
  orig_subset <- original_classifications[, c("file_name", "class_name", "score")]
  names(orig_subset) <- c("file_name", "original_class", "score")
  curr_subset <- classifications[, c("file_name", "class_name")]
  names(curr_subset) <- c("file_name", "validated_class")
  comparison <- merge(orig_subset, curr_subset, by = "file_name")
  comparison$correct <- comparison$original_class == comparison$validated_class
  comparison$annotator <- annotator
  comparison$annotation_date <- Sys.time()

  total <- nrow(comparison)
  correct <- sum(comparison$correct)
  incorrect <- total - correct

  # Summary statistics
 stats <- data.frame(
    sample = sample_name,
    annotator = annotator,
    annotation_date = Sys.time(),
    total_images = total,
    correct_classifications = correct,
    incorrect_classifications = incorrect,
    accuracy = if (total > 0) correct / total else NA
  )

  stats_file <- file.path(stats_folder, paste0(sample_name, "_validation_stats.csv"))
  utils::write.csv(stats, stats_file, row.names = FALSE)

  # Detailed statistics
  detailed_stats_file <- file.path(stats_folder, paste0(sample_name, "_validation_detailed.csv"))
  utils::write.csv(comparison, detailed_stats_file, row.names = FALSE)
}
