# Sample saving functions for ClassiPyR

#' @importFrom iRfcb ifcb_annotate_samples ifcb_create_class2use
NULL

#' Save sample annotations
#'
#' Saves the current annotations for a sample. By default annotations are
#' stored in a local SQLite database (\code{annotations.sqlite} in the database
#' folder). Optionally, a MATLAB-compatible \code{.mat} file can also be
#' written.
#'
#' @param sample_name Sample name (e.g., "D20230101T120000_IFCB134")
#' @param classifications Current classifications data frame
#' @param original_classifications Original classifications data frame (for comparison)
#' @param changes_log Changes log data frame from \code{\link{create_empty_changes_log}}
#' @param temp_png_folder Path to temporary folder with extracted PNG images
#' @param output_folder Output folder path for MAT files and statistics
#' @param png_output_folder PNG output folder path (organized by class)
#' @param roi_folder ROI folder path (for ADC file location, used as fallback)
#' @param class2use_path Path to class2use file
#' @param class2use Character vector of class names. When NULL (default), loaded
#'   from \code{class2use_path}.
#' @param annotator Annotator name for statistics
#' @param adc_folder Direct path to the ADC folder. When provided, this is used
#'   instead of constructing the path via \code{\link{get_sample_paths}}.
#'   This supports non-standard folder structures.
#' @param save_format One of \code{"sqlite"} (default), \code{"mat"}, or
#'   \code{"both"}. Controls which backend(s) are written.
#' @param db_folder Path to the database folder for SQLite storage. Defaults to
#'   \code{\link{get_default_db_dir}()}. Should be a local filesystem path,
#'   not a network drive.
#' @param export_statistics Logical. When \code{TRUE} (default), validation
#'   statistics CSV files are written to a \code{validation_statistics/}
#'   subfolder inside \code{output_folder}. Set to \code{FALSE} to skip this
#'   export, e.g. when annotating from scratch.
#' @param force Logical. By default (\code{FALSE}) a sample without any
#'   relabeled image is not saved, so that merely browsing samples does not
#'   mark them as annotated. Set to \code{TRUE} to save the classifications as
#'   they are, e.g. to record that every auto-classification of a sample was
#'   reviewed and found correct.
#' @return TRUE on success, FALSE when there is nothing to save (empty changes
#'   log and \code{force = FALSE}) or required inputs are missing. Errors raised while writing any
#'   backend propagate to the caller, so callers can distinguish a failed save
#'   from an empty one.
#' @export
#' @examples
#' \dontrun{
#' # Save annotations for a sample (default: SQLite)
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
                                     class2use = NULL,
                                     annotator = "Unknown",
                                     adc_folder = NULL,
                                     save_format = "sqlite",
                                     db_folder = get_default_db_dir(),
                                     export_statistics = TRUE,
                                     force = FALSE) {

  if (is.null(sample_name) || is.null(classifications)) {
    return(FALSE)
  }

  # class2use_path is only required for MAT format (SQLite uses class2use vector)
  if (is.null(class2use_path) && save_format %in% c("mat", "both") && is.null(class2use)) {
    return(FALSE)
  }

  # Only save if there are changes, unless the caller asks for the sample to
  # be saved as it is
  if (nrow(changes_log) == 0 && !isTRUE(force)) {
    return(FALSE)
  }

  # Create output folders if needed
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  if (!dir.exists(png_output_folder)) {
    dir.create(png_output_folder, recursive = TRUE)
  }

  # Copy images to class subfolders. The temporary copy is the input of
  # ifcb_annotate_samples(), so it is only made for a MAT save.
  write_mat <- save_format %in% c("mat", "both")
  temp_annotate_folder <- NULL
  if (write_mat) {
    temp_annotate_folder <- tempfile(pattern = "ifcb_annotate_")
    dir.create(temp_annotate_folder, recursive = TRUE)
    on.exit(unlink(temp_annotate_folder, recursive = TRUE), add = TRUE)
  }

  copy_images_to_class_folders(
    classifications = classifications,
    src_folder = file.path(temp_png_folder, sample_name),
    temp_folder = if (write_mat) file.path(temp_annotate_folder, "png"),
    output_folder = png_output_folder
  )
  remove_relabeled_class_images(classifications, changes_log, png_output_folder)

  # Save to SQLite
  if (save_format %in% c("sqlite", "both")) {
    # Load class list if not provided
    c2u <- class2use
    if (is.null(c2u)) {
      c2u <- load_class_list(class2use_path)
    }
    db_path <- get_db_path(db_folder)
    save_annotations_db(db_path, sample_name, classifications, c2u, annotator)
  }

  # Save to .mat
  if (write_mat) {
    # Find ADC folder: use provided path, or fall back to get_sample_paths()
    if (is.null(adc_folder)) {
      paths <- get_sample_paths(sample_name, roi_folder)
      adc_folder <- paths$adc_folder
    }

    # The class indices written to the file must come from the class list the
    # annotations were made with. A class2use file on disk can be older than
    # that list (classes added or renamed in the app), and every class it
    # lacks would be written as unclassified, so the list itself is used
    # whenever it is supplied.
    class2use_file <- class2use_path
    if (!is.null(class2use)) {
      class2use_file <- file.path(temp_annotate_folder, "class2use.mat")
      ifcb_create_class2use(class2use, class2use_file)
      on.exit(unlink(class2use_file), add = TRUE)
    }
    png_annotate_folder <- file.path(temp_annotate_folder, "png")
    if (!dir.exists(png_annotate_folder)) {
      stop("No images found for sample ", sample_name,
           "; cannot write the .mat file")
    }

    ifcb_annotate_samples(
      png_folder = png_annotate_folder,
      adc_folder = adc_folder,
      class2use_file = class2use_file,
      output_folder = output_folder,
      sample_names = sample_name,
      remove_trailing_numbers = FALSE
    )
  }

  # Save statistics (optional)
  if (isTRUE(export_statistics)) {
    stats_folder <- file.path(output_folder, "validation_statistics")
    if (!dir.exists(stats_folder)) {
      dir.create(stats_folder, recursive = TRUE)
    }
    save_validation_statistics(
      sample_name = sample_name,
      classifications = classifications,
      original_classifications = original_classifications,
      stats_folder = stats_folder,
      annotator = annotator
    )
  }

  TRUE
}

#' Copy images to class-organized folders
#'
#' Copies PNG images from a flat source folder into class-organized subfolders,
#' both for temporary use by ifcb_annotate_samples and for permanent storage.
#'
#' @param classifications Classifications data frame with file_name and class_name columns
#' @param src_folder Source folder containing PNG images
#' @param temp_folder Temporary folder for ifcb_annotate_samples, or
#'   \code{NULL} to skip that copy.
#' @param output_folder Permanent output folder for class-organized images. An
#'   image that is already there with the same size is not copied again.
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
  src <- file.path(src_folder, classifications$file_name)
  available <- file.exists(src)
  if (!any(available)) {
    return(invisible(NULL))
  }
  src <- src[available]
  file_name <- classifications$file_name[available]
  class_name <- classifications$class_name[available]

  copy_to <- function(base_folder, skip_identical) {
    class_folders <- file.path(base_folder, class_name)
    for (folder in unique(class_folders)) {
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    }
    dest <- file.path(class_folders, file_name)
    todo <- rep(TRUE, length(dest))
    if (skip_identical) {
      dest_size <- file.size(dest)
      todo <- is.na(dest_size) | dest_size != file.size(src)
    }
    if (any(todo)) {
      file.copy(src[todo], dest[todo], overwrite = TRUE)
    }
  }

  # Temp folder for ifcb_annotate_samples
  if (!is.null(temp_folder)) copy_to(temp_folder, skip_identical = FALSE)
  # Permanent output folder organized by class
  if (!is.null(output_folder)) copy_to(output_folder, skip_identical = TRUE)

  invisible(NULL)
}

# Remove the copies of relabeled images left in their previous class folders
#
# Saving copies every image into the folder of its current class. An image
# that was saved under one class and relabeled later would otherwise stay in
# the old class folder as well, leaving the PNG output with the same image
# under two classes.
#
# @param classifications Current classifications (file_name, class_name).
# @param changes_log Changes log (image, original_class, new_class).
# @param output_folder Class-organized PNG output folder.
# @return Character vector of the removed files, invisibly.
# @noRd
remove_relabeled_class_images <- function(classifications, changes_log,
                                          output_folder) {
  if (is.null(changes_log) || nrow(changes_log) == 0 ||
      is.null(output_folder) || !dir.exists(output_folder)) {
    return(invisible(character()))
  }
  previous_class <- as.character(changes_log$original_class)
  image <- as.character(changes_log$image)
  current_class <- classifications$class_name[match(image, classifications$file_name)]

  # A class name is used as a single folder name; anything else is left alone
  plain <- !is.na(previous_class) & nzchar(previous_class) &
    !grepl(PATH_SEPARATOR_CHARS, previous_class) & !grepl("\\.\\.", previous_class) &
    !grepl(PATH_SEPARATOR_CHARS, image)
  stale <- plain & !is.na(current_class) & previous_class != current_class
  paths <- unique(file.path(output_folder, previous_class[stale], image[stale]))
  paths <- paths[file.exists(paths)]
  unlink(paths)
  invisible(paths)
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
