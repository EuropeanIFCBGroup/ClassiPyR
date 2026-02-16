# SQLite database backend for ClassiPyR annotations
#
# Provides functions to store and retrieve annotations in a local SQLite
# database as an alternative to .mat files. SQLite is the default storage
# backend - it works out of the box with no Python dependency.

#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbExecute
#' @importFrom RSQLite SQLite
#' @importFrom iRfcb ifcb_create_manual_file ifcb_extract_pngs
NULL

#' Get path to the annotations SQLite database
#'
#' Returns the path to \code{annotations.sqlite} in the given database
#' directory. The database directory should be on a local filesystem, not a
#' network drive, because
#' \href{https://www.sqlite.org/useovernet.html}{SQLite file locking is
#' unreliable over network filesystems}.
#'
#' @param db_folder Path to the database directory. Defaults to
#'   \code{\link{get_default_db_dir}()}, a persistent local directory.
#' @return Path to the SQLite database file
#' @export
#' @seealso \code{\link{get_default_db_dir}} for the default database directory
#' @examples
#' # Use the default local database directory
#' get_db_path(get_default_db_dir())
#'
#' # Or specify a custom directory
#' get_db_path("/data/local_db")
get_db_path <- function(db_folder) {
  file.path(db_folder, "annotations.sqlite")
}

#' Initialize the annotations database schema
#'
#' Creates the \code{annotations} and \code{class_lists} tables if they do not
#' already exist.
#'
#' @param con A DBI connection object
#' @return NULL (called for side effects)
#' @keywords internal
init_db_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS annotations (
      sample_name TEXT NOT NULL,
      roi_number  INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      annotator   TEXT,
      timestamp   TEXT DEFAULT (datetime('now')),
      is_manual   INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (sample_name, roi_number)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS class_lists (
      sample_name TEXT NOT NULL,
      class_index INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      PRIMARY KEY (sample_name, class_index)
    )
  ")

  # Migration: add is_manual column to existing databases that lack it
  cols <- dbGetQuery(con, "PRAGMA table_info(annotations)")
  if (!"is_manual" %in% cols$name) {
    dbExecute(con, "ALTER TABLE annotations ADD COLUMN is_manual INTEGER NOT NULL DEFAULT 1")
  }

  invisible(NULL)
}

#' Save annotations to the SQLite database
#'
#' Writes (or replaces) annotations for a single sample. The existing rows for
#' the sample are deleted first so that re-saving acts as an upsert.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name (e.g., \code{"D20230101T120000_IFCB134"})
#' @param classifications Data frame with at least \code{file_name} and
#'   \code{class_name} columns
#' @param class2use Character vector of class names (preserves index order for
#'   .mat export)
#' @param annotator Annotator name
#' @param is_manual Integer vector of 0/1 flags indicating whether each ROI was
#'   manually reviewed (1) or not yet reviewed (0, corresponding to NaN in .mat
#'   files). If \code{NULL} (the default), all ROIs are treated as reviewed.
#' @return TRUE on success, FALSE on failure
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' save_annotations_db(db_path, "D20230101T120000_IFCB134",
#'                     classifications, class2use, "Jane")
#' }
save_annotations_db <- function(db_path, sample_name, classifications,
                                class2use, annotator = "Unknown",
                                is_manual = NULL) {
  if (is.null(classifications) || nrow(classifications) == 0) {
    return(FALSE)
  }

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  init_db_schema(con)

  # Extract ROI numbers from file_name (e.g., "D20230101T120000_IFCB134_00001.png" -> 1)
  roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", classifications$file_name))

  if (is.null(is_manual)) {
    is_manual <- rep(1L, nrow(classifications))
  }

  annotations_df <- data.frame(
    sample_name = sample_name,
    roi_number = roi_numbers,
    class_name = classifications$class_name,
    annotator = annotator,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    is_manual = as.integer(is_manual),
    stringsAsFactors = FALSE
  )

  tryCatch({
    dbExecute(con, "BEGIN TRANSACTION")

    # Delete existing annotations for this sample (upsert semantics)
    dbExecute(con, "DELETE FROM annotations WHERE sample_name = ?",
              params = list(sample_name))
    dbWriteTable(con, "annotations", annotations_df, append = TRUE)

    # Save class list for this sample (preserves index order for .mat export)
    dbExecute(con, "DELETE FROM class_lists WHERE sample_name = ?",
              params = list(sample_name))
    if (length(class2use) > 0) {
      class_list_df <- data.frame(
        sample_name = sample_name,
        class_index = seq_along(class2use),
        class_name = class2use,
        stringsAsFactors = FALSE
      )
      dbWriteTable(con, "class_lists", class_list_df, append = TRUE)
    }

    dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    warning("Failed to save annotations to database: ", e$message)
    FALSE
  })
}

#' Load annotations from the SQLite database
#'
#' Reads annotations for a single sample and returns a data frame in the same
#' format as \code{\link{load_from_mat}}.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name
#' @param roi_dimensions Data frame from \code{\link{read_roi_dimensions}} with
#'   columns \code{roi_number}, \code{width}, \code{height}, \code{area}
#' @return Data frame with columns: file_name, class_name, score, width, height,
#'   roi_area. Returns NULL if the sample has no annotations.
#' @export
#' @examples
#' \dontrun{
#' dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
#' db_path <- get_db_path("/data/manual")
#' classifications <- load_annotations_db(db_path, "D20230101T120000_IFCB134", dims)
#' }
load_annotations_db <- function(db_path, sample_name, roi_dimensions) {
  if (!file.exists(db_path)) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  rows <- dbGetQuery(con,
    "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name)
  )

  if (nrow(rows) == 0) {
    return(NULL)
  }

  # Match ROI dimensions by roi_number (safe lookup with NA fallback)
  roi_data <- lapply(rows$roi_number, function(rn) {
    idx <- which(roi_dimensions$roi_number == rn)
    if (length(idx) > 0) {
      list(width = roi_dimensions$width[idx],
           height = roi_dimensions$height[idx],
           area = roi_dimensions$area[idx])
    } else {
      list(width = NA_real_, height = NA_real_, area = NA_real_)
    }
  })

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, rows$roi_number),
    class_name = rows$class_name,
    score = NA_real_,
    width = vapply(roi_data, `[[`, numeric(1), "width"),
    height = vapply(roi_data, `[[`, numeric(1), "height"),
    roi_area = vapply(roi_data, `[[`, numeric(1), "area"),
    stringsAsFactors = FALSE
  )

  # Sort by area (descending) - consistent with load_from_mat
  classifications[order(-classifications$roi_area), ]
}

#' List samples with annotations in the database
#'
#' @param db_path Path to the SQLite database file
#' @return Character vector of sample names that have annotations
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' samples <- list_annotated_samples_db(db_path)
#' }
list_annotated_samples_db <- function(db_path) {
  if (!file.exists(db_path)) {
    return(character())
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  # Check that the annotations table exists
  tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
  if (!"annotations" %in% tables$name) {
    return(character())
  }

  result <- dbGetQuery(con, "SELECT DISTINCT sample_name FROM annotations ORDER BY sample_name")
  result$sample_name
}

#' Update the annotator name for one or more samples
#'
#' Changes the annotator field for all annotations belonging to the specified
#' sample(s). This is useful for correcting the annotator after bulk imports
#' or when transferring ownership of annotations.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_names Character vector of sample names to update
#' @param annotator New annotator name
#' @return Named integer vector with the number of rows updated per sample.
#'   Samples not found in the database are included with a count of 0.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#'
#' # Update a single sample
#' update_annotator(db_path, "D20230101T120000_IFCB134", "Jane")
#'
#' # Update multiple samples at once
#' update_annotator(db_path,
#'                  c("D20230101T120000_IFCB134", "D20230202T080000_IFCB134"),
#'                  "Jane")
#'
#' # Update all annotated samples
#' all_samples <- list_annotated_samples_db(db_path)
#' update_annotator(db_path, all_samples, "Jane")
#' }
update_annotator <- function(db_path, sample_names, annotator) {
  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path)
  }
  if (length(sample_names) == 0) {
    return(integer(0))
  }
  if (!is.character(annotator) || length(annotator) != 1 || is.na(annotator)) {
    stop("annotator must be a single non-NA character string")
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  counts <- vapply(sample_names, function(sn) {
    res <- dbExecute(con,
      "UPDATE annotations SET annotator = ? WHERE sample_name = ?",
      params = list(annotator, sn)
    )
    as.integer(res)
  }, integer(1))

  counts
}

#' Import a .mat annotation file into the SQLite database
#'
#' Reads an existing .mat annotation file and writes its data into the SQLite
#' database. The class list (\code{class2use_manual}) and classlist indices are
#' read directly from the .mat file to ensure a faithful import. ROIs with NaN
#' indices (not yet reviewed) are stored with \code{is_manual = 0}.
#'
#' @param mat_path Path to the .mat annotation file
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name
#' @param annotator Annotator name (defaults to \code{"imported"})
#' @return TRUE on success, FALSE on failure
#' @export
#' @examples
#' \dontrun{
#' import_mat_to_db(
#'   mat_path = "/data/manual/D20230101T120000_IFCB134.mat",
#'   db_path = get_db_path("/data/manual"),
#'   sample_name = "D20230101T120000_IFCB134"
#' )
#' }
import_mat_to_db <- function(mat_path, db_path, sample_name,
                             annotator = "imported") {
  if (!file.exists(mat_path)) {
    warning("MAT file not found: ", mat_path)
    return(FALSE)
  }

  tryCatch({
    # Read the class list embedded in the .mat file
    class2use <- as.character(ifcb_get_mat_variable(mat_path,
                                                     variable_name = "class2use_manual"))

    classlist <- ifcb_get_mat_variable(mat_path, variable_name = "classlist")
    roi_numbers <- classlist[, 1]
    class_indices <- classlist[, 2]

    # Detect NaN (not yet reviewed) vs classified ROIs
    is_nan <- is.nan(class_indices)
    is_manual <- ifelse(is_nan, 0L, 1L)

    class_names <- vapply(class_indices, function(idx) {
      if (is.na(idx) || is.nan(idx) || idx < 1 || idx > length(class2use)) {
        "unclassified"
      } else {
        class2use[idx]
      }
    }, character(1))

    # Build a classifications-like data frame for save_annotations_db
    classifications <- data.frame(
      file_name = sprintf("%s_%05d.png", sample_name, roi_numbers),
      class_name = class_names,
      stringsAsFactors = FALSE
    )

    save_annotations_db(db_path, sample_name, classifications, class2use,
                        annotator, is_manual = is_manual)
  }, error = function(e) {
    warning("Failed to import MAT file: ", e$message)
    FALSE
  })
}

#' Export annotations from SQLite to a .mat file
#'
#' Reads annotations for a single sample from the database and writes a
#' MATLAB-compatible annotation file using \code{iRfcb::ifcb_create_manual_file}.
#' Requires Python with scipy.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name
#' @param output_folder Folder where the .mat file will be written
#' @return TRUE on success, FALSE on failure
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' export_db_to_mat(db_path, "D20230101T120000_IFCB134", "/data/manual")
#' }
export_db_to_mat <- function(db_path, sample_name, output_folder) {
  if (!file.exists(db_path)) {
    warning("Database not found: ", db_path)
    return(FALSE)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  # Get annotations for this sample (including is_manual flag)
  rows <- dbGetQuery(con,
    "SELECT roi_number, class_name, is_manual FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name)
  )

  if (nrow(rows) == 0) {
    warning("No annotations found for sample: ", sample_name)
    return(FALSE)
  }

  # Get class list for this sample
  class_list <- dbGetQuery(con,
    "SELECT class_index, class_name FROM class_lists WHERE sample_name = ? ORDER BY class_index",
    params = list(sample_name)
  )

  if (nrow(class_list) == 0) {
    warning("No class list found for sample: ", sample_name)
    return(FALSE)
  }

  class2use <- class_list$class_name

  # Build classlist numeric vector: map class names to indices
  # Use NaN for unreviewed ROIs (is_manual == 0) to preserve the distinction
  classlist_indices <- match(rows$class_name, class2use)
  # Any unmatched classes default to 1 (typically "unclassified")
  classlist_indices[is.na(classlist_indices)] <- 1L
  classlist_indices <- as.numeric(classlist_indices)
  classlist_indices[rows$is_manual == 0L] <- NaN

  output_file <- file.path(output_folder, paste0(sample_name, ".mat"))

  tryCatch({
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
    ifcb_create_manual_file(
      roi_length = nrow(rows),
      class2use = class2use,
      output_file = output_file,
      classlist = classlist_indices
    )
    TRUE
  }, error = function(e) {
    warning("Failed to export to MAT: ", e$message)
    FALSE
  })
}

#' Bulk import .mat annotation files into the SQLite database
#'
#' Scans a folder for \code{.mat} annotation files (excluding classifier output
#' files matching \code{*_class*.mat}) and imports each into the database. Each
#' file's embedded \code{class2use_manual} is used for class-name mapping.
#'
#' @param mat_folder Folder containing .mat annotation files
#' @param db_path Path to the SQLite database file
#' @param annotator Annotator name (defaults to \code{"imported"})
#' @return Named list with counts: \code{success}, \code{failed}, \code{skipped}
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' result <- import_all_mat_to_db("/data/manual", db_path)
#' cat(result$success, "imported,", result$failed, "failed,", result$skipped, "skipped\n")
#' }
import_all_mat_to_db <- function(mat_folder, db_path,
                                  annotator = "imported") {
  mat_files <- list.files(mat_folder, pattern = "\\.mat$", full.names = TRUE)
  # Exclude classifier output files (*_class*.mat) and class2use files
  mat_files <- mat_files[!grepl("_class", basename(mat_files))]
  mat_files <- mat_files[!grepl("^class2use", basename(mat_files))]

  counts <- list(success = 0L, failed = 0L, skipped = 0L)

  if (length(mat_files) == 0) {
    return(counts)
  }

  # Get already-imported samples to allow skipping
  existing <- list_annotated_samples_db(db_path)

  for (mat_path in mat_files) {
    sample_name <- tools::file_path_sans_ext(basename(mat_path))

    if (sample_name %in% existing) {
      counts$skipped <- counts$skipped + 1L
      next
    }

    ok <- import_mat_to_db(mat_path, db_path, sample_name, annotator)
    if (isTRUE(ok)) {
      counts$success <- counts$success + 1L
    } else {
      counts$failed <- counts$failed + 1L
    }
  }

  counts
}

#' Bulk export all annotated samples from SQLite to .mat files
#'
#' Exports every sample in the database to a MATLAB-compatible annotation file.
#' Requires Python with scipy.
#'
#' @param db_path Path to the SQLite database file
#' @param output_folder Folder where .mat files will be written
#' @return Named list with counts: \code{success}, \code{failed}
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' result <- export_all_db_to_mat(db_path, "/data/manual")
#' cat(result$success, "exported,", result$failed, "failed\n")
#' }
export_all_db_to_mat <- function(db_path, output_folder) {
  samples <- list_annotated_samples_db(db_path)

  counts <- list(success = 0L, failed = 0L)

  if (length(samples) == 0) {
    return(counts)
  }

  for (sample_name in samples) {
    ok <- export_db_to_mat(db_path, sample_name, output_folder)
    if (isTRUE(ok)) {
      counts$success <- counts$success + 1L
    } else {
      counts$failed <- counts$failed + 1L
    }
  }

  counts
}

#' Export annotated images from SQLite to class-organized PNG folders
#'
#' Reads annotations for a single sample from the database and extracts PNG
#' images from the ROI file, placing each image into a subfolder named after
#' its assigned class.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name
#' @param roi_path Path to the \code{.roi} file for this sample
#' @param png_folder Base output folder. Images are written to
#'   \code{png_folder/<class_name>/}
#' @param skip_class Character vector of class names to exclude from export
#'   (e.g. \code{"unclassified"}). Default \code{NULL} exports all classes.
#' @return TRUE on success, FALSE on failure
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' export_db_to_png(db_path, "D20230101T120000_IFCB134",
#'                  "/data/raw/2023/D20230101/D20230101T120000_IFCB134.roi",
#'                  "/data/png_output",
#'                  skip_class = "unclassified")
#' }
export_db_to_png <- function(db_path, sample_name, roi_path, png_folder,
                             skip_class = NULL) {
  if (!file.exists(db_path)) {
    warning("Database not found: ", db_path)
    return(FALSE)
  }
  if (!file.exists(roi_path)) {
    warning("ROI file not found: ", roi_path)
    return(FALSE)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  rows <- dbGetQuery(con,
    "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name)
  )

  if (nrow(rows) == 0) {
    warning("No annotations found for sample: ", sample_name)
    return(FALSE)
  }

  # Filter out skipped classes
  if (!is.null(skip_class) && length(skip_class) > 0) {
    rows <- rows[!rows$class_name %in% skip_class, ]
    if (nrow(rows) == 0) {
      return(TRUE)  # All ROIs were in skipped classes — nothing to export
    }
  }

  dir.create(png_folder, recursive = TRUE, showWarnings = FALSE)

  # Group ROIs by class name and extract each group with taxaname for subfolder
  classes <- unique(rows$class_name)

  tryCatch({
    for (cls in classes) {
      roi_numbers <- rows$roi_number[rows$class_name == cls]
      ifcb_extract_pngs(
        roi_file = roi_path,
        out_folder = png_folder,
        ROInumbers = roi_numbers,
        taxaname = cls,
        verbose = FALSE
      )
    }
    TRUE
  }, error = function(e) {
    warning("Failed to export PNGs for ", sample_name, ": ", e$message)
    FALSE
  })
}

#' Bulk export all annotated samples from SQLite to class-organized PNGs
#'
#' Exports every annotated sample in the database to PNG images organized
#' into class subfolders.
#'
#' @param db_path Path to the SQLite database file
#' @param png_folder Base output folder for PNGs
#' @param roi_path_map Named list mapping sample names to \code{.roi} file
#'   paths. Samples without an entry are skipped.
#' @param skip_class Character vector of class names to exclude from export
#'   (e.g. \code{"unclassified"}). Default \code{NULL} exports all classes.
#' @return Named list with counts: \code{success}, \code{failed}, \code{skipped}
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' roi_map <- list("D20230101T120000_IFCB134" = "/data/raw/.../D20230101T120000_IFCB134.roi")
#' result <- export_all_db_to_png(db_path, "/data/png_output", roi_map,
#'                                skip_class = "unclassified")
#' cat(result$success, "exported,", result$failed, "failed,", result$skipped, "skipped\n")
#' }
export_all_db_to_png <- function(db_path, png_folder, roi_path_map,
                                 skip_class = NULL) {
  samples <- list_annotated_samples_db(db_path)

  counts <- list(success = 0L, failed = 0L, skipped = 0L)

  if (length(samples) == 0) {
    return(counts)
  }

  for (sample_name in samples) {
    roi_path <- roi_path_map[[sample_name]]
    if (is.null(roi_path) || !file.exists(roi_path)) {
      counts$skipped <- counts$skipped + 1L
      next
    }

    ok <- export_db_to_png(db_path, sample_name, roi_path, png_folder,
                           skip_class = skip_class)
    if (isTRUE(ok)) {
      counts$success <- counts$success + 1L
    } else {
      counts$failed <- counts$failed + 1L
    }
  }

  counts
}
