# SQLite database backend for ClassiPyR annotations
#
# Provides functions to store and retrieve annotations in a local SQLite
# database as an alternative to .mat files. SQLite is the default storage
# backend - it works out of the box with no Python dependency.

#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbExecute
#' @importFrom RSQLite SQLite
#' @importFrom iRfcb ifcb_create_manual_file ifcb_extract_pngs ifcb_get_ecotaxa_example ifcb_zip_pngs ifcb_create_class2use ifcb_zip_matlab
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

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS class_taxonomy (
      class_name TEXT PRIMARY KEY,
      aphia_id TEXT NOT NULL,
      scientific_name TEXT,
      accepted_name TEXT,
      accepted_aphia_id TEXT,
      updated_at TEXT DEFAULT (datetime('now'))
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS global_class_list (
      class_index INTEGER PRIMARY KEY,
      class_name  TEXT NOT NULL
    )
  ")

  # Migration: add is_manual column to existing databases that lack it
  cols <- dbGetQuery(con, "PRAGMA table_info(annotations)")
  if (!"is_manual" %in% cols$name) {
    dbExecute(con, "ALTER TABLE annotations ADD COLUMN is_manual INTEGER NOT NULL DEFAULT 1")
  }

  tax_cols <- dbGetQuery(con, "PRAGMA table_info(class_taxonomy)")
  if (nrow(tax_cols) > 0) {
    if (!"scientific_name" %in% tax_cols$name) {
      dbExecute(con, "ALTER TABLE class_taxonomy ADD COLUMN scientific_name TEXT")
    }
    if (!"accepted_aphia_id" %in% tax_cols$name) {
      dbExecute(con, "ALTER TABLE class_taxonomy ADD COLUMN accepted_aphia_id TEXT")
    }
  }

  invisible(NULL)
}

#' Save class taxonomy mappings to SQLite
#'
#' Stores class-to-AphiaID mappings (with optional accepted names) in the
#' \code{class_taxonomy} table of the annotations database.
#'
#' @param db_path Path to the SQLite database file.
#' @param class_aphia_map Named character vector mapping class names to AphiaID.
#' @param accepted_name_map Optional named character vector mapping class names
#'   to WoRMS accepted names.
#' @param scientific_name_map Optional named character vector mapping class
#'   names to matched scientific names (query record).
#' @param accepted_aphia_map Optional named character vector mapping class names
#'   to accepted WoRMS AphiaID values.
#' @return Logical \code{TRUE} on success, \code{FALSE} on failure.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path(get_default_db_dir())
#' save_class_taxonomy_db(
#'   db_path,
#'   class_aphia_map = c("Prorocentrum micans" = "109636")
#' )
#' }
save_class_taxonomy_db <- function(db_path, class_aphia_map, accepted_name_map = NULL,
                                   scientific_name_map = NULL, accepted_aphia_map = NULL) {
  if (length(class_aphia_map) == 0) {
    return(TRUE)
  }

  # Keep last value for duplicated class names to avoid ambiguous [[name]] access.
  if (!is.null(names(class_aphia_map))) {
    keep_last <- !duplicated(names(class_aphia_map), fromLast = TRUE)
    class_aphia_map <- class_aphia_map[keep_last]
  }

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    dbExecute(con, "BEGIN TRANSACTION")

    map_names <- names(class_aphia_map)
    for (i in seq_along(class_aphia_map)) {
      nm <- if (!is.null(map_names) && length(map_names) >= i) map_names[i] else NA_character_
      aphia <- as.character(class_aphia_map[i])[1]
      if (is.na(nm) || !nzchar(nm) || is.na(aphia) || !nzchar(aphia)) next

      accepted_name <- NA_character_
      if (!is.null(accepted_name_map) && !is.null(names(accepted_name_map))) {
        idx <- match(nm, names(accepted_name_map))
        if (!is.na(idx)) {
          accepted_name <- as.character(accepted_name_map[idx])[1]
        }
      }

      scientific_name <- NA_character_
      if (!is.null(scientific_name_map) && !is.null(names(scientific_name_map))) {
        idx <- match(nm, names(scientific_name_map))
        if (!is.na(idx)) {
          scientific_name <- as.character(scientific_name_map[idx])[1]
        }
      }

      accepted_aphia_id <- NA_character_
      if (!is.null(accepted_aphia_map) && !is.null(names(accepted_aphia_map))) {
        idx <- match(nm, names(accepted_aphia_map))
        if (!is.na(idx)) {
          accepted_aphia_id <- as.character(accepted_aphia_map[idx])[1]
        }
      }

      dbExecute(con, "
        INSERT INTO class_taxonomy (
          class_name, aphia_id, scientific_name, accepted_name, accepted_aphia_id, updated_at
        )
        VALUES (?, ?, ?, ?, ?, datetime('now'))
        ON CONFLICT(class_name) DO UPDATE SET
          aphia_id = excluded.aphia_id,
          scientific_name = CASE
            WHEN excluded.scientific_name IS NULL OR excluded.scientific_name = ''
            THEN class_taxonomy.scientific_name
            ELSE excluded.scientific_name
          END,
          accepted_name = CASE
            WHEN excluded.accepted_name IS NULL OR excluded.accepted_name = ''
            THEN class_taxonomy.accepted_name
            ELSE excluded.accepted_name
          END,
          accepted_aphia_id = CASE
            WHEN excluded.accepted_aphia_id IS NULL OR excluded.accepted_aphia_id = ''
            THEN class_taxonomy.accepted_aphia_id
            ELSE excluded.accepted_aphia_id
          END,
          updated_at = datetime('now')
      ", params = list(nm, aphia, scientific_name, accepted_name, accepted_aphia_id))
    }

    dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    warning("Failed to save class taxonomy to database: ", e$message)
    FALSE
  })
}

#' Load class taxonomy mappings from SQLite
#'
#' Reads class-to-AphiaID mappings from the \code{class_taxonomy} table.
#'
#' @param db_path Path to the SQLite database file.
#' @return Named character vector mapping class names to AphiaID.
#'   Returns empty vector if database/table is missing or empty.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path(get_default_db_dir())
#' map <- load_class_taxonomy_db(db_path)
#' }
load_class_taxonomy_db <- function(db_path) {
  if (!file.exists(db_path)) {
    return(stats::setNames(character(0), character(0)))
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  rows <- dbGetQuery(con, "
    SELECT class_name, aphia_id
    FROM class_taxonomy
    WHERE aphia_id IS NOT NULL AND aphia_id != ''
  ")

  if (nrow(rows) == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  out <- stats::setNames(as.character(rows$aphia_id), as.character(rows$class_name))
  out[!is.na(names(out)) & nzchar(names(out)) & !is.na(out) & nzchar(out)]
}

#' Save annotations to the SQLite database
#'
#' Writes (or replaces) annotations for a single sample. The existing rows for
#' the sample are deleted first so that re-saving acts as an upsert.
#'
#' Save global class list to SQLite
#'
#' Replaces the contents of the \code{global_class_list} table with the
#' supplied class names, preserving their index order. This is used to
#' auto-persist the in-app classlist so it survives across sessions.
#'
#' @param db_path Path to the SQLite database file.
#' @param class2use Character vector of class names.
#' @return Logical \code{TRUE} on success, \code{FALSE} on failure.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path(get_default_db_dir())
#' save_global_class_list_db(db_path, c("unclassified", "Diatom", "Ciliate"))
#' }
save_global_class_list_db <- function(db_path, class2use) {
  if (is.null(class2use) || length(class2use) == 0) {
    return(TRUE)
  }

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    dbExecute(con, "BEGIN TRANSACTION")
    dbExecute(con, "DELETE FROM global_class_list")
    for (i in seq_along(class2use)) {
      dbExecute(con, "INSERT INTO global_class_list (class_index, class_name) VALUES (?, ?)",
                params = list(i, class2use[i]))
    }
    dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(dbExecute(con, "ROLLBACK"), error = function(re) NULL)
    warning("Failed to save global class list: ", e$message, call. = FALSE)
    FALSE
  })
}

#' Load global class list from SQLite
#'
#' Returns the class list stored in the \code{global_class_list} table,
#' ordered by \code{class_index}. Returns \code{NULL} if the table is empty
#' or the database does not exist.
#'
#' @param db_path Path to the SQLite database file.
#' @return Character vector of class names, or \code{NULL} if unavailable.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path(get_default_db_dir())
#' classes <- load_global_class_list_db(db_path)
#' }
load_global_class_list_db <- function(db_path) {
  if (!file.exists(db_path)) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    df <- dbGetQuery(con, "SELECT class_name FROM global_class_list ORDER BY class_index")
    if (nrow(df) == 0) NULL else df$class_name
  }, error = function(e) {
    warning("Failed to load global class list: ", e$message, call. = FALSE)
    NULL
  })
}

#' Save annotations to SQLite
#'
#' Saves per-ROI classifications and the sample class list to the SQLite
#' database.
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

#' Delete annotations for a sample from the SQLite database
#'
#' Removes all rows for the given sample from both the \code{annotations} and
#' \code{class_lists} tables in a single transaction. This is a permanent
#' operation — the sample will appear unannotated after deletion.
#'
#' @param db_path Path to the SQLite database file
#' @param sample_name Sample name to delete
#' @return \code{TRUE} on success, \code{FALSE} on error (with a warning)
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/local_db")
#' delete_annotations_db(db_path, "D20230101T120000_IFCB134")
#' }
delete_annotations_db <- function(db_path, sample_name) {
  if (!file.exists(db_path)) {
    warning("Database file does not exist: ", db_path)
    return(FALSE)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  tryCatch({
    dbExecute(con, "BEGIN TRANSACTION")

    dbExecute(con, "DELETE FROM annotations WHERE sample_name = ?",
              params = list(sample_name))
    dbExecute(con, "DELETE FROM class_lists WHERE sample_name = ?",
              params = list(sample_name))

    dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    warning("Failed to delete annotations from database: ", e$message)
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
#' @param samples Optional character vector of sample names to export. When
#'   \code{NULL} (the default), all annotated samples in the database are
#'   exported.
#' @return Named list with counts: \code{success}, \code{failed}
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' result <- export_all_db_to_mat(db_path, "/data/manual")
#' cat(result$success, "exported,", result$failed, "failed\n")
#' }
export_all_db_to_mat <- function(db_path, output_folder, samples = NULL) {
  if (is.null(samples)) {
    samples <- list_annotated_samples_db(db_path)
  }

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

#' Import annotations from a PNG class folder into the SQLite database
#'
#' Scans a folder of PNG images organized in class-name subfolders (via
#' \code{\link{scan_png_class_folder}}) and imports the annotations into the
#' database. An optional \code{class_mapping} named vector remaps class names
#' before saving.
#'
#' @param png_folder Path to the top-level folder containing class subfolders
#' @param db_path Path to the SQLite database file
#' @param class2use Character vector of class names (preserves index order for
#'   .mat export)
#' @param class_mapping Optional named character vector mapping scanned class
#'   names to target class names. Names are the source classes, values are the
#'   target classes. Classes not in the mapping are kept as-is.
#' @param annotator Annotator name (defaults to \code{"imported"})
#' @return Named list with counts: \code{success}, \code{failed}
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' class2use <- c("Diatom", "Dinoflagellate", "Ciliate")
#' result <- import_png_folder_to_db(
#'   "/data/png_export", db_path, class2use,
#'   class_mapping = c("OldName" = "NewName"),
#'   annotator = "Jane"
#' )
#' cat(result$success, "imported,", result$failed, "failed\n")
#' }
import_png_folder_to_db <- function(png_folder, db_path, class2use,
                                     class_mapping = NULL,
                                     annotator = "imported") {
  scan_result <- scan_png_class_folder(png_folder)

  counts <- list(success = 0L, failed = 0L)

  if (nrow(scan_result$annotations) == 0) {
    return(counts)
  }

  annotations <- scan_result$annotations

  # Apply class mapping if provided
  if (!is.null(class_mapping) && length(class_mapping) > 0) {
    mapped <- class_mapping[annotations$class_name]
    has_mapping <- !is.na(mapped)
    annotations$class_name[has_mapping] <- mapped[has_mapping]
  }

  # Group by sample_name and save each sample

  sample_names <- unique(annotations$sample_name)

  for (sn in sample_names) {
    sample_rows <- annotations[annotations$sample_name == sn, ]

    classifications <- data.frame(
      file_name = sample_rows$file_name,
      class_name = sample_rows$class_name,
      stringsAsFactors = FALSE
    )

    ok <- save_annotations_db(db_path, sn, classifications, class2use,
                              annotator)
    if (isTRUE(ok)) {
      counts$success <- counts$success + 1L
    } else {
      counts$failed <- counts$failed + 1L
    }
  }

  counts
}

#' Backfill missing ROIs as "unclassified" in the database
#'
#' After a partial import (e.g. \code{\link{import_png_folder_to_db}} with only
#' a few selected taxa per sample), the database holds annotations for just the
#' imported ROIs. This helper reads each sample's complete ROI list from its
#' \code{.adc} file and inserts the ROIs that are not yet in the database as
#' \code{"unclassified"}, so the full sample is represented. Existing
#' annotations are never modified.
#'
#' Only ROIs with a real image (non-zero width and height) are added. Inserted
#' rows are marked \code{is_manual = 0} (not yet reviewed).
#'
#' @param db_path Path to the SQLite database file
#' @param roi_folder Base ROI folder path, following the standard IFCB folder
#'   structure (\code{roi_folder/YYYY/DYYYYMMDD/sample_name.adc}). Used to
#'   locate each sample's \code{.adc} file via \code{\link{get_sample_paths}}.
#' @param samples Optional character vector of sample names to backfill. When
#'   \code{NULL} (the default), all annotated samples in the database are used.
#' @param class_name Class name to assign to the missing ROIs. Default
#'   \code{"unclassified"}.
#' @param annotator Annotator name recorded for the inserted rows. Default
#'   \code{"imported"}.
#' @return Named list with counts: \code{added} (ROIs inserted), \code{samples}
#'   (samples that received at least one new ROI), \code{skipped} (samples with
#'   no reachable \code{.adc} file).
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' fill_unclassified_db(db_path, "/data/ifcb/raw")
#' }
fill_unclassified_db <- function(db_path, roi_folder, samples = NULL,
                                 class_name = "unclassified",
                                 annotator = "imported") {
  counts <- list(added = 0L, samples = 0L, skipped = 0L)

  if (!file.exists(db_path)) {
    warning("Database file does not exist: ", db_path)
    return(counts)
  }

  if (is.null(samples)) {
    samples <- list_annotated_samples_db(db_path)
  }

  if (length(samples) == 0) {
    return(counts)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  for (sn in samples) {
    paths <- get_sample_paths(sn, roi_folder)
    if (!file.exists(paths$adc_path)) {
      warning("ADC file not found for ", sn, ": ", paths$adc_path)
      counts$skipped <- counts$skipped + 1L
      next
    }

    # Complete ROI list from the raw data; keep only ROIs with a real image
    dims <- read_roi_dimensions(paths$adc_path)
    all_roi <- dims$roi_number[dims$width > 0 & dims$height > 0]

    have <- dbGetQuery(con,
      "SELECT roi_number FROM annotations WHERE sample_name = ?",
      params = list(sn))$roi_number

    missing <- setdiff(all_roi, have)
    if (length(missing) == 0) {
      next
    }

    new_rows <- data.frame(
      sample_name = sn,
      roi_number = as.integer(missing),
      class_name = class_name,
      annotator = annotator,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      is_manual = 0L,
      stringsAsFactors = FALSE
    )

    ok <- tryCatch({
      dbWriteTable(con, "annotations", new_rows, append = TRUE)
      TRUE
    }, error = function(e) {
      warning("Failed to backfill ", sn, ": ", e$message)
      FALSE
    })

    if (isTRUE(ok)) {
      counts$added <- counts$added + nrow(new_rows)
      counts$samples <- counts$samples + 1L
    }
  }

  counts
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
#' @param samples Optional character vector of sample names to export. When
#'   \code{NULL} (the default), all annotated samples in the database are
#'   exported.
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
                                 skip_class = NULL, samples = NULL) {
  if (is.null(samples)) {
    samples <- list_annotated_samples_db(db_path)
  }

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

# Parse IFCB PNG filename into date/time/ROI components
#
# @param file_name Basename of IFCB PNG file
# @return List with `object_id`, `object_date`, `object_time`, and
#   `object_roi_number` (integer-like character), or `NA_character_` values
#   when parsing fails.
# @keywords internal
parse_ifcb_png_name <- function(file_name) {
  object_id <- tools::file_path_sans_ext(file_name)
  parsed <- tryCatch(
    iRfcb::ifcb_convert_filenames(file_name),
    error = function(e) NULL
  )

  if (!is.null(parsed) && nrow(parsed) > 0) {
    timestamp <- suppressWarnings(as.POSIXct(parsed$timestamp[1], tz = "UTC"))
    object_date <- if (!is.na(timestamp)) {
      format(timestamp, "%Y%m%d", tz = "UTC")
    } else {
      NA_character_
    }
    object_time <- if (!is.na(timestamp)) {
      format(timestamp, "%H%M%S", tz = "UTC")
    } else {
      NA_character_
    }
    roi_number <- if ("roi" %in% names(parsed)) {
      suppressWarnings(as.integer(parsed$roi[1]))
    } else {
      NA_integer_
    }

    return(list(
      object_id = object_id,
      object_date = object_date,
      object_time = object_time,
      object_roi_number = if (is.na(roi_number)) NA_character_ else as.character(roi_number)
    ))
  }

  # Fallback parser for unrecognized names (keeps previous tolerant behavior).
  m <- regexec("^D([0-9]{8})T([0-9]{6})_.*_([0-9]+)\\.png$", file_name)
  hit <- regmatches(file_name, m)[[1]]
  if (length(hit) == 4) {
    roi_number <- suppressWarnings(as.integer(hit[4]))
    return(list(
      object_id = object_id,
      object_date = hit[2],
      object_time = hit[3],
      object_roi_number = if (is.na(roi_number)) NA_character_ else as.character(roi_number)
    ))
  }

  list(
    object_id = object_id,
    object_date = NA_character_,
    object_time = NA_character_,
    object_roi_number = NA_character_
  )
}

# Create per-class EcoTaxa inventory text files for exported PNG folders
#
# Writes one tab-separated `.txt` file inside each class subdirectory with
# column headers and the iRfcb type row (`[t]`/`[f]`), plus one row per PNG.
#
# @param png_folder Path to top-level folder containing class subdirectories
# @param db_path Path to SQLite database for annotation metadata
# @param txt_file_name Name of inventory file to write in each class folder
# @return Integer count of written inventory files
# @keywords internal
create_ecotaxa_inventory_txt <- function(png_folder, db_path,
                                         txt_file_name = NULL) {
  if (!dir.exists(png_folder) || !file.exists(db_path)) {
    return(0L)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  meta <- dbGetQuery(con, "
    SELECT
      printf('%s_%05d.png', a.sample_name, a.roi_number) AS img_file_name,
      a.class_name AS object_annotation_category,
      a.annotator AS object_annotation_person_name,
      t.aphia_id AS object_aphiaid,
      t.scientific_name AS object_annotation_hierarchy
    FROM annotations a
    LEFT JOIN class_taxonomy t
      ON a.class_name = t.class_name
  ")

  if (!nrow(meta)) {
    return(0L)
  }

  example <- ifcb_get_ecotaxa_example()
  cols <- c(
    "img_file_name",
    "object_id",
    "object_date",
    "object_time",
    "object_annotation_status",
    "object_annotation_person_name",
    "object_annotation_category",
    "object_aphiaid",
    "object_annotation_hierarchy",
    "object_roi_number"
  )

  type_row <- example[1, cols, drop = FALSE]
  class_dirs <- list.dirs(png_folder, recursive = FALSE, full.names = TRUE)
  written <- 0L

  for (dir_path in class_dirs) {
    png_files <- list.files(dir_path, pattern = "\\.png$", full.names = FALSE)
    if (!length(png_files)) {
      next
    }

    png_files <- sort(png_files)
    rows <- data.frame(
      img_file_name = png_files,
      object_id = character(length(png_files)),
      object_date = character(length(png_files)),
      object_time = character(length(png_files)),
      object_annotation_status = rep("validated", length(png_files)),
      object_annotation_person_name = rep(NA_character_, length(png_files)),
      object_annotation_category = rep(NA_character_, length(png_files)),
      object_aphiaid = rep(NA_character_, length(png_files)),
      object_annotation_hierarchy = rep(NA_character_, length(png_files)),
      object_roi_number = rep(NA_character_, length(png_files)),
      stringsAsFactors = FALSE
    )

    parsed <- lapply(png_files, parse_ifcb_png_name)
    rows$object_id <- vapply(parsed, `[[`, character(1), "object_id")
    rows$object_date <- vapply(parsed, `[[`, character(1), "object_date")
    rows$object_time <- vapply(parsed, `[[`, character(1), "object_time")
    rows$object_roi_number <- vapply(parsed, `[[`, character(1), "object_roi_number")

    idx <- match(rows$img_file_name, meta$img_file_name)
    has_meta <- !is.na(idx)

    rows$object_annotation_person_name[has_meta] <- meta$object_annotation_person_name[idx[has_meta]]
    rows$object_annotation_category[has_meta] <- meta$object_annotation_category[idx[has_meta]]
    rows$object_aphiaid[has_meta] <- meta$object_aphiaid[idx[has_meta]]
    rows$object_annotation_hierarchy[has_meta] <- meta$object_annotation_hierarchy[idx[has_meta]]

    out <- rbind(type_row, rows[, cols, drop = FALSE])
    file_name <- if (!is.null(txt_file_name) && nzchar(txt_file_name)) {
      txt_file_name
    } else {
      paste0("ecotaxa_", basename(dir_path), ".tsv")
    }
    out_path <- file.path(dir_path, file_name)
    utils::write.table(
      out,
      file = out_path,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      na = ""
    )
    written <- written + 1L
  }

  written
}

#' Bulk export all annotated samples from SQLite to EcoTaxa-ready ZIP
#'
#' Exports annotated samples to class-organized PNG folders, writes one
#' inventory \code{.txt} file per class folder, and then zips the result using
#' \code{iRfcb::ifcb_zip_pngs(include_txt = TRUE)}.
#'
#' @param db_path Path to the SQLite database file
#' @param zip_path Full output path for the resulting ZIP archive
#' @param roi_path_map Named list mapping sample names to \code{.roi} file
#'   paths. Samples without an entry are skipped.
#' @param skip_class Character vector of class names to exclude from export
#'   (e.g. \code{"unclassified"}). Default \code{NULL} exports all classes.
#' @param readme_file Optional README markdown file included in ZIP. Defaults to
#'   \code{system.file("exdata/README-template.md", package = "iRfcb")}.
#' @return Named list with counts: \code{success}, \code{failed},
#'   \code{skipped}, \code{inventory_files}, and \code{zip_path}.
#' @export
export_all_db_to_zip <- function(db_path, zip_path, roi_path_map,
                                 skip_class = NULL,
                                 readme_file = system.file("exdata/README-template.md",
                                                           package = "iRfcb")) {
  if (!nzchar(zip_path)) {
    warning("zip_path is empty")
    return(list(success = 0L, failed = 0L, skipped = 0L,
                inventory_files = 0L, zip_path = zip_path))
  }

  out_dir <- dirname(zip_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  temp_png <- tempfile("classipyr_zip_export_")
  dir.create(temp_png, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_png, recursive = TRUE, force = TRUE), add = TRUE)

  counts <- export_all_db_to_png(
    db_path = db_path,
    png_folder = temp_png,
    roi_path_map = roi_path_map,
    skip_class = skip_class
  )

  inventory_files <- create_ecotaxa_inventory_txt(temp_png, db_path)
  has_png <- length(list.files(temp_png, pattern = "\\.png$", recursive = TRUE)) > 0
  if (!has_png) {
    warning("No PNG files were exported; ZIP archive was not created")
    return(list(success = counts$success, failed = counts$failed,
                skipped = counts$skipped, inventory_files = inventory_files,
                zip_path = zip_path))
  }

  zip_ok <- tryCatch({
    ifcb_zip_pngs(
      png_folder = temp_png,
      zip_filename = zip_path,
      readme_file = readme_file,
      include_txt = TRUE,
      quiet = TRUE
    )
    TRUE
  }, error = function(e) {
    warning("Failed to create ZIP archive: ", e$message)
    FALSE
  })

  if (!isTRUE(zip_ok)) {
    return(list(success = counts$success, failed = counts$failed + 1L,
                skipped = counts$skipped, inventory_files = inventory_files,
                zip_path = zip_path))
  }

  list(
    success = counts$success,
    failed = counts$failed,
    skipped = counts$skipped,
    inventory_files = inventory_files,
    zip_path = zip_path
  )
}

#' List all classes with counts in the annotations database
#'
#' Queries the database for distinct class names and their annotation counts.
#' Useful for populating class review mode dropdowns. Optional filters restrict
#' results to annotations matching a given year, month, or instrument.
#'
#' @param db_path Path to the SQLite database file
#' @param year Optional year filter (e.g. \code{"2023"}). When not \code{"all"}
#'   or \code{NULL}, restricts to sample names starting with \code{DYYYY}.
#' @param month Optional month filter (e.g. \code{"03"}). When not \code{"all"}
#'   or \code{NULL}, restricts to sample names with that month at positions 6-7.
#' @param instrument Optional instrument filter (e.g. \code{"IFCB134"}). When
#'   not \code{"all"} or \code{NULL}, restricts to sample names ending with
#'   \code{_INSTRUMENT}.
#' @param annotator Optional annotator name filter (e.g. \code{"Jane"}). When
#'   not \code{"all"} or \code{NULL}, restricts to annotations by that annotator.
#' @return Data frame with columns \code{class_name} and \code{count}, ordered
#'   alphabetically by class name. Returns an empty data frame if the database
#'   does not exist or has no annotations.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' classes <- list_classes_db(db_path)
#' classes_2023 <- list_classes_db(db_path, year = "2023")
#' }
list_classes_db <- function(db_path, year = NULL, month = NULL,
                            instrument = NULL, annotator = NULL) {
  empty <- data.frame(class_name = character(), count = integer(),
                      stringsAsFactors = FALSE)

  if (!file.exists(db_path)) {
    return(empty)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
  if (!"annotations" %in% tables$name) {
    return(empty)
  }

  where <- build_sample_filter_clause(year, month, instrument,
                                       annotator = annotator)

  sql <- paste0(
    "SELECT class_name, COUNT(*) AS count FROM annotations",
    where$clause,
    " GROUP BY class_name ORDER BY class_name"
  )

  if (length(where$params) > 0) {
    dbGetQuery(con, sql, params = where$params)
  } else {
    dbGetQuery(con, sql)
  }
}

#' Load all annotations for a specific class from the database
#'
#' Returns every annotation matching \code{class_name}, with a computed
#' \code{file_name} column for gallery display. Optional filters restrict
#' results by year, month, or instrument.
#'
#' @param db_path Path to the SQLite database file
#' @param class_name Class name to load
#' @param year Optional year filter (e.g. \code{"2023"})
#' @param month Optional month filter (e.g. \code{"03"})
#' @param instrument Optional instrument filter (e.g. \code{"IFCB134"})
#' @param annotator Optional annotator name filter (e.g. \code{"Jane"})
#' @return Data frame with columns \code{sample_name}, \code{roi_number},
#'   \code{class_name}, and \code{file_name}. Returns \code{NULL} if no
#'   annotations match.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' diatoms <- load_class_annotations_db(db_path, "Diatom")
#' diatoms_2023 <- load_class_annotations_db(db_path, "Diatom", year = "2023")
#' }
load_class_annotations_db <- function(db_path, class_name, year = NULL,
                                      month = NULL, instrument = NULL,
                                      annotator = NULL) {
  if (!file.exists(db_path)) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  where <- build_sample_filter_clause(year, month, instrument,
                                       annotator = annotator)
  params <- c(list(class_name), where$params)

  rows <- dbGetQuery(con, paste0(
    "SELECT sample_name, roi_number, class_name FROM annotations WHERE class_name = ?",
    if (nzchar(where$clause)) gsub("^ WHERE ", " AND ", where$clause),
    " ORDER BY sample_name, roi_number"
  ), params = params)

  if (nrow(rows) == 0) {
    return(NULL)
  }

  rows$file_name <- sprintf("%s_%05d.png", rows$sample_name, rows$roi_number)
  rows
}

#' Save class review changes to the database
#'
#' Performs row-level UPDATEs for reclassified images identified during class
#' review mode. Only the changed rows are updated; other annotations for the
#' same samples are left untouched.
#'
#' @param db_path Path to the SQLite database file
#' @param changes_df Data frame with columns \code{sample_name},
#'   \code{roi_number}, and \code{new_class_name}
#' @param annotator Annotator name
#' @return Integer count of rows updated
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' changes <- data.frame(
#'   sample_name = "D20230101T120000_IFCB134",
#'   roi_number = 5L,
#'   new_class_name = "Ciliate"
#' )
#' save_class_review_changes_db(db_path, changes, "Jane")
#' }
save_class_review_changes_db <- function(db_path, changes_df, annotator) {
  if (is.null(changes_df) || nrow(changes_df) == 0) {
    return(0L)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  init_db_schema(con)

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  updated <- 0L

  tryCatch({
    dbExecute(con, "BEGIN TRANSACTION")

    for (i in seq_len(nrow(changes_df))) {
      n <- dbExecute(con,
        "UPDATE annotations SET class_name = ?, annotator = ?, timestamp = ?, is_manual = 1 WHERE sample_name = ? AND roi_number = ?",
        params = list(
          changes_df$new_class_name[i],
          annotator,
          timestamp,
          changes_df$sample_name[i],
          changes_df$roi_number[i]
        )
      )
      updated <- updated + as.integer(n)
    }

    dbExecute(con, "COMMIT")
    updated
  }, error = function(e) {
    tryCatch(dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    warning("Failed to save class review changes: ", e$message)
    0L
  })
}

#' List distinct years, months, and instruments from annotations
#'
#' Extracts metadata from sample names in the annotations table for use as
#' filter options. Sample names follow the IFCB naming convention
#' \code{DYYYYMMDDTHHMMSS_INSTRUMENT}.
#'
#' @param db_path Path to the SQLite database file
#' @return A list with character vectors: \code{years}, \code{months},
#'   \code{instruments}, and \code{annotators}. Returns empty vectors if the
#'   database does not exist or has no annotations.
#' @export
#' @examples
#' \dontrun{
#' db_path <- get_db_path("/data/manual")
#' meta <- list_annotation_metadata_db(db_path)
#' meta$years       # e.g. c("2022", "2023")
#' meta$months      # e.g. c("01", "06", "12")
#' meta$instruments # e.g. c("IFCB134", "IFCB135")
#' meta$annotators  # e.g. c("Jane", "imported")
#' }
list_annotation_metadata_db <- function(db_path) {
  empty <- list(years = character(), months = character(),
                instruments = character(), annotators = character())

  if (!file.exists(db_path)) {
    return(empty)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
  if (!"annotations" %in% tables$name) {
    return(empty)
  }

  samples <- dbGetQuery(con,
    "SELECT DISTINCT sample_name FROM annotations"
  )$sample_name

  annotators <- sort(dbGetQuery(con,
    "SELECT DISTINCT annotator FROM annotations WHERE annotator IS NOT NULL"
  )$annotator)

  if (length(samples) == 0) {
    return(list(years = character(), months = character(),
                instruments = character(), annotators = annotators))
  }

  years <- sort(unique(substr(samples, 2, 5)))
  months <- sort(unique(substr(samples, 6, 7)))
  instruments <- sort(unique(sub(".*_", "", samples)))

  list(years = years, months = months, instruments = instruments,
       annotators = annotators)
}

# Build WHERE clause fragments for sample_name filtering
#
# @param year Year string or "all"/NULL
# @param month Month string or "all"/NULL
# @param instrument Instrument string or "all"/NULL
# @return List with `clause` (SQL fragment starting with " WHERE " or "") and
#   `params` (list of bind values)
# @keywords internal
build_sample_filter_clause <- function(year = NULL, month = NULL,
                                       instrument = NULL,
                                       annotator = NULL) {
  conditions <- character()
  params <- list()

  if (!is.null(year) && year != "all") {
    conditions <- c(conditions, "sample_name LIKE ?")
    params <- c(params, list(paste0("D", year, "%")))
  }

  if (!is.null(month) && month != "all") {
    conditions <- c(conditions, "sample_name LIKE ?")
    params <- c(params, list(paste0("D____", month, "%")))
  }

  if (!is.null(instrument) && instrument != "all") {
    conditions <- c(conditions, "sample_name LIKE ?")
    params <- c(params, list(paste0("%_", instrument)))
  }

  if (!is.null(annotator) && annotator != "all") {
    conditions <- c(conditions, "annotator = ?")
    params <- c(params, list(annotator))
  }

  clause <- if (length(conditions) > 0) {
    paste0(" WHERE ", paste(conditions, collapse = " AND "))
  } else {
    ""
  }

  list(clause = clause, params = params)
}
