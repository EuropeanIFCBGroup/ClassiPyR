# Index cache of class-score files for the threshold review app
#
# Sampling ROIs across thousands of class-score files needs the winning class
# and score of every ROI at hand. Reading the HDF5 files each time is far too
# slow, so they are read once into a SQLite cache. The cache holds nothing that
# cannot be read again from the files: deleting it only costs a rebuild.

# Bump when the tables below change shape; an index written with another
# version is dropped and rebuilt rather than migrated.
THR_INDEX_SCHEMA_VERSION <- "1"

# Same pattern the main app uses to find classifier output
THR_H5_FILE_PATTERN <- "_class.*\\.h5$"

THR_LEGACY_MESSAGE <- paste(
  "No 'transform' attribute: the file was scored before ifcb-classify stopped",
  "applying training augmentation at inference, so its scores do not match",
  "current inference. Re-run the classification to review this sample."
)

#' Create the index cache tables
#'
#' Safe to call on a database that already has them.
#'
#' @param con A DBI connection to the index SQLite file.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_init_index_schema <- function(con) {
  statements <- c(
    "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS models (
       model_id TEXT PRIMARY KEY,
       classifier_name TEXT,
       checkpoint_sha256 TEXT,
       transform TEXT NOT NULL,
       class_labels_json TEXT NOT NULL
     )",
    "CREATE TABLE IF NOT EXISTS files (
       file_id INTEGER PRIMARY KEY,
       h5_path TEXT NOT NULL UNIQUE,
       sample_name TEXT,
       size REAL, mtime REAL,
       model_id TEXT,
       n_rois INTEGER,
       thresholds_json TEXT,
       status TEXT NOT NULL,
       message TEXT
     )",
    "CREATE TABLE IF NOT EXISTS rois (
       file_id INTEGER NOT NULL,
       roi_number INTEGER NOT NULL,
       class_idx INTEGER NOT NULL,
       score REAL NOT NULL,
       PRIMARY KEY (file_id, roi_number)
     ) WITHOUT ROWID",
    "CREATE INDEX IF NOT EXISTS rois_class_score ON rois (class_idx, score)",
    "CREATE INDEX IF NOT EXISTS files_model ON files (model_id, status)"
  )
  for (statement in statements) {
    DBI::dbExecute(con, statement)
  }
  DBI::dbExecute(
    con, "INSERT OR IGNORE INTO meta (key, value) VALUES ('schema_version', ?)",
    params = list(THR_INDEX_SCHEMA_VERSION)
  )
  invisible(NULL)
}

#' Make an index database ready for a build
#'
#' Drops the index tables when they were written with another schema version
#' (or have lost their version), then creates whatever is missing.
#'
#' @param con A DBI connection to the index SQLite file.
#' @return Invisibly, \code{TRUE} when an outdated index was dropped.
#' @noRd
thr_prepare_index <- function(con) {
  index_tables <- c("rois", "files", "models", "meta")
  present <- intersect(index_tables, DBI::dbListTables(con))
  stored <- character()
  if ("meta" %in% present) {
    stored <- DBI::dbGetQuery(
      con, "SELECT value FROM meta WHERE key = 'schema_version'"
    )$value
  }
  outdated <- length(present) > 0 && !identical(stored, THR_INDEX_SCHEMA_VERSION)
  if (outdated) {
    for (table in present) {
      DBI::dbExecute(con, paste("DROP TABLE", table))
    }
  }
  thr_init_index_schema(con)
  invisible(outdated)
}

#' Describe class-score files on disk
#'
#' @param paths Character vector of file paths.
#' @return Data frame with \code{h5_path} (normalised, forward slashes),
#'   \code{sample_name}, \code{size} and \code{mtime} (seconds, numeric), one
#'   row per distinct path, sorted by path.
#' @noRd
thr_stat_files <- function(paths) {
  paths <- unique(enc2utf8(normalizePath(paths, winslash = "/", mustWork = FALSE)))
  # Radix order is the C-locale order, the same on every machine and the same
  # as SQLite's default collation, which decides which duplicate is kept.
  paths <- paths[order(paths, method = "radix")]
  info <- file.info(paths, extra_cols = FALSE)
  data.frame(
    h5_path = paths,
    sample_name = sub(THR_H5_FILE_PATTERN, "", basename(paths)),
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime),
    stringsAsFactors = FALSE
  )
}

#' Find the class-score files in a folder
#'
#' @param h5_folder Folder to search, recursively, for \code{*_class*.h5}
#'   files.
#' @return Data frame with \code{h5_path}, \code{sample_name} (the file name
#'   without its \code{_class*.h5} suffix), \code{size} and \code{mtime},
#'   sorted by path.
#' @noRd
thr_scan_h5_folder <- function(h5_folder) {
  if (!thr_is_single_string(h5_folder)) {
    stop("`h5_folder` must be a single folder path.", call. = FALSE)
  }
  if (!dir.exists(h5_folder)) {
    stop("Class-score folder not found: ", h5_folder,
         ". Point to the folder holding the *_class.h5 files.", call. = FALSE)
  }
  thr_stat_files(list.files(h5_folder, pattern = THR_H5_FILE_PATTERN,
                            recursive = TRUE, full.names = TRUE))
}

#' Compare scanned files with the index
#'
#' @param con A DBI connection to the index SQLite file.
#' @param scanned Data frame from \code{thr_scan_h5_folder()}.
#' @return A list with \code{new} (scanned rows the index does not know),
#'   \code{changed} (scanned rows whose size or modification time differs from
#'   the index, or that the index had marked missing) and \code{missing}
#'   (paths the index knows that were not scanned and are no longer on disk).
#' @noRd
thr_stale_files <- function(con, scanned) {
  known <- DBI::dbGetQuery(con, "SELECT h5_path, size, mtime, status FROM files")
  at <- match(scanned$h5_path, known$h5_path)
  is_new <- is.na(at)
  candidates <- scanned[!is_new, , drop = FALSE]
  indexed <- known[at[!is_new], , drop = FALSE]

  differs <- indexed$size != candidates$size | indexed$mtime != candidates$mtime |
    indexed$status == "missing"
  # A size or time that could not be read or was never stored proves nothing
  differs[is.na(differs)] <- TRUE

  # Files indexed from another folder are left alone while they exist: a
  # frozen sampling frame may still point at them.
  unseen <- known$h5_path[!(known$h5_path %in% scanned$h5_path) &
                            known$status != "missing"]
  list(
    new = thr_reset_rownames(scanned[is_new, , drop = FALSE]),
    changed = thr_reset_rownames(candidates[differs, , drop = FALSE]),
    missing = unseen[!file.exists(unseen)]
  )
}

#' Drop the row names a subset leaves behind
#'
#' @param df A data frame.
#' @return The data frame with default row names.
#' @noRd
thr_reset_rownames <- function(df) {
  rownames(df) <- NULL
  df
}

#' Build or update the class-score index for threshold review
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads the winning class and score of every ROI from the ifcb-classify
#' class-score files (`*_class.h5`) under a folder into a SQLite cache, which
#' the threshold review app samples from. Only files that are new or whose
#' size or modification time changed are read, so repeat calls are cheap, and
#' an interrupted build picks up where it stopped.
#'
#' @details
#' Each file ends up with one status:
#'
#' * `ok`: indexed.
#' * `legacy`: the file has no `transform` attribute. It was scored before
#'   ifcb-classify stopped applying training augmentation at inference, so its
#'   scores do not match current inference and it is refused. Re-run the
#'   classification to review such a sample.
#' * `duplicate`: another file of the same model for the same sample exists;
#'   the first by path is used.
#' * `failed`: the file could not be read, is inconsistent, or is not named
#'   after an IFCB sample. The message says why. A failed file is read again
#'   only once it has changed on disk.
#' * `missing`: the file was indexed and is no longer on disk.
#'
#' A file that cannot be used never stops the build.
#'
#' Files are grouped by the model that scored them: the checkpoint hash (or
#' the classifier name, when the checkpoint could not be hashed), the
#' transform and the class labels. Models are never pooled.
#'
#' The index is a cache. It can be deleted at any time and is rebuilt from the
#' class-score files; it is also rebuilt when it was written by a version of
#' ClassiPyR with another index layout. Files indexed from a different folder
#' stay in the index for as long as they exist on disk.
#'
#' @param h5_folder Folder searched recursively for `*_class*.h5` files.
#' @param index_path Path to the index SQLite file, usually
#'   `threshold_index.sqlite` in the review folder. Created if needed.
#' @param reader Function that takes a file path and returns the top-1 summary
#'   of a class-score file. The default reads HDF5 and requires the
#'   \pkg{hdf5r} package; another reader is mainly useful for testing.
#' @param progress `NULL`, or a function called as `progress(done, total)`
#'   after each file that had to be read.
#' @param batch_size Number of files read between commits. An interrupted
#'   build loses at most one batch.
#' @return Invisibly, a list with `n_scanned` (class-score files found),
#'   `n_read` (files read in this call), `n_skipped` (files found but
#'   unchanged, so not read), `n_ok` (usable files in the index afterwards)
#'   and `failures` (a data frame with `h5_path`, `status` and `message` for
#'   every file in the index that is not usable).
#' @export
#' @examples
#' \dontrun{
#' review_dir <- "/data/ifcb/threshold_review"
#' index_path <- file.path(review_dir, "threshold_index.sqlite")
#'
#' result <- build_threshold_index("/data/ifcb/classified", index_path)
#' result$n_ok
#' result$failures
#'
#' # With a progress message every file
#' build_threshold_index(
#'   "/data/ifcb/classified", index_path,
#'   progress = function(done, total) message(done, " / ", total)
#' )
#' }
#' @md
build_threshold_index <- function(h5_folder, index_path,
                                  reader = read_class_scores_summary,
                                  progress = NULL, batch_size = 50) {
  thr_check_index_args(index_path, reader, progress, batch_size)
  scanned <- thr_scan_h5_folder(h5_folder)

  dir.create(dirname(index_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_prepare_index(con)

  stale <- thr_stale_files(con, scanned)
  thr_mark_missing(con, stale$missing)
  todo <- rbind(stale$new, stale$changed)
  todo <- todo[order(todo$h5_path, method = "radix"), , drop = FALSE]
  thr_index_files(con, todo, reader, progress, batch_size)
  promoted <- thr_promote_duplicates(con, reader, batch_size)

  read_paths <- c(todo$h5_path, promoted)
  invisible(list(
    n_scanned = nrow(scanned),
    n_read = length(read_paths),
    n_ok = DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM files WHERE status = 'ok'"
    )$n,
    n_skipped = sum(!(scanned$h5_path %in% read_paths)),
    failures = thr_failures_query(con)
  ))
}

#' Validate the arguments of build_threshold_index()
#'
#' @inheritParams build_threshold_index
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_index_args <- function(index_path, reader, progress, batch_size) {
  if (!thr_is_single_string(index_path)) {
    stop("`index_path` must be a single file path, e.g. ",
         "file.path(review_dir, \"", THR_INDEX_DB_FILE, "\").", call. = FALSE)
  }
  if (!is.function(reader)) {
    stop("`reader` must be a function that takes a file path.", call. = FALSE)
  }
  if (!is.null(progress) && !is.function(progress)) {
    stop("`progress` must be NULL or a function(done, total).", call. = FALSE)
  }
  valid_size <- is.numeric(batch_size) && length(batch_size) == 1 &&
    !is.na(batch_size) && batch_size >= 1 && batch_size == round(batch_size)
  if (!valid_size) {
    stop("`batch_size` must be a single whole number of at least 1.",
         call. = FALSE)
  }
  # Without this every file would be recorded as failed, and stay failed
  if (identical(reader, read_class_scores_summary)) {
    thr_require_hdf5r()
  }
  invisible(NULL)
}

#' Mark indexed files that are gone from disk
#'
#' @param con A DBI connection to the index SQLite file.
#' @param paths Character vector of indexed paths that no longer exist.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_mark_missing <- function(con, paths) {
  if (length(paths) == 0) {
    return(invisible(NULL))
  }
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(
      con,
      "DELETE FROM rois WHERE file_id IN
         (SELECT file_id FROM files WHERE h5_path = ?)",
      params = list(paths)
    )
    DBI::dbExecute(
      con,
      "UPDATE files SET status = 'missing',
         message = 'The file is no longer on disk.' WHERE h5_path = ?",
      params = list(paths)
    )
  })
  invisible(NULL)
}

#' Read files into the index, one transaction per batch
#'
#' @param con A DBI connection to the index SQLite file.
#' @param todo Data frame of files to read (\code{thr_stat_files()} columns),
#'   sorted by path.
#' @inheritParams build_threshold_index
#' @return \code{NULL}, invisibly.
#' @noRd
thr_index_files <- function(con, todo, reader, progress, batch_size) {
  total <- nrow(todo)
  batches <- split(seq_len(total), ceiling(seq_len(total) / batch_size))
  for (rows in batches) {
    entries <- vector("list", length(rows))
    for (k in seq_along(rows)) {
      entries[[k]] <- thr_read_index_entry(todo[rows[k], , drop = FALSE], reader)
      thr_report_progress(progress, rows[k], total)
    }
    thr_write_index_batch(con, entries)
  }
  invisible(NULL)
}

#' Report build progress without letting the callback stop the build
#'
#' @param progress \code{NULL} or a function(done, total).
#' @param done Number of files read so far.
#' @param total Number of files to read.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_report_progress <- function(progress, done, total) {
  if (is.function(progress)) {
    tryCatch(progress(done, total), error = function(e) NULL)
  }
  invisible(NULL)
}

#' Re-read duplicates left without a kept file
#'
#' When the file that was kept for a sample goes missing or stops being
#' usable, its duplicates have no ROI rows to fall back on, although their
#' size and modification time are unchanged. They are read again so the first
#' of them by path takes over.
#'
#' @param con A DBI connection to the index SQLite file.
#' @inheritParams build_threshold_index
#' @return Character vector of the paths that were read again.
#' @noRd
thr_promote_duplicates <- function(con, reader, batch_size) {
  orphans <- DBI::dbGetQuery(con, "
    SELECT d.h5_path FROM files d
    WHERE d.status = 'duplicate' AND NOT EXISTS (
      SELECT 1 FROM files k
      WHERE k.status = 'ok' AND k.model_id = d.model_id
        AND k.sample_name = d.sample_name)")$h5_path
  orphans <- orphans[file.exists(orphans)]
  if (length(orphans) == 0) {
    return(character())
  }
  todo <- thr_stat_files(orphans)
  thr_index_files(con, todo, reader, progress = NULL, batch_size = batch_size)
  todo$h5_path
}

#' Read one file into an index entry
#'
#' Never fails: a file that cannot be used becomes an entry with status
#' \code{failed} and the reason as its message.
#'
#' @param file One row of \code{thr_stat_files()} output.
#' @param reader Function returning the top-1 summary of a class-score file.
#' @return A list with \code{file} (one-row data frame for the \code{files}
#'   table), \code{model} (one-row data frame for \code{models}, or
#'   \code{NULL}) and \code{rois} (data frame for \code{rois}, or \code{NULL}).
#' @noRd
thr_read_index_entry <- function(file, reader) {
  if (!is_valid_sample_name(file$sample_name)) {
    return(thr_index_entry(file, "failed", message = paste0(
      "File name does not start with an IFCB sample name ",
      "(DYYYYMMDDTHHMMSS_IFCBNNN): got '", file$sample_name, "'."
    )))
  }
  # Only errors are caught: an interrupt must still stop the build
  tryCatch(
    thr_summary_entry(file, reader(file$h5_path)),
    error = function(e) thr_index_entry(file, "failed", message = conditionMessage(e))
  )
}

#' Turn a reader summary into an index entry
#'
#' @param file One row of \code{thr_stat_files()} output.
#' @param summary A list as returned by \code{read_class_scores_summary()}.
#' @return An index entry, see \code{thr_read_index_entry()}.
#' @noRd
thr_summary_entry <- function(file, summary) {
  thr_check_summary(summary)
  identity <- thr_model_identity(summary)
  n_rois <- length(summary$roi_numbers)
  if (identity$is_legacy) {
    return(thr_index_entry(file, "legacy", message = THR_LEGACY_MESSAGE,
                           n_rois = n_rois))
  }
  model <- data.frame(
    model_id = identity$model_id,
    classifier_name = identity$classifier_name,
    checkpoint_sha256 = identity$checkpoint_sha256,
    transform = identity$transform,
    class_labels_json = thr_labels_to_json(summary$class_labels),
    stringsAsFactors = FALSE
  )
  rois <- data.frame(
    roi_number = as.integer(summary$roi_numbers),
    class_idx = as.integer(summary$class_idx),
    score = as.numeric(summary$top_score)
  )
  thr_index_entry(file, "ok", model = model, rois = rois, n_rois = n_rois,
                  thresholds_json = thr_numbers_to_json(summary$thresholds))
}

#' Assemble an index entry
#'
#' @param file One row of \code{thr_stat_files()} output.
#' @param status File status.
#' @param message Why the file is not \code{ok}, or \code{NA}.
#' @param model One-row data frame for the \code{models} table, or \code{NULL}.
#' @param rois Data frame for the \code{rois} table, or \code{NULL}.
#' @param n_rois Number of ROIs in the file, or \code{NA} when unknown.
#' @param thresholds_json The file's thresholds as JSON text, or \code{NA}.
#' @return An index entry, see \code{thr_read_index_entry()}.
#' @noRd
thr_index_entry <- function(file, status, message = NA_character_, model = NULL,
                            rois = NULL, n_rois = NA_integer_,
                            thresholds_json = NA_character_) {
  row <- data.frame(
    h5_path = file$h5_path,
    sample_name = file$sample_name,
    size = file$size,
    mtime = file$mtime,
    model_id = if (is.null(model)) NA_character_ else model$model_id,
    n_rois = as.integer(n_rois),
    thresholds_json = thresholds_json,
    status = status,
    message = message,
    stringsAsFactors = FALSE
  )
  list(file = row, model = model, rois = rois)
}

#' Write one batch of index entries in a single transaction
#'
#' Old ROI rows of the files are removed first, so a changed file never keeps
#' rows from its previous content. Either the whole batch is stored or none of
#' it, which is what lets an interrupted build resume.
#'
#' @param con A DBI connection to the index SQLite file.
#' @param entries List of index entries from \code{thr_read_index_entry()}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_write_index_batch <- function(con, entries) {
  files <- do.call(rbind, lapply(entries, `[[`, "file"))
  DBI::dbWithTransaction(con, {
    resolved <- thr_resolve_duplicates(con, files)
    file_ids <- thr_upsert_files(con, resolved$files)
    DBI::dbExecute(con, "DELETE FROM rois WHERE file_id = ?",
                   params = list(file_ids))
    thr_demote_files(con, resolved$demote)
    keep <- resolved$files$status == "ok"
    thr_insert_models(con, entries[keep])
    thr_insert_rois(con, entries[keep], file_ids[keep])
  })
  invisible(NULL)
}

#' Decide which file is kept when a model scored a sample more than once
#'
#' The first file by path is kept. That can be a file of this batch or one
#' already in the index, in which case whichever sorts later is set aside.
#'
#' @param con A DBI connection to the index SQLite file.
#' @param files Data frame of \code{files} rows about to be written, sorted by
#'   path.
#' @return A list with \code{files} (the rows, with later duplicates given
#'   status \code{duplicate}) and \code{demote} (data frame of \code{h5_path}
#'   and \code{message} for indexed files that lose their place).
#' @noRd
thr_resolve_duplicates <- function(con, files) {
  demote <- data.frame(h5_path = character(), message = character(),
                       stringsAsFactors = FALSE)
  candidates <- which(files$status == "ok")
  if (length(candidates) == 0) {
    return(list(files = files, demote = demote))
  }
  indexed <- DBI::dbGetQuery(
    con,
    "SELECT h5_path, model_id, sample_name FROM files
     WHERE status = 'ok' AND sample_name = ?",
    params = list(unique(files$sample_name[candidates]))
  )
  # Rows of this batch are about to be replaced, so they do not compete
  indexed <- indexed[!(indexed$h5_path %in% files$h5_path), , drop = FALSE]
  kept <- stats::setNames(indexed$h5_path,
                          thr_sample_key(indexed$model_id, indexed$sample_name))

  for (i in candidates) {
    key <- thr_sample_key(files$model_id[i], files$sample_name[i])
    holder <- unname(kept[key])
    if (is.na(holder)) {
      kept[key] <- files$h5_path[i]
    } else if (thr_path_sorts_first(holder, files$h5_path[i])) {
      files$status[i] <- "duplicate"
      files$message[i] <- thr_duplicate_message(holder)
    } else {
      demote <- rbind(demote, data.frame(
        h5_path = holder, message = thr_duplicate_message(files$h5_path[i]),
        stringsAsFactors = FALSE
      ))
      kept[key] <- files$h5_path[i]
    }
  }
  list(files = files, demote = demote)
}

#' Key for one model's score of one sample
#'
#' @param model_id Character vector of model identifiers.
#' @param sample_name Character vector of sample names.
#' @return Character vector of keys.
#' @noRd
thr_sample_key <- function(model_id, sample_name) {
  paste(model_id, sample_name, sep = "\r")
}

#' Does one path sort before another?
#'
#' @param a,b Single paths.
#' @return \code{TRUE} when \code{a} comes first in C-locale order.
#' @noRd
thr_path_sorts_first <- function(a, b) {
  a != b && order(c(a, b), method = "radix")[1] == 1L
}

#' Message for a file set aside as a duplicate
#'
#' @param kept_path Path of the file that is kept instead.
#' @return One string.
#' @noRd
thr_duplicate_message <- function(kept_path) {
  paste0("The same model scored this sample in another file, which sorts ",
         "first and is used instead: ", kept_path)
}

#' Insert or update rows of the files table
#'
#' @param con A DBI connection to the index SQLite file.
#' @param files Data frame of \code{files} rows.
#' @return Integer vector of \code{file_id}, aligned with the rows. A file that
#'   was already indexed keeps its id.
#' @noRd
thr_upsert_files <- function(con, files) {
  columns <- c("h5_path", "sample_name", "size", "mtime", "model_id", "n_rois",
               "thresholds_json", "status", "message")
  DBI::dbExecute(
    con,
    "INSERT INTO files (h5_path, sample_name, size, mtime, model_id, n_rois,
                        thresholds_json, status, message)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT (h5_path) DO UPDATE SET
       sample_name = excluded.sample_name, size = excluded.size,
       mtime = excluded.mtime, model_id = excluded.model_id,
       n_rois = excluded.n_rois, thresholds_json = excluded.thresholds_json,
       status = excluded.status, message = excluded.message",
    params = unname(as.list(files[columns]))
  )
  ids <- DBI::dbGetQuery(con, "SELECT file_id, h5_path FROM files WHERE h5_path = ?",
                         params = list(files$h5_path))
  as.integer(ids$file_id[match(files$h5_path, ids$h5_path)])
}

#' Set indexed files aside as duplicates
#'
#' @param con A DBI connection to the index SQLite file.
#' @param demote Data frame with \code{h5_path} and \code{message}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_demote_files <- function(con, demote) {
  if (nrow(demote) == 0) {
    return(invisible(NULL))
  }
  DBI::dbExecute(
    con,
    "DELETE FROM rois WHERE file_id IN
       (SELECT file_id FROM files WHERE h5_path = ?)",
    params = list(demote$h5_path)
  )
  DBI::dbExecute(
    con, "UPDATE files SET status = 'duplicate', message = ? WHERE h5_path = ?",
    params = list(demote$message, demote$h5_path)
  )
  invisible(NULL)
}

#' Register the models of a batch of entries
#'
#' @param con A DBI connection to the index SQLite file.
#' @param entries List of index entries with status \code{ok}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_models <- function(con, entries) {
  if (length(entries) == 0) {
    return(invisible(NULL))
  }
  models <- do.call(rbind, lapply(entries, `[[`, "model"))
  models <- models[!duplicated(models$model_id), , drop = FALSE]
  DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO models (model_id, classifier_name, checkpoint_sha256,
                                   transform, class_labels_json)
     VALUES (?, ?, ?, ?, ?)",
    params = unname(as.list(models))
  )
  invisible(NULL)
}

#' Store the ROI rows of a batch of entries
#'
#' All rows of the batch go in with one statement: a file-by-file or
#' row-by-row insert would dominate the build at a million ROIs.
#'
#' @param con A DBI connection to the index SQLite file.
#' @param entries List of index entries with status \code{ok}.
#' @param file_ids Integer vector of \code{file_id}, aligned with
#'   \code{entries}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_rois <- function(con, entries, file_ids) {
  n_rois <- vapply(entries, function(entry) nrow(entry$rois), integer(1))
  if (sum(n_rois) == 0) {
    return(invisible(NULL))
  }
  rois <- do.call(rbind, lapply(entries, `[[`, "rois"))
  rois <- data.frame(file_id = rep(file_ids, n_rois), rois)
  DBI::dbAppendTable(con, "rois", rois)
  invisible(NULL)
}

#' Run a query against an index file that may not exist yet
#'
#' Asking about an index must not create an empty database as a side effect.
#'
#' @param index_path Path to the index SQLite file.
#' @param query Function of a DBI connection, called when the index exists.
#' @return The value of \code{query}, or \code{NULL} when there is no index.
#' @noRd
thr_with_index <- function(index_path, query) {
  if (!thr_is_single_string(index_path)) {
    stop("`index_path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(index_path)) {
    return(NULL)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!all(c("files", "models") %in% DBI::dbListTables(con))) {
    return(NULL)
  }
  query(con)
}

#' Models in the index
#'
#' @param index_path Path to the index SQLite file.
#' @return Data frame with one row per model that has usable files:
#'   \code{model_id}, \code{classifier_name}, \code{checkpoint_sha256},
#'   \code{transform}, \code{n_files}, \code{n_rois} and
#'   \code{n_threshold_sets} (distinct thresholds vectors stored in the files),
#'   counted over files with status \code{ok}. No rows when there is no index.
#' @noRd
thr_index_models <- function(index_path) {
  models <- thr_with_index(index_path, function(con) {
    DBI::dbGetQuery(con, "
      SELECT m.model_id, m.classifier_name, m.checkpoint_sha256, m.transform,
             COUNT(*) AS n_files,
             COALESCE(SUM(f.n_rois), 0) AS n_rois,
             COUNT(DISTINCT f.thresholds_json) AS n_threshold_sets
      FROM models m
      JOIN files f ON f.model_id = m.model_id AND f.status = 'ok'
      GROUP BY m.model_id
      ORDER BY m.classifier_name, m.model_id")
  })
  if (is.null(models)) {
    models <- data.frame(
      model_id = character(), classifier_name = character(),
      checkpoint_sha256 = character(), transform = character(),
      n_files = integer(), n_rois = integer(), n_threshold_sets = integer(),
      stringsAsFactors = FALSE
    )
  }
  for (column in c("n_files", "n_rois", "n_threshold_sets")) {
    models[[column]] <- as.integer(models[[column]])
  }
  models
}

#' Class labels of an indexed model
#'
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier, as listed by \code{thr_index_models()}.
#' @return Character vector of class labels in model order, exactly as stored
#'   in the class-score files.
#' @noRd
thr_index_class_labels <- function(index_path, model_id) {
  if (!thr_is_single_string(model_id)) {
    stop("`model_id` must be a single model identifier.", call. = FALSE)
  }
  stored <- thr_with_index(index_path, function(con) {
    DBI::dbGetQuery(con, "SELECT class_labels_json FROM models WHERE model_id = ?",
                    params = list(model_id))$class_labels_json
  })
  if (length(stored) == 0) {
    stop("Model '", model_id, "' is not in the index at ", index_path,
         ". Run build_threshold_index() on the class-score folder first.",
         call. = FALSE)
  }
  thr_labels_from_json(stored)
}

#' Files in the index that are not usable
#'
#' @param index_path Path to the index SQLite file.
#' @return Data frame with \code{h5_path}, \code{status} and \code{message}
#'   for every file whose status is not \code{ok}, sorted by path. No rows
#'   when there is no index.
#' @noRd
thr_index_failures <- function(index_path) {
  failures <- thr_with_index(index_path, thr_failures_query)
  if (is.null(failures)) {
    failures <- data.frame(h5_path = character(), status = character(),
                           message = character(), stringsAsFactors = FALSE)
  }
  failures
}

#' Query the files that are not usable
#'
#' @param con A DBI connection to the index SQLite file.
#' @return Data frame with \code{h5_path}, \code{status} and \code{message}.
#' @noRd
thr_failures_query <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT h5_path, status, message FROM files
    WHERE status != 'ok' ORDER BY h5_path")
}
