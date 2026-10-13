# The frozen sampling frame of the threshold review app
#
# The precision estimate weights each score bin by the number of predictions
# it holds. Those weights only mean something when the set of files they were
# counted over stays put, so the frame of a model is fixed once, at the first
# draw: its files, its exclusions, its score edges and its per-bin counts.
# Files indexed afterwards are reported, never added.

# Reasons a sample is left out of a frame
THR_REASON_TRAINING <- "training"
THR_REASON_NO_RAW <- "no_raw_data"

# A frame file counts as the file that was frozen only while the index still
# holds it for the same model with the same size and modification time. The
# same test decides which files are reported as changed and which may be drawn
# from, so a re-scored file can never slip ROIs into a frame.
THR_FRAME_INTACT_JOIN <- paste(
  "idx.files f ON f.h5_path = ff.h5_path AND f.model_id = ff.model_id",
  "AND f.sample_name = ff.sample_name AND f.status = 'ok'",
  "AND f.size = ff.size AND f.mtime = ff.mtime"
)

#' Has the sampling frame of a model been frozen?
#'
#' @param db_path Path to the verdict SQLite file. It is not created when it
#'   does not exist.
#' @param model_id Model identifier.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
thr_is_frozen <- function(db_path, model_id) {
  thr_check_db_path(db_path)
  thr_check_model_id(model_id)
  if (!file.exists(db_path)) {
    return(FALSE)
  }
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_model_exists(con, model_id)
}

#' Is a model registered in the verdict database?
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
thr_model_exists <- function(con, model_id) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM models WHERE model_id = ?",
                  params = list(model_id))$n > 0
}

#' Normalise a folder path for storing and prefix comparison
#'
#' @param folder A folder path.
#' @return The absolute path with forward slashes and no trailing slash.
#' @noRd
thr_normalise_folder <- function(folder) {
  path <- enc2utf8(normalizePath(folder, winslash = "/", mustWork = FALSE))
  if (nchar(path) > 1) sub("/+$", "", path) else path
}

#' Which paths lie under a folder?
#'
#' A path is under the folder when it starts with the folder followed by a
#' slash, so \code{/data/scores2/x.h5} is not under \code{/data/scores}.
#'
#' @param paths Character vector of normalised paths.
#' @param folder A folder from \code{thr_normalise_folder()}; \code{NA}
#'   matches every path.
#' @return Logical vector aligned with \code{paths}.
#' @noRd
thr_under_folder <- function(paths, folder) {
  if (is.na(folder)) {
    return(rep(TRUE, length(paths)))
  }
  prefix <- if (endsWith(folder, "/")) folder else paste0(folder, "/")
  startsWith(enc2utf8(paths), prefix)
}

#' Check a vector of sample names given to thr_freeze_frame()
#'
#' @param samples Character vector of sample names.
#' @param arg Name of the argument, for the error message.
#' @return The distinct names.
#' @noRd
thr_check_sample_list <- function(samples, arg) {
  if (is.null(samples)) {
    return(character())
  }
  if (!is.character(samples) || anyNA(samples)) {
    stop("`", arg, "` must be a character vector of sample names without ",
         "missing values.", call. = FALSE)
  }
  unique(enc2utf8(samples))
}

#' Check an optional folder argument
#'
#' @param folder A single path, or \code{NA}/\code{NULL} when not given.
#' @param arg Name of the argument, for the error message.
#' @return The normalised folder, or \code{NA}.
#' @noRd
thr_check_folder_arg <- function(folder, arg) {
  if (is.null(folder) || (length(folder) == 1 && is.na(folder))) {
    return(NA_character_)
  }
  if (!thr_is_single_string(folder)) {
    stop("`", arg, "` must be a single folder path, or NA.", call. = FALSE)
  }
  thr_normalise_folder(folder)
}

#' Check the fallback thresholds given to thr_freeze_frame()
#'
#' @param fallback \code{NULL} or a numeric vector.
#' @param fallback_source Where the fallback came from.
#' @return \code{NULL}, or the fallback as numeric with \code{NA} where the
#'   model has no threshold.
#' @noRd
thr_check_fallback_arg <- function(fallback, fallback_source) {
  is_json <- thr_is_single_string(fallback_source) &&
    startsWith(fallback_source, "json:") && nchar(fallback_source) > 5
  known <- is_json || identical(fallback_source, "h5") ||
    identical(fallback_source, "none")
  if (!known) {
    stop("`fallback_source` must be \"none\", \"h5\" or \"json:<path>\".",
         call. = FALSE)
  }
  if (is.null(fallback) != identical(fallback_source, "none")) {
    stop("`fallback` and `fallback_source` disagree: pass both the thresholds ",
         "and where they came from (\"h5\" or \"json:<path>\"), or neither.",
         call. = FALSE)
  }
  if (is.null(fallback)) {
    return(NULL)
  }
  if (!is.numeric(fallback) && !all(is.na(fallback))) {
    stop("`fallback` must be a numeric vector aligned with the class labels ",
         "(NA where the model has no threshold).", call. = FALSE)
  }
  fallback <- as.numeric(fallback)
  fallback[is.na(fallback)] <- NA_real_
  if (any(fallback < 0 | fallback > 1, na.rm = TRUE)) {
    stop("`fallback` thresholds must be between 0 and 1.", call. = FALSE)
  }
  fallback
}

#' Freeze the sampling frame of a model
#'
#' Fixes what a model's review is drawn from and weighted by: the model's
#' usable class-score files in the index (under \code{h5_folder}, when given)
#' minus the excluded samples, the score edges, and the number of predictions
#' per class and score bin. Everything is written in one transaction, so a
#' failure leaves no trace. A frame cannot be changed afterwards.
#'
#' Of the names in \code{excluded_samples} and \code{no_raw_samples}, only
#' those that actually removed one of the model's indexed samples are
#' recorded, so the stored exclusions are exactly the samples left out of the
#' frame. A sample on both lists is recorded as a training sample.
#'
#' @param db_path Path to the verdict SQLite file. Created if needed.
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier, as listed by \code{thr_index_models()}.
#' @param excluded_samples Sample names used to train the model. They may
#'   include samples that are not in the index.
#' @param no_raw_samples Sample names whose raw data cannot be found, so
#'   their images could not be shown.
#' @param fallback \code{NULL}, or the model's current thresholds: a numeric
#'   vector aligned with its class labels, \code{NA} where it has none.
#' @param fallback_source \code{"none"}, \code{"h5"} or
#'   \code{"json:<path>"}: where \code{fallback} came from.
#' @param h5_folder Class-score folder the review covers, or \code{NA} for
#'   every indexed file of the model.
#' @param roi_folder Raw data folder, recorded for reference.
#' @param edges Lower edges of the score bins.
#' @return Invisibly, a list with \code{model_id}, \code{n_frame_files},
#'   \code{n_rois}, \code{n_strata}, \code{n_excluded_training} and
#'   \code{n_no_raw}.
#' @noRd
thr_freeze_frame <- function(db_path, index_path, model_id,
                             excluded_samples = character(),
                             no_raw_samples = character(), fallback = NULL,
                             fallback_source = "none", h5_folder = NA_character_,
                             roi_folder = NA_character_,
                             edges = THR_SCORE_EDGES) {
  thr_check_db_path(db_path)
  thr_check_model_id(model_id)
  request <- list(
    excluded = thr_check_sample_list(excluded_samples, "excluded_samples"),
    no_raw = thr_check_sample_list(no_raw_samples, "no_raw_samples"),
    fallback = thr_check_fallback_arg(fallback, fallback_source),
    fallback_source = fallback_source,
    h5_folder = thr_check_folder_arg(h5_folder, "h5_folder"),
    roi_folder = thr_check_folder_arg(roi_folder, "roi_folder"),
    edges = thr_validate_edges(edges)
  )
  con <- thr_connect_verdicts(db_path, index_path, create = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  invisible(thr_write_transaction(con, function() {
    thr_write_frame(con, model_id, request)
  }))
}

#' Write a frame inside an open transaction
#'
#' @param con A DBI connection to the verdict SQLite file, with the index
#'   attached and a write transaction open.
#' @param model_id Model identifier.
#' @param request Checked arguments of \code{thr_freeze_frame()}.
#' @return The list \code{thr_freeze_frame()} returns.
#' @noRd
thr_write_frame <- function(con, model_id, request) {
  if (thr_model_exists(con, model_id)) {
    stop("The sampling frame of model '", model_id, "' is already frozen. A ",
         "frame and its exclusions cannot be changed; use another review ",
         "folder to start over.", call. = FALSE)
  }
  model <- thr_index_model_row(con, model_id)
  class_labels <- thr_labels_from_json(model$class_labels_json)
  fallback <- thr_align_fallback(request$fallback, class_labels)

  files <- thr_frame_candidates(con, model_id, request$h5_folder)
  left_out <- thr_frame_exclusions(files$sample_name, request$excluded,
                                   request$no_raw)
  frame <- files[!(files$sample_name %in% left_out$sample_name), , drop = FALSE]
  thr_check_frame_not_empty(frame, nrow(files), model_id)

  thr_insert_model(con, model, request, fallback)
  thr_insert_frame_files(con, model_id, frame)
  thr_insert_exclusions(con, model_id, left_out)
  strata <- thr_count_strata(con, model_id, class_labels, request$edges)
  if (sum(strata$n_population) != sum(frame$n_rois)) {
    stop("The index is inconsistent: its files list ", sum(frame$n_rois),
         " ROIs for this frame but ", sum(strata$n_population), " are ",
         "indexed. Delete the index file and run build_threshold_index() ",
         "again.", call. = FALSE)
  }
  thr_insert_strata(con, model_id, strata)
  list(
    model_id = model_id,
    n_frame_files = nrow(frame),
    n_rois = as.integer(sum(frame$n_rois)),
    n_strata = nrow(strata),
    n_excluded_training = sum(left_out$reason == THR_REASON_TRAINING),
    n_no_raw = sum(left_out$reason == THR_REASON_NO_RAW)
  )
}

#' Read a model's row from the attached index
#'
#' @param con A DBI connection with the index attached as \code{idx}.
#' @param model_id Model identifier.
#' @return One-row data frame of the index \code{models} table.
#' @noRd
thr_index_model_row <- function(con, model_id) {
  model <- DBI::dbGetQuery(con, "SELECT * FROM idx.models WHERE model_id = ?",
                           params = list(model_id))
  if (nrow(model) == 0) {
    stop("Model '", model_id, "' is not in the index. Run ",
         "build_threshold_index() on the class-score folder first.",
         call. = FALSE)
  }
  model
}

#' Align fallback thresholds with a model's class labels
#'
#' @param fallback \code{NULL} or a checked numeric vector.
#' @param class_labels The model's class labels.
#' @return Numeric vector with one value per class label.
#' @noRd
thr_align_fallback <- function(fallback, class_labels) {
  if (is.null(fallback)) {
    return(rep(NA_real_, length(class_labels)))
  }
  if (length(fallback) != length(class_labels)) {
    stop("`fallback` must have one value per class of the model: it has ",
         length(fallback), " values but the model has ", length(class_labels),
         " classes.", call. = FALSE)
  }
  fallback
}

#' The model's usable files in the index
#'
#' @param con A DBI connection that can read the index.
#' @param model_id Model identifier.
#' @param h5_folder Normalised class-score folder, or \code{NA} for no
#'   restriction.
#' @param files_table Name of the index \code{files} table as the connection
#'   sees it: \code{idx.files} when the index is attached to the verdict
#'   database, \code{files} on a connection to the index itself.
#' @return Data frame with \code{h5_path}, \code{sample_name}, \code{size},
#'   \code{mtime} and \code{n_rois}. A file whose size or modification time
#'   the index could not read is left out: it could never be recognised later
#'   as the file that was frozen.
#' @noRd
thr_frame_candidates <- function(con, model_id, h5_folder,
                                 files_table = "idx.files") {
  files <- DBI::dbGetQuery(con, paste("
    SELECT h5_path, sample_name, size, mtime, n_rois FROM", files_table, "
    WHERE model_id = ? AND status = 'ok'
      AND size IS NOT NULL AND mtime IS NOT NULL
    ORDER BY h5_path"),
    params = list(model_id))
  files$h5_path <- enc2utf8(files$h5_path)
  # The index keeps files of every folder it was ever built on
  files[thr_under_folder(files$h5_path, h5_folder), , drop = FALSE]
}

#' Samples a frame of a model would be drawn from
#'
#' The samples \code{thr_freeze_frame()} starts from, before any exclusion:
#' what to check exclusion lists and raw data against ahead of a freeze.
#'
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier, as listed by \code{thr_index_models()}.
#' @param h5_folder Class-score folder the review covers, or \code{NA} for
#'   every indexed file of the model.
#' @return Character vector of sample names, sorted; empty when the model has
#'   no usable file there.
#' @noRd
thr_frame_sample_names <- function(index_path, model_id,
                                   h5_folder = NA_character_) {
  thr_check_model_id(model_id)
  thr_check_index_exists(index_path)
  folder <- thr_check_folder_arg(h5_folder, "h5_folder")
  files <- thr_with_index(index_path, function(con) {
    thr_frame_candidates(con, model_id, folder, files_table = "files")
  })
  sort(files$sample_name, method = "radix")
}

#' Decide which indexed samples are left out of a frame, and why
#'
#' @param indexed_samples Sample names of the model's usable files.
#' @param excluded Training sample names.
#' @param no_raw Sample names without raw data.
#' @return Data frame with \code{sample_name} and \code{reason}; only samples
#'   that are among \code{indexed_samples}, each once.
#' @noRd
thr_frame_exclusions <- function(indexed_samples, excluded, no_raw) {
  training <- intersect(indexed_samples, excluded)
  # One reason per sample: a training sample stays one even without raw data
  without_raw <- setdiff(intersect(indexed_samples, no_raw), training)
  data.frame(
    sample_name = c(training, without_raw),
    reason = rep(c(THR_REASON_TRAINING, THR_REASON_NO_RAW),
                 c(length(training), length(without_raw))),
    stringsAsFactors = FALSE
  )
}

#' Stop when a frame would have nothing to review
#'
#' @param frame Data frame of the frame's files.
#' @param n_indexed Number of the model's usable files before exclusions.
#' @param model_id Model identifier.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_frame_not_empty <- function(frame, n_indexed, model_id) {
  if (nrow(frame) == 0) {
    stop("No samples are left to review for model '", model_id, "': ",
         n_indexed, " usable class-score file(s) were found for it in the ",
         "class-score folder and all of them are excluded. Check the folder ",
         "and the exclusion lists.", call. = FALSE)
  }
  if (sum(frame$n_rois) == 0) {
    stop("The ", nrow(frame), " sample(s) left for model '", model_id,
         "' hold no ROIs, so there is nothing to review.", call. = FALSE)
  }
  invisible(NULL)
}

#' Register a model in the verdict database
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model One-row data frame of the index \code{models} table.
#' @param request Checked arguments of \code{thr_freeze_frame()}.
#' @param fallback Numeric vector aligned with the class labels.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_model <- function(con, model, request, fallback) {
  DBI::dbExecute(con, "
    INSERT INTO models (model_id, classifier_name, checkpoint_sha256, transform,
                        class_labels_json, score_edges_json, frozen_at,
                        h5_folder, roi_folder, fallback_source,
                        fallback_thresholds_json)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      model$model_id, model$classifier_name, model$checkpoint_sha256,
      model$transform, model$class_labels_json,
      thr_numbers_to_json(request$edges), thr_now(), request$h5_folder,
      request$roi_folder, request$fallback_source,
      thr_numbers_to_json(fallback)
    ))
  invisible(NULL)
}

#' Store the files of a frame
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param frame Data frame of the frame's files.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_frame_files <- function(con, model_id, frame) {
  DBI::dbExecute(con, "
    INSERT INTO frame_files (model_id, sample_name, h5_path, size, mtime, n_rois)
    VALUES (?, ?, ?, ?, ?, ?)",
    params = list(rep(model_id, nrow(frame)), frame$sample_name, frame$h5_path,
                  frame$size, frame$mtime, frame$n_rois))
  invisible(NULL)
}

#' Store the samples left out of a frame
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param left_out Data frame with \code{sample_name} and \code{reason}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_exclusions <- function(con, model_id, left_out) {
  if (nrow(left_out) == 0) {
    return(invisible(NULL))
  }
  DBI::dbExecute(con, "
    INSERT INTO excluded_samples (model_id, sample_name, reason)
    VALUES (?, ?, ?)",
    params = list(rep(model_id, nrow(left_out)), left_out$sample_name,
                  left_out$reason))
  invisible(NULL)
}

#' SQL expression giving the score bin of a column
#'
#' Mirrors \code{thr_score_bin()}: a score below the second edge is in bin 1
#' and one at or above the last edge is in the last bin, whatever it is. The
#' edges themselves are bound as parameters (all but the first, in order), so
#' the comparison is made on the same doubles R uses.
#'
#' @param n_edges Number of score edges.
#' @param column Name of the score column.
#' @return One SQL expression with \code{n_edges - 1} placeholders.
#' @noRd
thr_bin_case_sql <- function(n_edges, column) {
  if (n_edges == 1) {
    return("1")
  }
  whens <- sprintf("WHEN %s < ? THEN %d", column, seq_len(n_edges - 1))
  paste("CASE", paste(whens, collapse = " "), "ELSE", n_edges, "END")
}

#' Count the predictions of a frame per class and score bin
#'
#' Counted in SQLite, so a frame of millions of ROIs never has to be read
#' into R.
#'
#' @param con A DBI connection with the index attached and the frame files
#'   of the model already stored.
#' @param model_id Model identifier.
#' @param class_labels The model's class labels.
#' @param edges Lower edges of the score bins.
#' @return Data frame with \code{class_name}, \code{bin} and
#'   \code{n_population}; strata without predictions are absent.
#' @noRd
thr_count_strata <- function(con, model_id, class_labels, edges) {
  sql <- paste0("
    SELECT r.class_idx AS class_idx, ", thr_bin_case_sql(length(edges), "r.score"),
    " AS bin, COUNT(*) AS n_population
    FROM frame_files ff
    JOIN ", THR_FRAME_INTACT_JOIN, "
    JOIN idx.rois r ON r.file_id = f.file_id
    WHERE ff.model_id = ?
    GROUP BY 1, 2 ORDER BY 1, 2")
  counts <- DBI::dbGetQuery(
    con, sql, params = unname(c(as.list(edges[-1]), list(model_id)))
  )
  data.frame(
    class_name = class_labels[counts$class_idx],
    bin = as.integer(counts$bin),
    n_population = as.integer(counts$n_population),
    stringsAsFactors = FALSE
  )
}

#' Store the strata of a frame
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param strata Data frame with \code{class_name}, \code{bin} and
#'   \code{n_population}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_strata <- function(con, model_id, strata) {
  DBI::dbExecute(con, "
    INSERT INTO strata (model_id, class_name, bin, n_population)
    VALUES (?, ?, ?, ?)",
    params = list(rep(model_id, nrow(strata)), strata$class_name, strata$bin,
                  strata$n_population))
  invisible(NULL)
}

#' State of a model's sampling frame
#'
#' @param db_path Path to the verdict SQLite file. It is not created when it
#'   does not exist.
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier.
#' @return A list with \code{frozen}; \code{n_frame_files} (samples in the
#'   frame); \code{n_excluded} (indexed samples of the model that were left
#'   out of the frame, for either reason), split into
#'   \code{n_excluded_training} and \code{n_no_raw}; \code{n_new_files}
#'   (usable files of the model in the index, under the folder the frame was
#'   frozen on, whose sample is neither in the frame nor excluded); and
#'   \code{n_changed_files} (frame files that the index no longer holds as
#'   they were: another size or modification time, missing, or not usable).
#'   All counts are 0 for a model that is not frozen.
#' @noRd
thr_frame_status <- function(db_path, index_path, model_id) {
  thr_check_db_path(db_path)
  thr_check_model_id(model_id)
  thr_check_index_exists(index_path)
  not_frozen <- list(frozen = FALSE, n_frame_files = 0L, n_excluded = 0L,
                     n_new_files = 0L, n_changed_files = 0L,
                     n_excluded_training = 0L, n_no_raw = 0L)
  if (!file.exists(db_path)) {
    return(not_frozen)
  }
  con <- thr_connect_verdicts(db_path, index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  model <- thr_model_query(con, model_id)
  if (is.null(model)) {
    return(not_frozen)
  }
  frame_samples <- DBI::dbGetQuery(
    con, "SELECT sample_name FROM frame_files WHERE model_id = ?",
    params = list(model_id)
  )$sample_name
  excluded <- DBI::dbGetQuery(
    con, "SELECT sample_name, reason FROM excluded_samples WHERE model_id = ?",
    params = list(model_id)
  )
  indexed <- thr_frame_candidates(con, model_id, model$h5_folder)
  is_new <- !(indexed$sample_name %in% c(frame_samples, excluded$sample_name))
  list(
    frozen = TRUE,
    n_frame_files = length(frame_samples),
    n_excluded = nrow(excluded),
    n_new_files = sum(is_new),
    n_changed_files = thr_count_changed_files(con, model_id),
    n_excluded_training = sum(excluded$reason == THR_REASON_TRAINING),
    n_no_raw = sum(excluded$reason == THR_REASON_NO_RAW)
  )
}

#' Count the frame files the index no longer holds as they were frozen
#'
#' @param con A DBI connection with the index attached as \code{idx}.
#' @param model_id Model identifier.
#' @return Number of changed, missing or unusable frame files.
#' @noRd
thr_count_changed_files <- function(con, model_id) {
  as.integer(DBI::dbGetQuery(con, paste("
    SELECT COUNT(*) AS n FROM frame_files ff
    LEFT JOIN", THR_FRAME_INTACT_JOIN, "
    WHERE ff.model_id = ? AND f.file_id IS NULL"),
    params = list(model_id))$n)
}
