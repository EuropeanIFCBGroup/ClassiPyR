# Verdict database of the threshold review app
#
# The verdict database is the durable record of a review: the frozen sampling
# frame of each model, every ROI that was drawn, and what the reviewer decided
# about it. Unlike the index cache it cannot be rebuilt from the class-score
# files, so nothing here ever drops or migrates a table on its own.

# Bump when the tables below change shape. A database written with another
# version is refused, never dropped: verdicts are hours of somebody's work.
THR_VERDICT_SCHEMA_VERSION <- "1"

# How long a connection waits for another session's write to finish
THR_DB_BUSY_MS <- 30000L

# Statements that create the verdict database tables
THR_VERDICT_SCHEMA_SQL <- c(
  "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS models (
     model_id TEXT PRIMARY KEY,
     classifier_name TEXT,
     checkpoint_sha256 TEXT,
     transform TEXT NOT NULL,
     class_labels_json TEXT NOT NULL,
     score_edges_json TEXT NOT NULL,
     frozen_at TEXT NOT NULL,
     h5_folder TEXT,
     roi_folder TEXT,
     fallback_source TEXT,
     fallback_thresholds_json TEXT
   )",
  "CREATE TABLE IF NOT EXISTS frame_files (
     model_id TEXT NOT NULL,
     sample_name TEXT NOT NULL,
     h5_path TEXT NOT NULL,
     size REAL, mtime REAL, n_rois INTEGER,
     PRIMARY KEY (model_id, sample_name)
   )",
  "CREATE TABLE IF NOT EXISTS excluded_samples (
     model_id TEXT NOT NULL,
     sample_name TEXT NOT NULL,
     reason TEXT NOT NULL,
     PRIMARY KEY (model_id, sample_name)
   )",
  "CREATE TABLE IF NOT EXISTS strata (
     model_id TEXT NOT NULL,
     class_name TEXT NOT NULL,
     bin INTEGER NOT NULL,
     n_population INTEGER NOT NULL,
     PRIMARY KEY (model_id, class_name, bin)
   )",
  "CREATE TABLE IF NOT EXISTS draws (
     draw_id INTEGER PRIMARY KEY,
     model_id TEXT NOT NULL,
     class_name TEXT NOT NULL,
     bin INTEGER NOT NULL,
     sample_name TEXT NOT NULL,
     roi_number INTEGER NOT NULL,
     score REAL NOT NULL,
     status TEXT NOT NULL DEFAULT 'pending',
     verdict INTEGER,
     annotator TEXT,
     drawn_at TEXT NOT NULL,
     confirmed_at TEXT,
     page_token TEXT,
     UNIQUE (model_id, sample_name, roi_number)
   )",
  "CREATE INDEX IF NOT EXISTS draws_class ON draws (model_id, class_name, status)",
  "CREATE TABLE IF NOT EXISTS threshold_choices (
     model_id TEXT NOT NULL,
     class_name TEXT NOT NULL,
     threshold REAL NOT NULL,
     updated_at TEXT NOT NULL,
     PRIMARY KEY (model_id, class_name)
   )"
)

#' Create the verdict database tables
#'
#' Safe to call on a database that already has them, and then only reads:
#' opening the database to look something up never waits for a write lock.
#' Stops when the database was written with another table layout.
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_init_verdict_schema <- function(con) {
  stored <- thr_stored_schema_version(con)
  if (length(stored) == 0) {
    for (statement in THR_VERDICT_SCHEMA_SQL) {
      DBI::dbExecute(con, statement)
    }
    # Written last: a database that has its version has all of its tables
    DBI::dbExecute(
      con, "INSERT OR IGNORE INTO meta (key, value) VALUES ('schema_version', ?)",
      params = list(THR_VERDICT_SCHEMA_VERSION)
    )
    stored <- thr_stored_schema_version(con)
  }
  if (!identical(stored, THR_VERDICT_SCHEMA_VERSION)) {
    stop("The verdict database has table layout version '", stored,
         "', but this version of ClassiPyR reads version '",
         THR_VERDICT_SCHEMA_VERSION, "'. It was written by another version ",
         "of ClassiPyR; open it with that version. Nothing was changed.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Table layout version stored in a verdict database
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @return The stored version, or an empty character vector when the database
#'   has none yet.
#' @noRd
thr_stored_schema_version <- function(con) {
  has_meta <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM sqlite_master
          WHERE type = 'table' AND name = 'meta'"
  )$n > 0
  if (!has_meta) {
    return(character())
  }
  DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key = 'schema_version'")$value
}

#' Stop unless a verdict database path is usable
#'
#' @param db_path Path to the verdict SQLite file.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_db_path <- function(db_path) {
  if (!thr_is_single_string(db_path)) {
    stop("`db_path` must be a single file path, e.g. ",
         "file.path(review_dir, \"", THR_VERDICT_DB_FILE, "\").", call. = FALSE)
  }
  invisible(NULL)
}

#' Stop unless a model identifier is usable
#'
#' @param model_id Model identifier.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_model_id <- function(model_id) {
  if (!thr_is_single_string(model_id)) {
    stop("`model_id` must be a single model identifier, as listed by ",
         "thr_index_models().", call. = FALSE)
  }
  invisible(NULL)
}

#' Stop unless the index cache exists
#'
#' Attaching a path that does not exist would create an empty database there,
#' so the file is checked first.
#'
#' @param index_path Path to the index SQLite file.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_index_exists <- function(index_path) {
  if (!thr_is_single_string(index_path)) {
    stop("`index_path` must be a single file path, e.g. ",
         "file.path(review_dir, \"", THR_INDEX_DB_FILE, "\").", call. = FALSE)
  }
  if (!file.exists(index_path)) {
    stop("No class-score index at ", index_path, ". Run ",
         "build_threshold_index() on the class-score folder first.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Open the verdict database
#'
#' The caller closes the connection with
#' \code{on.exit(DBI::dbDisconnect(con), add = TRUE)}.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param index_path \code{NULL}, or the path of the index cache to attach as
#'   \code{idx}, so one query can join the two.
#' @param create If \code{TRUE}, create the database when it does not exist.
#'   Otherwise a missing file is an error: reading must not leave an empty
#'   database behind.
#' @return A DBI connection with the verdict tables in place.
#' @noRd
thr_connect_verdicts <- function(db_path, index_path = NULL, create = FALSE) {
  thr_check_db_path(db_path)
  if (!is.null(index_path)) {
    thr_check_index_exists(index_path)
  }
  if (!file.exists(db_path)) {
    if (!isTRUE(create)) {
      stop("No verdict database at ", db_path, ". No sampling frame has been ",
           "frozen in this review folder yet; start a review first.",
           call. = FALSE)
    }
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  ready <- FALSE
  on.exit(if (!ready) DBI::dbDisconnect(con), add = TRUE)
  # Two browser sessions may write at once; wait rather than fail
  RSQLite::sqliteSetBusyHandler(con, THR_DB_BUSY_MS)
  thr_init_verdict_schema(con)
  if (!is.null(index_path)) {
    thr_attach_index(con, index_path)
  }
  ready <- TRUE
  con
}

#' Attach the index cache to a verdict database connection
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param index_path Path to an existing index SQLite file.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_attach_index <- function(con, index_path) {
  DBI::dbExecute(con, "ATTACH DATABASE ? AS idx", params = list(index_path))
  tables <- DBI::dbGetQuery(
    con, "SELECT name FROM idx.sqlite_master WHERE type = 'table'"
  )$name
  if (!all(c("models", "files", "rois") %in% tables)) {
    stop("The file at ", index_path, " is not a class-score index. Run ",
         "build_threshold_index() to build one.", call. = FALSE)
  }
  invisible(NULL)
}

#' Run code in one write transaction
#'
#' The write lock is taken at the start (\code{BEGIN IMMEDIATE}), so what the
#' code reads cannot change before it writes. Anything that stops the code,
#' an error or an interrupt, rolls every change back.
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param code Function of no arguments doing the reads and writes.
#' @return The value of \code{code()}.
#' @noRd
thr_write_transaction <- function(con, code) {
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  committed <- FALSE
  on.exit({
    if (!committed) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
  }, add = TRUE)
  result <- code()
  DBI::dbExecute(con, "COMMIT")
  committed <- TRUE
  result
}

#' Current time as stored in the verdict database
#'
#' @return One string, UTC, e.g. \code{"2026-03-01T14:05:09Z"}.
#' @noRd
thr_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Read the stored description of a frozen model
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return A list (see \code{thr_get_model()}), or \code{NULL} when the model
#'   has no frozen frame.
#' @noRd
thr_model_query <- function(con, model_id) {
  row <- DBI::dbGetQuery(con, "SELECT * FROM models WHERE model_id = ?",
                         params = list(model_id))
  if (nrow(row) == 0) {
    return(NULL)
  }
  class_labels <- thr_labels_from_json(row$class_labels_json)
  fallback <- thr_numbers_from_json(row$fallback_thresholds_json)
  if (is.null(fallback)) {
    fallback <- rep(NA_real_, length(class_labels))
  }
  text <- function(x) if (is.na(x)) NA_character_ else enc2utf8(x)
  list(
    model_id = row$model_id,
    classifier_name = text(row$classifier_name),
    checkpoint_sha256 = text(row$checkpoint_sha256),
    transform = text(row$transform),
    class_labels = class_labels,
    score_edges = thr_numbers_from_json(row$score_edges_json),
    frozen_at = row$frozen_at,
    h5_folder = text(row$h5_folder),
    roi_folder = text(row$roi_folder),
    fallback_source = text(row$fallback_source),
    fallback = fallback
  )
}

#' Read a frozen model or stop
#'
#' @inheritParams thr_model_query
#' @return A list, see \code{thr_get_model()}.
#' @noRd
thr_require_model <- function(con, model_id) {
  model <- thr_model_query(con, model_id)
  if (is.null(model)) {
    stop("Model '", model_id, "' has no frozen sampling frame in this review ",
         "folder. Freeze one with thr_freeze_frame() first.", call. = FALSE)
  }
  model
}

#' Stop unless a class belongs to the model
#'
#' @param class_name Class name(s) to check.
#' @param class_labels The model's class labels.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_class_name <- function(class_name, class_labels) {
  if (!is.character(class_name) || length(class_name) == 0 || anyNA(class_name)) {
    stop("`class_name` must be a class label of the model.", call. = FALSE)
  }
  unknown <- setdiff(enc2utf8(class_name), class_labels)
  if (length(unknown) > 0) {
    stop("Class '", unknown[1], "' is not a class of this model. Class names ",
         "must match the model's labels exactly.", call. = FALSE)
  }
  invisible(NULL)
}

#' Description of a frozen model
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return A list with \code{model_id}, \code{classifier_name},
#'   \code{checkpoint_sha256}, \code{transform}, \code{class_labels}
#'   (character, model order, exactly as in the class-score files),
#'   \code{score_edges} (numeric, the bin edges the frame was frozen with),
#'   \code{frozen_at}, \code{h5_folder}, \code{roi_folder},
#'   \code{fallback_source} and \code{fallback} (numeric aligned with
#'   \code{class_labels}, \code{NA} where the model has no threshold). Text
#'   fields that were not recorded are \code{NA}. Stops when the model has no
#'   frozen frame.
#' @noRd
thr_get_model <- function(db_path, model_id) {
  thr_check_model_id(model_id)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_require_model(con, model_id)
}

#' Stratum counts of a model, read on an open connection
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param class_labels The model's class labels, which give the row order.
#' @return Data frame with one row per stratum that has predictions:
#'   \code{class_name}, \code{bin}, \code{n_population}, \code{n_drawn},
#'   \code{n_verdicts}, \code{n_correct}, \code{n_pending} and
#'   \code{n_unavailable}, ordered by class (model order) and bin.
#' @noRd
thr_stratum_query <- function(con, model_id, class_labels) {
  counts <- DBI::dbGetQuery(con, "
    SELECT s.class_name, s.bin, s.n_population,
           COALESCE(d.n_drawn, 0) AS n_drawn,
           COALESCE(d.n_verdicts, 0) AS n_verdicts,
           COALESCE(d.n_correct, 0) AS n_correct,
           COALESCE(d.n_pending, 0) AS n_pending,
           COALESCE(d.n_unavailable, 0) AS n_unavailable
    FROM strata s
    LEFT JOIN (
      SELECT class_name, bin, COUNT(*) AS n_drawn,
             SUM(status = 'confirmed') AS n_verdicts,
             SUM(status = 'confirmed' AND verdict = 1) AS n_correct,
             SUM(status = 'pending') AS n_pending,
             SUM(status = 'unavailable') AS n_unavailable
      FROM draws WHERE model_id = ? GROUP BY class_name, bin
    ) d ON d.class_name = s.class_name AND d.bin = s.bin
    WHERE s.model_id = ?", params = list(model_id, model_id))
  counts$class_name <- enc2utf8(counts$class_name)
  for (column in setdiff(names(counts), "class_name")) {
    counts[[column]] <- as.integer(counts[[column]])
  }
  counts <- counts[order(match(counts$class_name, class_labels), counts$bin), ,
                   drop = FALSE]
  thr_reset_rownames(counts)
}

#' Review progress per score bin
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param class_name \code{NULL} for every class, or the class label(s) to
#'   report.
#' @return Data frame with one row per stratum that has predictions in the
#'   frame: \code{class_name}, \code{bin}, \code{n_population} (predictions in
#'   the frame), \code{n_verdicts} (confirmed), \code{n_correct},
#'   \code{n_pending} and \code{n_unavailable}, ordered by class (model order)
#'   and bin. Bins without predictions have no row.
#' @noRd
thr_stratum_counts <- function(db_path, model_id, class_name = NULL) {
  thr_check_model_id(model_id)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  model <- thr_require_model(con, model_id)
  counts <- thr_stratum_query(con, model_id, model$class_labels)
  if (!is.null(class_name)) {
    thr_check_class_name(class_name, model$class_labels)
    counts <- counts[counts$class_name %in% enc2utf8(class_name), , drop = FALSE]
  }
  columns <- c("class_name", "bin", "n_population", "n_verdicts", "n_correct",
               "n_pending", "n_unavailable")
  thr_reset_rownames(counts[columns])
}

#' Review progress per class
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return Data frame with one row per class label, in model order:
#'   \code{class_name}, \code{n_predicted} (predictions in the frame),
#'   \code{n_strata} (score bins holding predictions), \code{n_drawn} (ROIs
#'   drawn so far, whatever became of them), \code{n_pending},
#'   \code{n_verdicts}, \code{n_correct} and \code{n_unavailable}. A class the
#'   model never predicted in the frame has zeros.
#' @noRd
thr_class_overview <- function(db_path, model_id) {
  thr_check_model_id(model_id)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  model <- thr_require_model(con, model_id)
  counts <- thr_stratum_query(con, model_id, model$class_labels)

  class_at <- factor(counts$class_name, levels = model$class_labels)
  per_class <- function(values) {
    as.integer(unname(tapply(values, class_at, sum, default = 0L)))
  }
  data.frame(
    class_name = model$class_labels,
    n_predicted = per_class(counts$n_population),
    n_strata = as.integer(unname(table(class_at))),
    n_drawn = per_class(counts$n_drawn),
    n_pending = per_class(counts$n_pending),
    n_verdicts = per_class(counts$n_verdicts),
    n_correct = per_class(counts$n_correct),
    n_unavailable = per_class(counts$n_unavailable),
    stringsAsFactors = FALSE
  )
}

#' Manual thresholds of a model, read on an open connection
#'
#' @inheritParams thr_stratum_query
#' @return Data frame with \code{class_name} and \code{threshold}, in model
#'   order.
#' @noRd
thr_override_query <- function(con, model_id, class_labels) {
  overrides <- DBI::dbGetQuery(
    con,
    "SELECT class_name, threshold FROM threshold_choices WHERE model_id = ?",
    params = list(model_id)
  )
  overrides$class_name <- enc2utf8(overrides$class_name)
  overrides$threshold <- as.numeric(overrides$threshold)
  overrides <- overrides[order(match(overrides$class_name, class_labels)), ,
                         drop = FALSE]
  thr_reset_rownames(overrides)
}

#' Manual thresholds of a model
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return Data frame with \code{class_name} and \code{threshold}, one row per
#'   class that has a manual threshold, in model order.
#' @noRd
thr_get_overrides <- function(db_path, model_id) {
  thr_check_model_id(model_id)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  model <- thr_require_model(con, model_id)
  thr_override_query(con, model_id, model$class_labels)
}

#' Set or remove the manual threshold of a class
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param class_name One class label of the model.
#' @param threshold A number between 0 and 1, or \code{NULL}/\code{NA} to
#'   remove the manual threshold of the class.
#' @return Invisibly, the threshold now stored for the class (\code{NA} when
#'   it has none).
#' @noRd
thr_set_override <- function(db_path, model_id, class_name, threshold) {
  thr_check_model_id(model_id)
  if (!thr_is_single_string(class_name)) {
    stop("`class_name` must be a single class label of the model.",
         call. = FALSE)
  }
  remove <- is.null(threshold) || (length(threshold) == 1 && is.na(threshold))
  valid <- remove || (is.numeric(threshold) && length(threshold) == 1 &&
                        is.finite(threshold) && threshold >= 0 && threshold <= 1)
  if (!valid) {
    stop("`threshold` must be a single number between 0 and 1, or NULL to ",
         "remove the manual threshold.", call. = FALSE)
  }
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  model <- thr_require_model(con, model_id)
  thr_check_class_name(class_name, model$class_labels)

  key <- list(model_id, enc2utf8(class_name))
  if (remove) {
    DBI::dbExecute(con, "DELETE FROM threshold_choices
                         WHERE model_id = ? AND class_name = ?", params = key)
    return(invisible(NA_real_))
  }
  DBI::dbExecute(con, "
    INSERT INTO threshold_choices (model_id, class_name, threshold, updated_at)
    VALUES (?, ?, ?, ?)
    ON CONFLICT (model_id, class_name) DO UPDATE SET
      threshold = excluded.threshold, updated_at = excluded.updated_at",
    params = c(key, list(as.numeric(threshold), thr_now())))
  invisible(as.numeric(threshold))
}

#' Resolve the threshold of every class from the verdict database
#'
#' Reads the stratum counts, class labels, fallback thresholds, manual
#' thresholds and score edges of a frozen model in one go and passes them to
#' \code{thr_resolve_thresholds()}.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param target Target precision.
#' @param criterion \code{"estimate"} or \code{"lower"}.
#' @param ... Further settings for \code{thr_resolve_thresholds()}:
#'   \code{min_class_n}, \code{min_stratum_n}, \code{n_sim},
#'   \code{conf_level}, \code{seed}. The inputs that come from the database
#'   (\code{strata_counts}, \code{class_labels}, \code{fallback},
#'   \code{overrides}, \code{edges}) cannot be passed.
#' @return The data frame of \code{thr_resolve_thresholds()}: one row per
#'   class label, in model order.
#' @noRd
thr_resolve_thresholds_db <- function(db_path, model_id,
                                      target = THR_DEFAULT_TARGET_PRECISION,
                                      criterion = "estimate", ...) {
  thr_check_model_id(model_id)
  fixed <- c("strata_counts", "class_labels", "fallback", "overrides", "edges")
  clash <- intersect(names(list(...)), fixed)
  if (length(clash) > 0) {
    stop("`", clash[1], "` is read from the verdict database and cannot be ",
         "passed: the score edges, class labels and fallback thresholds were ",
         "fixed when the frame was frozen.", call. = FALSE)
  }
  inputs <- thr_resolve_inputs_db(db_path, model_id)
  thr_resolve_thresholds(
    strata_counts = inputs$strata_counts,
    class_labels = inputs$model$class_labels,
    fallback = inputs$model$fallback,
    overrides = inputs$overrides,
    edges = inputs$model$score_edges,
    target = target,
    criterion = criterion,
    ...
  )
}

#' Read everything a threshold fit needs from the verdict database
#'
#' One connection, so the counts and the manual thresholds belong to the same
#' moment.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return A list with \code{model} (see \code{thr_get_model()}),
#'   \code{strata_counts} and \code{overrides}.
#' @noRd
thr_resolve_inputs_db <- function(db_path, model_id) {
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  model <- thr_require_model(con, model_id)
  counts <- thr_stratum_query(con, model_id, model$class_labels)
  list(
    model = model,
    strata_counts = counts[c("class_name", "bin", "n_population", "n_verdicts",
                             "n_correct")],
    overrides = thr_override_query(con, model_id, model$class_labels)
  )
}
