# Tests for the verdict database of the threshold review app: schema,
# connection, model description, progress counts, manual thresholds and the
# database front end of the threshold fit.

library(testthat)

thr_test_memory_db <- function() {
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

# A synthetic model (240 predictions per class) frozen on three score bins,
# which keeps the end-to-end numbers easy to follow.
thr_test_small_frame <- function(fallback = c(0.3, NA, 0.9, 0.2, 0.6)) {
  space <- thr_test_db_space()
  model_id <- thr_test_synthetic_index(space)
  thr_freeze_frame(space$db_path, space$index_path, model_id,
                   fallback = fallback, fallback_source = "h5",
                   edges = c(0, 0.5, 0.9))
  c(space, list(model_id = model_id))
}

# Review every ROI of a class, marking those scored below `cut` as wrong
thr_test_review_class <- function(space, class_name, cut, page_size = 100) {
  repeat {
    page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                          class_name, page_size = page_size)
    if (nrow(page) == 0) break
    thr_test_confirm_page(space$db_path, page,
                          wrong = page$draw_id[page$score < cut])
  }
  invisible(NULL)
}

# --- schema ------------------------------------------------------------------

test_that("thr_init_verdict_schema creates the tables and is idempotent", {
  con <- thr_test_memory_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  thr_init_verdict_schema(con)
  expect_silent(thr_init_verdict_schema(con))

  expect_setequal(
    DBI::dbListTables(con),
    c("meta", "models", "frame_files", "excluded_samples", "strata", "draws",
      "threshold_choices")
  )
  columns <- function(table) {
    DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))$name
  }
  expect_identical(
    columns("models"),
    c("model_id", "classifier_name", "checkpoint_sha256", "transform",
      "class_labels_json", "score_edges_json", "frozen_at", "h5_folder",
      "roi_folder", "fallback_source", "fallback_thresholds_json")
  )
  expect_identical(columns("frame_files"),
                   c("model_id", "sample_name", "h5_path", "size", "mtime", "n_rois"))
  expect_identical(columns("excluded_samples"),
                   c("model_id", "sample_name", "reason"))
  expect_identical(columns("strata"),
                   c("model_id", "class_name", "bin", "n_population"))
  expect_identical(
    columns("draws"),
    c("draw_id", "model_id", "class_name", "bin", "sample_name", "roi_number",
      "score", "status", "verdict", "annotator", "drawn_at", "confirmed_at",
      "page_token")
  )
  expect_identical(columns("threshold_choices"),
                   c("model_id", "class_name", "threshold", "updated_at"))
  indexes <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index'")
  expect_true("draws_class" %in% indexes$name)
  version <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key = 'schema_version'")
  expect_identical(version$value, THR_VERDICT_SCHEMA_VERSION)
})

test_that("a half-created verdict database is completed on the next open", {
  con <- thr_test_memory_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # As left behind when creation stopped before the version was written
  DBI::dbExecute(con, "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
  DBI::dbExecute(con, "CREATE TABLE strata (
    model_id TEXT NOT NULL, class_name TEXT NOT NULL, bin INTEGER NOT NULL,
    n_population INTEGER NOT NULL, PRIMARY KEY (model_id, class_name, bin))")

  thr_init_verdict_schema(con)

  expect_true(all(c("models", "draws", "threshold_choices") %in% DBI::dbListTables(con)))
  expect_identical(thr_stored_schema_version(con), THR_VERDICT_SCHEMA_VERSION)
})

test_that("the draws table refuses the same ROI twice for a model", {
  con <- thr_test_memory_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_init_verdict_schema(con)
  insert <- function(model_id) {
    DBI::dbExecute(con, "
      INSERT INTO draws (model_id, class_name, bin, sample_name, roi_number,
                         score, drawn_at)
      VALUES (?, 'Debris', 1, 'D20240101T000000_IFCB134', 7, 0.5, 'now')",
      params = list(model_id))
  }

  insert("model-1")
  expect_error(insert("model-1"), "UNIQUE")
  expect_equal(insert("model-2"), 1)
  stored <- DBI::dbGetQuery(con, "SELECT status, verdict FROM draws")
  expect_identical(stored$status, c("pending", "pending"))
  expect_true(all(is.na(stored$verdict)))
})

test_that("a verdict database of another layout version is refused, not dropped", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_next_page(space$db_path, space$index_path, thr_test_model_a_id, "Debris")
  thr_test_execute(space$db_path,
                   "UPDATE meta SET value = '0' WHERE key = 'schema_version'")

  expect_error(thr_get_model(space$db_path, thr_test_model_a_id), "layout version")
  expect_error(
    thr_next_page(space$db_path, space$index_path, thr_test_model_a_id, "Debris"),
    "layout version"
  )
  # Nothing was dropped or rewritten
  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 12)
  expect_equal(nrow(thr_test_table(space$db_path, "models")), 1)
  expect_identical(
    thr_test_query(space$db_path,
                   "SELECT value FROM meta WHERE key = 'schema_version'")$value,
    "0"
  )
})

# --- connection helpers ------------------------------------------------------

test_that("the verdict database is only created when asked to", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(thr_connect_verdicts(space$db_path), "No verdict database")
  expect_false(file.exists(space$db_path))
  expect_error(thr_connect_verdicts(c("a", "b")), "db_path")

  nested <- file.path(space$root, "new", "review", "verdicts.sqlite")
  con <- thr_connect_verdicts(nested, create = TRUE)
  # Closed before the folder is removed
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_true(file.exists(nested))
  expect_true("draws" %in% DBI::dbListTables(con))
})

test_that("attaching needs a real index and never creates one", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(thr_connect_verdicts(space$db_path, space$index_path, create = TRUE),
               "build_threshold_index")
  expect_false(file.exists(space$index_path))
  # The verdict database is not created either when the index is missing
  expect_false(file.exists(space$db_path))
  expect_error(thr_connect_verdicts(space$db_path, 12, create = TRUE), "index_path")

  # Some other SQLite file is not an index
  other <- file.path(space$root, "other.sqlite")
  thr_test_execute(other, "CREATE TABLE notes (text TEXT)")
  expect_error(thr_connect_verdicts(space$db_path, other, create = TRUE),
               "not a class-score index")
})

test_that("an attached index can be joined with the verdict tables", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  con <- thr_connect_verdicts(space$db_path, space$index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  joined <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) AS n FROM frame_files ff
    JOIN idx.files f ON f.h5_path = ff.h5_path")

  expect_equal(joined$n, 6)
})

test_that("a write transaction commits on success and rolls back on failure", {
  con <- thr_test_memory_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_init_verdict_schema(con)
  add_choice <- function(class_name) {
    DBI::dbExecute(con, "
      INSERT INTO threshold_choices (model_id, class_name, threshold, updated_at)
      VALUES ('m', ?, 0.5, 'now')", params = list(class_name))
  }
  count <- function() {
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM threshold_choices")$n
  }

  value <- thr_write_transaction(con, function() {
    add_choice("a")
    "done"
  })
  expect_identical(value, "done")
  expect_equal(count(), 1)

  expect_error(thr_write_transaction(con, function() {
    add_choice("b")
    stop("halfway", call. = FALSE)
  }), "halfway")
  expect_equal(count(), 1)

  # The connection is usable again: no transaction was left open
  thr_write_transaction(con, function() add_choice("c"))
  expect_equal(count(), 2)
})

test_that("timestamps are UTC in ISO 8601", {
  stamp <- thr_now()

  expect_match(stamp, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  parsed <- as.POSIXct(stamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_lt(abs(as.numeric(difftime(Sys.time(), parsed, units = "secs"))), 60)
})

# --- thr_get_model -----------------------------------------------------------

test_that("thr_get_model describes a frozen model", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   fallback = c(0.3, 0.05, 0.9, NA, 0.6), fallback_source = "h5",
                   h5_folder = space$h5_folder, roi_folder = space$root)

  model <- thr_get_model(space$db_path, thr_test_model_a_id)

  expect_identical(model$model_id, thr_test_model_a_id)
  expect_identical(model$classifier_name, "fixture_A")
  expect_identical(model$checkpoint_sha256,
                   thr_test_expected()$files[[1]]$checkpoint_sha256)
  expect_identical(model$transform, "dataset_squarepad")
  expect_identical(model$class_labels, thr_test_labels)
  expect_identical(model$score_edges, THR_SCORE_EDGES)
  expect_identical(model$fallback, c(0.3, 0.05, 0.9, NA, 0.6))
  expect_identical(model$fallback_source, "h5")
  expect_identical(model$h5_folder, space$h5_folder)
  expect_identical(model$roi_folder, normalizePath(space$root, winslash = "/"))
  expect_match(model$frozen_at, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("a model frozen without fallback or folders has NA there", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id)

  model <- thr_get_model(space$db_path, thr_test_model_a_id)

  expect_identical(model$fallback, rep(NA_real_, 5))
  expect_identical(model$fallback_source, "none")
  expect_identical(model$h5_folder, NA_character_)
  expect_identical(model$roi_folder, NA_character_)
})

test_that("a model whose checkpoint could not be hashed has an NA hash", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(space$h5_folder, thr_test_a_samples[1],
                      thr_test_summary(checkpoint_sha256 = NULL))
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id
  thr_freeze_frame(space$db_path, space$index_path, model_id)

  model <- thr_get_model(space$db_path, model_id)

  expect_identical(model$checkpoint_sha256, NA_character_)
  expect_identical(model$classifier_name, "fake_A")
})

test_that("thr_get_model refuses a model that is not frozen", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(thr_get_model(space$db_path, thr_test_model_a_id),
               "No verdict database")
  expect_false(file.exists(space$db_path))
  thr_test_frozen_fixture(space)
  expect_error(thr_get_model(space$db_path, thr_test_model_b_id),
               "no frozen sampling frame")
  expect_error(thr_get_model(space$db_path, 42), "model_id")
})

# --- thr_frozen_models -------------------------------------------------------

test_that("thr_frozen_models lists the reviews of a folder without the index", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  none <- thr_frozen_models(space$db_path)
  expect_equal(nrow(none), 0)
  expect_named(none, c("model_id", "classifier_name", "checkpoint_sha256",
                       "transform", "frozen_at", "h5_folder", "n_frame_files",
                       "n_frame_rois"))
  expect_type(none$model_id, "character")
  expect_type(none$n_frame_files, "integer")
  # Asking must not create the database
  expect_false(file.exists(space$db_path))

  thr_test_fixture_index(space)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   excluded_samples = thr_test_a_samples[1],
                   h5_folder = space$h5_folder)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id,
                   h5_folder = space$h5_folder)
  frame <- thr_test_table(space$db_path, "frame_files")
  # The verdict database alone answers: the index may be gone by now
  unlink(space$index_path)

  frozen <- thr_frozen_models(space$db_path)

  expect_setequal(frozen$model_id, c(thr_test_model_a_id, thr_test_model_b_id))
  a <- frozen[frozen$model_id == thr_test_model_a_id, ]
  expect_identical(a$n_frame_files, 5L)
  expect_identical(a$n_frame_rois,
                   as.integer(sum(frame$n_rois[frame$model_id == a$model_id])))
  expect_equal(a$h5_folder, space$h5_folder)
  expect_equal(a$classifier_name,
               thr_get_model(space$db_path, thr_test_model_a_id)$classifier_name)
  expect_error(thr_frozen_models(NULL), "db_path")
})

# --- thr_stratum_counts and thr_class_overview -------------------------------

test_that("stratum counts of a fresh frame are the frame's populations", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  counts <- thr_stratum_counts(space$db_path, thr_test_model_a_id)

  expect_identical(names(counts), c("class_name", "bin", "n_population",
                                    "n_verdicts", "n_correct", "n_pending",
                                    "n_unavailable"))
  hand <- thr_test_hand_strata(thr_test_expected_files("model_a"), thr_test_labels,
                               THR_SCORE_EDGES)
  # Ordered by class in model order, then by bin: the order of the hand count
  expect_identical(counts[c("class_name", "bin", "n_population")], hand)
  for (column in c("n_verdicts", "n_correct", "n_pending", "n_unavailable")) {
    expect_identical(counts[[column]], rep(0L, nrow(counts)))
  }
  expect_type(counts$bin, "integer")
  expect_type(counts$n_population, "integer")
})

test_that("stratum counts can be limited to a class", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  one <- thr_stratum_counts(space$db_path, thr_test_model_a_id,
                            "Snowella/Woronichinia")
  two <- thr_stratum_counts(space$db_path, thr_test_model_a_id,
                            c("Debris", "Öresund_ciliate"))

  expect_identical(one$bin, c(5L, 7L, 8L, 15L))
  expect_identical(one$n_population, c(2L, 1L, 1L, 3L))
  expect_true(all(one$class_name == "Snowella/Woronichinia"))
  expect_identical(unique(two$class_name), c("Öresund_ciliate", "Debris"))
  expect_equal(sum(two$n_population), 24)
  expect_error(thr_stratum_counts(space$db_path, thr_test_model_a_id, "Debris_typo"),
               "not a class of this model")
  expect_error(thr_stratum_counts(space$db_path, thr_test_model_a_id, NA_character_),
               "class_name")
  expect_error(thr_stratum_counts(space$db_path, thr_test_model_b_id),
               "no frozen sampling frame")
})

test_that("stratum counts and the overview follow the draws and verdicts", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  page <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                        "Debris")
  page <- page[order(page$draw_id), ]
  # The raw file of the ninth draw was read and holds no image for it
  page$extracted <- seq_len(nrow(page)) != 9
  page$no_image_reason <- ifelse(page$extracted, NA_character_,
                                 "not_extractable")
  ids <- page$draw_id
  # 8 verdicts (2 wrong), 1 unavailable, 3 left pending
  resolved <- thr_resolve_page_verdicts(page, loaded = ids[1:8], failed = NULL,
                                        wrong = ids[1:2], token = "t",
                                        expected_token = "t")
  thr_save_verdicts(space$db_path, resolved, "Anna", "page-1")

  counts <- thr_stratum_counts(space$db_path, thr_test_model_a_id, "Debris")
  overview <- thr_class_overview(space$db_path, thr_test_model_a_id)

  per_bin <- function(rows) {
    as.integer(table(factor(page$bin[rows], levels = counts$bin)))
  }
  expect_identical(counts$n_population, per_bin(1:12))
  expect_identical(counts$n_verdicts, per_bin(1:8))
  expect_identical(counts$n_correct, per_bin(3:8))
  expect_identical(counts$n_unavailable, per_bin(9))
  expect_identical(counts$n_pending, per_bin(10:12))

  expect_identical(names(overview), c("class_name", "n_predicted", "n_strata",
                                      "n_drawn", "n_pending", "n_verdicts",
                                      "n_correct", "n_unavailable"))
  expect_identical(overview$class_name, thr_test_labels)
  expect_identical(overview$n_predicted, c(18L, 7L, 16L, 12L, 12L))
  expect_identical(overview$n_strata, c(7L, 4L, 8L, 4L, 5L))
  expect_identical(overview$n_drawn, c(0L, 0L, 0L, 0L, 12L))
  expect_identical(overview$n_pending, c(0L, 0L, 0L, 0L, 3L))
  expect_identical(overview$n_verdicts, c(0L, 0L, 0L, 0L, 8L))
  expect_identical(overview$n_correct, c(0L, 0L, 0L, 0L, 6L))
  expect_identical(overview$n_unavailable, c(0L, 0L, 0L, 0L, 1L))

  # Other classes and other models are not touched by these verdicts
  others <- thr_stratum_counts(space$db_path, thr_test_model_a_id,
                               "Skeletonema_spp")
  expect_true(all(others$n_verdicts == 0 & others$n_pending == 0))
})

test_that("the overview has a row of zeros for a class that was never predicted", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  # The default stand-in file predicts classes 1 to 3 only
  thr_test_write_fake(space$h5_folder, thr_test_a_samples[1])
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id
  thr_freeze_frame(space$db_path, space$index_path, model_id)

  overview <- thr_class_overview(space$db_path, model_id)

  expect_identical(overview$class_name, thr_test_labels)
  expect_identical(overview$n_predicted, c(2L, 1L, 1L, 0L, 0L))
  expect_identical(overview$n_strata, c(2L, 1L, 1L, 0L, 0L))
  expect_identical(overview$n_drawn, rep(0L, 5))
  expect_error(thr_class_overview(space$db_path, "no-such-model"),
               "no frozen sampling frame")
})

test_that("counts of two frozen models are kept apart", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id)
  page <- thr_next_page(space$db_path, space$index_path, thr_test_model_b_id,
                        "Debris")
  thr_test_confirm_page(space$db_path, page)

  overview_a <- thr_class_overview(space$db_path, thr_test_model_a_id)
  overview_b <- thr_class_overview(space$db_path, thr_test_model_b_id)

  expect_identical(overview_a$n_drawn, rep(0L, 5))
  expect_identical(overview_b$n_verdicts[5], overview_b$n_predicted[5])
  expect_equal(sum(overview_b$n_predicted), 20)
})

# --- overrides ---------------------------------------------------------------

test_that("manual thresholds are set, replaced and removed", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  set <- function(class_name, threshold) {
    thr_set_override(space$db_path, thr_test_model_a_id, class_name, threshold)
  }
  overrides <- function() thr_get_overrides(space$db_path, thr_test_model_a_id)

  none <- overrides()
  expect_identical(names(none), c("class_name", "threshold"))
  expect_equal(nrow(none), 0)
  expect_type(none$class_name, "character")
  expect_type(none$threshold, "double")

  expect_identical(set("Debris", 0.42), 0.42)
  set("Snowella/Woronichinia", 0.1 + 0.2)
  set("Öresund_ciliate", 0)
  set("Pauliella_taeniata_001", 1L)
  # Model order, whatever the order they were set in
  expect_identical(overrides()$class_name, thr_test_labels[c(2, 3, 4, 5)])
  expect_identical(overrides()$threshold, c(0.1 + 0.2, 1, 0, 0.42))

  set("Debris", 0.77)
  expect_identical(overrides()$threshold[4], 0.77)
  expect_equal(nrow(overrides()), 4)
  stamp <- thr_test_table(space$db_path, "threshold_choices")$updated_at
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}T", stamp)))

  expect_identical(set("Debris", NULL), NA_real_)
  set("Öresund_ciliate", NA)
  expect_identical(overrides()$class_name, thr_test_labels[2:3])
  # Removing what is not there is not an error
  expect_identical(set("Debris", NA_real_), NA_real_)
  expect_equal(nrow(overrides()), 2)
})

test_that("thr_set_override validates the class and the value", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  set <- function(class_name = "Debris", threshold = 0.5,
                  model_id = thr_test_model_a_id) {
    thr_set_override(space$db_path, model_id, class_name, threshold)
  }

  expect_error(set(threshold = 1.2), "between 0 and 1")
  expect_error(set(threshold = -0.1), "between 0 and 1")
  expect_error(set(threshold = "0.5"), "between 0 and 1")
  expect_error(set(threshold = c(0.1, 0.2)), "between 0 and 1")
  expect_error(set(threshold = Inf), "between 0 and 1")
  expect_error(set(class_name = "Debris_typo"), "not a class of this model")
  expect_error(set(class_name = "snowella/woronichinia"), "not a class of this model")
  expect_error(set(class_name = c("Debris", "Skeletonema_spp")), "class_name")
  expect_error(set(model_id = thr_test_model_b_id), "no frozen sampling frame")
  expect_error(thr_get_overrides(space$db_path, thr_test_model_b_id),
               "no frozen sampling frame")
  expect_equal(nrow(thr_get_overrides(space$db_path, thr_test_model_a_id)), 0)
})

test_that("manual thresholds of two models do not mix", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id)

  thr_set_override(space$db_path, thr_test_model_a_id, "Debris", 0.3)
  thr_set_override(space$db_path, thr_test_model_b_id, "Debris", 0.8)

  expect_identical(thr_get_overrides(space$db_path, thr_test_model_a_id)$threshold, 0.3)
  expect_identical(thr_get_overrides(space$db_path, thr_test_model_b_id)$threshold, 0.8)
})

# --- thr_resolve_thresholds_db -----------------------------------------------

test_that("a fresh frame resolves every class to its fallback", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   fallback = c(0.3, 0.05, 0.9, NA, 0.6), fallback_source = "h5")

  resolved <- thr_resolve_thresholds_db(space$db_path, thr_test_model_a_id)

  expect_identical(resolved$class_name, thr_test_labels)
  # Where the model has no threshold, 0 is written: same behaviour, no NaN
  expect_identical(resolved$threshold, c(0.3, 0.05, 0.9, 0, 0.6))
  expect_identical(resolved$previous_threshold, c(0.3, 0.05, 0.9, NA, 0.6))
  expect_true(all(resolved$source == "fallback"))
  # Fewer than 50 predictions per class in these small fixtures
  expect_true(all(resolved$status == "too_few_predictions"))
  expect_identical(resolved$n_predicted, c(18L, 7L, 16L, 12L, 12L))
  expect_identical(resolved$n_verdicts, rep(0L, 5))
})

test_that("thresholds are resolved end to end from a reviewed frame", {
  space <- thr_test_small_frame()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  # Debris: reviewed completely; everything scored below 0.5 is wrong
  thr_test_review_class(space, "Debris", cut = 0.5)
  # Öresund_ciliate: one page of 20, all correct
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        "Öresund_ciliate", page_size = 20)
  thr_test_confirm_page(space$db_path, page)
  thr_set_override(space$db_path, space$model_id, "Pauliella_taeniata_001", 0.42)

  resolved <- thr_resolve_thresholds_db(space$db_path, space$model_id, seed = 1)

  expect_identical(resolved$class_name, thr_test_labels)
  expect_identical(resolved$n_predicted, rep(240L, 5))
  expect_identical(resolved$n_verdicts, c(0L, 0L, 0L, 20L, 240L))
  expect_identical(resolved$status,
                   c("not_reviewed", "not_reviewed", "manual", "under_reviewed",
                     "fitted"))
  expect_identical(resolved$source,
                   c("fallback", "fallback", "manual", "fallback", "fitted"))
  # Fallback 0.3; no threshold -> 0; override; fallback 0.2; fitted at the
  # edge above which every reviewed ROI was correct
  expect_identical(resolved$threshold, c(0.3, 0, 0.42, 0.2, 0.5))
  expect_identical(resolved$previous_threshold, c(0.3, NA, 0.9, 0.2, 0.6))
  expect_identical(resolved$fitted_threshold[5], 0.5)
  # A census: the precision above the cut is exact
  expect_identical(resolved$precision[5], 1)
  expect_identical(resolved$lower[5], 1)
  expect_identical(resolved$upper[5], 1)
  debris <- thr_stratum_counts(space$db_path, space$model_id, "Debris")
  expect_equal(resolved$kept_share[5], sum(debris$n_population[debris$bin >= 2]) / 240)
  expect_identical(resolved$n_correct[5], sum(debris$n_correct))
  expect_false(anyNA(resolved$threshold))
})

test_that("the database front end passes exactly the stored inputs on", {
  space <- thr_test_small_frame()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_review_class(space, "Debris", cut = 0.5)
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        "Skeletonema_spp", page_size = 60)
  thr_test_confirm_page(space$db_path, page, wrong = page$draw_id[1:7])
  thr_set_override(space$db_path, space$model_id, "Snowella/Woronichinia", 0.33)
  model <- thr_get_model(space$db_path, space$model_id)
  counts <- thr_stratum_counts(space$db_path, space$model_id)

  from_db <- thr_resolve_thresholds_db(space$db_path, space$model_id,
                                       target = 0.8, criterion = "lower",
                                       min_class_n = 30, n_sim = 500, seed = 3)
  direct <- thr_resolve_thresholds(
    counts[c("class_name", "bin", "n_population", "n_verdicts", "n_correct")],
    class_labels = model$class_labels, fallback = model$fallback,
    overrides = thr_get_overrides(space$db_path, space$model_id),
    edges = c(0, 0.5, 0.9), target = 0.8, criterion = "lower",
    min_class_n = 30, n_sim = 500, seed = 3
  )

  expect_identical(from_db, direct)
  expect_identical(from_db$status[2], "manual")
})

test_that("target, criterion and the evidence settings reach the fit", {
  space <- thr_test_small_frame()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_review_class(space, "Debris", cut = 0.5)
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        "Öresund_ciliate", page_size = 20)
  thr_test_confirm_page(space$db_path, page)
  resolve <- function(...) {
    thr_resolve_thresholds_db(space$db_path, space$model_id, seed = 1, ...)
  }

  strict <- resolve(target = 0.9)
  lenient <- resolve(target = 0.3)
  relaxed <- resolve(min_class_n = 10, min_stratum_n = 5)

  # About half of Debris is correct overall: no cut is needed for a low target
  expect_identical(strict$threshold[5], 0.5)
  expect_identical(lenient$threshold[5], 0)
  # 20 verdicts are too few by default, enough once the minimum is lowered
  expect_identical(strict$status[4], "under_reviewed")
  expect_identical(relaxed$status[4], "fitted")
  expect_identical(relaxed$threshold[4], 0)
  expect_error(resolve(criterion = "upper"), "arg")
  expect_error(resolve(target = 1.5), "target")
})

test_that("what the frame fixed cannot be passed to the fit", {
  space <- thr_test_small_frame()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  resolve <- function(...) {
    thr_resolve_thresholds_db(space$db_path, space$model_id, ...)
  }

  expect_error(resolve(edges = THR_SCORE_EDGES), "`edges` is read from")
  expect_error(resolve(fallback = rep(0.5, 5)), "`fallback` is read from")
  expect_error(resolve(overrides = NULL), "`overrides` is read from")
  expect_error(thr_resolve_thresholds_db(space$db_path, "no-such-model"),
               "no frozen sampling frame")
  expect_error(thr_resolve_thresholds_db(file.path(space$root, "none.sqlite"),
                                         space$model_id), "No verdict database")
})

test_that("resolved thresholds and the stored model build an export", {
  space <- thr_test_small_frame()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_review_class(space, "Debris", cut = 0.5)
  model <- thr_get_model(space$db_path, space$model_id)
  resolved <- thr_resolve_thresholds_db(space$db_path, space$model_id, seed = 1)

  export <- build_thresholds_export(resolved, model, target = 0.9,
                                    fallback_source = model$fallback_source,
                                    edges = model$score_edges)

  expect_identical(names(export$class_metrics), thr_test_labels)
  expect_identical(export$validation_transform, "dataset_squarepad")
  expect_identical(export$class_metrics[["Debris"]]$threshold, 0.5)
  expect_identical(export$class_metrics[["Snowella/Woronichinia"]]$threshold, 0)
})
