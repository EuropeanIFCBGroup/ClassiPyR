# Tests for the Results and Export page helpers of the threshold review app

thr_test_app_class <- "Skeletonema_spp"

# A frozen synthetic model in which one class was reviewed in full: every
# draw scoring below 0.5 was marked wrong, the rest correct.
thr_test_app_reviewed_space <- function() {
  space <- thr_test_app_frozen_space()
  thr_test_app_review_class(space, thr_test_app_class, wrong_below = 0.5)
  space
}

thr_test_app_resolve <- function(space, target = 0.9, criterion = "estimate") {
  thr_resolve_thresholds_db(space$db_path, space$model_id, target = target,
                            criterion = criterion, seed = THR_APP_SEED)
}

# ---- labels and formats ------------------------------------------------------

test_that("thr_status_label puts every status in plain words", {
  statuses <- c("fitted", "manual", "not_reviewed", "under_reviewed",
                "too_few_predictions", "target_unreachable")

  labels <- thr_status_label(statuses)

  expect_length(labels, 6)
  expect_false(any(grepl("_", labels, fixed = TRUE)))
  expect_equal(anyDuplicated(labels), 0)
  expect_equal(thr_status_label("something_new"), "something_new")
  expect_equal(thr_status_label(character()), character())
})

test_that("thr_format_interval prints an estimate with its interval", {
  expect_equal(thr_format_interval(0.931, 0.88, 0.965),
               "93.1% (88.0% to 96.5%)")
  expect_equal(thr_format_interval(1, 1, 1), "100.0% (100.0% to 100.0%)")
  expect_equal(thr_format_interval(c(0.5, NA), c(0.4, NA), c(0.6, NA)),
               c("50.0% (40.0% to 60.0%)", ""))
  expect_equal(thr_format_interval(0.5, NA, NA), "50.0%")
})

# ---- thr_results_table -------------------------------------------------------

test_that("thr_results_table shows one row per class in model order", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  resolved <- thr_test_app_resolve(space)

  table <- thr_results_table(resolved)

  expect_equal(table$Class, thr_test_labels)
  expect_equal(
    names(table),
    c("Class", "Predicted", "Verdicts", "Status", "Source",
      "Previous threshold", "Chosen threshold", "Precision (95% interval)",
      "Share kept")
  )
  expect_equal(table$Predicted, resolved$n_predicted)
  expect_equal(table$Verdicts, resolved$n_verdicts)
  expect_equal(table[["Chosen threshold"]], resolved$threshold)
  reviewed <- table[table$Class == thr_test_app_class, ]
  expect_equal(reviewed$Status, "Fitted")
  expect_equal(reviewed$Source, "fitted")
  expect_match(reviewed[["Precision (95% interval)"]], "^100\\.0% \\(")
  expect_match(reviewed[["Share kept"]], "%$")
  others <- table[table$Class != thr_test_app_class, ]
  expect_equal(unique(others$Status), "Not reviewed")
  expect_equal(unique(others[["Precision (95% interval)"]]), "")
})

test_that("thr_results_table names the interval after its level", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  table <- thr_results_table(thr_test_app_resolve(space), conf_level = 0.9)

  expect_true("Precision (90% interval)" %in% names(table))
  expect_error(thr_results_table(data.frame(class_name = "a")), "resolved")
})

# ---- thr_class_curve ---------------------------------------------------------

test_that("thr_class_curve is the curve the resolved thresholds rest on", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  resolved <- thr_test_app_resolve(space)
  row <- resolved[resolved$class_name == thr_test_app_class, ]

  curve <- thr_class_curve(space$db_path, space$model_id, thr_test_app_class)

  expect_equal(curve$threshold, THR_SCORE_EDGES)
  at <- curve[curve$threshold == row$threshold, ]
  expect_equal(at$precision, row$precision)
  expect_equal(at$lower, row$lower)
  expect_equal(at$upper, row$upper)
  expect_equal(at$kept_share, row$kept_share)
  expect_identical(
    thr_class_curve(space$db_path, space$model_id, thr_test_app_class), curve
  )
  expect_error(thr_class_curve(space$db_path, space$model_id, "No such"),
               "not a class of this model")
})

# ---- plot --------------------------------------------------------------------

test_that("thr_threshold_position places a threshold on the bin axis", {
  edges <- c(0, 0.5, 0.9)

  expect_equal(thr_threshold_position(c(0, 0.5, 0.9), edges), c(1, 2, 3))
  expect_equal(thr_threshold_position(0.25, edges), 1.5)
  expect_equal(thr_threshold_position(0.95, edges), 3.5)
  expect_equal(thr_threshold_position(1, edges), 4)
  expect_true(is.na(thr_threshold_position(NA, edges)))
  expect_equal(thr_threshold_position(numeric(), edges), numeric())
})

test_that("thr_plot_precision_curve draws a reviewed class", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  curve <- thr_class_curve(space$db_path, space$model_id, thr_test_app_class)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  drawn <- thr_plot_precision_curve(curve, target = 0.9, chosen = 0.5,
                                    previous = 0.3, main = "Snowella/W <b>")

  expect_equal(drawn$x, seq_len(nrow(curve)))
  expect_equal(drawn$chosen_x, 6)
  expect_equal(drawn$previous_x, 4)
  expect_equal(drawn$n_estimated, sum(!is.na(curve$precision)))
  expect_gt(drawn$n_estimated, 0)
})

test_that("thr_plot_precision_curve draws a class without verdicts", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  curve <- thr_class_curve(space$db_path, space$model_id, "Debris")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(
    drawn <- thr_plot_precision_curve(curve, target = 0.9)
  )

  expect_equal(drawn$n_estimated, 0)
  expect_true(is.na(drawn$chosen_x))
  expect_true(is.na(drawn$previous_x))
})

test_that("thr_plot_precision_curve handles thresholds between the edges", {
  curve <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges(), seed = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  drawn <- thr_plot_precision_curve(curve, target = 0.8, chosen = 0.7,
                                    previous = 1)

  expect_equal(drawn$chosen_x, 2.5)
  expect_equal(drawn$previous_x, 4)
  expect_error(thr_plot_precision_curve(curve[c("threshold", "precision")],
                                        target = 0.9), "curve")
  expect_error(thr_plot_precision_curve(curve, target = 2), "target")
})

test_that("thr_plot_precision_curve leaves the graphics settings as found", {
  curve <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges(), seed = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  before <- graphics::par(c("mar", "mfrow", "xpd"))

  thr_plot_precision_curve(curve, target = 0.9)

  expect_equal(graphics::par(c("mar", "mfrow", "xpd")), before)
})

# ---- export ------------------------------------------------------------------

test_that("thr_export_summary counts the classes by source and status", {
  resolved <- data.frame(
    class_name = letters[1:6],
    source = c("fitted", "fitted", "manual", "fallback", "fallback", "fallback"),
    status = c("fitted", "fitted", "manual", "not_reviewed", "not_reviewed",
               "target_unreachable"),
    stringsAsFactors = FALSE
  )

  summary <- thr_export_summary(resolved)

  expect_equal(summary$Source, c("fitted", "manual", "fallback", "fallback"))
  expect_equal(summary$Status, c("Fitted", "Set manually", "Not reviewed",
                                 "Target not reachable"))
  expect_equal(summary$Classes, c(2L, 1L, 2L, 1L))
  expect_equal(sum(summary$Classes), nrow(resolved))
  expect_equal(nrow(thr_export_summary(resolved[0, ])), 0)
})

test_that("thr_export_paths derives the three file names from one", {
  folder <- tempfile("thr_export_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)

  paths <- thr_export_paths(paste0(folder, "  "),
                            " model_holdout_thresholds.json ")

  expect_equal(dirname(paths$json), thr_normalise_folder(folder))
  expect_equal(basename(paths$json), "model_holdout_thresholds.json")
  expect_equal(basename(paths$csv), "model_holdout_thresholds.csv")
  expect_equal(basename(paths$report), "model_holdout_thresholds_report.csv")
})

test_that("thr_export_paths refuses names that leave the folder", {
  folder <- tempfile("thr_export_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)

  for (name in c("../escape.json", "sub/model.json", "a\\b.json",
                 "/etc/model.json", "..")) {
    expect_error(thr_export_paths(folder, name), "file name")
  }
  expect_error(thr_export_paths(folder, ""), "file name")
  expect_error(thr_export_paths(folder, NULL), "file name")
  expect_error(thr_export_paths(folder, "model.yaml"), "\\.json")
  expect_error(thr_export_paths(folder, "model.JSON"), "\\.json")
  expect_error(thr_export_paths(file.path(folder, "absent"), "m.json"),
               "does not exist")
  expect_error(thr_export_paths("", "m.json"), "export folder")
})

test_that("thr_export_review writes the three files for every class", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  out <- file.path(space$root, "export")
  dir.create(out)

  result <- thr_export_review(space$db_path, space$model_id, target = 0.9,
                              criterion = "estimate", out_folder = out,
                              file_name = "fake_A_holdout_thresholds.json")

  expect_true(all(file.exists(c(result$json, result$csv, result$report))))
  resolved <- thr_test_app_resolve(space)
  # What is written is what the Results page showed, digit for digit
  expect_equal(result$resolved, resolved)

  json <- jsonlite::fromJSON(result$json, simplifyVector = FALSE)
  expect_equal(names(json$class_metrics), thr_test_labels)
  expect_equal(json$num_classes, 5)
  expect_equal(json$validation_transform, "dataset_squarepad")
  expect_equal(json$target_precision, 0.9)
  expect_equal(json$criterion, "estimate")
  expect_equal(json$fallback_source, "none")
  expect_false(is.null(json$generated_at))
  thresholds <- vapply(json$class_metrics, function(entry) {
    as.numeric(entry$threshold)
  }, numeric(1))
  expect_identical(unname(thresholds), resolved$threshold)
  expect_equal(json$class_metrics[[thr_test_app_class]]$source, "fitted")
  # ifcb-classify opens the file with the platform encoding
  expect_false(any(utf8ToInt(paste(readLines(result$json, warn = FALSE),
                                   collapse = "")) > 127))

  csv <- utils::read.csv(result$csv, encoding = "UTF-8",
                         stringsAsFactors = FALSE)
  expect_equal(names(csv), c("class", "threshold"))
  expect_equal(csv$class, thr_test_labels)
  expect_equal(csv$threshold, resolved$threshold)
  report <- utils::read.csv(result$report, encoding = "UTF-8",
                            stringsAsFactors = FALSE)
  expect_equal(nrow(report), 5)
  expect_equal(report$class_name, thr_test_labels)
  expect_match(result$hint, "--thresholds", fixed = TRUE)
  expect_match(result$hint, result$json, fixed = TRUE)
})

test_that("results, curve and export of a frozen review need no index", {
  for (how in c("deleted", "archived")) {
    space <- thr_test_app_reviewed_space()
    out <- file.path(space$root, "export")
    dir.create(out)
    with_index <- thr_test_app_resolve(space)
    curve <- thr_class_curve(space$db_path, space$model_id, thr_test_app_class)

    thr_test_lose_index(space, how)

    expect_equal(thr_test_app_resolve(space), with_index, info = how)
    expect_equal(thr_class_curve(space$db_path, space$model_id,
                                 thr_test_app_class), curve, info = how)
    result <- thr_export_review(space$db_path, space$model_id, target = 0.9,
                                criterion = "estimate", out_folder = out,
                                file_name = "fake_A_holdout_thresholds.json")
    expect_true(all(file.exists(c(result$json, result$csv, result$report))),
                info = how)
    expect_equal(result$resolved, with_index, info = how)
    if (how == "deleted") {
      expect_false(file.exists(space$index_path))
    }
    unlink(space$root, recursive = TRUE)
  }
})

test_that("thr_export_review writes nothing when one of the files exists", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  out <- file.path(space$root, "export")
  dir.create(out)
  writeLines("mine", file.path(out, "m_holdout_thresholds_report.csv"))
  export <- function(...) {
    thr_export_review(space$db_path, space$model_id, target = 0.9,
                      criterion = "estimate", out_folder = out,
                      file_name = "m_holdout_thresholds.json", ...)
  }

  expect_error(export(), "already exist")

  expect_equal(list.files(out), "m_holdout_thresholds_report.csv")
  expect_equal(readLines(file.path(out, "m_holdout_thresholds_report.csv")),
               "mine")

  result <- export(overwrite = TRUE)
  expect_length(list.files(out), 3)
  expect_gt(length(readLines(result$report)), 1)
})

test_that("thr_export_review refuses names ifcb-classify would auto-detect", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  out <- file.path(space$root, "export")
  dir.create(out)

  for (name in c("thresholds.json", "m_thresholds_and_metrics.json")) {
    expect_error(
      thr_export_review(space$db_path, space$model_id, target = 0.9,
                        criterion = "estimate", out_folder = out,
                        file_name = name),
      "loads automatically"
    )
  }
  expect_length(list.files(out), 0)
})

test_that("thr_export_review follows the target, criterion and overrides", {
  space <- thr_test_app_reviewed_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  out <- file.path(space$root, "export")
  dir.create(out)
  thr_set_override(space$db_path, space$model_id, "Debris", 0.42)

  result <- thr_export_review(space$db_path, space$model_id, target = 0.3,
                              criterion = "lower", out_folder = out,
                              file_name = "m_holdout_thresholds.json")

  json <- jsonlite::fromJSON(result$json, simplifyVector = FALSE)
  expect_equal(json$target_precision, 0.3)
  expect_equal(json$criterion, "lower")
  expect_equal(json$class_metrics$Debris$threshold, 0.42)
  expect_equal(json$class_metrics$Debris$source, "manual")
  expect_equal(result$resolved, thr_test_app_resolve(space, 0.3, "lower"))
  expect_error(
    thr_export_review(space$db_path, space$model_id, target = 1.5,
                      criterion = "estimate", out_folder = out,
                      file_name = "other_holdout_thresholds.json"),
    "target"
  )
  expect_false(file.exists(file.path(out, "other_holdout_thresholds.json")))
})

test_that("thr_export_hint quotes the path for the command line", {
  hint <- thr_export_hint("/data/my models/m_holdout_thresholds.json")

  expect_equal(hint,
               "--thresholds \"/data/my models/m_holdout_thresholds.json\"")
})
