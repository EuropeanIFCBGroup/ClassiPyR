# Tests for the thresholds export (JSON for ifcb-classify, CSV for people)

library(testthat)

export_temp_path <- function(name = "fixture_A_holdout_thresholds.json") {
  dir <- tempfile("thr_export_")
  dir.create(dir)
  file.path(dir, name)
}

export_read_text <- function(path) {
  rawToChar(readBin(path, "raw", file.size(path)))
}

# A minimal export for the class labels given, with one threshold each.
export_for_labels <- function(labels, threshold = seq_along(labels) / 10) {
  resolved <- data.frame(
    class_name = labels, threshold = threshold, source = "manual",
    status = "manual", reason = "Set manually.", n_predicted = 0L,
    n_verdicts = 0L, n_correct = 0L, precision = NA_real_, lower = NA_real_,
    upper = NA_real_, kept_share = NA_real_, previous_threshold = NA_real_,
    stringsAsFactors = FALSE
  )
  build_thresholds_export(resolved, strata_model_fixture(labels), target = 0.9,
                          generated_by = "ClassiPyR test")
}

# =============================================================================
# The awkward labels themselves
# =============================================================================

test_that("the fixture labels are the awkward ones the export must carry", {
  expect_identical(
    strata_awkward_labels(),
    c("Skeletonema_spp", "Snowella/Woronichinia", "Pauliella_taeniata_001",
      "\u00d6resund_ciliate", "Debris")
  )
})

# =============================================================================
# build_thresholds_export
# =============================================================================

test_that("build_thresholds_export returns the documented structure", {
  export <- strata_export_fixture()

  expect_named(export, c(
    "model_name", "num_classes", "validation_transform", "checkpoint_sha256",
    "model_id", "threshold_method", "target_precision", "criterion",
    "fallback_source", "score_edges", "generated_by", "class_metrics"
  ))
  expect_identical(export$model_name, "fixture_A")
  expect_identical(export$num_classes, 5L)
  expect_identical(export$validation_transform, "dataset_squarepad")
  expect_identical(export$checkpoint_sha256, strata_model_fixture()$checkpoint_sha256)
  expect_identical(export$model_id, strata_model_fixture()$model_id)
  expect_identical(export$threshold_method, "holdout_review")
  expect_identical(export$target_precision, 0.9)
  expect_identical(export$criterion, "estimate")
  expect_identical(export$fallback_source, "h5")
  expect_identical(export$score_edges, THR_SCORE_EDGES)
  expect_identical(export$generated_by, "ClassiPyR test")
})

test_that("class_metrics holds every class, keyed by the raw label", {
  labels <- strata_awkward_labels()
  resolved <- strata_resolved_fixture()

  metrics <- strata_export_fixture()$class_metrics

  expect_identical(names(metrics), labels)
  for (i in seq_along(labels)) {
    entry <- metrics[[labels[i]]]
    expect_named(entry, c(
      "class_name", "threshold", "source", "status", "reason", "n_predicted",
      "n_verdicts", "n_correct", "precision", "precision_lower",
      "precision_upper", "kept_share", "previous_threshold"
    ))
    expect_identical(entry$class_name, labels[i])
    expect_identical(entry$threshold, resolved$threshold[i])
    expect_identical(entry$source, resolved$source[i])
    expect_identical(entry$n_predicted, resolved$n_predicted[i])
    expect_identical(entry$precision_lower, resolved$lower[i])
    expect_identical(entry$precision_upper, resolved$upper[i])
    expect_identical(entry$previous_threshold, resolved$previous_threshold[i])
  }
})

test_that("class_metrics follows the model order whatever the row order", {
  shuffled <- strata_resolved_fixture()[c(4, 2, 5, 1, 3), ]

  export <- build_thresholds_export(shuffled, strata_model_fixture(),
                                    target = 0.9, fallback_source = "h5",
                                    generated_by = "ClassiPyR test")

  expect_identical(export, strata_export_fixture())
})

test_that("generated_at is included only when given", {
  build <- function(...) {
    build_thresholds_export(strata_resolved_fixture(), strata_model_fixture(),
                            target = 0.9, ...)
  }
  stamp <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")

  expect_false("generated_at" %in% names(build()))
  expect_identical(build(generated_at = "2026-01-02T03:04:05Z")$generated_at,
                   "2026-01-02T03:04:05Z")
  expect_identical(build(generated_at = stamp)$generated_at,
                   "2026-01-02T03:04:05Z")
  expect_error(build(generated_at = 5), "generated_at")
})

test_that("generated_by names the package and its version by default", {
  export <- build_thresholds_export(strata_resolved_fixture(),
                                    strata_model_fixture(), target = 0.9)

  expect_match(export$generated_by, "^ClassiPyR [0-9.]+$")
  expect_identical(export$fallback_source, NA_character_)
})

test_that("build_thresholds_export refuses a model without a transform", {
  build <- function(transform) {
    model <- strata_model_fixture()
    model$transform <- transform
    build_thresholds_export(strata_resolved_fixture(), model, target = 0.9)
  }

  expect_error(build(NA_character_), "transform")
  expect_error(build(""), "transform")
  expect_error(build(NULL), "transform")
  # The message says what the consequence would be
  expect_error(build(NA_character_), "validation_transform")
})

test_that("build_thresholds_export refuses classes that do not match the model", {
  resolved <- strata_resolved_fixture()
  build <- function(rows) {
    build_thresholds_export(rows, strata_model_fixture(), target = 0.9)
  }
  renamed <- resolved
  renamed$class_name[4] <- "Oresund_ciliate"

  expect_error(build(resolved[-2, ]), "Snowella/Woronichinia", fixed = TRUE)
  expect_error(build(resolved[-2, ]), "missing")
  expect_error(build(renamed), "Oresund_ciliate")
  expect_error(build(rbind(resolved, resolved[1, ])), "more than once")
  expect_error(build(resolved[, -2]), "threshold")
})

test_that("build_thresholds_export refuses thresholds ifcb-classify cannot use", {
  build <- function(value) {
    resolved <- strata_resolved_fixture()
    resolved$threshold[3] <- value
    build_thresholds_export(resolved, strata_model_fixture(), target = 0.9)
  }

  expect_error(build(NA_real_), "Pauliella_taeniata_001")
  expect_error(build(NaN), "between 0 and 1")
  expect_error(build(Inf), "between 0 and 1")
  expect_error(build(-0.01), "between 0 and 1")
  expect_error(build(1.01), "between 0 and 1")
})

test_that("build_thresholds_export validates the model and settings", {
  resolved <- strata_resolved_fixture()
  model <- strata_model_fixture()
  duplicated_labels <- model
  duplicated_labels$class_labels[2] <- duplicated_labels$class_labels[1]

  expect_error(build_thresholds_export(resolved, "fixture_A", target = 0.9),
               "model")
  expect_error(build_thresholds_export(resolved, model[-5], target = 0.9),
               "class_labels")
  expect_error(build_thresholds_export(resolved, duplicated_labels, 0.9),
               "more than once")
  expect_error(build_thresholds_export(resolved, model, target = 2), "target")
  expect_error(build_thresholds_export(resolved, model, 0.9, criterion = "x"))
  expect_error(build_thresholds_export(resolved, model, 0.9, edges = c(0.5, 1)),
               "edges")
})

# =============================================================================
# write_thresholds_json: content
# =============================================================================

test_that("write_thresholds_json writes every class and returns the path", {
  path <- export_temp_path()
  labels <- strata_awkward_labels()

  result <- withVisible(write_thresholds_json(strata_export_fixture(), path))

  expect_identical(result$value, path)
  expect_false(result$visible)
  expect_true(file.exists(path))

  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(parsed$num_classes, 5L)
  expect_identical(parsed$validation_transform, "dataset_squarepad")
  expect_identical(parsed$threshold_method, "holdout_review")
  expect_identical(length(parsed$class_metrics), 5L)
  expect_identical(names(parsed$class_metrics), labels)
})

test_that("awkward labels survive the JSON byte for byte", {
  path <- export_temp_path()
  labels <- strata_awkward_labels()
  write_thresholds_json(strata_export_fixture(), path)

  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  keys <- names(parsed$class_metrics)
  class_names <- vapply(parsed$class_metrics, function(m) m$class_name, "")

  expect_identical(lapply(enc2utf8(keys), charToRaw),
                   lapply(enc2utf8(labels), charToRaw))
  expect_identical(lapply(enc2utf8(unname(class_names)), charToRaw),
                   lapply(enc2utf8(labels), charToRaw))
  expect_true("Snowella/Woronichinia" %in% keys)
  expect_true("Pauliella_taeniata_001" %in% keys)
})

test_that("thresholds are identical after the JSON round trip", {
  path <- export_temp_path()
  labels <- strata_awkward_labels()
  expected <- c(0.1 + 0.2, 0.9999, 1 / 3, 0, 1)
  write_thresholds_json(strata_export_fixture(), path)

  metrics <- jsonlite::fromJSON(path)$class_metrics

  for (i in seq_along(labels)) {
    expect_identical(metrics[[labels[i]]]$threshold, expected[i])
  }
  expect_false(identical(metrics[[labels[1]]]$threshold, 0.3))
  # Whole-number thresholds must come back as doubles, not integers
  expect_type(metrics[[labels[4]]]$threshold, "double")
  expect_type(metrics[[labels[5]]]$threshold, "double")
  # Every other number is exact too
  expect_identical(metrics[[labels[2]]]$precision, 2 / 3)
  expect_identical(metrics[[labels[1]]]$previous_threshold, 0.3)
  expect_identical(jsonlite::fromJSON(path)$score_edges, THR_SCORE_EDGES)
  expect_identical(jsonlite::fromJSON(path)$target_precision, 0.9)
})

test_that("arbitrary doubles round-trip exactly", {
  set.seed(11)
  thresholds <- c(runif(300), 0, 1, .Machine$double.eps, 1 - 2^-53, 1e-300,
                  5e-324)
  labels <- sprintf("class_%03d", seq_along(thresholds))
  path <- export_temp_path()
  write_thresholds_json(export_for_labels(labels, thresholds), path)

  metrics <- jsonlite::fromJSON(path, simplifyVector = FALSE)$class_metrics
  read <- vapply(metrics, function(m) as.numeric(m$threshold), numeric(1))

  expect_identical(strata_double_bits(unname(read)),
                   strata_double_bits(thresholds))
})

test_that("only digits = I(17) with always_decimal round-trips in jsonlite", {
  # Pins the jsonlite settings write_thresholds_json() depends on.
  values <- c(0.1 + 0.2, 0.9999, 1 / 3)
  round_trip <- function(...) {
    jsonlite::fromJSON(jsonlite::toJSON(values, ...))
  }

  expect_false(identical(round_trip(digits = NA), values))
  expect_false(identical(round_trip(digits = 15), values))
  expect_identical(round_trip(digits = I(17)), values)

  # A lone 0 or 1 is read back as an integer unless it is written as 0.0 / 1.0
  scalar <- function(x, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(list(t = x), auto_unbox = TRUE,
                                        digits = I(17), ...))$t
  }
  expect_type(scalar(0), "integer")
  expect_identical(scalar(0, always_decimal = TRUE), 0)
  expect_identical(scalar(1, always_decimal = TRUE), 1)

  path <- export_temp_path()
  write_thresholds_json(strata_export_fixture(), path)
  text <- export_read_text(path)
  expect_match(text, "\"threshold\": 0.30000000000000004,", fixed = TRUE)
  expect_match(text, "\"threshold\": 0.0,", fixed = TRUE)
  expect_match(text, "\"threshold\": 1.0,", fixed = TRUE)
  # Counts stay integers
  expect_match(text, "\"n_predicted\": 400,", fixed = TRUE)
  expect_match(text, "\"num_classes\": 5,", fixed = TRUE)
})

test_that("missing metrics are written as null and thresholds never are", {
  path <- export_temp_path()
  export <- strata_export_fixture()
  export$checkpoint_sha256 <- NA_character_
  export$fallback_source <- NA_character_
  write_thresholds_json(export, path)
  text <- export_read_text(path)

  expect_match(text, "\"precision\": null", fixed = TRUE)
  expect_match(text, "\"kept_share\": null", fixed = TRUE)
  expect_match(text, "\"previous_threshold\": null", fixed = TRUE)
  expect_match(text, "\"checkpoint_sha256\": null", fixed = TRUE)
  expect_match(text, "\"fallback_source\": null", fixed = TRUE)
  expect_no_match(text, "\"threshold\": null", fixed = TRUE)
  expect_no_match(text, "NA", fixed = TRUE)
  expect_no_match(text, "NaN", fixed = TRUE)

  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_null(parsed$class_metrics[["Debris"]]$precision)
  expect_null(parsed$checkpoint_sha256)
  expect_false(is.null(parsed$validation_transform))
})

test_that("score_edges stays an array even with a single edge", {
  path <- export_temp_path()
  export <- strata_export_fixture()
  export$score_edges <- 0
  write_thresholds_json(export, path)

  expect_match(export_read_text(path), "\"score_edges\": [0.0]", fixed = TRUE)
})

# =============================================================================
# write_thresholds_json: ASCII output
# =============================================================================

test_that("the JSON is pure ASCII with non-ASCII labels as \\u escapes", {
  path <- export_temp_path()
  write_thresholds_json(strata_export_fixture(), path)
  bytes <- readBin(path, "raw", file.size(path))
  text <- rawToChar(bytes)

  expect_true(all(utf8ToInt(text) < 128))
  expect_true(all(as.integer(bytes) < 128))
  expect_match(text, "\"\\u00d6resund_ciliate\": {", fixed = TRUE)
  expect_match(text, "\"class_name\": \"\\u00d6resund_ciliate\"", fixed = TRUE)
  # Unix line endings and a final newline, whatever the platform
  expect_false(any(bytes == as.raw(0x0d)))
  expect_identical(bytes[length(bytes)], as.raw(0x0a))
})

test_that("characters above the BMP are written as surrogate pairs", {
  labels <- c("Microbe_\U0001F9A0", "Dinophysis_\u00e5\u00e4\u00f6",
              "\u6d77\u85fb")
  path <- export_temp_path()
  write_thresholds_json(export_for_labels(labels), path)
  text <- export_read_text(path)

  expect_true(all(utf8ToInt(text) < 128))
  expect_match(text, "Microbe_\\ud83e\\udda0", fixed = TRUE)
  expect_match(text, "Dinophysis_\\u00e5\\u00e4\\u00f6", fixed = TRUE)
  expect_match(text, "\\u6d77\\u85fb", fixed = TRUE)
  expect_identical(names(jsonlite::fromJSON(path)$class_metrics), labels)
})

test_that("existing escapes and JSON syntax characters are not corrupted", {
  labels <- c(
    "quote\"inside", "back\\slash", "literal\\u00e9text", "tab\there",
    "line\nbreak", "caf\u00e9\\u00e9", "ends_with_backslash\\",
    "{\"json\": [1, 2]}", "control\u0001char"
  )
  path <- export_temp_path()
  write_thresholds_json(export_for_labels(labels), path)
  text <- export_read_text(path)
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)$class_metrics

  expect_true(all(utf8ToInt(text) < 128))
  expect_identical(names(parsed), labels)
  expect_identical(
    unname(vapply(parsed, function(m) m$class_name, character(1))), labels
  )
  # The six characters backslash-u-0-0-e-9 stay six characters
  expect_identical(nchar(names(parsed)[3]), nchar("literal") + 6L + nchar("text"))
})

test_that("thr_json_escape_non_ascii only touches non-ASCII characters", {
  ascii <- "{\"a\\\"b\": \"c\\\\d \\u00e9 \\n\", \"n\": 1.5}"

  expect_identical(thr_json_escape_non_ascii(ascii), ascii)
  expect_identical(thr_json_escape_non_ascii("\"\u00d6\""), "\"\\u00d6\"")
  expect_identical(thr_json_escape_non_ascii("\"\U0001F600\""),
                   "\"\\ud83d\\ude00\"")
  expect_identical(thr_json_escape_non_ascii("\"\uffff\U00010000\U0010FFFF\""),
                   "\"\\uffff\\ud800\\udc00\\udbff\\udfff\"")
  expect_identical(thr_json_escape_non_ascii(""), "")

  latin1 <- "\"\xd6\""
  Encoding(latin1) <- "latin1"
  expect_identical(thr_json_escape_non_ascii(latin1), "\"\\u00d6\"")

  invalid <- "\"\xff\xfe\""
  Encoding(invalid) <- "UTF-8"
  expect_error(thr_json_escape_non_ascii(invalid), "UTF-8")
})

# =============================================================================
# write_thresholds_json: stored expected file
# =============================================================================

# The expected text is a stored file rather than a testthat snapshot because
# tests/testthat/_snaps/ is git-ignored in this repository. After an intended
# change of the format, review the difference and regenerate it with:
#   devtools::load_all()
#   source("tests/testthat/helper-threshold-strata.R")
#   write_thresholds_json(
#     strata_export_fixture(jsonlite::fromJSON(
#       "tests/testthat/test_data/threshold_review/expected.json")$class_labels),
#     "tests/testthat/test_data/threshold_review/export/fixture_A_holdout_thresholds.json",
#     overwrite = TRUE)
test_that("the JSON text matches the stored expected file", {
  expected_path <- test_path("test_data", "threshold_review", "export",
                             "fixture_A_holdout_thresholds.json")
  path <- export_temp_path()
  write_thresholds_json(strata_export_fixture(), path)

  expect_true(file.exists(expected_path))
  # Compared line by line, not byte by byte: a Windows checkout may give the
  # stored file CRLF line endings. The bytes of a freshly written file are
  # checked in the ASCII test above.
  expect_identical(readLines(path), readLines(expected_path))
})

# =============================================================================
# write_thresholds_json: refusals
# =============================================================================

test_that("write_thresholds_json refuses paths ifcb-classify would read as YAML", {
  export <- strata_export_fixture()
  dir <- dirname(export_temp_path())

  for (name in c("export.JSON", "export.yaml", "export.json.txt", "export",
                 "export.Json")) {
    path <- file.path(dir, name)
    expect_error(write_thresholds_json(export, path), "\\.json")
    expect_error(write_thresholds_json(export, path), "YAML")
    expect_false(file.exists(path))
  }
})

test_that("write_thresholds_json refuses names that are auto-detected", {
  export <- strata_export_fixture()
  dir <- dirname(export_temp_path())

  for (name in c("thresholds.json", "THRESHOLDS.json",
                 "fixture_A_thresholds_and_metrics.json",
                 "_thresholds_and_metrics.json",
                 "run_Thresholds_And_Metrics.json")) {
    path <- file.path(dir, name)
    expect_error(write_thresholds_json(export, path), "next to a checkpoint")
    expect_error(write_thresholds_json(export, path, overwrite = TRUE),
                 "holdout_thresholds.json")
    expect_false(file.exists(path))
  }
  # Similar names that are not picked up automatically are fine
  for (name in c("my_thresholds.json", "thresholds_and_metrics_review.json")) {
    path <- file.path(dir, name)
    expect_no_error(write_thresholds_json(export, path))
    expect_true(file.exists(path))
  }
})

test_that("write_thresholds_json refuses to replace a file unless asked to", {
  path <- export_temp_path()
  writeLines("keep me", path)

  expect_error(write_thresholds_json(strata_export_fixture(), path),
               "already exists")
  expect_error(write_thresholds_json(strata_export_fixture(), path),
               "overwrite = TRUE")
  expect_identical(readLines(path), "keep me")

  write_thresholds_json(strata_export_fixture(), path, overwrite = TRUE)
  expect_identical(jsonlite::fromJSON(path)$num_classes, 5L)
})

test_that("write_thresholds_json explains an unusable path", {
  export <- strata_export_fixture()
  missing_dir <- file.path(tempfile("thr_missing_"), "export.json")

  expect_error(write_thresholds_json(export, missing_dir), "does not exist")
  expect_error(write_thresholds_json(export, NA_character_), "path")
  expect_error(write_thresholds_json(export, c("a.json", "b.json")), "path")
  expect_error(write_thresholds_json(export, ""), "path")
  expect_error(write_thresholds_json(export, export_temp_path(),
                                     overwrite = NA), "overwrite")
  # A folder that happens to be named like a JSON file
  folder <- export_temp_path("folder.json")
  dir.create(folder)
  expect_error(write_thresholds_json(export, folder, overwrite = TRUE),
               "is a folder")
})

test_that("a failed write names the file", {
  not_a_folder <- export_temp_path("plain_file")
  writeLines("x", not_a_folder)

  expect_error(thr_write_text("text", file.path(not_a_folder, "export.json")),
               "Could not write")
})

test_that("write_thresholds_json refuses an export ifcb-classify would misread", {
  good <- strata_export_fixture()
  write <- function(export) {
    write_thresholds_json(export, export_temp_path())
  }
  change <- function(field, value) {
    export <- good
    export[field] <- list(value)
    export
  }
  with_threshold <- function(value) {
    export <- good
    export$class_metrics[["Debris"]]["threshold"] <- list(value)
    export
  }
  unnamed <- good
  names(unnamed$class_metrics) <- NULL
  duplicated_key <- good
  names(duplicated_key$class_metrics)[2] <- "Debris"

  expect_error(write("not an export"), "export")
  expect_error(write(change("validation_transform", NULL)),
               "validation_transform")
  expect_error(write(change("validation_transform", NA_character_)),
               "validation_transform")
  expect_error(write(change("validation_transform", "")),
               "validation_transform")
  expect_error(write(change("class_metrics", NULL)), "class_metrics")
  expect_error(write(change("class_metrics", list())), "class_metrics")
  expect_error(write(unnamed), "named")
  expect_error(write(duplicated_key), "more than once")
  expect_error(write(with_threshold(NA_real_)), "Debris")
  expect_error(write(with_threshold(NULL)), "Debris")
  expect_error(write(with_threshold(1.5)), "between 0 and 1")
  expect_error(write(with_threshold("0.5")), "between 0 and 1")
  expect_error(write(with_threshold(c(0.1, 0.2))), "between 0 and 1")
})

# =============================================================================
# CSV writers
# =============================================================================

test_that("write_thresholds_csv writes class and threshold for every class", {
  path <- export_temp_path("thresholds.csv")
  labels <- strata_awkward_labels()

  result <- withVisible(write_thresholds_csv(strata_export_fixture(), path))
  read <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE,
                          colClasses = c("character", "numeric"),
                          stringsAsFactors = FALSE)

  expect_identical(result$value, path)
  expect_false(result$visible)
  expect_identical(readLines(path, n = 1), "class,threshold")
  expect_named(read, c("class", "threshold"))
  expect_identical(enc2utf8(read$class), labels)
  expect_identical(read$threshold, c(0.1 + 0.2, 0.9999, 1 / 3, 0, 1))
})

test_that("write_thresholds_csv writes UTF-8 and readable numbers", {
  path <- export_temp_path("thresholds.csv")
  write_thresholds_csv(strata_export_fixture(), path)
  bytes <- readBin(path, "raw", file.size(path))
  lines <- readLines(path, encoding = "UTF-8")

  # U+00D6 is C3 96 in UTF-8
  expect_true(grepl("c396", paste(as.character(bytes), collapse = "")))
  expect_false(any(bytes == as.raw(0x0d)))
  expect_identical(lines[2], "Skeletonema_spp,0.30000000000000004")
  # No more digits than needed to read the same number back
  expect_identical(lines[3], "Snowella/Woronichinia,0.9999")
  expect_identical(lines[6], "Debris,1")
})

test_that("CSV fields with commas, quotes and line breaks are quoted", {
  labels <- c("a,b", "say \"hi\"", "plain", "two\nlines")
  path <- export_temp_path("thresholds.csv")
  write_thresholds_csv(export_for_labels(labels), path)

  read <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE,
                          stringsAsFactors = FALSE)

  expect_identical(read$class, labels)
  expect_identical(read$threshold, c(0.1, 0.2, 0.3, 0.4))
  expect_identical(readLines(path)[2:3],
                   c("\"a,b\",0.1", "\"say \"\"hi\"\"\",0.2"))
})

test_that("write_thresholds_report_csv writes one row per class with every field", {
  path <- export_temp_path("report.csv")
  resolved <- strata_resolved_fixture()

  result <- withVisible(
    write_thresholds_report_csv(strata_export_fixture(), path)
  )
  read <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE,
                          stringsAsFactors = FALSE)

  expect_identical(result$value, path)
  expect_false(result$visible)
  expect_named(read, c(
    "class_name", "threshold", "source", "status", "reason", "n_predicted",
    "n_verdicts", "n_correct", "precision", "precision_lower",
    "precision_upper", "kept_share", "previous_threshold"
  ))
  expect_identical(nrow(read), 5L)
  expect_identical(enc2utf8(read$class_name), resolved$class_name)
  expect_identical(read$threshold, resolved$threshold)
  expect_identical(read$source, resolved$source)
  expect_identical(read$status, resolved$status)
  expect_identical(read$reason, resolved$reason)
  expect_identical(read$n_predicted, resolved$n_predicted)
  expect_identical(read$n_verdicts, resolved$n_verdicts)
  expect_identical(read$precision, resolved$precision)
  expect_identical(read$precision_lower, resolved$lower)
  expect_identical(read$precision_upper, resolved$upper)
  expect_identical(read$kept_share, resolved$kept_share)
  expect_identical(read$previous_threshold, resolved$previous_threshold)
})

test_that("the report CSV leaves fields absent from a hand-made export empty", {
  export <- list(
    validation_transform = "dataset_squarepad",
    class_metrics = list(
      Debris = list(class_name = "Debris", threshold = 0.5),
      Other = list(class_name = "Other", threshold = 0.25, status = "manual")
    )
  )
  path <- export_temp_path("report.csv")
  write_thresholds_report_csv(export, path)

  read <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_identical(read$class_name, c("Debris", "Other"))
  expect_identical(read$threshold, c(0.5, 0.25))
  expect_identical(read$status, c("", "manual"))
  expect_true(all(is.na(read$precision)))
})

test_that("the CSV writers refuse to replace a file unless asked to", {
  export <- strata_export_fixture()
  writers <- list(write_thresholds_csv, write_thresholds_report_csv)

  for (writer in writers) {
    path <- export_temp_path("existing.csv")
    writeLines("keep me", path)

    expect_error(writer(export, path), "already exists")
    expect_identical(readLines(path), "keep me")
    writer(export, path, overwrite = TRUE)
    expect_gt(length(readLines(path)), 5)

    expect_error(writer(export, file.path(tempfile("thr_missing_"), "x.csv")),
                 "does not exist")
    expect_error(writer("not an export", export_temp_path("x.csv")), "export")
  }
})

# =============================================================================
# thr_default_export_name
# =============================================================================

test_that("thr_default_export_name builds a name that is not auto-detected", {
  expect_identical(thr_default_export_name("fixture_A"),
                   "fixture_A_holdout_thresholds.json")
  expect_identical(thr_default_export_name("resnet50 v2 (2026)"),
                   "resnet50_v2_(2026)_holdout_thresholds.json")
  expect_identical(thr_default_export_name("a/b\\c:d*e?f\"g<h>i|j"),
                   "a_b_c_d_e_f_g_h_i_j_holdout_thresholds.json")
  expect_identical(thr_default_export_name("\u00d6resund"),
                   "\u00d6resund_holdout_thresholds.json")
  expect_identical(thr_default_export_name(NA_character_),
                   "model_holdout_thresholds.json")
  expect_identical(thr_default_export_name(""),
                   "model_holdout_thresholds.json")
  expect_identical(thr_default_export_name(NULL),
                   "model_holdout_thresholds.json")
  expect_identical(thr_default_export_name("..hidden"),
                   "hidden_holdout_thresholds.json")
})

test_that("default names are accepted by write_thresholds_json", {
  export <- strata_export_fixture()

  for (name in c("fixture_A", "thresholds", "run_thresholds_and_metrics",
                 "a/b", NA_character_)) {
    path <- export_temp_path(thr_default_export_name(name))
    expect_no_error(write_thresholds_json(export, path))
    expect_true(file.exists(path))
  }
})

# =============================================================================
# End to end: counts -> resolved -> export
# =============================================================================

test_that("resolved thresholds reach the JSON unchanged", {
  labels <- strata_awkward_labels()
  strata <- rbind(
    strata_for_class(labels[2], 1:3, c(100, 50, 50), c(20, 20, 20),
                     c(10, 16, 19)),
    strata_for_class(labels[4], 1:3, c(100, 50, 50), c(20, 20, 20),
                     c(19, 19, 20))
  )
  overrides <- data.frame(class_name = labels[3], threshold = 1 / 3,
                          stringsAsFactors = FALSE)
  resolved <- thr_resolve_thresholds(
    strata, labels, fallback = c(0.1 + 0.2, NA, 0.9, NA, NaN),
    overrides = overrides, edges = strata_small_edges(), seed = 1
  )
  export <- build_thresholds_export(resolved, strata_model_fixture(),
                                    target = 0.9, fallback_source = "h5",
                                    edges = strata_small_edges())
  path <- export_temp_path()
  write_thresholds_json(export, path)

  metrics <- jsonlite::fromJSON(path, simplifyVector = FALSE)$class_metrics
  read <- vapply(metrics, function(m) as.numeric(m$threshold), numeric(1))

  expect_identical(names(read), labels)
  expect_identical(unname(read), resolved$threshold)
  expect_identical(unname(read), c(0.1 + 0.2, 0.9, 1 / 3, 0, 0))
  expect_identical(
    unname(vapply(metrics, function(m) m$source, character(1))),
    c("fallback", "fitted", "manual", "fitted", "fallback")
  )
})

# =============================================================================
# Opt-in: the real ifcb-classify loader
# =============================================================================

test_that("ifcb-classify loads the export with identical thresholds", {
  python <- Sys.getenv("IFCB_CLASSIFY_PYTHON", "")
  skip_if(!nzchar(python),
          "Set IFCB_CLASSIFY_PYTHON to a Python with ifcb_classify installed")

  labels <- c(strata_awkward_labels(), "Microbe_\U0001F9A0", "quote\"and\\slash")
  thresholds <- c(0.1 + 0.2, 0.9999, 1 / 3, 0, 1, 2 / 3, 0.995)
  path <- export_temp_path()
  write_thresholds_json(export_for_labels(labels, thresholds), path)

  # Labels go through a UTF-8 file: a command line would depend on the locale
  labels_path <- file.path(dirname(path), "labels.json")
  con <- file(labels_path, "wb")
  writeLines(enc2utf8(as.character(jsonlite::toJSON(labels))), con,
             useBytes = TRUE)
  close(con)

  script <- paste(
    "import json, locale, struct, sys",
    "from ifcb_classify.thresholds import load_thresholds_json, thresholds_fitted_on_augmented",
    "labels = json.load(open(sys.argv[2], encoding='utf-8'))",
    "values = [float(x) for x in load_thresholds_json(sys.argv[1], labels)]",
    paste0("print(json.dumps({'repr': [repr(x) for x in values], ",
           "'hex': [x.hex() for x in values], ",
           "'bits': [struct.pack('>d', x).hex() for x in values], ",
           "'augmented': thresholds_fitted_on_augmented(sys.argv[1]), ",
           "'n_labels': len(labels), ",
           "'encoding': locale.getpreferredencoding(False)}))"),
    sep = "; "
  )
  stderr_path <- file.path(dirname(path), "stderr.txt")
  # LC_ALL=C makes open() default to ASCII, standing in for the non-UTF-8
  # default encoding of Windows: the export must load there too.
  output <- system2(
    python, c("-X", "utf8=0", "-c", shQuote(script), shQuote(path),
              shQuote(labels_path)),
    stdout = TRUE, stderr = stderr_path,
    env = c("LC_ALL=C", "PYTHONCOERCECLOCALE=0", "PYTHONUTF8=0")
  )
  stderr_text <- paste(readLines(stderr_path, warn = FALSE), collapse = "\n")
  expect_null(attr(output, "status"), info = stderr_text)
  result <- jsonlite::fromJSON(output[length(output)], simplifyVector = TRUE)

  expect_identical(result$n_labels, length(labels))
  # Bit-for-bit, without trusting any decimal parser
  expect_identical(result$bits, strata_double_bits(thresholds),
                   info = paste(result$repr, collapse = " "))
  expect_identical(as.numeric(result$repr), thresholds)
  expect_identical(result$repr[1:5],
                   c("0.30000000000000004", "0.9999", "0.3333333333333333",
                     "0.0", "1.0"))
  expect_false(result$augmented)
  # A label ifcb-classify cannot match is logged as an unknown class
  expect_no_match(stderr_text, "Unknown class", fixed = TRUE)
  message("ifcb-classify loaded the export under encoding ", result$encoding,
          ": ", paste(result$repr, collapse = ", "))
})
