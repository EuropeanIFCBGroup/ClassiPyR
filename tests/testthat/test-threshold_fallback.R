# Tests for the fallback threshold readers and the exclusion-file reader

library(testthat)

# Write a training-style thresholds JSON by hand, so the test controls every
# byte (Python's json.dump writes NaN as a bare token, which is not JSON).
thr_test_write_text <- function(text, path = tempfile(fileext = ".json")) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
  path
}

thr_test_metrics_json <- function(entries) {
  body <- sprintf('        "%s": {"class_name": "%s", "threshold": %s, "f1": 0.9}',
                  names(entries), names(entries), unlist(entries))
  paste0('{\n    "model_name": "run",\n    "num_classes": ', length(entries),
         ',\n    "validation_transform": "dataset_squarepad",\n',
         '    "class_metrics": {\n', paste(body, collapse = ",\n"),
         '\n    },\n    "macro_F1": 0.9\n}\n')
}

# The fixture labels in a different order than the model's, like a file that
# was written from a sorted class list
thr_test_fallback_entries <- function() {
  list("Debris" = "0.6", "Skeletonema_spp" = "0.30000000000000004",
       "Öresund_ciliate" = "0.9999",
       "Pauliella_taeniata_001" = "0.001037058886140585",
       "Snowella/Woronichinia" = "1")
}

# --- thr_read_fallback_json --------------------------------------------------

test_that("thresholds are read by class label, in the model's order", {
  path <- thr_test_write_text(thr_test_metrics_json(thr_test_fallback_entries()))
  on.exit(unlink(path), add = TRUE)

  thresholds <- thr_read_fallback_json(path, thr_test_labels)

  expect_identical(as.numeric(thresholds),
                   c(0.1 + 0.2, 1, 0.001037058886140585, 0.9999, 0.6))
  expect_identical(attr(thresholds, "absent"), character())
})

test_that("non-ASCII labels match when the file escapes them, as Python does", {
  text <- thr_test_metrics_json(thr_test_fallback_entries())
  text <- gsub("Ö", "\\u00d6", text, fixed = TRUE)
  expect_false(grepl("Ö", text, fixed = TRUE))
  path <- thr_test_write_text(text)
  on.exit(unlink(path), add = TRUE)

  thresholds <- thr_read_fallback_json(path, thr_test_labels)

  expect_identical(thresholds[4], 0.9999)
})

test_that("classes without an entry or without a threshold are NA", {
  entries <- thr_test_fallback_entries()[c("Debris", "Skeletonema_spp")]
  entries[["Debris"]] <- "null"
  entries[["Some_other_class"]] <- "0.4"
  path <- thr_test_write_text(thr_test_metrics_json(entries))
  on.exit(unlink(path), add = TRUE)

  thresholds <- thr_read_fallback_json(path, thr_test_labels)

  expect_identical(as.numeric(thresholds), c(0.1 + 0.2, NA, NA, NA, NA))
  # The classes the file says nothing about, for the caller to show
  expect_identical(attr(thresholds, "absent"), thr_test_labels[2:4])
})

test_that("an entry without a threshold field is NA", {
  path <- thr_test_write_text(paste0(
    '{"class_metrics": {"Debris": {"f1": 0.5}, ',
    '"Skeletonema_spp": {"threshold": 0.25}}}'
  ))
  on.exit(unlink(path), add = TRUE)

  thresholds <- thr_read_fallback_json(path, thr_test_labels)

  expect_identical(as.numeric(thresholds), c(0.25, NA, NA, NA, NA))
  expect_identical(attr(thresholds, "absent"), thr_test_labels[2:4])
})

test_that("a bare NaN, as Python writes it, is read as no threshold", {
  entries <- thr_test_fallback_entries()
  entries[["Debris"]] <- "NaN"
  entries[["Skeletonema_spp"]] <- "NaN"
  text <- thr_test_metrics_json(entries)
  text <- sub('"macro_F1": 0.9', '"macro_F1": NaN', text, fixed = TRUE)
  expect_error(jsonlite::parse_json(text))
  path <- thr_test_write_text(text)
  on.exit(unlink(path), add = TRUE)

  thresholds <- thr_read_fallback_json(path, thr_test_labels)

  expect_identical(as.numeric(thresholds),
                   c(NA, 1, 0.001037058886140585, 0.9999, NA))
  expect_identical(attr(thresholds, "absent"), character())
})

test_that("NaN inside a class label is left alone when a bare NaN is repaired", {
  labels <- c("NaN", "Odd: NaN, \\\"NaN\\\" label", "Plain")
  entries <- stats::setNames(list("0.5", "NaN", "0.25"), labels)
  path <- thr_test_write_text(thr_test_metrics_json(entries))
  on.exit(unlink(path), add = TRUE)

  thresholds <- thr_read_fallback_json(
    path, c("NaN", "Odd: NaN, \"NaN\" label", "Plain")
  )

  expect_identical(as.numeric(thresholds), c(0.5, NA, 0.25))
  expect_identical(attr(thresholds, "absent"), character())
})

test_that("a file that is not JSON is refused with an explanation", {
  not_json <- thr_test_write_text("model_name: run\nclass_metrics:\n  Debris: 0.5\n")
  truncated <- thr_test_write_text('{"class_metrics": {"Debris": {"threshold": 0.5')
  infinite <- thr_test_write_text(
    '{"class_metrics": {"Debris": {"threshold": Infinity}}}'
  )
  url_like <- thr_test_write_text("https://example.invalid/thresholds.json")
  empty <- thr_test_write_text("")
  on.exit(unlink(c(not_json, truncated, infinite, url_like, empty)), add = TRUE)

  expect_error(thr_read_fallback_json(not_json, thr_test_labels), "not valid JSON")
  expect_error(thr_read_fallback_json(truncated, thr_test_labels), "not valid JSON")
  expect_error(thr_read_fallback_json(infinite, thr_test_labels), "Infinity")
  expect_error(thr_read_fallback_json(url_like, thr_test_labels), "not valid JSON")
  expect_error(thr_read_fallback_json(empty, thr_test_labels), "not valid JSON")
})

test_that("a JSON file without class_metrics is refused", {
  no_metrics <- thr_test_write_text('{"model_name": "run", "thresholds": [0.1, 0.2]}')
  array_metrics <- thr_test_write_text('{"class_metrics": [0.1, 0.2, 0.3, 0.4, 0.5]}')
  an_array <- thr_test_write_text("[1, 2, 3]")
  on.exit(unlink(c(no_metrics, array_metrics, an_array)), add = TRUE)

  expect_error(thr_read_fallback_json(no_metrics, thr_test_labels), "class_metrics")
  expect_error(thr_read_fallback_json(array_metrics, thr_test_labels), "class_metrics")
  expect_error(thr_read_fallback_json(an_array, thr_test_labels), "class_metrics")
})

test_that("a file for another class list is refused", {
  other <- thr_test_write_text(thr_test_metrics_json(
    list("Aphanizomenon" = "0.5", "Dinophysis" = "0.4")
  ))
  indexed <- thr_test_write_text(thr_test_metrics_json(
    list("0" = "0.5", "1" = "0.4", "2" = "0.3", "3" = "0.2", "4" = "0.1")
  ))
  on.exit(unlink(c(other, indexed)), add = TRUE)

  expect_error(thr_read_fallback_json(other, thr_test_labels),
               "None of the 5 classes")
  # Keys that are class indices (an old format) are not matched by position
  expect_error(thr_read_fallback_json(indexed, thr_test_labels), "class indices")
})

test_that("a threshold that is not a number between 0 and 1 is refused", {
  write_one <- function(value) {
    thr_test_write_text(thr_test_metrics_json(list("Debris" = value)))
  }
  too_high <- write_one("1.5")
  negative <- write_one("-0.1")
  text <- write_one('"0.5"')
  several <- write_one("[0.1, 0.2]")
  not_object <- thr_test_write_text('{"class_metrics": {"Debris": 0.5}}')
  on.exit(unlink(c(too_high, negative, text, several, not_object)), add = TRUE)

  expect_error(thr_read_fallback_json(too_high, thr_test_labels), "Debris")
  expect_error(thr_read_fallback_json(too_high, thr_test_labels), "between 0 and 1")
  expect_error(thr_read_fallback_json(negative, thr_test_labels), "between 0 and 1")
  expect_error(thr_read_fallback_json(text, thr_test_labels), "Debris")
  expect_error(thr_read_fallback_json(several, thr_test_labels), "Debris")
  expect_error(thr_read_fallback_json(not_object, thr_test_labels), "Debris")
})

test_that("thr_read_fallback_json validates its arguments", {
  path <- thr_test_write_text(thr_test_metrics_json(thr_test_fallback_entries()))
  on.exit(unlink(path), add = TRUE)

  expect_error(thr_read_fallback_json(file.path(tempdir(), "no_such.json"),
                                      thr_test_labels), "not found")
  expect_error(thr_read_fallback_json(tempdir(), thr_test_labels), "not found")
  expect_error(thr_read_fallback_json(c(path, path), thr_test_labels), "json_path")
  expect_error(thr_read_fallback_json(path, character()), "class_labels")
  expect_error(thr_read_fallback_json(path, 1:5), "class_labels")
  expect_error(thr_read_fallback_json(path, c("Debris", NA)), "class_labels")
})

test_that("an export of this app reads back as the thresholds it holds", {
  skip_if_not(exists("strata_export_fixture"),
              "needs the export fixture of the estimate tests")
  folder <- tempfile("thr_export_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  path <- file.path(folder, "fixture_A_holdout_thresholds.json")
  write_thresholds_json(strata_export_fixture(), path)

  thresholds <- thr_read_fallback_json(path, strata_awkward_labels())

  expect_identical(as.numeric(thresholds), strata_resolved_fixture()$threshold)
})

# --- thr_h5_fallback ---------------------------------------------------------

test_that("files that share one thresholds vector give it back", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  consistent <- thr_h5_fallback(space$index_path, thr_test_model_a_id,
                                sample_names = thr_test_a_samples[-3])

  expect_identical(consistent$n_sets, 1L)
  expect_identical(consistent$thresholds, c(0.3, 0.05, 0.9, NA, 0.6))
  # No threshold is NA, never NaN
  expect_false(is.nan(consistent$thresholds[4]))

  model_b <- thr_h5_fallback(space$index_path, thr_test_model_b_id)
  expect_identical(model_b, list(thresholds = rep(0.2, 5), n_sets = 1L))
})

test_that("files that disagree give no thresholds and say how many sets there are", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  # The 0103 file of model A was written with other thresholds on purpose
  everything <- thr_h5_fallback(space$index_path, thr_test_model_a_id)

  expect_null(everything$thresholds)
  expect_identical(everything$n_sets, 2L)
  only_odd <- thr_h5_fallback(space$index_path, thr_test_model_a_id,
                              sample_names = thr_test_a_samples[3])
  expect_identical(only_odd, list(thresholds = rep(0.5, 5), n_sets = 1L))
})

test_that("the h5 fallback agrees with the real class-score files", {
  skip_if_not_installed("hdf5r")
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_copy_fixtures(space$h5_folder)
  build_threshold_index(space$h5_folder, space$index_path)

  expect_identical(thr_h5_fallback(space$index_path, thr_test_model_a_id)$n_sets, 2L)
  expect_identical(
    thr_h5_fallback(space$index_path, thr_test_model_a_id,
                    sample_names = thr_test_a_samples[-3]),
    list(thresholds = c(0.3, 0.05, 0.9, NA, 0.6), n_sets = 1L)
  )
})

test_that("only usable files count towards the h5 fallback", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(file.path(space$h5_folder, "a"), thr_test_a_samples[1])
  # A second file for the same sample is set aside as a duplicate
  thr_test_write_fake(file.path(space$h5_folder, "b"), thr_test_a_samples[1],
                      thr_test_summary(thresholds = rep(0.7, 5)))
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id

  expect_identical(thr_h5_fallback(space$index_path, model_id),
                   list(thresholds = c(0.3, 0.05, 0.9, NA, 0.6), n_sets = 1L))
})

test_that("a model without any stored threshold gives all NA", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(space$h5_folder, thr_test_a_samples[1],
                      thr_test_summary(thresholds = rep(NaN, 5)))
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id

  expect_identical(thr_h5_fallback(space$index_path, model_id),
                   list(thresholds = rep(NA_real_, 5), n_sets = 1L))
})

test_that("files outside the review's class-score folder do not count", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  # The same model scored another sample elsewhere, with other thresholds
  expected <- thr_test_expected()$files[[1]]
  elsewhere <- file.path(space$root, "other_scores")
  thr_test_write_fake(
    elsewhere, "D20240301T000000_IFCB134",
    thr_test_summary(checkpoint_sha256 = expected$checkpoint_sha256,
                     classifier_name = expected$classifier_name,
                     thresholds = rep(0.7, 5))
  )
  build_threshold_index(elsewhere, space$index_path, reader = thr_test_fake_reader())

  everywhere <- thr_h5_fallback(space$index_path, thr_test_model_a_id)
  here <- thr_h5_fallback(space$index_path, thr_test_model_a_id,
                          sample_names = c(thr_test_a_samples[-3],
                                           "D20240301T000000_IFCB134"),
                          h5_folder = space$h5_folder)
  there <- thr_h5_fallback(space$index_path, thr_test_model_a_id,
                           h5_folder = paste0(elsewhere, "/"))

  expect_identical(everywhere$n_sets, 3L)
  expect_identical(here, list(thresholds = c(0.3, 0.05, 0.9, NA, 0.6), n_sets = 1L))
  expect_identical(there, list(thresholds = rep(0.7, 5), n_sets = 1L))
  expect_error(thr_h5_fallback(space$index_path, thr_test_model_a_id,
                               h5_folder = c("a", "b")), "h5_folder")
})

test_that("a sample that is usable twice counts once, as in the frame", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  sample_name <- "D20240101T000000_IFCB134"
  first <- file.path(space$h5_folder, "a_first")
  copy <- file.path(space$h5_folder, "b_rerun")
  thr_test_write_fake(first, sample_name)
  thr_test_write_fake(copy, sample_name,
                      thr_test_summary(thresholds = rep(0.7, 5)))
  # Built one folder at a time, so neither file is set aside for the other
  build_threshold_index(first, space$index_path, reader = thr_test_fake_reader())
  build_threshold_index(copy, space$index_path, reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id

  # The frame on the enclosing folder takes the first file by path, and so
  # does the fallback: the unused copy's thresholds are not a second set
  expect_identical(
    thr_h5_fallback(space$index_path, model_id, h5_folder = space$h5_folder),
    list(thresholds = c(0.3, 0.05, 0.9, NA, 0.6), n_sets = 1L)
  )
})

test_that("a file indexed without a thresholds vector counts as having none", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  thr_test_execute(space$index_path,
                   "UPDATE files SET thresholds_json = NULL WHERE model_id = ?",
                   params = list(thr_test_model_b_id))

  expect_identical(thr_h5_fallback(space$index_path, thr_test_model_b_id),
                   list(thresholds = rep(NA_real_, 5), n_sets = 1L))
})

test_that("no matching files gives no thresholds and zero sets", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  none <- thr_h5_fallback(space$index_path, thr_test_model_a_id,
                          sample_names = "D20190101T000000_IFCB134")
  empty <- thr_h5_fallback(space$index_path, thr_test_model_a_id,
                           sample_names = character())

  expect_identical(none, list(thresholds = NULL, n_sets = 0L))
  expect_identical(empty, list(thresholds = NULL, n_sets = 0L))
})

test_that("thr_h5_fallback refuses what it cannot answer", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(thr_h5_fallback(space$index_path, thr_test_model_a_id),
               "build_threshold_index")
  expect_false(file.exists(space$index_path))
  thr_test_fixture_index(space)
  expect_error(thr_h5_fallback(space$index_path, "no-such-model"),
               "not in the index")
  expect_error(thr_h5_fallback(space$index_path, NA_character_), "model_id")
  expect_error(thr_h5_fallback(space$index_path, thr_test_model_a_id,
                               sample_names = 1:3), "sample_names")
  expect_error(thr_h5_fallback(space$index_path, thr_test_model_a_id,
                               sample_names = c("a", NA)), "sample_names")
})

# --- thr_read_exclusion_file -------------------------------------------------

test_that("sample names are read one per line, skipping blanks and comments", {
  path <- thr_test_write_text(paste(
    "# training set of model V6",
    "D20240101T000000_IFCB134",
    "",
    "   D20240102T000000_IFCB134  ",
    "\t# an indented comment",
    "D20240103T000000_IFCB134 # with a remark",
    "D20240101T000000_IFCB134",
    "D20240104T000000_IFCB1",
    sep = "\n"
  ), tempfile(fileext = ".txt"))
  on.exit(unlink(path), add = TRUE)

  samples <- thr_read_exclusion_file(path)

  expect_identical(as.character(samples),
                   c("D20240101T000000_IFCB134", "D20240102T000000_IFCB134",
                     "D20240103T000000_IFCB134", "D20240104T000000_IFCB1"))
  expect_identical(attr(samples, "invalid"), character())
})

test_that("lines that are not sample names are reported, not returned", {
  path <- thr_test_write_text(paste(
    "sample_name",
    "D20240101T000000_IFCB134",
    "D20240101T000000_IFCB134.roi",
    "D20240102T000000_IFCB134,training",
    "20240103T000000_IFCB134",
    "D20240104T000000_IFCB134 D20240105T000000_IFCB134",
    "sample_name",
    sep = "\n"
  ), tempfile(fileext = ".txt"))
  on.exit(unlink(path), add = TRUE)

  samples <- thr_read_exclusion_file(path)

  expect_identical(as.character(samples), "D20240101T000000_IFCB134")
  expect_identical(attr(samples, "invalid"),
                   c("sample_name", "D20240101T000000_IFCB134.roi",
                     "D20240102T000000_IFCB134,training",
                     "20240103T000000_IFCB134",
                     "D20240104T000000_IFCB134 D20240105T000000_IFCB134"))
})

test_that("Windows line endings, a byte order mark and odd bytes are handled", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeBin(c(
    as.raw(c(0xef, 0xbb, 0xbf)),
    charToRaw("D20240101T000000_IFCB134\r\n"),
    # A comment saved as Latin-1, which is not valid UTF-8
    charToRaw("# pr"), as.raw(0xf6), charToRaw("v\r\n"),
    charToRaw("D20240102T000000_IFCB134\r\n"),
    charToRaw("D20240103T000000_IFCB134")
  ), con)
  close(con)

  samples <- thr_read_exclusion_file(path)

  expect_identical(as.character(samples),
                   sprintf("D202401%02dT000000_IFCB134", 1:3))
  expect_identical(attr(samples, "invalid"), character())
})

test_that("an empty exclusion file gives no samples", {
  empty <- thr_test_write_text("", tempfile(fileext = ".txt"))
  comments <- thr_test_write_text("# nothing yet\n\n", tempfile(fileext = ".txt"))
  on.exit(unlink(c(empty, comments)), add = TRUE)

  expect_identical(as.character(thr_read_exclusion_file(empty)), character())
  expect_identical(attr(thr_read_exclusion_file(empty), "invalid"), character())
  expect_identical(as.character(thr_read_exclusion_file(comments)), character())
})

test_that("thr_read_exclusion_file refuses a missing file", {
  expect_error(thr_read_exclusion_file(file.path(tempdir(), "no_such_list.txt")),
               "not found")
  expect_error(thr_read_exclusion_file(tempdir()), "not found")
  expect_error(thr_read_exclusion_file(NULL), "path")
  expect_error(thr_read_exclusion_file(c("a", "b")), "path")
})
