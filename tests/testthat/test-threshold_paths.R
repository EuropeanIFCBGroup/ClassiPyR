# Tests for the threshold-review path, settings and encoding helpers

library(testthat)

# Evaluate `code` with get_config_dir() pointed at tempdir(), the way it
# behaves under R CMD check, so nothing is written to the user's config dir.
thr_test_with_temp_config <- function(code) {
  old <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "ClassiPyR")
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
    } else {
      Sys.setenv("_R_CHECK_PACKAGE_NAME_" = old)
    },
    add = TRUE
  )
  force(code)
}

test_that("database paths sit inside the review folder", {
  expect_equal(thr_verdict_db_path("/data/review"),
               file.path("/data/review", "threshold_verdicts.sqlite"))
  expect_equal(thr_index_db_path("/data/review"),
               file.path("/data/review", "threshold_index.sqlite"))
})

test_that("thr_settings_path creates the config dir and names its own file", {
  thr_test_with_temp_config({
    path <- thr_settings_path()

    expect_equal(basename(path), "threshold_settings.json")
    expect_equal(dirname(path), get_config_dir())
    expect_true(dir.exists(dirname(path)))
    # Must not collide with the main app's settings file
    expect_false(identical(path, get_settings_path()))
  })
})

test_that("thr_load_settings returns an empty list when absent or unreadable", {
  missing_file <- tempfile(fileext = ".json")
  expect_identical(thr_load_settings(missing_file), list())

  garbage <- tempfile(fileext = ".json")
  on.exit(unlink(garbage), add = TRUE)
  writeLines("{ this is not json", garbage)
  expect_identical(thr_load_settings(garbage), list())

  # Valid JSON that is not an object is no use as settings either
  writeLines("[1, 2, 3]", garbage)
  expect_identical(thr_load_settings(garbage), list())
})

test_that("thr_save_settings merges into the existing file", {
  settings_file <- tempfile(fileext = ".json")
  on.exit(unlink(settings_file), add = TRUE)

  thr_save_settings(list(h5_folder = "/data/h5", page_size = 100), settings_file)
  merged <- thr_save_settings(list(page_size = 50, annotator = "AT"), settings_file)

  loaded <- thr_load_settings(settings_file)
  expect_equal(loaded$h5_folder, "/data/h5")
  expect_equal(loaded$page_size, 50)
  expect_equal(loaded$annotator, "AT")
  expect_equal(merged, loaded)

  # A NULL value removes the key (update_settings_file() semantics)
  thr_save_settings(list(annotator = NULL), settings_file)
  expect_null(thr_load_settings(settings_file)$annotator)
  expect_equal(thr_load_settings(settings_file)$h5_folder, "/data/h5")
})

test_that("thr_save_settings creates the folder and keeps non-ASCII paths", {
  settings_dir <- tempfile("thr_settings_")
  on.exit(unlink(settings_dir, recursive = TRUE), add = TRUE)
  settings_file <- file.path(settings_dir, "nested", "threshold_settings.json")

  thr_save_settings(list(h5_folder = "/data/Öresund"), settings_file)
  expect_true(file.exists(settings_file))
  expect_equal(thr_load_settings(settings_file)$h5_folder, "/data/Öresund")
})

test_that("thr_save_settings uses the default settings path", {
  thr_test_with_temp_config({
    settings_file <- thr_settings_path()
    expect_true(startsWith(normalizePath(dirname(settings_file), winslash = "/"),
                           normalizePath(tempdir(), winslash = "/")))

    thr_save_settings(list(target_precision = 0.95))
    expect_equal(thr_load_settings()$target_precision, 0.95)
    unlink(settings_file)
  })
})

test_that("thr_save_settings rejects settings that are not a named list", {
  settings_file <- tempfile(fileext = ".json")
  expect_error(thr_save_settings("h5_folder", settings_file), "named list")
  expect_error(thr_save_settings(list("a", "b"), settings_file), "named list")
  expect_error(thr_save_settings(list(a = 1, 2), settings_file), "named list")
  expect_false(file.exists(settings_file))
})

test_that("thr_validate_review_dir returns the normalised path of a folder", {
  review_dir <- tempfile("thr_review_")
  dir.create(review_dir)
  on.exit(unlink(review_dir, recursive = TRUE), add = TRUE)

  out <- thr_validate_review_dir(file.path(review_dir, "."))
  expect_equal(out, normalizePath(review_dir, winslash = "/"))
  expect_false(grepl("\\\\", out))
})

test_that("thr_validate_review_dir creates the folder only when asked", {
  review_dir <- file.path(tempfile("thr_review_"), "nested")
  on.exit(unlink(dirname(review_dir), recursive = TRUE), add = TRUE)

  expect_error(thr_validate_review_dir(review_dir), "does not exist")
  expect_false(dir.exists(review_dir))

  out <- thr_validate_review_dir(review_dir, create = TRUE)
  expect_true(dir.exists(review_dir))
  expect_equal(out, normalizePath(review_dir, winslash = "/"))
})

test_that("thr_validate_review_dir rejects files and malformed arguments", {
  a_file <- tempfile(fileext = ".txt")
  writeLines("x", a_file)
  on.exit(unlink(a_file), add = TRUE)

  expect_error(thr_validate_review_dir(a_file), "not a folder")
  expect_error(thr_validate_review_dir(a_file, create = TRUE), "not a folder")
  expect_error(thr_validate_review_dir(NULL), "single")
  expect_error(thr_validate_review_dir(NA_character_), "single")
  expect_error(thr_validate_review_dir(""), "single")
  expect_error(thr_validate_review_dir(c("a", "b")), "single")
  expect_error(thr_validate_review_dir(42), "single")
})

test_that("thr_validate_review_dir reports a folder that cannot be created", {
  a_file <- tempfile(fileext = ".txt")
  writeLines("x", a_file)
  on.exit(unlink(a_file), add = TRUE)

  # A folder cannot be created underneath a regular file
  expect_error(thr_validate_review_dir(file.path(a_file, "review"), create = TRUE),
               "Could not create")
})

test_that("thr_validate_review_dir refuses a folder it cannot write to", {
  skip_on_os("windows")
  review_dir <- tempfile("thr_readonly_")
  dir.create(review_dir)
  on.exit({
    Sys.chmod(review_dir, "755")
    unlink(review_dir, recursive = TRUE)
  }, add = TRUE)
  Sys.chmod(review_dir, "555")
  # Root can write anywhere, so the permission bits prove nothing
  skip_if(file.access(review_dir, mode = 2) == 0, "folder is writable regardless")

  expect_error(thr_validate_review_dir(review_dir), "not writable")
})

test_that("thr_string_md5 matches known md5 digests", {
  expect_identical(thr_string_md5("abc"), "900150983cd24fb0d6963f7d28e17f72")
  expect_identical(thr_string_md5(""), "d41d8cd98f00b204e9800998ecf8427e")
  # md5 of the UTF-8 bytes c3 96, from Python's hashlib
  expect_identical(thr_string_md5("Ö"), "b3426522c7ec61900c76a863534f6a6a")
  expect_null(names(thr_string_md5("abc")))
})

test_that("thr_string_md5 hashes the UTF-8 bytes whatever the input encoding", {
  latin1 <- iconv("Ö", from = "UTF-8", to = "latin1")
  expect_identical(Encoding(latin1), "latin1")
  expect_identical(thr_string_md5(latin1), thr_string_md5("Ö"))
})

test_that("thr_string_md5 keeps embedded newlines and rejects non-strings", {
  expect_false(identical(thr_string_md5("a\nb"), thr_string_md5("ab")))
  expect_error(thr_string_md5(c("a", "b")), "single")
  expect_error(thr_string_md5(NA_character_), "single")
  expect_error(thr_string_md5(1), "single")
})

test_that("labels survive the JSON round trip byte for byte", {
  labels <- c(thr_test_labels, "NA", "quo\"te", "back\\slash", "tab\there", "")
  json <- thr_labels_to_json(labels)

  expect_type(json, "character")
  expect_length(json, 1)
  expect_identical(thr_labels_from_json(json), labels)
  # A single label must stay a JSON array, not become a bare string
  expect_identical(thr_labels_to_json("only"), "[\"only\"]")
  expect_identical(thr_labels_from_json(thr_labels_to_json("only")), "only")
  expect_identical(thr_labels_from_json("[]"), character())
})

test_that("numbers survive the JSON round trip exactly and NaN becomes null", {
  values <- c(0.1 + 0.2, 0.9999, NaN, 1 / 3, 0.05, 1, 0, NA)
  json <- thr_numbers_to_json(values)

  expect_match(json, "null", fixed = TRUE)
  expect_false(grepl("NaN|NA", json))
  back <- thr_numbers_from_json(json)
  expect_identical(back[c(1, 2, 4:7)], values[c(1, 2, 4:7)])
  expect_identical(back[c(3, 8)], c(NA_real_, NA_real_))
})

test_that("number JSON helpers handle all-missing, empty and absent input", {
  expect_identical(thr_numbers_to_json(c(NaN, NaN)), "[null,null]")
  expect_identical(thr_numbers_from_json("[null,null]"), c(NA_real_, NA_real_))
  expect_identical(thr_numbers_from_json(thr_numbers_to_json(numeric())), numeric())
  expect_identical(thr_numbers_from_json(thr_numbers_to_json(0.5)), 0.5)
  # A NULL column value from the database means "no vector stored"
  expect_null(thr_numbers_from_json(NA_character_))
  expect_null(thr_numbers_from_json(NULL))
})
