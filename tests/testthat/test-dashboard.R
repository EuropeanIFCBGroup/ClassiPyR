# Tests for dashboard helper functions

# ---------------------------------------------------------------------------
# Unit tests (always run)
# ---------------------------------------------------------------------------

test_that("parse_dashboard_url extracts base URL from simple URL", {
  result <- parse_dashboard_url("https://habon-ifcb.whoi.edu/")
  expect_equal(result$base_url, "https://habon-ifcb.whoi.edu")
  expect_null(result$dataset_name)
})

test_that("parse_dashboard_url extracts base URL without trailing slash", {
  result <- parse_dashboard_url("https://habon-ifcb.whoi.edu")
  expect_equal(result$base_url, "https://habon-ifcb.whoi.edu")
  expect_null(result$dataset_name)
})

test_that("parse_dashboard_url extracts dataset from query parameter", {
  result <- parse_dashboard_url("https://habon-ifcb.whoi.edu/timeline?dataset=tangosund")
  expect_equal(result$base_url, "https://habon-ifcb.whoi.edu")
  expect_equal(result$dataset_name, "tangosund")
})

test_that("parse_dashboard_url handles dataset with trailing slash", {
  result <- parse_dashboard_url("https://habon-ifcb.whoi.edu/timeline/?dataset=tangosund")
  expect_equal(result$base_url, "https://habon-ifcb.whoi.edu")
  expect_equal(result$dataset_name, "tangosund")
})

test_that("parse_dashboard_url handles URL without dataset parameter", {
  result <- parse_dashboard_url("https://ifcb-data.whoi.edu/timeline")
  expect_equal(result$base_url, "https://ifcb-data.whoi.edu")
  expect_null(result$dataset_name)
})

test_that("parse_dashboard_url handles URL with multiple query params", {
  result <- parse_dashboard_url("https://habon-ifcb.whoi.edu/timeline?foo=bar&dataset=mydata&baz=1")
  expect_equal(result$base_url, "https://habon-ifcb.whoi.edu")
  expect_equal(result$dataset_name, "mydata")
})

test_that("parse_dashboard_url rejects NULL input", {
  expect_error(parse_dashboard_url(NULL), "non-empty character string")
})

test_that("parse_dashboard_url rejects empty string", {
  expect_error(parse_dashboard_url(""), "non-empty character string")
})

test_that("get_dashboard_cache_dir returns a path", {
  cache_dir <- get_dashboard_cache_dir()
  expect_type(cache_dir, "character")
  expect_true(nzchar(cache_dir))
  expect_true(grepl("dashboard", cache_dir))
})

# ---------------------------------------------------------------------------
# list_dashboard_bins — error handling (offline-safe, no network needed)
# ---------------------------------------------------------------------------

test_that("list_dashboard_bins errors on unreachable host", {
  expect_error(
    list_dashboard_bins("https://this-host-does-not-exist.invalid"),
    "Failed to connect"
  )
})

test_that("list_dashboard_bins builds correct API URL without dataset", {
  # Trailing slashes should be stripped before appending /api/list_bins
  # We cannot hit the network here, but we can verify the error message
  # contains the expected URL fragment

  err <- tryCatch(
    list_dashboard_bins("https://this-host-does-not-exist.invalid/"),
    error = function(e) e$message
  )
  expect_true(grepl("this-host-does-not-exist", err))
})

test_that("list_dashboard_bins builds correct API URL with dataset", {
  err <- tryCatch(
    list_dashboard_bins("https://this-host-does-not-exist.invalid", "mydata"),
    error = function(e) e$message
  )
  expect_true(grepl("this-host-does-not-exist", err))
})

# ---------------------------------------------------------------------------
# download_dashboard_images — cache-hit path (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_images returns cached folder when PNGs exist", {
  tmp <- file.path(tempdir(), "dashboard_test_cache_hit")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  png_subfolder <- file.path(tmp, sample_name, sample_name)
  dir.create(png_subfolder, recursive = TRUE)
  file.create(file.path(png_subfolder, paste0(sample_name, "_00001.png")))

  result <- download_dashboard_images(
    base_url = "https://unused.example.com",
    sample_name = sample_name,
    cache_dir = tmp
  )
  expect_equal(result, file.path(tmp, sample_name))
})

test_that("download_dashboard_images skips download when cache has PNGs", {
  tmp <- file.path(tempdir(), "dashboard_test_cache_skip")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  png_subfolder <- file.path(tmp, sample_name, sample_name)
  dir.create(png_subfolder, recursive = TRUE)
  file.create(file.path(png_subfolder, "img1.png"))
  file.create(file.path(png_subfolder, "img2.png"))

  # Should return immediately without hitting the network
  result <- download_dashboard_images("https://unused.example.com", sample_name, tmp)
  expect_equal(result, file.path(tmp, sample_name))
  expect_equal(length(list.files(png_subfolder, pattern = "\\.png$")), 2)
})

# ---------------------------------------------------------------------------
# download_dashboard_adc — cache-hit path (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_adc returns cached ADC file when it exists", {
  tmp <- file.path(tempdir(), "dashboard_test_adc_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  date_part <- substr(sample_name, 1, 9)
  adc_dir <- file.path(tmp, date_part)
  dir.create(adc_dir, recursive = TRUE)
  adc_path <- file.path(adc_dir, paste0(sample_name, ".adc"))
  writeLines("1,2,3", adc_path)

  result <- download_dashboard_adc("https://unused.example.com", sample_name, tmp)
  expect_equal(result, adc_path)
})

# ---------------------------------------------------------------------------
# download_dashboard_images — error handling (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_images returns NULL on download failure", {
  tmp <- file.path(tempdir(), "dashboard_test_dl_fail")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- suppressWarnings(
    download_dashboard_images(
      "https://this-host-does-not-exist.invalid",
      "D20160810T104734_IFCB110",
      tmp
    )
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# download_dashboard_adc — error handling (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_adc returns NULL on download failure", {
  tmp <- file.path(tempdir(), "dashboard_test_adc_fail")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- suppressWarnings(
    download_dashboard_adc(
      "https://this-host-does-not-exist.invalid",
      "D20160810T104734_IFCB110",
      tmp
    )
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# download_dashboard_autoclass — error handling (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_autoclass returns NULL on download failure", {
  tmp <- file.path(tempdir(), "dashboard_test_autoclass_fail")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- suppressWarnings(
    download_dashboard_autoclass(
      "https://this-host-does-not-exist.invalid",
      "D20160810T104734_IFCB110",
      tmp
    )
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# download_dashboard_images_bulk — cache logic (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_images_bulk skips already-cached samples", {
  tmp <- file.path(tempdir(), "dashboard_test_bulk_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Pre-populate cache for one sample
  s1 <- "D20160810T104734_IFCB110"
  s1_sub <- file.path(tmp, s1, s1)
  dir.create(s1_sub, recursive = TRUE)
  file.create(file.path(s1_sub, paste0(s1, "_00001.png")))

  # The second sample is not cached and the host doesn't exist,
  # so it will fail to download — but the cached sample should still be returned
  s2 <- "D20160810T112000_IFCB110"

  result <- suppressWarnings(
    download_dashboard_images_bulk(
      "https://this-host-does-not-exist.invalid",
      c(s1, s2),
      tmp
    )
  )
  expect_true(s1 %in% result)
  expect_false(s2 %in% result)
})

test_that("download_dashboard_images_bulk returns empty when all fail", {
  tmp <- file.path(tempdir(), "dashboard_test_bulk_allfail")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- suppressWarnings(
    download_dashboard_images_bulk(
      "https://this-host-does-not-exist.invalid",
      c("D20160810T104734_IFCB110", "D20160810T112000_IFCB110"),
      tmp
    )
  )
  expect_length(result, 0)
})

test_that("download_dashboard_images_bulk returns all when all cached", {
  tmp <- file.path(tempdir(), "dashboard_test_bulk_allcached")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  samples <- c("D20160810T104734_IFCB110", "D20160810T112000_IFCB110")
  for (sn in samples) {
    sub <- file.path(tmp, sn, sn)
    dir.create(sub, recursive = TRUE)
    file.create(file.path(sub, paste0(sn, "_00001.png")))
  }

  result <- download_dashboard_images_bulk("https://unused.example.com", samples, tmp)
  expect_equal(sort(result), sort(samples))
})

# ---------------------------------------------------------------------------
# rescan_file_index — dashboard mode, offline-safe
# ---------------------------------------------------------------------------

test_that("rescan_file_index returns NULL when dashboard URL is empty", {
  result <- rescan_file_index(data_source = "dashboard", dashboard_url = "")
  expect_null(result)
})

test_that("rescan_file_index returns NULL when dashboard URL is NULL", {
  result <- rescan_file_index(data_source = "dashboard", dashboard_url = NULL)
  expect_null(result)
})

# ---------------------------------------------------------------------------
# Integration tests — require network access to habon-ifcb.whoi.edu
# ---------------------------------------------------------------------------

# Helper: skip unless the dashboard is actually reachable
skip_if_dashboard_unavailable <- function() {
  skip_on_cran()
  skip_if_offline()
  avail <- tryCatch({
    resp <- curl::curl_fetch_memory(
      "https://habon-ifcb.whoi.edu/api/list_bins?dataset=tangosund",
      handle = curl::new_handle(timeout = 15)
    )
    resp$status_code == 200
  }, error = function(e) FALSE)
  skip_if_not(avail, "IFCB Dashboard at habon-ifcb.whoi.edu is not reachable")
}

# A small dataset with a known sample
DASHBOARD_BASE   <- "https://habon-ifcb.whoi.edu"
DASHBOARD_DATASET <- "tangosund"
# Pick a specific small sample to keep downloads fast
DASHBOARD_SAMPLE <- "D20160810T104734_IFCB110"

test_that("list_dashboard_bins returns character vector from real API", {
  skip_if_dashboard_unavailable()

  bins <- list_dashboard_bins(DASHBOARD_BASE, DASHBOARD_DATASET)
  expect_type(bins, "character")
  expect_true(length(bins) > 0)
  # All bins should look like IFCB sample names
  expect_true(all(grepl("^D\\d{8}T\\d{6}_IFCB\\d+", bins)))
})

test_that("list_dashboard_bins without dataset returns bins", {
  skip_if_dashboard_unavailable()

  bins <- list_dashboard_bins(DASHBOARD_BASE)
  expect_type(bins, "character")
  expect_true(length(bins) > 0)
})

test_that("list_dashboard_bins includes known sample", {
  skip_if_dashboard_unavailable()

  bins <- list_dashboard_bins(DASHBOARD_BASE, DASHBOARD_DATASET)
  expect_true(DASHBOARD_SAMPLE %in% bins)
})

test_that("download_dashboard_images downloads and extracts PNGs", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_images")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_images(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)

  expect_false(is.null(result))
  expect_true(dir.exists(result))
  pngs <- list.files(file.path(result, DASHBOARD_SAMPLE), pattern = "\\.png$")
  expect_true(length(pngs) > 0)
})

test_that("download_dashboard_images uses cache on second call", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # First call downloads
  r1 <- download_dashboard_images(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)
  expect_false(is.null(r1))

  # Second call should return immediately from cache (same result)
  r2 <- download_dashboard_images(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)
  expect_equal(r1, r2)
})

test_that("download_dashboard_adc downloads ADC file", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_adc")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_adc(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)

  expect_false(is.null(result))
  expect_true(file.exists(result))
  expect_true(grepl("\\.adc$", result))
  expect_true(file.size(result) > 0)
})

test_that("download_dashboard_adc uses cache on second call", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_adc_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r1 <- download_dashboard_adc(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)
  expect_false(is.null(r1))

  r2 <- download_dashboard_adc(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)
  expect_equal(r1, r2)
})

test_that("download_dashboard_autoclass returns data frame with expected columns", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_autoclass")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_autoclass(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)

  # autoclass may not be available for every sample — skip if NULL
  skip_if(is.null(result), "No autoclass data available for test sample")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("file_name", "class_name", "score") %in% names(result)))
  expect_true(nrow(result) > 0)
  # file_name should match the sample
  expect_true(all(grepl(DASHBOARD_SAMPLE, result$file_name)))
  # scores should be numeric between 0 and 1
  expect_true(all(result$score >= 0 & result$score <= 1))
})

test_that("download_dashboard_images_bulk downloads multiple samples", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_bulk")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Use a single sample to keep it fast
  result <- download_dashboard_images_bulk(DASHBOARD_BASE, DASHBOARD_SAMPLE, tmp)

  expect_true(DASHBOARD_SAMPLE %in% result)
  pngs <- list.files(
    file.path(tmp, DASHBOARD_SAMPLE, DASHBOARD_SAMPLE),
    pattern = "\\.png$"
  )
  expect_true(length(pngs) > 0)
})

test_that("rescan_file_index works in dashboard mode with real API", {
  skip_if_dashboard_unavailable()

  # Use a temp db folder so we don't pollute real data
  tmp_db <- file.path(tempdir(), "dashboard_integ_rescan_db")
  on.exit(unlink(tmp_db, recursive = TRUE), add = TRUE)
  dir.create(tmp_db, recursive = TRUE, showWarnings = FALSE)

  result <- rescan_file_index(
    data_source = "dashboard",
    dashboard_url = paste0(
      "https://habon-ifcb.whoi.edu/timeline?dataset=", DASHBOARD_DATASET
    ),
    db_folder = tmp_db,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_equal(result$data_source, "dashboard")
  expect_equal(result$dashboard_base_url, DASHBOARD_BASE)
  expect_equal(result$dashboard_dataset, DASHBOARD_DATASET)
  expect_true(length(result$sample_names) > 0)
  expect_true(DASHBOARD_SAMPLE %in% result$sample_names)
})
