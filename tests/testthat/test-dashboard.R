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

test_that("parse_dashboard_url rejects non-HTTP URLs", {
  expect_error(parse_dashboard_url("file:///etc/passwd"), "http:// or https://")
  expect_error(parse_dashboard_url("ftp://example.com"), "http:// or https://")
  expect_error(parse_dashboard_url("javascript:alert(1)"), "http:// or https://")
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
  # Trailing slashes should be stripped before building the export_metadata URL.
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
# resolve_sample_dataset — offline-safe
# ---------------------------------------------------------------------------

test_that("resolve_sample_dataset returns NULL on connection failure", {
  result <- resolve_sample_dataset(
    "https://this-host-does-not-exist.invalid",
    "D20160810T104734_IFCB110"
  )
  expect_null(result)
})

test_that("resolve_sample_dataset returns NULL for non-existent sample", {
  result <- resolve_sample_dataset(
    "https://this-host-does-not-exist.invalid",
    "D99991231T235959_IFCB999"
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
# download_dashboard_image_single — cache-hit path (offline-safe)
# ---------------------------------------------------------------------------

test_that("download_dashboard_image_single returns cached file when it exists", {
  tmp <- file.path(tempdir(), "dashboard_test_single_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  roi_number <- 42
  file_name <- sprintf("%s_%05d.png", sample_name, roi_number)
  dest_folder <- file.path(tmp, sample_name)
  dir.create(dest_folder, recursive = TRUE)
  dest_path <- file.path(dest_folder, file_name)
  writeBin(charToRaw("fake png"), dest_path)

  result <- download_dashboard_image_single(
    base_url = "https://unused.example.com",
    sample_name = sample_name,
    roi_number = roi_number,
    dest_dir = tmp
  )
  expect_equal(result, dest_path)
})

test_that("download_dashboard_image_single returns NULL on download failure", {
  tmp <- file.path(tempdir(), "dashboard_test_single_fail")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_image_single(
    base_url = "https://this-host-does-not-exist.invalid",
    sample_name = "D20160810T104734_IFCB110",
    roi_number = 1,
    dest_dir = tmp,
    max_retries = 1
  )
  expect_null(result)
})

test_that("download_dashboard_image_single creates dest directory", {
  tmp <- file.path(tempdir(), "dashboard_test_single_mkdir")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Will fail to download but should still create the directory
  result <- download_dashboard_image_single(
    base_url = "https://this-host-does-not-exist.invalid",
    sample_name = "D20160810T104734_IFCB110",
    roi_number = 1,
    dest_dir = tmp,
    max_retries = 1
  )
  expect_null(result)
  expect_true(dir.exists(file.path(tmp, "D20160810T104734_IFCB110")))
})

test_that("download_dashboard_image_single formats ROI number with 5 digits", {
  tmp <- file.path(tempdir(), "dashboard_test_single_format")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  # Pre-create the file with the expected 5-digit format
  dest_folder <- file.path(tmp, sample_name)
  dir.create(dest_folder, recursive = TRUE)
  expected_file <- file.path(dest_folder, paste0(sample_name, "_00007.png"))
  writeBin(charToRaw("fake png"), expected_file)

  result <- download_dashboard_image_single(
    base_url = "https://unused.example.com",
    sample_name = sample_name,
    roi_number = 7,
    dest_dir = tmp
  )
  expect_equal(result, expected_file)
})

# ---------------------------------------------------------------------------
# download_dashboard_images_individual — offline-safe
# ---------------------------------------------------------------------------

test_that("download_dashboard_images_individual returns cached files", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  dest_folder <- file.path(tmp, sample_name)
  dir.create(dest_folder, recursive = TRUE)

  # Pre-create two files
  f1 <- paste0(sample_name, "_00001.png")
  f2 <- paste0(sample_name, "_00042.png")
  writeBin(charToRaw("fake png 1"), file.path(dest_folder, f1))
  writeBin(charToRaw("fake png 2"), file.path(dest_folder, f2))

  result <- download_dashboard_images_individual(
    base_url = "https://unused.example.com",
    file_names = c(f1, f2),
    dest_dir = tmp
  )
  expect_equal(sort(result), sort(c(f1, f2)))
})

test_that("download_dashboard_images_individual returns empty on all failures", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_fail")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- suppressWarnings(
    download_dashboard_images_individual(
      base_url = "https://this-host-does-not-exist.invalid",
      file_names = c("D20160810T104734_IFCB110_00001.png",
                     "D20160810T104734_IFCB110_00002.png"),
      dest_dir = tmp,
      max_retries = 1
    )
  )
  expect_length(result, 0)
})

test_that("download_dashboard_images_individual handles mixed cached and uncached", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_mixed")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sample_name <- "D20160810T104734_IFCB110"
  dest_folder <- file.path(tmp, sample_name)
  dir.create(dest_folder, recursive = TRUE)

  # Pre-create only the first file
  f1 <- paste0(sample_name, "_00001.png")
  f2 <- paste0(sample_name, "_00099.png")
  writeBin(charToRaw("fake png"), file.path(dest_folder, f1))

  result <- download_dashboard_images_individual(
    base_url = "https://this-host-does-not-exist.invalid",
    file_names = c(f1, f2),
    dest_dir = tmp,
    max_retries = 1
  )
  expect_true(f1 %in% result)
  expect_false(f2 %in% result)
})

test_that("download_dashboard_images_individual skips malformed file names", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_malformed")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_images_individual(
    base_url = "https://unused.example.com",
    file_names = c("not_a_valid_filename.txt", "also-bad"),
    dest_dir = tmp,
    max_retries = 1
  )
  expect_length(result, 0)
})

test_that("download_dashboard_images_individual parses legacy IFCB file names", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_legacy")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  called_sample <- NULL
  called_roi <- NULL

  local_mocked_bindings(
    download_dashboard_image_single = function(base_url, sample_name, roi_number,
                                               dest_dir, max_retries = 3) {
      called_sample <<- sample_name
      called_roi <<- roi_number
      dir.create(file.path(dest_dir, sample_name), recursive = TRUE, showWarnings = FALSE)
      out <- file.path(dest_dir, sample_name,
                       "IFCB134_2023_072_004021_00002.png")
      writeBin(charToRaw("fake"), out)
      out
    },
    .package = "ClassiPyR"
  )

  result <- download_dashboard_images_individual(
    base_url = "https://unused.example.com",
    file_names = "IFCB134_2023_072_004021_00002.png",
    dest_dir = tmp,
    max_retries = 1
  )

  expect_equal(result, "IFCB134_2023_072_004021_00002.png")
  expect_equal(called_sample, "IFCB134_2023_072_004021")
  expect_equal(called_roi, 2L)
})

test_that("download_dashboard_images_individual skips unavailable samples after threshold", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_skip")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # 5 images from a non-existent host — after 2 failures the rest should be skipped
  file_names <- sprintf("D20160810T104734_IFCB999_%05d.png", 1:5)

  expect_warning(
    result <- download_dashboard_images_individual(
      base_url = "https://this-host-does-not-exist.invalid",
      file_names = file_names,
      dest_dir = tmp,
      max_retries = 1,
      sample_fail_threshold = 2
    ),
    "Skipping remaining images"
  )
  expect_length(result, 0)
})

test_that("download_dashboard_images_individual handles files from multiple samples", {
  tmp <- file.path(tempdir(), "dashboard_test_indiv_multi")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  s1 <- "D20160810T104734_IFCB110"
  s2 <- "D20160811T090000_IFCB110"
  dir.create(file.path(tmp, s1), recursive = TRUE)
  dir.create(file.path(tmp, s2), recursive = TRUE)

  f1 <- paste0(s1, "_00001.png")
  f2 <- paste0(s2, "_00005.png")
  writeBin(charToRaw("fake"), file.path(tmp, s1, f1))
  writeBin(charToRaw("fake"), file.path(tmp, s2, f2))

  result <- download_dashboard_images_individual(
    base_url = "https://unused.example.com",
    file_names = c(f1, f2),
    dest_dir = tmp
  )
  expect_equal(sort(result), sort(c(f1, f2)))
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
# download_dashboard_images_individual, parallel — offline-safe
# ---------------------------------------------------------------------------

# A stand-in for fetch_dashboard_image_batch(): `status_for(url)` decides the
# HTTP status of each request (NA: no response). Like a real request, it
# leaves a file behind for every response, also for an error page.
local_fake_image_fetcher <- function(status_for, env = parent.frame()) {
  log <- new.env()
  log$batches <- list()
  local_mocked_bindings(
    fetch_dashboard_image_batch = function(urls, dest_paths,
                                           parallel_downloads, multi_timeout) {
      log$batches <- c(log$batches, list(urls))
      log$parallel_downloads <- parallel_downloads
      log$multi_timeout <- multi_timeout
      status <- vapply(urls, status_for, integer(1), USE.NAMES = FALSE)
      for (path in dest_paths[!is.na(status)]) writeBin(charToRaw("body"), path)
      status
    },
    download_dashboard_image_single = function(...) {
      stop("the one-by-one download must not be used")
    },
    .package = "ClassiPyR",
    .env = env
  )
  log
}

test_that("parallel download fetches the missing images in one batch", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  sample_name <- "D20160810T104734_IFCB110"
  file_names <- sprintf("%s_%05d.png", sample_name, 1:4)
  dir.create(file.path(tmp, sample_name), recursive = TRUE)
  writeBin(charToRaw("cached"), file.path(tmp, sample_name, file_names[1]))

  log <- local_fake_image_fetcher(function(url) {
    if (grepl("_00003\\.png$", url)) 404L else 200L
  })

  result <- download_dashboard_images_individual(
    base_url = "https://dashboard.example.com/",
    file_names = file_names, dest_dir = tmp,
    parallel_downloads = 4, multi_timeout = 60
  )

  expect_equal(result, file_names[c(1, 2, 4)])
  # The file already on disk is not requested again, a 404 is not retried
  expect_length(log$batches, 1)
  expect_equal(log$batches[[1]],
               paste0("https://dashboard.example.com/data/", file_names[2:4]))
  expect_equal(log$parallel_downloads, 4)
  expect_equal(log$multi_timeout, 60)
  # The error page of the 404 is not left behind as an image
  expect_setequal(list.files(file.path(tmp, sample_name)), file_names[c(1, 2, 4)])
  expect_equal(readBin(file.path(tmp, sample_name, file_names[1]), "raw", 6),
               charToRaw("cached"))
})

test_that("parallel download retries failures that are not a 404", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  sample_name <- "D20160810T104734_IFCB110"
  file_names <- sprintf("%s_%05d.png", sample_name, 1:3)

  attempts <- new.env()
  log <- local_fake_image_fetcher(function(url) {
    n <- (if (is.null(attempts[[url]])) 0L else attempts[[url]]) + 1L
    assign(url, n, attempts)
    if (grepl("_00001", url)) return(200L)
    # 2: a server error that passes on the second attempt; 3: never answers
    if (grepl("_00002", url)) return(if (n < 2L) 500L else 200L)
    NA_integer_
  })

  result <- suppressWarnings(download_dashboard_images_individual(
    "https://dashboard.example.com", file_names, tmp,
    max_retries = 3, parallel_downloads = 4
  ))

  expect_equal(result, file_names[1:2])
  expect_equal(lengths(log$batches), c(3, 2, 1))
  expect_equal(list.files(file.path(tmp, sample_name)), file_names[1:2])
})

test_that("parallel download gives up on a sample that only fails", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  missing <- sprintf("D20160810T104734_IFCB999_%05d.png", 1:250)
  present <- sprintf("D20160811T090000_IFCB110_%05d.png", 1:3)

  log <- local_fake_image_fetcher(function(url) {
    if (grepl("IFCB999", url)) 404L else 200L
  })

  expect_warning(
    result <- download_dashboard_images_individual(
      "https://dashboard.example.com", c(missing, present), tmp,
      parallel_downloads = 4
    ),
    "Skipping remaining images from D20160810T104734_IFCB999"
  )

  expect_equal(result, present)
  # Only the first chunk of the missing sample was requested
  requested <- unlist(log$batches)
  expect_lt(sum(grepl("IFCB999", requested)), 250)
  expect_equal(sum(grepl("IFCB110", requested)), 3)
})

test_that("parallel download keeps going for a sample with some missing images", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file_names <- sprintf("D20160810T104734_IFCB110_%05d.png", 1:250)

  log <- local_fake_image_fetcher(function(url) {
    roi <- as.integer(sub(".*_(\\d+)\\.png$", "\\1", url))
    if (roi %% 2 == 0) 404L else 200L
  })

  expect_no_warning(
    result <- download_dashboard_images_individual(
      "https://dashboard.example.com", file_names, tmp, parallel_downloads = 4
    )
  )
  expect_equal(result, file_names[seq(1, 250, by = 2)])
  expect_equal(length(unlist(log$batches)), 250)
})

test_that("parallel download reports progress and skips unusable names", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  good <- sprintf("D20160810T104734_IFCB110_%05d.png", 1:150)
  file_names <- c("not_a_valid_filename.txt", good,
                  "IFCB134_2023_072_004021_00002.png")

  log <- local_fake_image_fetcher(function(url) 200L)
  calls <- list()

  result <- download_dashboard_images_individual(
    "https://dashboard.example.com", file_names, tmp, parallel_downloads = 4,
    progress = function(done, total) calls[[length(calls) + 1]] <<- c(done, total)
  )

  # Only names in the D<timestamp>_IFCB<n> form can be requested from /data/
  expect_equal(result, good)
  expect_equal(length(unlist(log$batches)), 150)
  expect_gte(length(calls), 2)
  done <- vapply(calls, `[`, numeric(1), 1)
  expect_false(is.unsorted(done))
  expect_equal(calls[[length(calls)]], c(152, 152))
})

test_that("a failing progress callback does not stop the download", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file_names <- sprintf("D20160810T104734_IFCB110_%05d.png", 1:2)
  log <- local_fake_image_fetcher(function(url) 200L)

  result <- download_dashboard_images_individual(
    "https://dashboard.example.com", file_names, tmp, parallel_downloads = 2,
    progress = function(done, total) stop("broken callback")
  )
  expect_equal(result, file_names)
})

test_that("one download at a time keeps the one-by-one path", {
  tmp <- tempfile("dashboard_parallel_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file_names <- sprintf("D20160810T104734_IFCB110_%05d.png", 1:2)
  requested <- integer()

  local_mocked_bindings(
    fetch_dashboard_image_batch = function(...) stop("must not be used"),
    download_dashboard_image_single = function(base_url, sample_name, roi_number,
                                               dest_dir, max_retries = 3) {
      requested <<- c(requested, roi_number)
      file.path(dest_dir, sample_name)
    },
    .package = "ClassiPyR"
  )
  calls <- list()
  result <- download_dashboard_images_individual(
    "https://dashboard.example.com", file_names, tmp,
    progress = function(done, total) calls[[length(calls) + 1]] <<- c(done, total)
  )

  expect_equal(result, file_names)
  expect_equal(requested, 1:2)
  expect_equal(calls, list(c(1, 2), c(2, 2)))
})

test_that("fetch_dashboard_image_batch reports unreachable hosts without leaving files", {
  tmp <- tempfile("dashboard_fetch_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dest <- file.path(tmp, c("a.png", "b.png"))

  status <- ClassiPyR:::fetch_dashboard_image_batch(
    urls = paste0("https://this-host-does-not-exist.invalid/data/", basename(dest)),
    dest_paths = dest, parallel_downloads = 2, multi_timeout = 30
  )

  expect_equal(status, c(NA_integer_, NA_integer_))
  expect_false(any(file.exists(dest) & file.size(dest) > 0, na.rm = TRUE))
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
      "https://habon-ifcb.whoi.edu/api/export_metadata/tangosund",
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

# Pick a sample with class_scores
DASHBOARD_CLASS_DATASET <- "mvco"
DASHBOARD_CLASS_SAMPLE <- "D20190402T200352_IFCB010"

test_that("list_dashboard_bins returns character vector from real API", {
  skip_if_dashboard_unavailable()

  bins <- list_dashboard_bins(DASHBOARD_BASE, DASHBOARD_DATASET)
  expect_type(bins, "character")
  expect_true(length(bins) > 0)
  # All bins should look like IFCB sample names
  expect_true(all(grepl("^D\\d{8}T\\d{6}_IFCB\\d+", bins)))
})

# Note: a no-dataset call (list_dashboard_bins(DASHBOARD_BASE)) is intentionally
# not tested against the live API. The api/export_metadata endpoint downloads
# metadata for *every* dataset when no slug is given, which times out (504) on
# production dashboards. The dataset-scoped path above covers the real usage.

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

test_that("resolve_sample_dataset returns correct dataset for known sample", {
  skip_if_dashboard_unavailable()

  result <- resolve_sample_dataset(DASHBOARD_BASE, DASHBOARD_SAMPLE)
  expect_type(result, "character")
  expect_equal(result, DASHBOARD_DATASET)
})

test_that("resolve_sample_dataset returns dataset for class_scores sample", {
  skip_if_dashboard_unavailable()

  result <- resolve_sample_dataset(DASHBOARD_BASE, DASHBOARD_CLASS_SAMPLE)
  expect_type(result, "character")
  expect_equal(result, DASHBOARD_CLASS_DATASET)
})

test_that("download_dashboard_autoclass auto-resolves dataset when not provided", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_autoclass_resolve")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Call without dataset_name — should auto-resolve via bin API
  result <- download_dashboard_autoclass(DASHBOARD_BASE, DASHBOARD_CLASS_SAMPLE, tmp)

  skip_if(is.null(result), "No autoclass data available for test sample")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("file_name", "class_name", "score") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("download_dashboard_autoclass returns data frame with expected columns", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_autoclass")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_autoclass(DASHBOARD_BASE, DASHBOARD_CLASS_SAMPLE, tmp, dataset_name = DASHBOARD_CLASS_DATASET)

  # autoclass may not be available for every sample — skip if NULL
  skip_if(is.null(result), "No autoclass data available for test sample")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("file_name", "class_name", "score") %in% names(result)))
  expect_true(nrow(result) > 0)
  # file_name should match the sample
  expect_true(all(grepl(DASHBOARD_CLASS_SAMPLE, result$file_name)))
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

test_that("download_dashboard_image_single downloads a real PNG", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_single")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- download_dashboard_image_single(
    base_url = DASHBOARD_BASE,
    sample_name = DASHBOARD_SAMPLE,
    roi_number = 1,
    dest_dir = tmp
  )

  expect_false(is.null(result))
  expect_true(file.exists(result))
  expect_true(file.info(result)$size > 0)
  expect_true(grepl("\\.png$", result))
})

test_that("download_dashboard_image_single uses cache on second call", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_single_cache")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r1 <- download_dashboard_image_single(DASHBOARD_BASE, DASHBOARD_SAMPLE, 1, tmp)
  expect_false(is.null(r1))

  r2 <- download_dashboard_image_single(DASHBOARD_BASE, DASHBOARD_SAMPLE, 1, tmp)
  expect_equal(r1, r2)
})

test_that("download_dashboard_images_individual downloads real PNGs", {
  skip_if_dashboard_unavailable()

  tmp <- file.path(tempdir(), "dashboard_integ_individual")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  file_names <- c(
    paste0(DASHBOARD_SAMPLE, "_00001.png"),
    paste0(DASHBOARD_SAMPLE, "_00002.png")
  )

  result <- download_dashboard_images_individual(
    base_url = DASHBOARD_BASE,
    file_names = file_names,
    dest_dir = tmp
  )

  expect_true(length(result) > 0)
  # At least the first ROI should exist
  expect_true(file_names[1] %in% result)
  # Verify the files actually exist on disk
  for (f in result) {
    sample_name <- sub("_\\d+\\.png$", "", f)
    expect_true(file.exists(file.path(tmp, sample_name, f)))
  }
})

test_that("download_dashboard_images_individual downloads in parallel from a real dashboard", {
  skip_if_dashboard_unavailable()

  tmp <- tempfile("dashboard_parallel_live_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  present <- sprintf("%s_%05d.png", DASHBOARD_SAMPLE, 1:12)
  absent <- sprintf("D20160810T104734_IFCB999_%05d.png", 1:3)
  calls <- list()

  expect_warning(
    result <- download_dashboard_images_individual(
      DASHBOARD_BASE, c(present, absent), tmp,
      parallel_downloads = 4, multi_timeout = 60,
      progress = function(done, total) calls[[length(calls) + 1]] <<- c(done, total)
    ),
    "Skipping remaining images from D20160810T104734_IFCB999"
  )

  # ROI numbers without an image (triggers with no ROI) are 404s on the
  # dashboard, so not every one of the first twelve has to exist
  expect_true(length(result) > 0)
  expect_true(all(result %in% present))
  saved <- file.path(tmp, DASHBOARD_SAMPLE, result)
  expect_true(all(file.exists(saved)))
  png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47))
  expect_true(all(vapply(saved, function(f) {
    identical(readBin(f, "raw", 4), png_magic)
  }, logical(1))))
  expect_setequal(list.files(file.path(tmp, DASHBOARD_SAMPLE)), result)
  expect_false(dir.exists(file.path(tmp, "D20160810T104734_IFCB999")) &&
                 length(list.files(file.path(tmp, "D20160810T104734_IFCB999"))) > 0)
  expect_equal(calls[[length(calls)]], c(15, 15))

  # The one-by-one path gets the same images
  tmp2 <- tempfile("dashboard_single_live_")
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)
  one_by_one <- suppressWarnings(download_dashboard_images_individual(
    DASHBOARD_BASE, present, tmp2, max_retries = 2
  ))
  expect_setequal(result, one_by_one)
})
