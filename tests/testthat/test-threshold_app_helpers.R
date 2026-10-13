# Tests for the display helpers of the threshold review app: score
# formatting, picker choices, the per-bin coverage table and the gallery.

# ---- thr_format_score --------------------------------------------------------

test_that("thr_format_score tells the top score bins apart", {
  expect_equal(thr_format_score(0.5), "50.0%")
  expect_equal(thr_format_score(0.95), "95.0%")
  expect_equal(thr_format_score(0.995), "99.5%")
  expect_equal(thr_format_score(0.999), "99.9%")
  expect_equal(thr_format_score(0.9999), "99.99%")
  expect_equal(thr_format_score(0.99999), "99.999%")
  expect_equal(thr_format_score(0.9999999), "99.99999%")
  expect_equal(thr_format_score(1), "100%")
  expect_equal(thr_format_score(0), "0.0%")
})

test_that("thr_format_score cuts digits off instead of rounding up", {
  # Rounded, these would read as the next score bin up
  expect_equal(thr_format_score(0.99895), "99.89%")
  expect_equal(thr_format_score(0.0996), "9.9%")
  expect_equal(thr_format_score(0.99999999999), "99.99999999%")
  expect_false(thr_format_score(1 - 1e-15) == "100%")
  # A value stored just below its decimal form is not cut one digit short
  expect_equal(thr_format_score(0.9995), "99.95%")
  expect_equal(thr_format_score(0.7), "70.0%")
  expect_equal(thr_format_score(0.1 + 0.2), "30.0%")
})

test_that("thr_format_score never shows more than the score", {
  scores <- c(thr_test_spread_scores(500), THR_SCORE_EDGES,
              1 - 10^-(1:9), 0.5 + 10^-(2:9))
  shown <- as.numeric(sub("%", "", thr_format_score(scores))) / 100

  expect_true(all(shown <= scores + 1e-12))
  # And close enough to tell which score bin the image is in (the slack only
  # absorbs the division by 100 above)
  expect_equal(thr_score_bin(pmin(shown + 1e-12, 1)), thr_score_bin(scores))
})

test_that("thr_format_score prints no padding zeros", {
  shown <- thr_format_score(THR_SCORE_EDGES)

  expect_false(any(grepl("0{2,}%$", shown)))
  expect_equal(anyDuplicated(shown), 0)
  expect_equal(shown[13:15], c("99.5%", "99.9%", "99.99%"))
})

test_that("thr_format_score is vectorised and handles odd input", {
  expect_equal(thr_format_score(c(0.25, NA, 1)), c("25.0%", "", "100%"))
  expect_equal(thr_format_score(numeric()), character())
  expect_equal(thr_format_score(NULL), character())
  # A float that strays just outside the range is clamped, as in thr_score_bin()
  expect_equal(thr_format_score(c(1 + 1e-7, -1e-9)), c("100%", "0.0%"))
})

# ---- thr_bin_labels ----------------------------------------------------------

test_that("thr_bin_labels names each score bin by its bounds", {
  labels <- thr_bin_labels(c(0, 0.5, 0.999, 0.9999))

  expect_equal(labels, c("0.0% to 50.0%", "50.0% to 99.9%",
                         "99.9% to 99.99%", "99.99% to 100%"))
  expect_length(thr_bin_labels(THR_SCORE_EDGES), length(THR_SCORE_EDGES))
  expect_equal(thr_bin_labels(0), "0.0% to 100%")
})

# ---- thr_class_choices -------------------------------------------------------

test_that("thr_class_choices labels classes and keys them by index", {
  overview <- data.frame(
    class_name = thr_test_labels,
    n_predicted = c(240L, 12L, 0L, 7L, 100L),
    n_verdicts = c(100L, 0L, 0L, 7L, 3L),
    stringsAsFactors = FALSE
  )

  choices <- thr_class_choices(overview)

  expect_equal(unname(choices), as.character(1:5))
  expect_equal(names(choices)[1], "Skeletonema_spp (100/240)")
  # Labels are shown as they are and never become a value
  expect_equal(names(choices)[2], "Snowella/Woronichinia (0/12)")
  expect_equal(names(choices)[4], "Öresund_ciliate (7/7)")
  expect_false(any(grepl("[^0-9]", choices)))
})

test_that("thr_class_choices handles a model without classes to show", {
  empty <- data.frame(class_name = character(), n_predicted = integer(),
                      n_verdicts = integer(), stringsAsFactors = FALSE)

  expect_length(thr_class_choices(empty), 0)
  expect_error(thr_class_choices(data.frame(class_name = "a")), "n_predicted")
})

test_that("thr_class_from_choice maps a picker value back to its label", {
  expect_equal(thr_class_from_choice("2", thr_test_labels),
               "Snowella/Woronichinia")
  expect_equal(thr_class_from_choice(5, thr_test_labels), "Debris")
  for (bad in list(NULL, "", NA, "0", "6", "abc", "1.5", c("1", "2"),
                   "Skeletonema_spp", "-1")) {
    expect_null(thr_class_from_choice(bad, thr_test_labels))
  }
})

# ---- thr_model_choices -------------------------------------------------------

test_that("thr_model_choices labels each model and keys it by model id", {
  models <- data.frame(
    model_id = c(thr_test_model_a_id, thr_test_model_b_id),
    classifier_name = c("fixture_A", NA),
    n_files = c(6L, 1L), n_rois = c(65L, 20L),
    stringsAsFactors = FALSE
  )

  choices <- thr_model_choices(models)

  expect_equal(unname(choices), models$model_id)
  expect_match(names(choices)[1], "fixture_A", fixed = TRUE)
  expect_match(names(choices)[1], "6 files", fixed = TRUE)
  expect_match(names(choices)[1], "65 ROIs", fixed = TRUE)
  expect_match(names(choices)[1], substr(thr_test_model_a_id, 1, 8),
               fixed = TRUE)
  expect_match(names(choices)[2], "unnamed model", fixed = TRUE)
  expect_length(thr_model_choices(models[0, ]), 0)
})

# ---- thr_coverage_table ------------------------------------------------------

test_that("thr_coverage_table describes the bins that hold predictions", {
  counts <- data.frame(
    class_name = "Debris", bin = c(1L, 3L, 4L),
    n_population = c(200L, 40L, 6L), n_verdicts = c(9L, 10L, 6L),
    n_correct = c(4L, 9L, 6L), n_pending = c(3L, 0L, 0L),
    n_unavailable = c(1L, 0L, 0L), stringsAsFactors = FALSE
  )

  table <- thr_coverage_table(counts, edges = c(0, 0.5, 0.9, 0.99),
                              min_stratum_n = 10)

  expect_equal(nrow(table), 3)
  expect_equal(table[["Score bin"]],
               c("0.0% to 50.0%", "90.0% to 99.0%", "99.0% to 100%"))
  expect_equal(table$Predictions, c(200L, 40L, 6L))
  expect_equal(table$Verdicts, c(9L, 10L, 6L))
  expect_equal(table$Correct, c(4L, 9L, 6L))
  expect_equal(table$Pending, c(3L, 0L, 0L))
  expect_equal(table$Unavailable, c(1L, 0L, 0L))
  # 9 of 10 needed; 10 of 10; a bin of 6 reviewed in full
  expect_equal(table[["Enough verdicts"]], c("no (9 of 10)", "yes", "yes"))
})

test_that("thr_coverage_table handles a class without predictions", {
  counts <- data.frame(
    class_name = character(), bin = integer(), n_population = integer(),
    n_verdicts = integer(), n_correct = integer(), n_pending = integer(),
    n_unavailable = integer(), stringsAsFactors = FALSE
  )

  table <- thr_coverage_table(counts)

  expect_equal(nrow(table), 0)
  expect_true("Score bin" %in% names(table))
  expect_error(thr_coverage_table(data.frame(bin = 1L)), "n_population")
})

# ---- gallery -----------------------------------------------------------------

thr_test_app_gallery_page <- function() {
  page <- thr_test_app_page(roi_number = c(2L, 3L, 12L),
                            score = c(0.9999, 0.5, 0.99895))
  page$png_file <- thr_png_file_names(page$sample_name, page$roi_number)
  page$extracted <- c(TRUE, TRUE, FALSE)
  page$width <- c(160L, NA, 80L)
  page$height <- c(74L, NA, 40L)
  page
}

test_that("thr_gallery_card builds a card with a lazily loaded image", {
  page <- thr_test_app_gallery_page()

  html <- thr_test_app_html(thr_gallery_card(page[1, ], "thr_images_abc"))

  expect_match(html, 'class="thr-card"', fixed = TRUE)
  expect_match(html, 'data-draw="1"', fixed = TRUE)
  expect_match(html, 'loading="lazy"', fixed = TRUE)
  expect_match(
    html,
    paste0('src="thr_images_abc/', thr_test_app_raw_sample, "/",
           thr_test_app_raw_sample, '_00002.png"'),
    fixed = TRUE
  )
  expect_match(html, 'width="160"', fixed = TRUE)
  expect_match(html, 'height="74"', fixed = TRUE)
  expect_match(html, "99.99%", fixed = TRUE)
  expect_match(html, thr_test_app_raw_sample, fixed = TRUE)
  expect_match(html, "ROI 2", fixed = TRUE)
})

test_that("thr_gallery_card leaves out a size it does not know", {
  page <- thr_test_app_gallery_page()

  html <- thr_test_app_html(thr_gallery_card(page[2, ], "thr_images_abc"))

  expect_match(html, "<img", fixed = TRUE)
  expect_no_match(html, "width=", fixed = TRUE)
  expect_no_match(html, "height=", fixed = TRUE)
  # Pages without the size columns work too
  bare <- page[2, setdiff(names(page), c("width", "height"))]
  expect_match(thr_test_app_html(thr_gallery_card(bare, "p")), "<img",
               fixed = TRUE)
})

test_that("thr_gallery_card greys out a draw without an image", {
  page <- thr_test_app_gallery_page()

  html <- thr_test_app_html(thr_gallery_card(page[3, ], "thr_images_abc"))

  expect_match(html, 'class="thr-card unavailable"', fixed = TRUE)
  expect_match(html, 'data-draw="3"', fixed = TRUE)
  expect_no_match(html, "<img", fixed = TRUE)
  expect_match(html, "Image unavailable", fixed = TRUE)
  expect_match(html, "99.89%", fixed = TRUE)
  expect_match(html, "ROI 12", fixed = TRUE)
})

test_that("thr_gallery_card treats a draw without a file name as unavailable", {
  page <- thr_test_app_gallery_page()
  page$png_file[1] <- NA

  html <- thr_test_app_html(thr_gallery_card(page[1, ], "thr_images_abc"))

  expect_match(html, "unavailable", fixed = TRUE)
  expect_no_match(html, "<img", fixed = TRUE)
})

test_that("thr_gallery_card escapes everything that comes from data", {
  page <- thr_test_app_gallery_page()
  page$sample_name[1] <- "<script>alert('x')</script>\"&"
  page$png_file[1] <- "a\"><script>alert(1)</script>.png"

  html <- thr_test_app_html(thr_gallery_card(page[1, ], "thr_images_abc"))

  expect_no_match(html, "<script>", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("thr_gallery_ui wraps the cards in a container carrying the token", {
  page <- thr_test_app_gallery_page()

  html <- thr_test_app_html(thr_gallery_ui(page, "thr_images_abc", "tok123"))

  expect_match(html, 'id="thr-gallery"', fixed = TRUE)
  expect_match(html, 'data-token="tok123"', fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("thr-card", html))), 3)
  expect_equal(lengths(regmatches(html, gregexpr("<img", html))), 2)
  # Highest score first, whatever order the page came in
  shuffled <- thr_test_app_html(
    thr_gallery_ui(page[c(2, 3, 1), ], "thr_images_abc", "tok123")
  )
  at <- function(text, pattern) regexpr(pattern, text, fixed = TRUE)[[1]]
  expect_lt(at(shuffled, 'data-draw="1"'), at(shuffled, 'data-draw="3"'))
  expect_lt(at(shuffled, 'data-draw="3"'), at(shuffled, 'data-draw="2"'))
})

test_that("thr_gallery_ui explains an empty page", {
  page <- thr_test_app_gallery_page()[0, ]

  html <- thr_test_app_html(thr_gallery_ui(page, "thr_images_abc", "tok123"))

  expect_no_match(html, "thr-card", fixed = TRUE)
  expect_match(html, "Nothing left to review", fixed = TRUE)
  expect_match(html, 'data-token="tok123"', fixed = TRUE)

  none <- thr_test_app_html(thr_gallery_ui(NULL, "thr_images_abc", NULL))
  expect_match(none, "No page loaded", fixed = TRUE)
  expect_no_match(none, "data-token", fixed = TRUE)
})

test_that("thr_gallery_ui checks the page it is given", {
  expect_error(thr_gallery_ui(data.frame(draw_id = 1L), "p", "t"), "page")
})

test_that("thr_page_caption counts what a page holds", {
  page <- thr_test_app_gallery_page()
  page$is_new <- c(TRUE, FALSE, TRUE)

  caption <- thr_page_caption(page, "Snowella/Woronichinia")

  expect_match(caption, "3 images", fixed = TRUE)
  expect_match(caption, "Snowella/Woronichinia", fixed = TRUE)
  expect_match(caption, "1 without an image", fixed = TRUE)
  expect_match(caption, "1 shown before", fixed = TRUE)
  expect_equal(thr_page_caption(NULL, "x"), "")
  expect_match(thr_page_caption(page[0, ], "Debris"), "Debris", fixed = TRUE)
})

# ---- thr_app_try -------------------------------------------------------------

test_that("thr_app_try returns the value of a step that works", {
  shown <- character()
  notify <- function(message, ...) shown <<- c(shown, message)

  expect_equal(thr_app_try(1 + 1, notify = notify), 2)
  expect_equal(shown, character())
})

test_that("thr_app_try shows an error and returns the default", {
  shown <- list()
  notify <- function(message, ...) shown <<- list(message = message, ...)

  result <- thr_app_try(stop("the disk is full", call. = FALSE),
                        default = FALSE, notify = notify)

  expect_false(result)
  expect_equal(shown$message, "the disk is full")
  expect_equal(shown$type, "error")
  expect_null(thr_app_try(stop("x"), notify = function(...) NULL))
})
