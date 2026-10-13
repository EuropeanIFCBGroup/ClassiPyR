# Tests for the threshold review app when its data is away, has changed on
# disk or is gone: nothing a reviewer did may be lost or made unreachable

library(testthat)
library(shiny)

test_that("the server does not freeze from an index the disk has moved on from", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  space <- thr_test_open_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  files <- list.files(space$h5_folder, full.names = TRUE)

  thr_test_app_with_config({
    thr_save_settings(list(review_dir = dirname(space$index_path),
                           h5_folder = space$h5_folder,
                           roi_folder = space$roi_folder))
    server <- app_env$thr_app_server(reader = thr_test_fake_reader())
    testServer(server, {
      session$setInputs(thr_model = space$model_id, thr_fallback_mode = "h5")
      session$setInputs(thr_preflight = 1)
      expect_true(state$preflight$can_freeze)

      # A file is rewritten while the confirmation dialog is open
      session$setInputs(thr_freeze = 1)
      Sys.setFileTime(files[1], Sys.time() + 3600)
      session$setInputs(thr_freeze_confirm = 1)
      expect_false(state$frozen)
      expect_false(thr_is_frozen(space$db_path, space$model_id))

      # Check says why, and the freeze goes through once the index is updated
      session$setInputs(thr_preflight = 2)
      expect_false(state$preflight$can_freeze)
      expect_match(output$thr_preflight_ui$html, "since the index was built",
                   fixed = TRUE)
      session$setInputs(thr_build_index = 1)
      session$setInputs(thr_preflight = 3)
      expect_true(state$preflight$can_freeze)
      session$setInputs(thr_freeze = 2)
      session$setInputs(thr_freeze_confirm = 2)
      expect_true(state$frozen)
    })
    unlink(thr_settings_path())
  })
})

test_that("a frozen review is reached, shown and exported without its index", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()

  for (how in c("deleted", "archived")) {
    space <- thr_test_app_frozen_space()
    thr_test_app_review_class(space, thr_test_labels[1], wrong_below = 0.5)
    review <- dirname(space$db_path)
    out <- file.path(space$root, "export")
    dir.create(out)
    thr_test_lose_index(space, how)

    thr_test_app_with_config({
      thr_save_settings(list(review_dir = review, h5_folder = space$h5_folder,
                             roi_folder = space$roi_folder))
      server <- app_env$thr_app_server(
        reader = thr_test_fake_reader(),
        extractor = thr_test_app_fake_extractor()$extractor
      )
      testServer(server, {
        # ---- the model is offered and can be picked --------------------------
        expect_match(output$thr_index_status, "No usable class-score files",
                     fixed = TRUE)
        expect_match(output$thr_models_table, "frozen, not in the index",
                     fixed = TRUE)
        session$setInputs(thr_model = space$model_id)
        expect_equal(state$model_id, space$model_id, info = how)
        expect_true(state$frozen)
        expect_equal(output$thr_frame_state, "frozen")
        frozen_html <- output$thr_frozen_ui$html
        expect_match(frozen_html, "Samples in the frame", fixed = TRUE)
        expect_match(frozen_html, "Results and Export", fixed = TRUE)

        # ---- results ---------------------------------------------------------
        session$setInputs(thr_target = 0.9, thr_criterion = "estimate")
        resolved <- thr_results$resolved()
        expect_equal(resolved$class_name, thr_test_labels)
        expect_equal(resolved$status[1], "fitted")
        session$setInputs(thr_results_table_rows_selected = 1)
        expect_no_error(output$thr_curve_plot)

        # ---- export ----------------------------------------------------------
        session$setInputs(thr_export_folder = out,
                          thr_export_name = "frozen_holdout_thresholds.json",
                          thr_export_overwrite = FALSE)
        session$setInputs(thr_export_btn = 1)
        expect_null(state$export_result$error)
        expect_true(file.exists(state$export_result$json))

        # ---- a new page needs the index, and nothing breaks without it -------
        session$setInputs(thr_class = "2", thr_page_size = "50")
        session$setInputs(thr_load_page = 1)
        expect_true(is.null(state$page) || nrow(state$page) == 0)
      })
      unlink(thr_settings_path())
    })
    if (how == "deleted") {
      expect_false(file.exists(space$index_path))
    }
    unlink(space$root, recursive = TRUE)
  }
})

test_that("raw data that is away retires nothing and is found again", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  space <- thr_test_review_workspace()
  away <- paste0(space$raw, "_unmounted")
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  pending_page <- function(state, click, session) {
    session$setInputs(thr_load_page = click)
    state$page
  }

  thr_test_app_with_config({
    server <- app_env$thr_app_server(reader = thr_test_fake_reader(),
                                     extractor = fake$extractor)
    testServer(server, {
      thr_test_review_freeze(session, space)
      expect_true(state$frozen)
      session$setInputs(thr_class = "1", thr_page_size = "100")

      # ---- the disk with the raw data is not mounted -------------------------
      file.rename(space$raw, away)
      first_ids <- NULL
      for (click in 1:3) {
        session$setInputs(thr_load_page = click)
        page <- state$page
        expect_equal(nrow(page), 100)
        expect_false(any(page$extracted))
        expect_equal(unique(page$no_image_reason), "no_raw")
        first_ids <- first_ids %||% page$draw_id
        expect_setequal(page$draw_id, first_ids)
        # The reviewer is told which folder and how many images
        gallery <- output$thr_gallery$html
        expect_match(gallery, "100 of the 100 image(s) on this page cannot be shown",
                     fixed = TRUE)
        expect_match(gallery, basename(space$raw), fixed = TRUE)
        expect_no_match(gallery, "<img", fixed = TRUE)
        expect_match(gallery, "thr-card unavailable no-raw", fixed = TRUE)
        expect_match(output$thr_page_caption,
                     "100 whose raw data was not found", fixed = TRUE)

        # Confirming the empty gallery records nothing
        token <- state$page_token
        session$setInputs(thr_confirm = thr_test_app_browser(token, integer()))
        expect_match(state$review_message, "Nothing was recorded", fixed = TRUE)
        expect_match(state$review_message, "raw data", fixed = TRUE)
        expect_equal(state$page_token, token)
        expect_length(state$undo_stack, 0)
        draws <- thr_test_review_draws(space)
        expect_equal(unique(draws$status), "pending")
        expect_equal(nrow(draws), 100)
      }
      expect_equal(fake$n_calls(), 0)

      # ---- it is mounted again: no restart, no new settings ------------------
      file.rename(away, space$raw)
      session$setInputs(thr_load_page = 4)
      page <- state$page
      expect_setequal(page$draw_id, first_ids)
      expect_true(all(page$extracted))
      gallery <- output$thr_gallery$html
      expect_no_match(gallery, "cannot be shown", fixed = TRUE)
      expect_equal(lengths(regmatches(gallery, gregexpr("<img", gallery))), 100)

      # ---- away once more, now with every raw file already looked up ---------
      file.rename(space$raw, away)
      session$setInputs(thr_load_page = 5)
      expect_false(any(state$page$extracted))
      expect_match(output$thr_gallery$html, "cannot be shown", fixed = TRUE)
      file.rename(away, space$raw)
      session$setInputs(thr_load_page = 6)
      page <- state$page
      expect_true(all(page$extracted))
      expect_setequal(page$draw_id, first_ids)

      # ---- the page is reviewed as if nothing had happened -------------------
      session$setInputs(thr_confirm = thr_test_app_browser(
        state$page_token, loaded = page$draw_id
      ))
      draws <- thr_test_review_draws(space)
      expect_equal(sum(draws$status == "confirmed"), 100)
      expect_equal(sum(draws$status == "unavailable"), 0)
      expect_length(state$undo_stack, 1)
    })
  })
})

test_that("raw data missing for part of a page leaves the rest reviewable", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  space <- thr_test_review_workspace()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  lost <- thr_test_app_samples()[2]
  lost_roi <- file.path(space$raw, "2023", substr(lost, 1, 9),
                        paste0(lost, ".roi"))
  expect_true(file.exists(lost_roi))

  thr_test_app_with_config({
    server <- app_env$thr_app_server(reader = thr_test_fake_reader(),
                                     extractor = fake$extractor)
    testServer(server, {
      thr_test_review_freeze(session, space)
      session$setInputs(thr_class = "1", thr_page_size = "500")
      unlink(lost_roi)
      session$setInputs(thr_load_page = 1)
      page <- state$page
      expect_equal(nrow(page), 240)
      expect_equal(page$extracted, page$sample_name != lost)
      gallery <- output$thr_gallery$html
      expect_match(gallery, "40 of the 240 image(s) on this page cannot be shown",
                   fixed = TRUE)
      expect_match(gallery, lost, fixed = TRUE)

      # The 200 images that are there are recorded, the 40 others stay pending
      shown <- page$draw_id[page$extracted]
      session$setInputs(thr_confirm = thr_test_app_browser(
        state$page_token, loaded = shown
      ))
      expect_match(state$review_message, "Recorded 200 verdict", fixed = TRUE)
      expect_match(state$review_message,
                   "40 image(s) could not be shown because their raw data",
                   fixed = TRUE)
      draws <- thr_test_review_draws(space)
      expect_equal(sum(draws$status == "confirmed"), 200)
      expect_equal(sum(draws$status == "pending"), 40)
      expect_equal(sum(draws$status == "unavailable"), 0)
      # They are on the next page, still without an image and still said so
      expect_setequal(state$page$draw_id, setdiff(page$draw_id, shown))
      expect_match(output$thr_gallery$html, "40 of the 40 image(s)",
                   fixed = TRUE)

      # The file is restored: they get their images
      file.create(lost_roi)
      session$setInputs(thr_load_page = 2)
      expect_true(all(state$page$extracted))
      expect_equal(nrow(state$page), 40)
    })
  })
})

test_that("an empty page only says the class is finished when it is", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  space <- thr_test_review_workspace()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  confirm_all <- function(session, state) {
    session$setInputs(thr_confirm = thr_test_app_browser(
      state$page_token, loaded = state$page$draw_id
    ))
  }

  thr_test_app_with_config({
    server <- app_env$thr_app_server(reader = thr_test_fake_reader(),
                                     extractor = fake$extractor)
    testServer(server, {
      thr_test_review_freeze(session, space)

      # A class reviewed in full
      session$setInputs(thr_class = "1", thr_page_size = "500")
      session$setInputs(thr_load_page = 1)
      expect_equal(nrow(state$page), 240)
      confirm_all(session, state)
      expect_equal(nrow(state$page), 0)
      finished <- output$thr_gallery$html
      expect_match(finished, "Nothing left to review", fixed = TRUE)
      expect_match(finished, "all 240 predictions", fixed = TRUE)

      # The class-score file of one sample goes away after the freeze
      gone <- thr_test_app_samples()[4]
      unlink(file.path(space$h5, paste0(gone, "_class.h5")))
      session$setInputs(thr_build_index = 2)
      session$setInputs(thr_class = "2")
      session$setInputs(thr_load_page = 2)
      expect_equal(nrow(state$page), 200)
      expect_false(gone %in% state$page$sample_name)
      confirm_all(session, state)
      expect_equal(nrow(state$page), 0)
      unfinished <- output$thr_gallery$html
      expect_no_match(unfinished, "Nothing left to review", fixed = TRUE)
      expect_match(unfinished, "40 of its 240 predictions were never shown",
                   fixed = TRUE)
      expect_match(unfinished, "1 class-score file(s)", fixed = TRUE)

      # Loading another page of that class says the same
      session$setInputs(thr_load_page = 3)
      expect_match(output$thr_gallery$html, "40 of its 240 predictions",
                   fixed = TRUE)
      # And the class reviewed in full is still called finished
      session$setInputs(thr_class = "1")
      session$setInputs(thr_load_page = 4)
      expect_match(output$thr_gallery$html, "Nothing left to review",
                   fixed = TRUE)
    })
  })
})
