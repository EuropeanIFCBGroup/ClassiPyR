# Tests for the threshold review Shiny app in inst/threshold-app

library(testthat)
library(shiny)

thr_test_app_files <- c(
  "app.R", "global.R", "ui.R", "server.R",
  file.path("modules", c("setup_server.R", "review_server.R",
                         "gallery_server.R", "results_server.R",
                         "export_server.R")),
  file.path("www", c("threshold-gallery.js", "threshold-app.css"))
)

# The UI object, built the way tests/testthat/test-app.R builds the main app's
thr_test_app_ui <- function() {
  app_env <- new.env(parent = globalenv())
  library(shinyjs)
  library(shinyFiles)
  library(bslib)
  library(DT)
  source(file.path(thr_test_app_dir(), "ui.R"), local = app_env)
  app_env
}

thr_test_app_read <- function(file) {
  paste(readLines(file.path(thr_test_app_dir(), file), warn = FALSE),
        collapse = "\n")
}

# Names of the form <prefix>$thr_xxx used in the module files
thr_test_app_ids <- function(prefix) {
  modules <- grep("^modules", thr_test_app_files, value = TRUE)
  code <- vapply(modules, thr_test_app_read, character(1))
  pattern <- paste0(prefix, "\\$(thr_[a-z0-9_]+)")
  found <- unlist(regmatches(code, gregexpr(pattern, code)))
  sort(unique(sub(paste0(prefix, "\\$"), "", found)))
}

thr_test_app_upload <- function(path) {
  data.frame(name = basename(path), size = file.size(path),
             type = "text/plain", datapath = path, stringsAsFactors = FALSE)
}

# ---- files -------------------------------------------------------------------

test_that("threshold app files exist and the R files parse", {
  app_dir <- thr_test_app_dir()
  skip_if(app_dir == "", "Package not installed")

  for (file in thr_test_app_files) {
    expect_true(file.exists(file.path(app_dir, file)), info = file)
  }
  for (file in grep("\\.R$", thr_test_app_files, value = TRUE)) {
    expect_no_error(parse(file.path(app_dir, file)))
  }
})

test_that("the app directory is separate from the main app", {
  skip_if(thr_test_app_dir() == "", "Package not installed")

  for (file in grep("\\.R$", thr_test_app_files, value = TRUE)) {
    code <- thr_test_app_read(file)
    # Nothing is sourced from, or written to, the main app
    expect_no_match(code, "system.file(\"app\"", fixed = TRUE, info = file)
    expect_no_match(code, "update_settings_file", fixed = TRUE, info = file)
    expect_no_match(code, "get_settings_path", fixed = TRUE, info = file)
  }
})

test_that("the gallery script is trimmed of the main app's server round trips", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  js <- thr_test_app_read(file.path("www", "threshold-gallery.js"))

  # Leftovers of the main app's gallery would throw or call the server
  expect_no_match(js, "measureMode", fixed = TRUE)
  expect_no_match(js, "toggle_image", fixed = TRUE)
  expect_no_match(js, "drag_select", fixed = TRUE)
  expect_no_match(js, "image-card", fixed = TRUE)
  # One input, sent on confirm only
  expect_equal(lengths(regmatches(js, gregexpr("setInputValue", js))), 1)
  expect_match(js, "Shiny.setInputValue('thr_confirm'", fixed = TRUE)
  expect_match(js, "priority: 'event'", fixed = TRUE)
  for (field in c("token:", "loaded:", "failed:", "wrong:", "nonce:")) {
    expect_match(js, field, fixed = TRUE)
  }
  expect_match(js, "addCustomMessageHandler('thrClearMarks'", fixed = TRUE)
  expect_match(js, ".thr-card", fixed = TRUE)

  css <- thr_test_app_read(file.path("www", "threshold-app.css"))
  for (rule in c(".thr-card {", ".thr-card.wrong {", ".thr-card.unavailable",
                 "#thr-selection-box {")) {
    expect_match(css, rule, fixed = TRUE)
  }
})

test_that("the gallery script only watches a card once its image has loaded", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  js <- thr_test_app_read(file.path("www", "threshold-gallery.js"))
  body_of <- function(name) {
    start <- regexpr(paste0("function ", name, "("), js, fixed = TRUE)[[1]]
    expect_gt(start, 0)
    rest <- substring(js, start)
    substr(rest, 1, regexpr("\n  }\n", rest)[[1]])
  }

  # This only reads the script; it does not run it. A card has its full size
  # before its image loads, so a card watched from the moment it is rendered
  # counts as seen while it is still blank.
  expect_no_match(js, "seenObserver.observe(this)", fixed = TRUE)
  expect_equal(lengths(regmatches(js, gregexpr("seenObserver.observe(", js,
                                               fixed = TRUE))), 1)
  # The one place a card starts being watched asks for a loaded image
  expect_match(body_of("watchCard"), "canMark(card)", fixed = TRUE)
  expect_match(body_of("setImageState"), "watchCard(card)", fixed = TRUE)
  # The sweep at confirm never marks a card as seen
  expect_no_match(body_of("sweepLoaded"), "seen", fixed = TRUE)
  expect_no_match(body_of("sweepLoaded"), "setImageState", fixed = TRUE)
  # Cards of a page that is replaced are no longer watched
  expect_match(js, "seenObserver.disconnect()", fixed = TRUE)
  # A page of nothing but cards without raw data cannot be confirmed
  expect_match(js, ".not('.no-raw')", fixed = TRUE)
})

# ---- UI ----------------------------------------------------------------------

test_that("threshold app UI can be created without errors", {
  skip_if(thr_test_app_dir() == "", "Package not installed")

  expect_no_error(app_env <- thr_test_app_ui())

  expect_true(exists("ui", envir = app_env))
  expect_true(inherits(app_env$ui, "shiny.tag") ||
                inherits(app_env$ui, "shiny.tag.list"))
  html <- paste(as.character(app_env$ui), collapse = "\n")
  expect_match(html, "threshold-gallery.js", fixed = TRUE)
  # The style sheet is linked from the page head, which is not part of the
  # body text, so look for it in the tag object itself
  expect_true("threshold-app.css" %in% unlist(app_env$ui))
  for (panel in c("Setup", "Review", "Results", "Export")) {
    expect_match(html, paste0(">", panel, "<"), fixed = TRUE)
  }
  expect_match(html, 'id="thr-selection-box"', fixed = TRUE)
  expect_match(html, 'id="thr_confirm_btn"', fixed = TRUE)
  expect_match(html, "sampling error within the reviewed score bins only",
               fixed = TRUE)
  expect_match(html, "NOT the class", fixed = TRUE)
  # The review folder is never pre-filled by the page itself
  expect_match(html, 'id="thr_review_dir"[^>]*value=""')
})

test_that("every input and output the server uses is on the page", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  html <- paste(as.character(thr_test_app_ui()$ui), collapse = "\n")
  has_id <- function(id) grepl(paste0('id="', id, '"'), html, fixed = TRUE)

  # Sent by the gallery script, a modal button and the results table
  not_on_page <- c("thr_confirm", "thr_freeze_confirm",
                   "thr_results_table_rows_selected")
  inputs <- setdiff(thr_test_app_ids("input"), not_on_page)
  expect_gt(length(inputs), 20)
  for (id in inputs) {
    expect_true(has_id(id), info = paste("input", id))
  }
  # Only read by the conditional panels
  outputs <- setdiff(thr_test_app_ids("output"), "thr_frame_state")
  expect_gt(length(outputs), 12)
  for (id in outputs) {
    expect_true(has_id(id), info = paste("output", id))
  }
  expect_match(html, "output.thr_frame_state == &#39;open&#39;", fixed = TRUE)
})

# ---- server ------------------------------------------------------------------

test_that("threshold app server function can be created without errors", {
  skip_if(thr_test_app_dir() == "", "Package not installed")

  expect_no_error(app_env <- thr_test_app_source_server())

  expect_true(is.function(app_env$server))
  expect_named(formals(app_env$server), c("input", "output", "session"))
  expect_true(is.function(app_env$thr_app_server))
})

test_that("the server and its modules see the package's internal functions", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  namespace <- asNamespace("ClassiPyR")
  below_namespace <- function(env) {
    while (!identical(env, emptyenv())) {
      if (identical(env, namespace)) {
        return(TRUE)
      }
      env <- parent.env(env)
    }
    FALSE
  }

  # In an installed package only exported functions are attached, so the app
  # code must reach the internal ones through the namespace
  expect_true(below_namespace(environment(app_env$server)))
  expect_true(below_namespace(environment(app_env$thr_app_server)))
  modules <- app_env$thr_app_env
  for (name in c("setup_thr_setup_server", "setup_thr_review_server",
                 "setup_thr_gallery_server", "setup_thr_results_server",
                 "setup_thr_export_server")) {
    expect_true(is.function(modules[[name]]), info = name)
    expect_true(below_namespace(environment(modules[[name]])), info = name)
  }
})

test_that("files outside that environment use no internal function", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  exported <- sub("^export\\((.*)\\)$", "\\1", grep(
    "^export\\(", readLines(system.file("NAMESPACE", package = "ClassiPyR")),
    value = TRUE
  ))
  internal <- setdiff(ls(asNamespace("ClassiPyR"), all.names = TRUE), exported)
  internal <- internal[grepl("^(thr_|THR_)", internal)]
  expect_gt(length(internal), 50)

  for (file in c("app.R", "global.R", "ui.R")) {
    tokens <- utils::getParseData(
      parse(file.path(thr_test_app_dir(), file), keep.source = TRUE)
    )
    used <- tokens$text[tokens$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")]
    expect_equal(intersect(used, internal), character(), info = file)
  }
})

test_that("the server starts with nothing applied", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()

  thr_test_app_with_config({
    testServer(app_env$server, {
      expect_null(state$review_dir)
      expect_null(state$model_id)
      expect_false(state$frozen)
      expect_equal(output$thr_frame_state, "none")
      expect_match(output$thr_settings_status, "No review folder")
      expect_match(output$thr_gallery$html, "No page loaded", fixed = TRUE)

      # Nothing happens, and nothing breaks, before the setup is done
      session$setInputs(thr_build_index = 1)
      session$setInputs(thr_load_page = 1)
      session$setInputs(thr_undo = 1)
      session$setInputs(thr_export_btn = 1)
      session$setInputs(thr_override_set = 1)
      session$setInputs(thr_freeze = 1)
      session$setInputs(thr_confirm = thr_test_app_browser("no-token", 1:3))
      expect_null(state$page)
      expect_match(state$review_message, "Nothing was recorded", fixed = TRUE)

      # An invalid folder is reported, not applied
      session$setInputs(thr_review_dir = "", thr_apply_settings = 1)
      expect_null(state$review_dir)
    })
  })
})

test_that("the server restores a review folder that was saved earlier", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  review <- tempfile("thr_review_")
  dir.create(review)
  on.exit(unlink(review, recursive = TRUE), add = TRUE)

  thr_test_app_with_config({
    thr_save_settings(list(review_dir = review, h5_folder = "/no/such/h5",
                           annotator = "Anna"))
    before <- readLines(thr_settings_path())
    testServer(app_env$server, {
      expect_equal(state$review_dir, normalizePath(review, winslash = "/"))
      # A data folder that has gone is not in use, without blocking the start
      expect_null(state$h5_folder)
      expect_match(output$thr_settings_status, basename(review), fixed = TRUE)
      # The reviewer is told, since the field still shows the folder
      expect_match(output$thr_settings_status, "/no/such/h5", fixed = TRUE)
      expect_match(output$thr_settings_status, "not in use", fixed = TRUE)
    })
    # Starting the app saved nothing: the folder is back once its disk is
    expect_identical(readLines(thr_settings_path()), before)
    expect_equal(thr_load_settings()$h5_folder, "/no/such/h5")
    unlink(thr_settings_path())
  })
})

test_that("Check and Freeze are refused while no class-score folder is in use", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  space <- thr_test_open_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  thr_test_app_with_config({
    thr_save_settings(list(review_dir = dirname(space$index_path),
                           h5_folder = file.path(space$root, "unmounted"),
                           roi_folder = space$roi_folder))
    server <- app_env$thr_app_server(reader = thr_test_fake_reader())
    testServer(server, {
      expect_null(state$h5_folder)
      session$setInputs(thr_model = space$model_id, thr_fallback_mode = "h5")
      expect_equal(state$model_id, space$model_id)

      # Without a folder the frame would cover every folder ever indexed
      session$setInputs(thr_preflight = 1)
      expect_false(isTRUE(state$preflight$can_freeze))
      expect_match(state$preflight$problems, "No class-score folder is in use",
                   fixed = TRUE)
      expect_match(output$thr_preflight_ui$html, "No class-score folder",
                   fixed = TRUE)
      session$setInputs(thr_freeze = 1)
      session$setInputs(thr_freeze_confirm = 1)
      expect_false(state$frozen)
    })
    expect_false(file.exists(space$db_path))
    unlink(thr_settings_path())
  })
})

test_that("the whole review runs through the server", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  app_env <- thr_test_app_source_server()
  root <- tempfile("thr_app_")
  h5 <- file.path(root, "h5")
  raw <- file.path(root, "raw")
  review <- file.path(root, "review")
  out <- file.path(root, "export")
  dir.create(h5, recursive = TRUE)
  dir.create(raw)
  dir.create(out)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  # Six samples of 200 ROIs; every class is predicted 40 times per sample.
  # Sample 1 is a training sample and sample 6 has no raw data, which leaves
  # four samples and 160 predictions per class.
  model_id <- thr_test_app_write_synthetic(h5)
  samples <- thr_test_app_samples()
  thr_test_app_raw_layout(raw, samples[1:5])
  exclude <- file.path(root, "training.txt")
  writeLines(c("# training bins", samples[1], "not a sample name"), exclude)
  fake <- thr_test_app_fake_extractor()
  main_settings <- NULL
  image_folder <- NULL
  resource_name <- NULL
  draws <- function() {
    thr_test_query(thr_verdict_db_path(review),
                   "SELECT * FROM draws ORDER BY draw_id")
  }

  thr_test_app_with_config({
    main_settings <- file.path(get_config_dir(), "settings.json")
    main_before <- if (file.exists(main_settings)) readLines(main_settings)
    server <- app_env$thr_app_server(reader = thr_test_fake_reader(),
                                     extractor = fake$extractor)
    testServer(server, {
      image_folder <<- state$image_folder
      resource_name <<- state$resource_name
      expect_true(dir.exists(image_folder))
      expect_true(resource_name %in% names(shiny::resourcePaths()))

      # ---- apply settings ----------------------------------------------------
      session$setInputs(thr_review_dir = review, thr_h5_folder = h5,
                        thr_roi_folder = raw, thr_annotator = "Tester")
      session$setInputs(thr_apply_settings = 1)
      expect_true(dir.exists(review))
      expect_equal(state$review_dir, normalizePath(review, winslash = "/"))
      expect_equal(thr_load_settings()$review_dir, state$review_dir)
      expect_equal(thr_load_settings()$annotator, "Tester")

      # ---- build the index ---------------------------------------------------
      session$setInputs(thr_build_index = 1)
      expect_true(file.exists(thr_index_db_path(review)))
      expect_match(state$index_message, "6 class-score file", fixed = TRUE)
      expect_match(output$thr_index_status, "6 usable", fixed = TRUE)
      expect_match(output$thr_models_table, "not started", fixed = TRUE)

      # ---- pick the model ----------------------------------------------------
      expect_equal(output$thr_frame_state, "none")
      session$setInputs(thr_model = model_id)
      expect_equal(state$model_id, model_id)
      expect_false(state$frozen)
      expect_equal(output$thr_frame_state, "open")

      # ---- pre-flight and freeze ---------------------------------------------
      session$setInputs(thr_freeze = 1)
      expect_false(state$frozen)
      session$setInputs(thr_exclusion_file = thr_test_app_upload(exclude),
                        thr_fallback_mode = "h5")
      session$setInputs(thr_preflight = 1)
      expect_true(state$preflight$can_freeze)
      expect_equal(state$preflight$n_samples, 6L)
      expect_equal(state$preflight$n_excluded, 1L)
      expect_equal(state$preflight$n_no_raw, 1L)
      expect_equal(state$preflight$n_frame, 4L)
      expect_equal(state$preflight$invalid_lines, "not a sample name")
      preflight_html <- output$thr_preflight_ui$html
      expect_match(preflight_html, "4 sample(s) would be in the frame",
                   fixed = TRUE)
      expect_match(preflight_html, "not sample names", fixed = TRUE)
      # Changing what goes into the frame makes the check out of date
      session$setInputs(thr_fallback_mode = "json")
      expect_null(state$preflight)
      session$setInputs(thr_fallback_mode = "h5")
      session$setInputs(thr_preflight = 2)
      expect_true(state$preflight$can_freeze)

      session$setInputs(thr_freeze = 2)
      expect_false(state$frozen)
      session$setInputs(thr_freeze_confirm = 1)
      expect_true(state$frozen)
      expect_equal(output$thr_frame_state, "frozen")
      expect_true(thr_is_frozen(thr_verdict_db_path(review), model_id))
      frozen_html <- output$thr_frozen_ui$html
      expect_match(frozen_html, "Samples in the frame", fixed = TRUE)
      expect_match(output$thr_models_table, "frozen", fixed = TRUE)

      # ---- load a review page ------------------------------------------------
      session$setInputs(thr_class = "1", thr_page_size = "500")
      expect_match(output$thr_coverage, "Score bin", fixed = TRUE)
      session$setInputs(thr_load_page = 1)
      page <- state$page
      first_token <- state$page_token
      expect_equal(nrow(page), 160)
      expect_equal(state$page_class, "Skeletonema_spp")
      expect_true(all(page$extracted))
      expect_setequal(unique(page$sample_name), samples[2:5])
      gallery <- output$thr_gallery$html
      expect_match(gallery, paste0('data-token="', first_token, '"'),
                   fixed = TRUE)
      expect_equal(lengths(regmatches(gallery, gregexpr("<img", gallery))),
                   160)
      expect_match(gallery, paste0('src="', resource_name, "/"), fixed = TRUE)
      expect_match(output$thr_page_caption, "160 images", fixed = TRUE)
      expect_equal(unique(draws()$status), "pending")

      # ---- confirm: five images not loaded, two failed -----------------------
      not_loaded <- page$draw_id[1:5]
      failed <- page$draw_id[6:7]
      seen <- setdiff(page$draw_id, c(not_loaded, failed))
      wrong <- intersect(seen, page$draw_id[page$score < 0.5])
      session$setInputs(thr_confirm = thr_test_app_browser(
        first_token, loaded = seen, wrong = wrong, failed = failed
      ))
      after <- draws()
      status <- stats::setNames(after$status, after$draw_id)
      expect_equal(unique(status[as.character(seen)]), "confirmed")
      # An image that failed to load once is not retired
      expect_equal(unique(status[as.character(failed)]), "pending")
      expect_equal(unique(status[as.character(not_loaded)]), "pending")
      verdict <- stats::setNames(after$verdict, after$draw_id)
      expect_equal(unname(verdict[as.character(seen)]),
                   as.integer(!(seen %in% wrong)))
      expect_equal(unique(after$annotator[after$status == "confirmed"]),
                   "Tester")
      # The reviewer is told what happened to every image
      expect_match(state$review_message, "Recorded 153 verdict", fixed = TRUE)
      expect_match(state$review_message, "2 image(s) failed to load",
                   fixed = TRUE)
      expect_match(state$review_message, "5 image(s) had not been viewed",
                   fixed = TRUE)
      expect_no_match(state$review_message, "left out", fixed = TRUE)
      # The next page is loaded: the five that had not loaded and the two
      # that failed come back
      second_token <- state$page_token
      expect_false(second_token == first_token)
      expect_setequal(state$page$draw_id, c(not_loaded, failed))
      expect_false(any(state$page$is_new))

      # ---- confirm again with the old token: nothing is written --------------
      before_stale <- draws()
      session$setInputs(thr_confirm = thr_test_app_browser(
        first_token, loaded = not_loaded, wrong = not_loaded
      ))
      expect_equal(draws(), before_stale)
      expect_match(state$review_message, "Nothing was recorded", fixed = TRUE)
      expect_equal(state$page_token, second_token)

      # ---- a wrong mark on an image that never loaded: nothing is written ----
      session$setInputs(thr_confirm = thr_test_app_browser(
        second_token, loaded = not_loaded[1:4], wrong = not_loaded[5]
      ))
      expect_equal(draws(), before_stale)
      expect_equal(state$page_token, second_token)

      # ---- confirm the second page; the class is then exhausted --------------
      session$setInputs(thr_confirm = thr_test_app_browser(
        second_token, loaded = c(not_loaded, failed)
      ))
      expect_equal(sum(draws()$status == "pending"), 0)
      expect_equal(sum(draws()$status == "confirmed"), 160)
      expect_equal(nrow(state$page), 0)
      empty_token <- state$page_token
      expect_false(empty_token %in% c(first_token, second_token))
      expect_match(output$thr_gallery$html, "Nothing left to review",
                   fixed = TRUE)
      expect_length(state$undo_stack, 2)

      # ---- undo: the rows are pending again ----------------------------------
      session$setInputs(thr_undo = 1)
      expect_equal(sum(draws()$status == "pending"), 7)
      expect_setequal(state$page$draw_id, c(not_loaded, failed))
      expect_match(state$review_message, "7 image(s) are pending again",
                   fixed = TRUE)
      session$setInputs(thr_undo = 2)
      restored <- draws()
      expect_equal(unique(restored$status), "pending")
      expect_true(all(is.na(restored$verdict)))
      expect_true(all(is.na(restored$page_token)))
      expect_equal(nrow(state$page), 160)
      expect_length(state$undo_stack, 0)
      session$setInputs(thr_undo = 3)
      expect_equal(unique(draws()$status), "pending")

      # ---- review the class in full: wrong below 0.5 -------------------------
      page <- state$page
      session$setInputs(thr_confirm = thr_test_app_browser(
        state$page_token, loaded = page$draw_id,
        wrong = page$draw_id[page$score < 0.5]
      ))
      expect_equal(sum(draws()$status == "confirmed"), 160)
      expect_match(output$thr_coverage, "yes", fixed = TRUE)

      # ---- results: the target decides the threshold -------------------------
      session$setInputs(thr_target = 0.9, thr_criterion = "estimate")
      strict <- thr_results$resolved()
      expect_equal(strict$class_name, thr_test_labels)
      expect_equal(strict$status[1], "fitted")
      expect_equal(strict$threshold[1], 0.5)
      expect_equal(unique(strict$status[-1]), "not_reviewed")
      session$setInputs(thr_target = 0.3)
      lenient <- thr_results$resolved()
      expect_equal(lenient$status[1], "fitted")
      expect_equal(lenient$threshold[1], 0)
      expect_false(identical(strict$threshold, lenient$threshold))
      session$setInputs(thr_target = 0.9)
      expect_match(output$thr_export_summary, "Fitted", fixed = TRUE)

      # ---- the selected class: reason and curve ------------------------------
      session$setInputs(thr_results_table_rows_selected = 1)
      expect_equal(state$results_class, 1)
      expect_match(output$thr_selected_class$html, "Skeletonema_spp",
                   fixed = TRUE)
      expect_no_error(output$thr_curve_plot)

      # ---- set and clear a manual threshold ----------------------------------
      session$setInputs(thr_results_table_rows_selected = 2)
      session$setInputs(thr_override_value = 0.42)
      session$setInputs(thr_override_set = 1)
      overrides <- thr_get_overrides(thr_verdict_db_path(review), model_id)
      expect_equal(overrides$class_name, "Snowella/Woronichinia")
      expect_equal(overrides$threshold, 0.42)
      manual <- thr_results$resolved()
      expect_equal(manual$source[2], "manual")
      expect_equal(manual$threshold[2], 0.42)
      # A value outside 0 to 1 is refused and the override stays
      session$setInputs(thr_override_value = 1.5)
      session$setInputs(thr_override_set = 2)
      expect_equal(
        thr_get_overrides(thr_verdict_db_path(review), model_id)$threshold,
        0.42
      )
      session$setInputs(thr_override_clear = 1)
      expect_equal(
        nrow(thr_get_overrides(thr_verdict_db_path(review), model_id)), 0
      )
      expect_equal(thr_results$resolved()$source[2], "fallback")

      # ---- export ------------------------------------------------------------
      session$setInputs(thr_export_folder = out,
                        thr_export_name = "synthetic_holdout_thresholds.json",
                        thr_export_overwrite = FALSE)
      session$setInputs(thr_export_btn = 1)
      result <- state$export_result
      expect_null(result$error)
      expect_true(all(file.exists(c(result$json, result$csv, result$report))))
      expect_setequal(
        list.files(out),
        c("synthetic_holdout_thresholds.json", "synthetic_holdout_thresholds.csv",
          "synthetic_holdout_thresholds_report.csv")
      )
      json <- jsonlite::fromJSON(result$json, simplifyVector = FALSE)
      expect_equal(names(json$class_metrics), thr_test_labels)
      expect_equal(json$class_metrics$Skeletonema_spp$threshold, 0.5)
      expect_equal(json$class_metrics$Skeletonema_spp$source, "fitted")
      expect_equal(json$target_precision, 0.9)
      expect_equal(json$validation_transform, "dataset_squarepad")
      expect_match(output$thr_export_result$html, "--thresholds", fixed = TRUE)

      # A second export does not overwrite, and says so
      session$setInputs(thr_export_btn = 2)
      expect_match(state$export_result$error, "already exist", fixed = TRUE)
      expect_match(output$thr_export_result$html, "already exist",
                   fixed = TRUE)
      session$setInputs(thr_export_overwrite = TRUE)
      session$setInputs(thr_export_btn = 3)
      expect_null(state$export_result$error)
    })
    # The main app's settings file was not created or changed
    main_after <- if (file.exists(main_settings)) readLines(main_settings)
    expect_identical(main_after, main_before)
  })

  # The session's images are gone with the session
  expect_false(dir.exists(image_folder))
  expect_false(resource_name %in% names(shiny::resourcePaths()))
})

test_that("the shipped server reads real class scores and raw data", {
  skip_if(thr_test_app_dir() == "", "Package not installed")
  skip_if_not_installed("hdf5r")
  app_env <- thr_test_app_source_server()
  root <- tempfile("thr_app_real_")
  review <- file.path(root, "review")
  h5 <- thr_test_copy_fixtures(file.path(root, "h5"))
  # A file scored before inference stopped augmenting
  dir.create(file.path(h5, "legacy"))
  file.copy(thr_test_legacy_h5(), file.path(h5, "legacy"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  image_folder <- NULL
  db_path <- thr_verdict_db_path(review)

  thr_test_app_with_config({
    testServer(app_env$server, {
      image_folder <<- state$image_folder
      session$setInputs(thr_review_dir = review, thr_h5_folder = h5,
                        thr_roi_folder = thr_test_app_raw_folder(),
                        thr_annotator = "")
      session$setInputs(thr_apply_settings = 1)
      session$setInputs(thr_build_index = 1)

      # Two models, listed separately; the legacy file is explained
      expect_match(state$index_message, "8 class-score file", fixed = TRUE)
      expect_match(state$index_message, "7 usable", fixed = TRUE)
      models <- thr_index_models(thr_index_db_path(review))
      expect_setequal(models$model_id,
                      c(thr_test_model_a_id, thr_test_model_b_id))
      failures_html <- output$thr_failures_ui$html
      expect_match(failures_html, "1 legacy", fixed = TRUE)
      expect_match(failures_html, "Re-run inference", fixed = TRUE)

      # Only one sample of model A has raw data
      session$setInputs(thr_model = thr_test_model_a_id)
      session$setInputs(thr_preflight = 1)
      expect_equal(state$preflight$n_samples, 6L)
      expect_equal(state$preflight$n_no_raw, 5L)
      expect_equal(state$preflight$n_frame, 1L)
      expect_true(state$preflight$can_freeze)
      expect_true(any(grepl("No training samples are excluded",
                            state$preflight$notices, fixed = TRUE)))
      session$setInputs(thr_freeze = 1)
      session$setInputs(thr_freeze_confirm = 1)
      expect_true(state$frozen)

      # The class predicted most often in that sample
      overview <- thr_class_overview(db_path, thr_test_model_a_id)
      expect_equal(sum(overview$n_predicted), 7)
      position <- which.max(overview$n_predicted)
      session$setInputs(thr_class = as.character(position),
                        thr_page_size = "100")
      session$setInputs(thr_load_page = 1)
      page <- state$page
      expect_equal(nrow(page), overview$n_predicted[position])
      expect_equal(unique(page$sample_name), thr_test_app_raw_sample)
      # The images were cut out of the real raw file
      expect_true(all(page$extracted))
      expect_true(all(file.exists(file.path(image_folder, page$png_file))))
      expect_true(all(page$width > 0 & page$height > 0))
      expect_match(output$thr_gallery$html, 'loading="lazy"', fixed = TRUE)

      session$setInputs(thr_confirm = thr_test_app_browser(
        state$page_token, loaded = page$draw_id, wrong = page$draw_id[1]
      ))
      draws <- thr_test_query(db_path, "SELECT * FROM draws ORDER BY draw_id")
      expect_equal(unique(draws$status), "confirmed")
      expect_equal(sum(draws$verdict == 0), 1)
      expect_equal(unique(draws$annotator), "Unknown")
      expect_equal(nrow(state$page), 0)
    })
  })

  expect_false(dir.exists(image_folder))
})
