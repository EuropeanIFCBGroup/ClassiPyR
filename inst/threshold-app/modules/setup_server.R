# Setup page: folders, the class-score index, the model, and the pre-flight
# and freeze of the sampling frame

# Folders offered by the Browse buttons
thr_browse_roots <- function() {
  c("Working directory" = getOption("ClassiPyR.startup_wd", getwd()),
    shinyFiles::getVolumes()())
}

# Connect a Browse button to the text field holding its folder
thr_wire_folder_browser <- function(input, session, button_id, text_id, roots) {
  shinyFiles::shinyDirChoose(input, button_id, roots = roots, session = session)
  observeEvent(input[[button_id]], {
    if (is.integer(input[[button_id]])) {
      return()
    }
    folder <- shinyFiles::parseDirPath(roots, input[[button_id]])
    if (length(folder) > 0) {
      updateTextInput(session, text_id, value = as.character(folder))
    }
  })
}

# One alert box per message
thr_alerts <- function(messages, class) {
  lapply(messages, function(message) {
    div(class = paste("alert", class), style = "margin-top: 8px;", message)
  })
}

# The two databases of the review folder in use
thr_state_index_path <- function(state) thr_index_db_path(state$review_dir)
thr_state_db_path <- function(state) thr_verdict_db_path(state$review_dir)

setup_thr_setup_server <- function(input, output, session, state,
                                   reader = read_class_scores_summary) {
  setup_thr_folders_server(input, output, session, state)
  models <- setup_thr_index_server(input, output, session, state, reader)
  setup_thr_model_server(input, output, session, state, models)
  setup_thr_preflight_server(input, output, session, state)
  setup_thr_freeze_server(input, output, session, state)
  invisible(NULL)
}

# ---- Folders -----------------------------------------------------------------

setup_thr_folders_server <- function(input, output, session, state) {
  roots <- thr_browse_roots()
  thr_wire_folder_browser(input, session, "thr_browse_review_dir",
                          "thr_review_dir", roots)
  thr_wire_folder_browser(input, session, "thr_browse_h5_folder",
                          "thr_h5_folder", roots)
  thr_wire_folder_browser(input, session, "thr_browse_roi_folder",
                          "thr_roi_folder", roots)

  apply_settings <- function(review_dir, h5_folder, roi_folder, annotator) {
    applied <- thr_app_try(
      thr_apply_settings(review_dir, h5_folder, roi_folder, annotator)
    )
    if (is.null(applied)) {
      return(invisible(FALSE))
    }
    state$review_dir <- applied$review_dir
    state$h5_folder <- applied$h5_folder
    state$roi_folder <- applied$roi_folder
    state$index_message <- NULL
    # isolate(): this also runs once at start, outside any observer
    state$index_version <- isolate(state$index_version) + 1
    invisible(TRUE)
  }

  # Restore what was saved last time. Only the threshold app's own settings
  # are read here; the review folder is never guessed.
  saved <- thr_load_settings()
  restore <- function(key) thr_clean_path_input(saved[[key]]) %||% ""
  updateTextInput(session, "thr_review_dir", value = restore("review_dir"))
  updateTextInput(session, "thr_h5_folder", value = restore("h5_folder"))
  updateTextInput(session, "thr_roi_folder", value = restore("roi_folder"))
  updateTextInput(session, "thr_annotator", value = restore("annotator"))
  if (nzchar(restore("review_dir")) && dir.exists(restore("review_dir"))) {
    # A data folder that has gone since last time must not block the start
    existing <- function(key) if (dir.exists(restore(key))) restore(key) else ""
    apply_settings(restore("review_dir"), existing("h5_folder"),
                   existing("roi_folder"), restore("annotator"))
  }

  observeEvent(input$thr_apply_settings, {
    if (apply_settings(input$thr_review_dir, input$thr_h5_folder,
                       input$thr_roi_folder, input$thr_annotator)) {
      showNotification("Folders applied and saved.", type = "message")
    }
  })

  output$thr_settings_status <- renderText({
    if (is.null(state$review_dir)) {
      return("No review folder is in use yet. Choose one and press Apply.")
    }
    paste0("Review folder in use: ", state$review_dir)
  })
}

# ---- Index -------------------------------------------------------------------

# Returns the reactive holding the indexed models
setup_thr_index_server <- function(input, output, session, state, reader) {
  observeEvent(input$thr_build_index, {
    if (is.null(state$review_dir) || is.null(state$h5_folder)) {
      showNotification(
        "Enter a review folder and a class-score folder and press Apply first.",
        type = "warning"
      )
      return()
    }
    result <- thr_app_try(
      withProgress(message = "Reading class-score files", value = 0, {
        build_threshold_index(
          state$h5_folder, thr_state_index_path(state), reader = reader,
          progress = function(done, total) {
            setProgress(done / max(total, 1),
                        detail = sprintf("%d of %d", done, total))
          }
        )
      })
    )
    if (!is.null(result)) {
      state$index_message <- thr_index_build_message(result)
      state$index_version <- state$index_version + 1
    }
  })

  models <- reactive({
    req(state$review_dir)
    state$index_version
    thr_app_try(thr_index_models(thr_state_index_path(state)))
  })

  failures <- reactive({
    req(state$review_dir)
    state$index_version
    thr_app_try(thr_index_failures(thr_state_index_path(state)))
  })

  output$thr_index_status <- renderText({
    req(state$review_dir)
    if (!is.null(state$index_message)) {
      return(state$index_message)
    }
    indexed <- models()
    if (is.null(indexed) || nrow(indexed) == 0) {
      return("No usable class-score files are indexed yet. Build the index.")
    }
    sprintf("The index holds %d usable class-score file(s) of %d model(s).",
            sum(indexed$n_files), nrow(indexed))
  })

  # One row per model: models are listed separately and never pooled
  output$thr_models_table <- renderTable({
    indexed <- req(models())
    req(nrow(indexed) > 0)
    state$frame_version
    thr_app_try(thr_models_table(indexed, thr_state_db_path(state)))
  })

  output$thr_failures_ui <- renderUI({
    overview <- thr_failure_overview(req(failures()))
    req(nrow(overview) > 0)
    tagList(
      div(class = "alert alert-warning", style = "margin-top: 8px;",
          sprintf("%d class-score file(s) cannot be used:",
                  sum(overview$Files))),
      tags$ul(lapply(seq_len(nrow(overview)), function(i) {
        tags$li(tags$strong(sprintf("%d %s: ", overview$Files[i],
                                    overview$Status[i])),
                overview$Meaning[i])
      }))
    )
  })

  # Paths and messages come from the files: always escaped
  output$thr_failures_table <- renderDT({
    datatable(req(failures()), rownames = FALSE, escape = TRUE,
              selection = "none", options = list(pageLength = 10))
  })

  models
}

# ---- Model -------------------------------------------------------------------

setup_thr_model_server <- function(input, output, session, state, models) {
  observe({
    indexed <- models()
    choices <- if (is.null(indexed)) character() else thr_model_choices(indexed)
    current <- isolate(input$thr_model)
    updateSelectInput(
      session, "thr_model", choices = choices,
      selected = if (isTRUE(current %in% choices)) current else NULL
    )
  })

  observe({
    indexed <- models()
    chosen <- input$thr_model
    known <- !is.null(indexed) && length(chosen) == 1 &&
      chosen %in% indexed$model_id
    state$model_id <- if (known) chosen else NULL
  })

  observe({
    state$frame_version
    state$frozen <- !is.null(state$review_dir) && !is.null(state$model_id) &&
      isTRUE(thr_app_try(thr_is_frozen(thr_state_db_path(state),
                                       state$model_id), default = FALSE))
  })

  output$thr_frame_state <- renderText({
    if (is.null(state$model_id)) {
      "none"
    } else if (isTRUE(state$frozen)) {
      "frozen"
    } else {
      "open"
    }
  })
  # The two cards of the page are shown and hidden by this value alone
  outputOptions(output, "thr_frame_state", suspendWhenHidden = FALSE)
}

# ---- Pre-flight --------------------------------------------------------------

setup_thr_preflight_server <- function(input, output, session, state) {
  roots <- thr_browse_roots()
  shinyFiles::shinyFileChoose(input, "thr_browse_fallback_json", roots = roots,
                              session = session, filetypes = "json")
  observeEvent(input$thr_browse_fallback_json, {
    if (is.integer(input$thr_browse_fallback_json)) {
      return()
    }
    chosen <- shinyFiles::parseFilePaths(roots, input$thr_browse_fallback_json)
    if (nrow(chosen) > 0) {
      updateTextInput(session, "thr_fallback_json",
                      value = as.character(chosen$datapath[1]))
    }
  })

  # The main app's settings are read, never written, and only to suggest
  # where its annotation database is
  updateTextInput(session, "thr_annotations_folder",
                  value = thr_app_try(thr_main_app_db_folder(), default = ""))

  observeEvent(input$thr_read_annotations, {
    state$annotations_folder <- thr_clean_path_input(input$thr_annotations_folder)
  })
  observeEvent(input$thr_clear_annotations, {
    state$annotations_folder <- NULL
  })

  output$thr_annotations_status <- renderText({
    if (is.null(state$annotations_folder)) {
      return("The annotation database is not used.")
    }
    paste0("Samples annotated in ", get_db_path(state$annotations_folder),
           " will be excluded. Press Check to see how many.")
  })

  # Anything that goes into a frame changing makes the last check out of date
  observeEvent(
    list(input$thr_exclusion_file, state$annotations_folder,
         input$thr_fallback_mode, input$thr_fallback_json, state$model_id,
         state$review_dir, state$h5_folder, state$roi_folder,
         state$index_version),
    {
      state$preflight <- NULL
    },
    ignoreInit = TRUE,
    # Before the Check button's observer, should both fire in one round
    priority = 10
  )

  observeEvent(input$thr_preflight, {
    req(state$review_dir, state$model_id)
    state$preflight <- thr_app_try(thr_setup_preflight(
      thr_state_index_path(state), state$model_id,
      h5_folder = state$h5_folder %||% NA_character_,
      roi_folder = state$roi_folder %||% NA_character_,
      exclusion_file = input$thr_exclusion_file$datapath,
      annotations_folder = state$annotations_folder,
      fallback_mode = input$thr_fallback_mode %||% "h5",
      fallback_json = input$thr_fallback_json
    ))
  })

  output$thr_preflight_ui <- renderUI({
    preflight <- state$preflight
    if (is.null(preflight)) {
      return(helpText("Press Check to see what the frame would hold."))
    }
    sources <- preflight$exclusions
    tagList(
      tags$ul(
        style = "margin-top: 10px;",
        tags$li(sprintf("%d usable sample(s) of the model in the class-score folder",
                        preflight$n_samples)),
        lapply(seq_len(nrow(sources)), function(i) {
          tags$li(sprintf("%s: lists %d sample(s), of which %d are in this frame",
                          sources$source[i], sources$n_listed[i],
                          sources$n_in_frame[i]))
        }),
        tags$li(sprintf("%d sample(s) excluded as training samples",
                        preflight$n_excluded)),
        tags$li(sprintf("%d sample(s) left out for lack of raw data",
                        preflight$n_no_raw)),
        tags$li(tags$strong(sprintf("%d sample(s) would be in the frame",
                                    preflight$n_frame))),
        if (preflight$can_freeze) {
          tags$li(sprintf("Fallback thresholds for %d class(es), from %s",
                          preflight$n_fallback,
                          thr_fallback_source_label(preflight$fallback_source)))
        }
      ),
      thr_alerts(preflight$notices, "alert-warning"),
      thr_alerts(preflight$problems, "alert-danger")
    )
  })
}

# ---- Freeze ------------------------------------------------------------------

setup_thr_freeze_server <- function(input, output, session, state) {
  observeEvent(input$thr_freeze, {
    if (is.null(state$preflight)) {
      showNotification("Press Check first.", type = "warning")
      return()
    }
    if (!state$preflight$can_freeze) {
      showNotification("The frame cannot be frozen yet. See the problems listed.",
                       type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Freeze the sampling frame?",
      p(sprintf("The review of this model will be drawn from %d sample(s).",
                state$preflight$n_frame)),
      p("The frame and its exclusions cannot be changed afterwards. Class-score",
        "files added later will not be part of this review. To start over,",
        "use another review folder."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("thr_freeze_confirm", "Freeze and start",
                     class = "btn-danger")
      )
    ))
  })

  observeEvent(input$thr_freeze_confirm, {
    removeModal()
    preflight <- state$preflight
    req(state$review_dir, state$model_id, isTRUE(preflight$can_freeze))
    frozen <- thr_app_try(do.call(thr_freeze_frame, c(
      list(db_path = thr_state_db_path(state),
           index_path = thr_state_index_path(state),
           model_id = state$model_id),
      preflight$freeze_args
    )))
    if (is.null(frozen)) {
      return()
    }
    state$preflight <- NULL
    state$frame_version <- state$frame_version + 1
    showNotification(
      sprintf("Frame frozen: %d sample(s), %d ROIs. Go to Review to start.",
              frozen$n_frame_files, frozen$n_rois),
      type = "message"
    )
  })

  output$thr_frozen_ui <- renderUI({
    req(isTRUE(state$frozen))
    state$index_version
    summary <- thr_app_try(thr_frozen_summary(
      thr_get_model(thr_state_db_path(state), state$model_id),
      thr_frame_status(thr_state_db_path(state), thr_state_index_path(state),
                       state$model_id)
    ))
    req(summary)
    tagList(
      tags$table(
        class = "thr-facts",
        lapply(seq_len(nrow(summary$facts)), function(i) {
          tags$tr(tags$td(summary$facts$Item[i]),
                  tags$td(summary$facts$Value[i]))
        })
      ),
      thr_alerts(summary$notices, "alert-warning")
    )
  })
}
