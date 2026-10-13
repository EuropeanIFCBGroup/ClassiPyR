# Review page: choosing a class, loading a page, confirming and undoing it

setup_thr_review_server <- function(input, output, session, state,
                                    extractor = iRfcb::ifcb_extract_pngs) {
  index_path <- function() thr_index_db_path(state$review_dir)
  db_path <- function() thr_verdict_db_path(state$review_dir)

  model <- reactive({
    req(isTRUE(state$frozen))
    thr_app_try(thr_get_model(db_path(), state$model_id))
  })

  # A page belongs to one model in one review folder
  observeEvent(list(state$review_dir, state$model_id), {
    state$page <- NULL
    state$page_token <- NULL
    state$page_class <- NULL
    state$undo_stack <- list()
    state$review_message <- NULL
  }, ignoreInit = TRUE)

  # ---- Class picker and coverage ---------------------------------------------

  overview <- reactive({
    req(isTRUE(state$frozen))
    state$data_version
    thr_app_try(thr_class_overview(db_path(), state$model_id))
  })

  # The picker is keyed by the position of the class, never by its label
  observe({
    classes <- overview()
    choices <- if (is.null(classes)) character() else thr_class_choices(classes)
    current <- isolate(input$thr_class)
    updateSelectizeInput(
      session, "thr_class", choices = choices,
      selected = if (isTRUE(current %in% choices)) current else character()
    )
  })

  selected_class <- reactive({
    thr_class_from_choice(input$thr_class, req(model())$class_labels)
  })

  output$thr_coverage <- renderTable({
    class_name <- req(selected_class())
    state$data_version
    thr_app_try(thr_coverage_table(
      thr_stratum_counts(db_path(), state$model_id, class_name),
      edges = model()$score_edges
    ))
  })

  # ---- Loading a page --------------------------------------------------------

  # The raw file of every sample of the frame, looked up once
  roi_paths <- reactive({
    frame_model <- req(model())
    roi_folder <- state$roi_folder %||% frame_model$roi_folder
    thr_app_try(thr_frame_roi_paths(db_path(), state$model_id, roi_folder))
  })

  load_page <- function(class_name) {
    # Whatever happens next, the page shown so far can no longer be confirmed
    state$page <- NULL
    state$page_token <- NULL
    state$page_class <- NULL
    paths <- roi_paths()
    if (is.null(paths)) {
      return(invisible(FALSE))
    }
    page_size <- suppressWarnings(as.integer(input$thr_page_size))
    if (length(page_size) != 1 || is.na(page_size)) {
      page_size <- THR_APP_DEFAULT_PAGE_SIZE
    }
    loaded <- thr_app_try(
      withProgress(message = "Drawing images and reading raw data", value = 0.5, {
        thr_load_review_page(
          db_path(), index_path(), state$model_id, class_name, page_size,
          paths, state$image_folder, extractor = extractor
        )
      })
    )
    if (is.null(loaded)) {
      return(invisible(FALSE))
    }
    state$page <- loaded$page
    state$page_token <- loaded$token
    state$page_class <- loaded$class_name
    # New draws are pending draws: the counts shown have changed
    state$data_version <- state$data_version + 1
    invisible(TRUE)
  }

  observeEvent(input$thr_load_page, {
    if (!isTRUE(state$frozen)) {
      showNotification("Freeze a sampling frame on the Setup page first.",
                       type = "warning")
      return()
    }
    class_name <- selected_class()
    if (is.null(class_name)) {
      showNotification("Choose a class first.", type = "warning")
      return()
    }
    state$review_message <- NULL
    load_page(class_name)
  })

  # ---- Confirming a page -----------------------------------------------------

  observeEvent(input$thr_confirm, {
    result <- thr_confirm_page(db_path(), state$page, input$thr_confirm,
                               state$page_token, input$thr_annotator)
    state$review_message <- result$message
    showNotification(
      result$message,
      type = switch(result$status, saved = "message", stale = "warning",
                    "error"),
      duration = if (result$status == "saved") 6 else NULL
    )
    if (result$status != "saved") {
      return()
    }
    class_name <- state$page_class
    state$undo_stack <- c(
      state$undo_stack,
      list(list(token = state$page_token, class_name = class_name))
    )
    state$data_version <- state$data_version + 1
    load_page(class_name)
  })

  # ---- Undo ------------------------------------------------------------------

  observeEvent(input$thr_undo, {
    stack <- state$undo_stack
    if (length(stack) == 0) {
      showNotification("No page confirmed in this session is left to undo.",
                       type = "warning")
      return()
    }
    last <- stack[[length(stack)]]
    restored <- thr_app_try(thr_undo_page(db_path(), last$token))
    if (is.null(restored)) {
      return()
    }
    state$undo_stack <- stack[-length(stack)]
    state$review_message <- thr_undo_message(restored, last$class_name)
    showNotification(state$review_message, type = "message")
    state$data_version <- state$data_version + 1
    position <- match(last$class_name, model()$class_labels)
    updateSelectizeInput(session, "thr_class",
                         selected = as.character(position))
    load_page(last$class_name)
  })

  output$thr_review_message <- renderText({
    state$review_message %||% ""
  })

  invisible(NULL)
}
