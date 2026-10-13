# Results page: the chosen threshold of every class, the precision curve of
# the selected class, and manual thresholds

setup_thr_results_server <- function(input, output, session, state) {
  db_path <- function() thr_verdict_db_path(state$review_dir)

  # Shared with the Export page. The fixed seed makes the intervals, and with
  # them a choice made on the lower bound, the same here and in the export.
  resolved <- reactive({
    req(isTRUE(state$frozen))
    state$data_version
    target <- input$thr_target
    validate(need(
      is.numeric(target) && length(target) == 1 && !is.na(target) &&
        target > 0 && target <= 1,
      "Enter a target precision above 0 and at most 1."
    ))
    thr_resolve_thresholds_db(
      db_path(), state$model_id, target = target,
      criterion = input$thr_criterion %||% "estimate", seed = THR_APP_SEED
    )
  })

  # The table is redrawn whenever verdicts or settings change; the selected
  # class is kept across redraws
  observeEvent(list(state$review_dir, state$model_id), {
    state$results_class <- NULL
  }, ignoreInit = TRUE)

  output$thr_results_table <- renderDT({
    datatable(
      thr_results_table(resolved()),
      rownames = FALSE, escape = TRUE,
      selection = list(mode = "single",
                       selected = isolate(state$results_class)),
      options = list(pageLength = 25)
    )
  })

  observeEvent(input$thr_results_table_rows_selected, {
    state$results_class <- input$thr_results_table_rows_selected
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  selected <- reactive({
    rows <- state$results_class
    table <- resolved()
    req(length(rows) == 1, rows >= 1, rows <= nrow(table))
    table[rows, , drop = FALSE]
  })

  output$thr_selected_class <- renderUI({
    row <- selected()
    div(
      style = "margin-top: 15px;",
      tags$h5(row$class_name),
      tags$p(tags$strong(paste0(thr_status_label(row$status), ". ")),
             row$reason)
    )
  })

  output$thr_curve_plot <- renderPlot({
    row <- selected()
    curve <- thr_class_curve(db_path(), state$model_id, row$class_name)
    thr_plot_precision_curve(curve, target = input$thr_target,
                             chosen = row$threshold,
                             previous = row$previous_threshold,
                             main = row$class_name)
  })

  # ---- Manual thresholds -----------------------------------------------------

  set_override <- function(threshold) {
    rows <- state$results_class
    if (!isTRUE(state$frozen) || length(rows) != 1) {
      showNotification("Select a class in the table first.", type = "warning")
      return(invisible(FALSE))
    }
    done <- thr_app_try({
      # Rows of the table are the classes of the model, in model order
      labels <- thr_get_model(db_path(), state$model_id)$class_labels
      thr_set_override(db_path(), state$model_id, labels[rows], threshold)
      TRUE
    }, default = FALSE)
    if (done) {
      state$data_version <- state$data_version + 1
    }
    invisible(done)
  }

  observeEvent(input$thr_override_set, {
    value <- input$thr_override_value
    if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
      showNotification("Enter a threshold between 0 and 1.", type = "warning")
      return()
    }
    set_override(value)
  })

  observeEvent(input$thr_override_clear, {
    set_override(NULL)
  })

  list(resolved = resolved)
}
