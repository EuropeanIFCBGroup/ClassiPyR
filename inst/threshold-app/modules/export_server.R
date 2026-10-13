# Export page: writing the thresholds files

setup_thr_export_server <- function(input, output, session, state, resolved) {
  db_path <- function() thr_verdict_db_path(state$review_dir)

  thr_wire_folder_browser(input, session, "thr_browse_export_folder",
                          "thr_export_folder", thr_browse_roots())

  # Offer the review folder and a name ifcb-classify will not pick up on its
  # own, once a frozen model is in use
  observeEvent(list(state$frozen, state$model_id, state$review_dir), {
    state$export_result <- NULL
    req(isTRUE(state$frozen))
    model <- thr_app_try(thr_get_model(db_path(), state$model_id))
    req(model)
    updateTextInput(session, "thr_export_folder", value = state$review_dir)
    updateTextInput(session, "thr_export_name",
                    value = thr_default_export_name(model$classifier_name))
  })

  output$thr_export_summary <- renderTable({
    thr_export_summary(resolved())
  })

  observeEvent(input$thr_export_btn, {
    if (!isTRUE(state$frozen)) {
      showNotification("Freeze a sampling frame on the Setup page first.",
                       type = "warning")
      return()
    }
    # Errors of the writers are shown as they are, never swallowed
    state$export_result <- tryCatch(
      thr_export_review(
        db_path(), state$model_id, target = input$thr_target,
        criterion = input$thr_criterion %||% "estimate",
        out_folder = input$thr_export_folder,
        file_name = input$thr_export_name,
        overwrite = isTRUE(input$thr_export_overwrite)
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (is.null(state$export_result$error)) {
      showNotification("Thresholds exported.", type = "message")
    } else {
      showNotification(state$export_result$error, type = "error",
                       duration = NULL)
    }
  })

  output$thr_export_result <- renderUI({
    result <- req(state$export_result)
    if (!is.null(result$error)) {
      return(div(class = "alert alert-danger", style = "margin-top: 10px;",
                 result$error))
    }
    div(
      class = "alert alert-success thr-path", style = "margin-top: 10px;",
      tags$p("Written:"),
      tags$ul(
        tags$li("Thresholds for ifcb-classify: ", tags$code(result$json)),
        tags$li("Class and threshold table: ", tags$code(result$csv)),
        tags$li("Review report: ", tags$code(result$report))
      ),
      tags$p("Pass the thresholds to ifcb-classify with:"),
      tags$pre(result$hint)
    )
  })

  invisible(NULL)
}
