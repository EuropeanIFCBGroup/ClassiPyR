# Save button handler

setup_manual_save_server <- function(input, output, session, rv, config,
                                     roi_path_map, annotated_samples,
                                     disable_nav_buttons, enable_nav_buttons,
                                     update_current_sample_status_fn) {
  # --- Clear Annotations button (conditionally rendered) ---
  output$clear_btn_ui <- renderUI({
    is_enabled <- isTRUE(rv$is_annotation_mode) &&
      !is.null(rv$current_sample) &&
      !isTRUE(rv$class_review_mode)

    actionButton("clear_btn", "Clear Annotations",
                 icon = icon("trash"),
                 class = if (is_enabled) "btn-danger" else "btn-outline-secondary",
                 width = "100%",
                 disabled = if (!is_enabled) "disabled" else NULL)
  })

  observeEvent(input$clear_btn, {
    req(rv$current_sample)
    showModal(modalDialog(
      title = "Clear Annotations",
      tags$p(
        "This will permanently delete all annotations for ",
        tags$strong(rv$current_sample),
        " from the database."
      ),
      tags$p("This action cannot be undone."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_annotations", "Delete",
                     class = "btn-danger", icon = icon("trash"))
      )
    ))
  })

  observeEvent(input$confirm_clear_annotations, {
    removeModal()

    sample_name <- rv$current_sample
    db_path <- get_db_path(config$db_folder)

    # Delete from SQLite
    deleted <- delete_annotations_db(db_path, sample_name)
    if (!isTRUE(deleted)) {
      showNotification("Failed to delete annotations from database", type = "error")
      return()
    }

    # Delete .mat file if it exists
    mat_path <- file.path(config$output_folder, paste0(sample_name, ".mat"))
    if (file.exists(mat_path)) {
      file.remove(mat_path)
    }

    # Reconstruct roi_dimensions from current classifications
    roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", rv$classifications$file_name))
    roi_dimensions <- data.frame(
      roi_number = roi_numbers,
      width = rv$classifications$width,
      height = rv$classifications$height,
      area = rv$classifications$roi_area,
      stringsAsFactors = FALSE
    )

    # Reset classifications to blank unclassified state
    rv$classifications <- create_new_classifications(sample_name, roi_dimensions)
    rv$original_classifications <- rv$classifications
    rv$changes_log <- create_empty_changes_log()

    # Remove from annotated samples list
    current_annotated <- annotated_samples()
    annotated_samples(setdiff(current_annotated, sample_name))
    update_current_sample_status_fn(sample_name)

    # Clear session cache entry
    rv$session_cache[[sample_name]] <- NULL

    showNotification(
      paste("Annotations cleared for", sample_name),
      type = "message"
    )
  })

  observeEvent(input$save_btn, {
    req(rv$classifications)
    req(rv$class2use)
    req(rv$current_sample)
    # class2use_path is only required for MAT saves; SQLite uses rv$class2use directly
    if (config$save_format %in% c("mat", "both")) {
      req(rv$class2use_path)
    }

    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })

    annotator <- input$annotator_name
    if (is.null(annotator) || annotator == "") annotator <- "Unknown"

    # Check for unmatched classes and warn
    current_classes <- unique(rv$classifications$class_name)
    unmatched <- setdiff(current_classes, c(rv$class2use, "unclassified"))
    if (length(unmatched) > 0) {
      showNotification(
        paste0("Warning: Some classes are not in the class list and may not be saved correctly: ",
               paste(unmatched, collapse = ", ")),
        type = "warning",
        duration = 10
      )
    }

    tryCatch({
      is_dashboard <- identical(config$data_source, "dashboard")

      roi_path <- roi_path_map()[[rv$current_sample]]
      adc_folder <- if (!is.null(roi_path)) dirname(roi_path) else NULL

      # In dashboard mode, adc_folder may be NULL since there are no local ROI files
      if (is.null(adc_folder) && is_dashboard) {
        # Try to get ADC from dashboard cache for MAT saving
        cache_dir <- get_dashboard_cache_dir()
        parsed <- parse_dashboard_url(config$dashboard_url)
        adc_path <- download_dashboard_adc(parsed$base_url, rv$current_sample, cache_dir,
                                           parallel_downloads = config$dashboard_parallel_downloads,
                                           sleep_time = config$dashboard_sleep_time,
                                           multi_timeout = config$dashboard_multi_timeout,
                                           max_retries = config$dashboard_max_retries)
        adc_folder <- if (!is.null(adc_path)) dirname(adc_path) else NULL
      }

      save_fmt <- config$save_format

      # In dashboard mode, if MAT save is requested but no ADC available, fall back to SQLite
      if (is_dashboard && is.null(adc_folder) && save_fmt %in% c("mat", "both")) {
        if (save_fmt == "mat") {
          showNotification("MAT saving requires ADC data (not available from dashboard). Saving to SQLite instead.",
                           type = "warning", duration = 8)
          save_fmt <- "sqlite"
        } else {
          showNotification("ADC data not available from dashboard. Skipping MAT save, saving to SQLite only.",
                           type = "warning", duration = 8)
          save_fmt <- "sqlite"
        }
      }

      if (is.null(adc_folder) && !is_dashboard) {
        showNotification("Cannot find ROI data folder for this sample", type = "error")
        return()
      }

      progress_msg <- switch(save_fmt,
        sqlite = "Saving to database...",
        mat = "Saving MAT file...",
        both = "Saving annotations...",
        "Saving..."
      )

      withProgress(message = progress_msg, {
        result <- save_sample_annotations(
          sample_name = rv$current_sample,
          classifications = rv$classifications,
          original_classifications = rv$original_classifications,
          changes_log = rv$changes_log,
          temp_png_folder = rv$temp_png_folder,
          output_folder = config$output_folder,
          png_output_folder = config$png_output_folder,
          roi_folder = config$roi_folder,
          class2use_path = rv$class2use_path,
          class2use = rv$class2use,
          annotator = annotator,
          adc_folder = adc_folder,
          save_format = save_fmt,
          db_folder = config$db_folder,
          export_statistics = config$export_statistics
        )
      })

      if (!isTRUE(result)) {
        showNotification("Save returned no changes", type = "warning")
        return()
      }

      # Update annotated samples list to reflect new manual annotation
      current_annotated <- annotated_samples()
      if (!rv$current_sample %in% current_annotated) {
        annotated_samples(c(current_annotated, rv$current_sample))
        update_current_sample_status_fn(rv$current_sample)
      }

      save_msg <- switch(save_fmt,
        sqlite = paste("Saved to database in", config$db_folder),
        mat = paste("Saved to", config$output_folder),
        both = paste("Saved to database and", config$output_folder),
        paste("Saved to", config$output_folder)
      )
      showNotification(save_msg, type = "message")

    }, error = function(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
    })
  })
}
