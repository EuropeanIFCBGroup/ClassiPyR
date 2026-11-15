# Title bar, mode indicators, mode switching, UI outputs

setup_ui_outputs_server <- function(input, output, session, rv, config,
                                    calculate_stats, roi_path_map,
                                    classifier_mat_files, classifier_h5_files,
                                    last_sync_time, csv_path_map,
                                    png_sample_path_map,
                                    save_to_cache = function() NULL,
                                    load_dashboard_autoclass = function(...) NULL) {
  output$cache_age_text <- renderUI({
    invalidateLater(60000)
    ts <- last_sync_time()
    if (!is.null(ts)) {
      cache_time <- as.POSIXct(ts)
      age_secs <- as.numeric(difftime(Sys.time(), cache_time, units = "secs"))
      age_text <- if (age_secs < 60) {
        "just now"
      } else if (age_secs < 3600) {
        paste0(round(age_secs / 60), " min ago")
      } else if (age_secs < 86400) {
        paste0(round(age_secs / 3600), " hours ago")
      } else {
        paste0(round(age_secs / 86400), " days ago")
      }
      div(
        style = "font-size: 11px; color: #999; margin-bottom: 5px;",
        icon("clock", style = "margin-right: 3px;"),
        paste0("Last synced ", age_text)
      )
    }
  })

  observe({
    session$sendCustomMessage("updatePixelsPerMicron", config$pixels_per_micron)
  })

  output$loading_overlay <- renderUI({
    if (rv$is_loading) {
      div(
        class = "loading-overlay",
        div(
          style = "text-align: center;",
          div(class = "spinner-border text-primary", role = "status",
              style = "width: 3rem; height: 3rem;"),
          div(style = "margin-top: 10px; font-weight: bold;", "Loading...")
        )
      )
    }
  })

  output$dynamic_title <- renderUI({
    mode_class <- if (rv$class_review_mode) {
      "navbar-mode-class-review"
    } else if (is.null(rv$current_sample)) {
      "navbar-mode-none"
    } else if (rv$is_annotation_mode) {
      "navbar-mode-annotation"
    } else {
      "navbar-mode-validation"
    }

    all_mode_classes <- "navbar-mode-none navbar-mode-annotation navbar-mode-validation navbar-mode-class-review"

    tagList(
      tags$script(HTML(sprintf("
        $(document).ready(function() {
          $('.navbar').removeClass('%s').addClass('%s');
        });
        $('.navbar').removeClass('%s').addClass('%s');
      ", all_mode_classes, mode_class, all_mode_classes, mode_class))),
      div(
        style = "display: flex; align-items: baseline; gap: 20px;",
        actionLink(
          "reset_to_home",
          label = span(paste("ClassiPyR", app_version), style = "color: white; font-size: 18px;"),
          style = "text-decoration: none;",
          title = "Click to unload sample and return to initial state"
        ),
        div(style = "display: inline;", uiOutput("mode_indicator_inline", inline = TRUE))
      )
    )
  })

  output$mode_indicator_inline <- renderUI({
    if (rv$class_review_mode) {
      n_images <- if (!is.null(rv$classifications)) nrow(rv$classifications) else 0
      n_samples <- length(rv$class_review_samples)
      n_changed <- 0L
      if (!is.null(rv$class_review_original) && !is.null(rv$classifications)) {
        n_changed <- sum(rv$classifications$class_name != rv$class_review_original$class_name)
      }
      change_text <- if (n_changed > 0) sprintf(", %d changed", n_changed) else ""

      span(
        style = "font-size: 14px; color: white;",
        tags$span(
          style = "font-weight: bold; margin-right: 8px;",
          if (identical(rv$class_review_source, "external")) "CLASS REVIEW (EXTERNAL)" else "CLASS REVIEW"
        ),
        tags$span(rv$class_review_class),
        tags$span(
          style = "margin-left: 10px; opacity: 0.9;",
          if (identical(rv$class_review_source, "external")) {
            sprintf("(%d images%s)", n_images, change_text)
          } else {
            sprintf("(%d images, %d samples%s)", n_images, n_samples, change_text)
          }
        )
      )
    } else if (is.null(rv$current_sample)) {
      span(
        style = "font-size: 14px; color: white; font-weight: 500;",
        "No sample loaded"
      )
    } else if (rv$is_annotation_mode) {
      total <- nrow(rv$classifications)
      classified <- sum(rv$classifications$class_name != "unclassified")
      # 0/0 is NaN, and sprintf("%d", NaN) errors and kills the title bar
      pct <- if (total > 0) round((classified / total) * 100) else 0

      switch_btn <- if (rv$has_classification) {
        actionLink(
          "switch_to_validation",
          label = tags$span(style = "font-size: 12px; color: white;", "\u2192 Validation"),
          style = "margin-left: 10px;"
        )
      }

      span(
        style = "font-size: 14px; color: white;",
        tags$span(style = "font-weight: bold; margin-right: 8px;", "ANNOTATION"),
        tags$span(rv$current_sample),
        tags$span(
          style = "margin-left: 10px; opacity: 0.9;",
          sprintf("(%d/%d - %d%%)", classified, total, pct)
        ),
        switch_btn
      )
    } else {
      stats <- calculate_stats()

      switch_btn <- if (rv$has_classification) {
        actionLink(
          "switch_to_annotation",
          label = tags$span(style = "font-size: 12px; color: white;", "\u2192 Manual"),
          style = "margin-left: 10px;"
        )
      }

      span(
        style = "font-size: 14px; color: white;",
        tags$span(style = "font-weight: bold; margin-right: 8px;", "VALIDATION"),
        tags$span(rv$current_sample),
        tags$span(
          style = "margin-left: 10px; opacity: 0.9;",
          sprintf("(%d changed - %.1f%% acc)", stats$incorrect_classifications, stats$accuracy * 100)
        ),
        switch_btn
      )
    }
  })

  # Shared helper: switch current sample to validation mode
  find_csv_file <- function(sample_name) {
    csv_map <- csv_path_map()
    path <- csv_map[[sample_name]]
    if (!is.null(path) && file.exists(path)) return(path)
    NULL
  }

  find_classifier_h5 <- function(sample_name) {
    h5_map <- classifier_h5_files()
    if (sample_name %in% names(h5_map)) return(h5_map[[sample_name]])
    NULL
  }

  find_sample_png_dir <- function(sample_name) {
    png_map <- png_sample_path_map()
    path <- png_map[[sample_name]]
    if (!is.null(path) && dir.exists(path)) return(path)
    # Fall back to dashboard cache folder (PNGs stored in temp_png_folder/sample_name/)
    if (!is.null(rv$temp_png_folder)) {
      dash_path <- file.path(rv$temp_png_folder, sample_name)
      if (dir.exists(dash_path)) return(dash_path)
    }
    NULL
  }

  # ROI dimensions of the current sample: from its ADC file, the dashboard
  # cache, or the extracted PNG files
  current_roi_dims <- function(sample_name) {
    roi_path <- roi_path_map()[[sample_name]]
    if (!is.null(roi_path) && !file.exists(roi_path)) roi_path <- NULL
    adc_path <- if (!is.null(roi_path)) sub("\\.roi$", ".adc", roi_path) else NULL
    if (is.null(adc_path) && identical(config$data_source, "dashboard")) {
      cached_adc <- file.path(get_dashboard_cache_dir(), substr(sample_name, 1, 9),
                              paste0(sample_name, ".adc"))
      if (file.exists(cached_adc)) adc_path <- cached_adc
    }
    if (!is.null(adc_path) && file.exists(adc_path)) {
      dims <- tryCatch(read_roi_dimensions(adc_path), error = function(e) NULL)
      if (!is.null(dims)) return(dims)
    }
    ClassiPyR:::infer_roi_dims_from_png(sample_name, find_sample_png_dir(sample_name))
  }

  # Show a freshly loaded set of classifications for the current sample.
  # Only ROIs that have an image are kept, exactly as when a sample is
  # loaded: the classifier and the database can list triggers without an
  # image, which would otherwise appear as "Not found" tiles and be saved.
  show_switched_classifications <- function(classifications, annotation_mode) {
    sample_name <- rv$current_sample
    classifications <- filter_to_extracted(
      classifications, file.path(rv$temp_png_folder %||% "", sample_name)
    )

    rv$original_classifications <- classifications
    rv$classifications <- classifications
    rv$is_annotation_mode <- annotation_mode
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    available_classes <- sort(unique(classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices(available_classes, unmatched),
                      selected = "all")
  }

  do_switch_to_validation <- function() {
    req(rv$current_sample, rv$has_classification)

    # Relabels made in manual mode are stored before the view is replaced;
    # switching used to discard them without a word
    save_to_cache()

    sample_name <- rv$current_sample
    roi_dims <- current_roi_dims(sample_name)

    csv_path <- find_csv_file(sample_name)
    classifier_h5_path <- find_classifier_h5(sample_name)
    classifier_mat_path <- classifier_mat_files()[[sample_name]]
    is_dashboard <- identical(config$data_source, "dashboard")
    source_label <- NULL

    classifications <- tryCatch({
      if (!is.null(csv_path)) {
        source_label <- "CSV"
        csv <- load_from_csv(csv_path, use_threshold = config$use_threshold)
        dims <- ClassiPyR:::match_roi_dims(
          ClassiPyR:::roi_number_from_file_name(csv$file_name), roi_dims
        )
        csv$width <- dims$width
        csv$height <- dims$height
        csv$roi_area <- dims$roi_area
        csv
      } else if (!is.null(classifier_h5_path)) {
        source_label <- "H5"
        load_from_h5(classifier_h5_path, sample_name, roi_dims,
                     use_threshold = config$use_threshold)
      } else if (!is.null(classifier_mat_path)) {
        source_label <- "MAT"
        load_from_classifier_mat(classifier_mat_path, sample_name, rv$class2use,
                                 roi_dims, use_threshold = config$use_threshold)
      } else if (!is.null(rv$cached_validation_classifications) &&
                 nrow(rv$cached_validation_classifications) > 0) {
        # Restore cached validation classifications (e.g., dashboard autoclass)
        source_label <- "cached"
        rv$cached_validation_classifications
      } else if (is_dashboard && isTRUE(config$dashboard_autoclass)) {
        # A sample opened in manual mode never downloaded its dashboard
        # auto-classifications; fetch them now
        source_label <- "Dashboard autoclass"
        load_dashboard_autoclass(sample_name, roi_dims)
      }
    }, error = function(e) {
      showNotification(paste("Could not load classifications:", e$message),
                       type = "error")
      NULL
    })

    if (is.null(classifications) || nrow(classifications) == 0) {
      showNotification("No classification data available", type = "warning")
      return()
    }

    show_switched_classifications(classifications, annotation_mode = FALSE)
    showNotification(paste0("Switched to Validation mode (", source_label, ")"),
                     type = "message")
  }

  observeEvent(input$switch_to_validation, {
    do_switch_to_validation()
  })

  observeEvent(input$switch_to_annotation, {
    req(rv$current_sample, rv$has_classification)

    # Save current validation classifications so we can switch back
    if (!isTRUE(rv$is_annotation_mode) && !is.null(rv$original_classifications)) {
      rv$cached_validation_classifications <- rv$original_classifications
    }

    # Corrections made in validation mode are stored before the view is
    # replaced; switching used to discard them without a word. The manual
    # annotations loaded below then include them.
    save_to_cache()

    sample_name <- rv$current_sample
    roi_dims <- current_roi_dims(sample_name)

    db_path <- get_db_path(config$db_folder)
    annotation_mat_path <- file.path(config$output_folder, paste0(sample_name, ".mat"))
    has_db <- ClassiPyR:::sample_has_annotations_db(db_path, sample_name)
    has_mat <- file.exists(annotation_mat_path)

    if (is.null(roi_dims)) {
      showNotification("No ROI dimensions available for manual annotations", type = "error")
      return()
    }

    classifications <- tryCatch({
      if (has_db) {
        load_from_db(db_path, sample_name, roi_dims)
      } else if (has_mat) {
        load_from_mat(annotation_mat_path, sample_name, rv$class2use, roi_dims)
      } else {
        # No existing annotations: blank classifications from ROI dimensions
        create_new_classifications(sample_name, roi_dims)
      }
    }, error = function(e) {
      showNotification(paste("Could not load annotations:", e$message), type = "error")
      NULL
    })
    if (is.null(classifications)) return()

    show_switched_classifications(classifications, annotation_mode = TRUE)
    showNotification(
      if (has_db || has_mat) "Switched to Manual annotation mode" else "Switched to Annotation mode (new)",
      type = "message"
    )
  })

  # Return do_switch_to_validation for use by statistics tab link
  list(do_switch_to_validation = do_switch_to_validation)
}
