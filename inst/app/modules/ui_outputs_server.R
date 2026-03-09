# Title bar, mode indicators, mode switching, UI outputs

setup_ui_outputs_server <- function(input, output, session, rv, config,
                                    calculate_stats, roi_path_map,
                                    classifier_mat_files, classifier_h5_files,
                                    last_sync_time, csv_path_map,
                                    png_sample_path_map) {
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

  output$python_warning <- renderUI({
    needs_python <- config$save_format %in% c("mat", "both")
    if (!python_available && needs_python) {
      div(
        class = "alert alert-warning",
        style = "margin-top: 10px; padding: 8px; font-size: 12px;",
        "Python not available. Saving .mat files will not work. ",
        "Switch to SQLite storage format in Settings, or install Python: ",
        "run ifcb_py_install() in R console. ",
        "MAT files are only needed for ",
        tags$a(href = "https://github.com/hsosik/ifcb-analysis", target = "_blank", "ifcb-analysis"),
        " compatibility."
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
      pct <- round((classified / total) * 100)

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

  infer_roi_dims_from_png_local <- function(sample_name, sample_png_dir) {
    if (is.null(sample_png_dir) || !dir.exists(sample_png_dir)) return(NULL)
    pattern <- paste0("^", sample_name, "_\\d+\\.png$")
    png_files <- list.files(sample_png_dir, pattern = pattern, full.names = FALSE)
    if (length(png_files) == 0) return(NULL)

    rows <- lapply(png_files, function(fn) {
      roi_number <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", fn))
      dims <- ClassiPyR:::read_png_dimensions(file.path(sample_png_dir, fn))
      data.frame(roi_number = roi_number, width = dims$width, height = dims$height, stringsAsFactors = FALSE)
    })
    dims_df <- do.call(rbind, rows)
    dims_df <- dims_df[!is.na(dims_df$roi_number), ]
    if (nrow(dims_df) == 0) return(NULL)
    dims_df <- dims_df[!duplicated(dims_df$roi_number), ]
    dims_df <- dims_df[order(dims_df$roi_number), ]
    dims_df$area <- dims_df$width * dims_df$height
    dims_df
  }

  do_switch_to_validation <- function() {
    req(rv$current_sample, rv$has_classification)

    sample_name <- rv$current_sample
    roi_path <- roi_path_map()[[sample_name]]
    if (!is.null(roi_path) && !file.exists(roi_path)) roi_path <- NULL
    sample_png_dir <- find_sample_png_dir(sample_name)
    adc_path <- if (!is.null(roi_path)) sub("\\.roi$", ".adc", roi_path) else NULL
    roi_dims <- if (!is.null(adc_path) && file.exists(adc_path)) {
      read_roi_dimensions(adc_path)
    } else {
      infer_roi_dims_from_png_local(sample_name, sample_png_dir)
    }

    csv_path <- find_csv_file(sample_name)
    classifier_h5_path <- find_classifier_h5(sample_name)
    classifier_mat_path <- classifier_mat_files()[[sample_name]]

    if (!is.null(csv_path)) {
      classifications <- load_from_csv(csv_path, use_threshold = config$use_threshold)
      showNotification("Switched to Validation mode (CSV)", type = "message")
    } else if (!is.null(classifier_h5_path)) {
      if (is.null(roi_dims)) {
        showNotification("No ROI dimensions available for H5 classifications", type = "error")
        return()
      }
      classifications <- load_from_h5(
        classifier_h5_path, sample_name, roi_dims,
        use_threshold = config$use_threshold
      )
      showNotification("Switched to Validation mode (H5)", type = "message")
    } else if (!is.null(classifier_mat_path)) {
      if (is.null(roi_dims)) {
        showNotification("No ROI dimensions available for MAT classifications", type = "error")
        return()
      }
      classifications <- load_from_classifier_mat(
        classifier_mat_path, sample_name, rv$class2use, roi_dims,
        use_threshold = config$use_threshold
      )
      showNotification("Switched to Validation mode (MAT)", type = "message")
    } else {
      showNotification("No classification data available", type = "warning")
      return()
    }

    rv$original_classifications <- classifications
    rv$classifications <- classifications
    rv$is_annotation_mode <- FALSE
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    available_classes <- sort(unique(classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)),
                      selected = "all")
  }

  observeEvent(input$switch_to_validation, {
    do_switch_to_validation()
  })

  observeEvent(input$switch_to_annotation, {
    req(rv$current_sample, rv$has_classification)

    sample_name <- rv$current_sample
    roi_path <- roi_path_map()[[sample_name]]
    if (!is.null(roi_path) && !file.exists(roi_path)) roi_path <- NULL
    sample_png_dir <- find_sample_png_dir(sample_name)
    adc_path <- if (!is.null(roi_path)) sub("\\.roi$", ".adc", roi_path) else NULL

    db_path <- get_db_path(config$db_folder)
    annotation_mat_path <- file.path(config$output_folder, paste0(sample_name, ".mat"))
    has_db <- sample_name %in% list_annotated_samples_db(db_path)
    has_mat <- file.exists(annotation_mat_path)

    roi_dims <- if (!is.null(adc_path) && file.exists(adc_path)) {
      read_roi_dimensions(adc_path)
    } else {
      infer_roi_dims_from_png_local(sample_name, sample_png_dir)
    }

    if (has_db || has_mat) {
      if (is.null(roi_dims)) {
        showNotification("No ROI dimensions available for manual annotations", type = "error")
        return()
      }
      if (has_db) {
        classifications <- load_from_db(db_path, sample_name, roi_dims)
      } else {
        classifications <- load_from_mat(annotation_mat_path, sample_name, rv$class2use, roi_dims)
      }
      showNotification("Switched to Manual annotation mode", type = "message")
    } else {
      # No existing annotations — create blank classifications from ROI dimensions
      if (is.null(roi_dims)) {
        showNotification("No ROI dimensions available to create annotations", type = "error")
        return()
      }
      classifications <- create_new_classifications(sample_name, roi_dims)
      showNotification("Switched to Annotation mode (new)", type = "message")
    }

    rv$original_classifications <- classifications
    rv$classifications <- classifications
    rv$is_annotation_mode <- TRUE
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    available_classes <- sort(unique(classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)),
                      selected = "all")
  })

  # Return do_switch_to_validation for use by statistics tab link
  list(do_switch_to_validation = do_switch_to_validation)
}
