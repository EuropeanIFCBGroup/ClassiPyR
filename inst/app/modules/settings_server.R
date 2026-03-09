# Settings modal, file browsers, apply_settings

setup_settings_server <- function(input, output, session, rv, config,
                                  get_browse_volumes, make_dynamic_roots,
                                  setup_path_validation, persist_settings,
                                  load_worms_map, rescan_trigger) {
  # Settings modal
  observeEvent(input$settings_btn, {
    showModal(modalDialog(
      title = "Settings",
      size = "l",
      easyClose = TRUE,

      # -- Data Source
      h5("Data Source"),

      radioButtons("cfg_data_source", NULL,
                   choices = c("Local Folders" = "local",
                               "IFCB Dashboard" = "dashboard"),
                   selected = config$data_source, inline = TRUE),

      conditionalPanel(
        condition = "input.cfg_data_source == 'dashboard'",
        textInput("cfg_dashboard_url", "Dashboard URL",
                  value = config$dashboard_url, width = "100%",
                  placeholder = "https://habon-ifcb.whoi.edu/timeline?dataset=tangosund"),
        tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                   "Enter the IFCB Dashboard URL. Dataset can be specified via ?dataset=name."),
        checkboxInput("cfg_dashboard_autoclass", "Use dashboard auto-classifications",
                      value = config$dashboard_autoclass),
        tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                   "When enabled, downloads auto-classification scores from the dashboard for validation mode."),
        tags$details(
          tags$summary(style = "cursor: pointer; margin-bottom: 10px; color: #666;",
                       "Advanced Download Settings"),
          fluidRow(
            column(6, numericInput("cfg_dashboard_parallel_downloads",
                                   "Parallel Downloads",
                                   value = config$dashboard_parallel_downloads,
                                   min = 1, max = 20, step = 1)),
            column(6, numericInput("cfg_dashboard_sleep_time",
                                   "Sleep Time (seconds)",
                                   value = config$dashboard_sleep_time,
                                   min = 0, max = 30, step = 0.5))
          ),
          fluidRow(
            column(6, numericInput("cfg_dashboard_multi_timeout",
                                   "Download Timeout (seconds)",
                                   value = config$dashboard_multi_timeout,
                                   min = 10, max = 600, step = 10)),
            column(6, numericInput("cfg_dashboard_max_retries",
                                   "Max Retries",
                                   value = config$dashboard_max_retries,
                                   min = 1, max = 10, step = 1))
          ),
          tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                     "Settings for zip/ADC/autoclass downloads from the dashboard.")
        )
      ),

      # -- Input Folders
      h5("Input Folders"),

      # ROI/PNG Data — primary input, listed first
      conditionalPanel(
        condition = "input.cfg_data_source == 'local'",
        div(
          style = "display: flex; gap: 5px; align-items: flex-end;",
          div(style = "flex: 1;",
              textInput("cfg_roi_folder", "ROI/PNG Data Folder",
                        value = config$roi_folder, width = "100%")),
          shinyDirButton("browse_roi_folder", "Browse", "Select ROI/PNG Data Folder",
                         class = "btn-outline-secondary", style = "margin-bottom: 15px;")
        ),
        tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 20px;",
                   "Folder containing raw IFCB files (.roi/.adc/.hdr) or extracted PNG images.")
      ),

      # Classification folder
      div(
        style = "display: flex; gap: 5px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("cfg_csv_folder", "Classification Folder (CSV/H5/MAT)",
                      value = config$csv_folder, width = "100%")),
        shinyDirButton("browse_csv_folder", "Browse", "Select Classification Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),

      conditionalPanel(
        condition = "input.cfg_data_source == 'local'",
        tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 20px;",
                   "Folder with pre-computed classifications used to pre-populate class labels for validation.")
      ),
      conditionalPanel(
        condition = "input.cfg_data_source == 'dashboard'",
        tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 20px;",
                   "Optional. Use local classification files instead of dashboard auto-classifications.")
      ),

      checkboxInput("cfg_use_threshold", "Apply classification threshold",
                    value = config$use_threshold),
      tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                 "When enabled, classifications below the confidence threshold are marked as 'unclassified'."),

      checkboxInput("cfg_auto_sync", "Sync folders automatically on startup",
                    value = config$auto_sync),
      tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                 "When disabled, the app loads from cache on startup. Use the sync button to update manually."),

      hr(),

      # -- Output
      h5("Output"),

      # Database folder — primary output
      div(
        style = "display: flex; gap: 5px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("cfg_db_folder", "Database Folder (SQLite)",
                      value = config$db_folder, width = "100%")),
        shinyDirButton("browse_db_folder", "Browse", "Select Database Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),
      tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 20px;",
                 "Where annotation databases are stored. Must be a local drive \u2014 SQLite databases are",
                 tags$a(href = "https://www.sqlite.org/useovernet.html", target = "_blank",
                        "not safe on network filesystems"),
                 "due to unreliable file locking."),

      # Annotation storage format
      selectInput("cfg_save_format", "Annotation Storage Format",
                  choices = c(
                    "SQLite (recommended)" = "sqlite",
                    "MAT file (MATLAB compatible)" = "mat",
                    "Both SQLite and MAT" = "both"
                  ),
                  selected = config$save_format),
      tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                 "SQLite works out of the box. MAT files require Python and are only needed for ifcb-analysis compatibility."),

      # Output folder — only relevant for MAT/statistics
      div(
        style = "display: flex; gap: 5px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("cfg_output_folder", "MAT / Statistics Output Folder",
                      value = config$output_folder, width = "100%")),
        shinyDirButton("browse_output_folder", "Browse", "Select Output Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),
      tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 20px;",
                 "Folder for MAT annotation files and validation statistics CSV files."),

      checkboxInput("cfg_export_statistics", "Export validation statistics",
                    value = config$export_statistics),
      tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                 "Write per-sample CSV files with classification accuracy to the output folder above."),

      # PNG output folder
      div(
        style = "display: flex; gap: 5px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("cfg_png_output_folder", "PNG Export Folder",
                      value = config$png_output_folder, width = "100%")),
        shinyDirButton("browse_png_folder", "Browse", "Select PNG Export Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),
      tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 20px;",
                 "Folder for exporting classified images sorted into class subfolders."),

      hr(),

      # -- Class List
      h5("Class List"),

      fileInput("class2use_file", "Load class list file (.mat or .txt)",
                accept = c(".mat", ".txt")),

      div(
        style = "display: flex; align-items: center; gap: 10px;",
        actionButton("open_class_editor", "Edit Class List",
                     icon = icon("list"), class = "btn-outline-primary"),
        tags$span(class = "text-muted", style = "font-size: 12px;",
                  textOutput("class_count_text", inline = TRUE))
      ),

      hr(),

      # -- Import / Export
      h5("Import / Export"),

      tags$label("Import to SQLite", style = "font-weight: 600; display: block; margin-bottom: 5px;"),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 5px;",
        actionButton("import_mat_to_db_btn", ".mat \u2192 SQLite",
                     icon = icon("file-import"), class = "btn-outline-secondary btn-sm"),
        actionButton("import_png_to_db_btn", "PNG \u2192 SQLite",
                     icon = icon("file-import"), class = "btn-outline-secondary btn-sm")
      ),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
                 "Bulk import annotated samples from .mat files or PNG class folders."),

      tags$label("Export from SQLite", style = "font-weight: 600; display: block; margin-bottom: 5px;"),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 5px;",
        actionButton("export_db_to_mat_btn", "SQLite \u2192 .mat",
                     icon = icon("file-export"), class = "btn-outline-secondary btn-sm"),
        actionButton("export_db_to_png_btn", "SQLite \u2192 PNG",
                     icon = icon("file-export"), class = "btn-outline-secondary btn-sm")
      ),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 5px;",
        actionButton("export_db_to_zip_btn", "SQLite \u2192 ZIP",
                     icon = icon("file-export"), class = "btn-outline-secondary btn-sm"),
        actionButton("export_db_to_matlab_zip_btn", "SQLite \u2192 MATLAB ZIP",
                     icon = icon("file-export"), class = "btn-outline-secondary btn-sm")
      ),
      div(
        style = "margin-bottom: 5px;",
        textInput("cfg_skip_class_png", "Skip class in PNG export",
                  value = if (nzchar(config$skip_class_png)) config$skip_class_png
                          else if (!is.null(rv$class2use) && length(rv$class2use) > 0) rv$class2use[1]
                          else "",
                  width = "250px"),
        tags$small(class = "text-muted",
                   "Images with this class are excluded from PNG export.",
                   "Leave empty to export all classes.")
      ),

      hr(),

      # -- Live Prediction
      h5("Live Prediction"),

      textInput("cfg_gradio_url", "Gradio API URL",
                value = config$gradio_url, width = "100%",
                placeholder = "https://irfcb-classify.hf.space"),
      tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                 "Enter Gradio API URL for CNN classification. Example: https://irfcb-classify.hf.space"),

      selectInput("cfg_prediction_model", "Prediction Model",
                  choices = if (nzchar(config$prediction_model)) config$prediction_model else NULL,
                  selected = if (nzchar(config$prediction_model)) config$prediction_model else NULL,
                  width = "100%"),
      tags$small(class = "text-muted", style = "display: block; margin-top: -5px; margin-bottom: 20px;",
                 "Select a CNN model for classification. Models are fetched from the Gradio API."),

      hr(),

      # -- IFCB Options
      h5("IFCB Options"),

      div(
        style = "display: flex; gap: 10px; align-items: center; margin-top: 10px;",
        numericInput("cfg_pixels_per_micron", "Pixels per micron",
                     value = config$pixels_per_micron, min = 0.1, max = 20, step = 0.1,
                     width = "150px"),
        tags$small(class = "text-muted", "Scale calibration for the measuring tool. IFCB default: 3.4 px/\u00b5m.")
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_settings", "Apply", class = "btn-outline-primary"),
        actionButton("save_settings", "Save Settings", class = "btn-primary")
      )
    ))
  })

  # shinyFiles directory browser setup
  shinyDirChoose(input, "browse_csv_folder",
                 roots = make_dynamic_roots("cfg_csv_folder"), session = session)
  shinyDirChoose(input, "browse_roi_folder",
                 roots = make_dynamic_roots("cfg_roi_folder"), session = session)
  shinyDirChoose(input, "browse_output_folder",
                 roots = make_dynamic_roots("cfg_output_folder"), session = session)
  shinyDirChoose(input, "browse_db_folder",
                 roots = make_dynamic_roots("cfg_db_folder"), session = session)
  shinyDirChoose(input, "browse_png_folder",
                 roots = make_dynamic_roots("cfg_png_output_folder"), session = session)
  shinyDirChoose(input, "browse_png_import_folder",
                 roots = make_dynamic_roots("cfg_png_import_folder"), session = session)
  shinyDirChoose(input, "browse_cr_external_folder",
                 roots = make_dynamic_roots("cr_external_folder"), session = session)
  shinyDirChoose(input, "browse_cr_external_export_folder",
                 roots = make_dynamic_roots("cr_external_export_folder"), session = session)

  setup_path_validation("cfg_csv_folder", "browse_csv_folder", "Classification Folder")
  setup_path_validation("cfg_roi_folder", "browse_roi_folder", "ROI/PNG Data Folder")
  setup_path_validation("cfg_output_folder", "browse_output_folder", "Output Folder", notify_invalid = FALSE)
  setup_path_validation("cfg_db_folder", "browse_db_folder", "Database Folder", notify_invalid = FALSE)
  setup_path_validation("cfg_png_output_folder", "browse_png_folder", "PNG Output Folder", notify_invalid = FALSE)
  setup_path_validation("cfg_png_import_folder", "browse_png_import_folder", "PNG Import Folder")
  setup_path_validation("cr_external_folder", "browse_cr_external_folder", "External PNG Folder")
  setup_path_validation("cr_external_export_folder", "browse_cr_external_export_folder", "External Export Folder", notify_invalid = FALSE)

  # Browse button observers
  observeEvent(input$browse_csv_folder, {
    if (!is.integer(input$browse_csv_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_csv_folder), input$browse_csv_folder)
      if (length(folder) > 0) updateTextInput(session, "cfg_csv_folder", value = as.character(folder))
    }
  })

  observeEvent(input$browse_roi_folder, {
    if (!is.integer(input$browse_roi_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_roi_folder), input$browse_roi_folder)
      if (length(folder) > 0) updateTextInput(session, "cfg_roi_folder", value = as.character(folder))
    }
  })

  observeEvent(input$browse_output_folder, {
    if (!is.integer(input$browse_output_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_output_folder), input$browse_output_folder)
      if (length(folder) > 0) updateTextInput(session, "cfg_output_folder", value = as.character(folder))
    }
  })

  observeEvent(input$browse_db_folder, {
    if (!is.integer(input$browse_db_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_db_folder), input$browse_db_folder)
      if (length(folder) > 0) updateTextInput(session, "cfg_db_folder", value = as.character(folder))
    }
  })

  observeEvent(input$browse_png_folder, {
    if (!is.integer(input$browse_png_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_png_output_folder), input$browse_png_folder)
      if (length(folder) > 0) updateTextInput(session, "cfg_png_output_folder", value = as.character(folder))
    }
  })

  observeEvent(input$browse_cr_external_folder, {
    if (!is.integer(input$browse_cr_external_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cr_external_folder), input$browse_cr_external_folder)
      if (length(folder) > 0) updateTextInput(session, "cr_external_folder", value = as.character(folder))
    }
  })

  observeEvent(input$browse_cr_external_export_folder, {
    if (!is.integer(input$browse_cr_external_export_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cr_external_export_folder), input$browse_cr_external_export_folder)
      if (length(folder) > 0) updateTextInput(session, "cr_external_export_folder", value = as.character(folder))
    }
  })

  # Fetch available models when Gradio URL changes (debounced)
  gradio_url_debounced <- debounce(reactive(input$cfg_gradio_url), 1500)

  observeEvent(gradio_url_debounced(), {
    url <- gradio_url_debounced()
    if (is.null(url) || !nzchar(url)) {
      updateSelectInput(session, "cfg_prediction_model", choices = character(0))
      return()
    }
    tryCatch({
      models <- iRfcb::ifcb_classify_models(url)
      if (length(models) > 0) {
        current <- config$prediction_model
        selected <- if (nzchar(current) && current %in% models) current else models[1]
        updateSelectInput(session, "cfg_prediction_model",
                          choices = models, selected = selected)
      } else {
        updateSelectInput(session, "cfg_prediction_model", choices = character(0))
        showNotification("No models found at the provided URL.", type = "warning")
      }
    }, error = function(e) {
      updateSelectInput(session, "cfg_prediction_model", choices = character(0))
      showNotification(paste("Could not fetch models:", e$message), type = "error")
    })
  })

  output$class_count_text <- renderText({
    if (is.null(rv$class2use)) {
      "No class list loaded"
    } else {
      paste(length(rv$class2use), "classes loaded")
    }
  })

  # apply_settings closure
  apply_settings <- function(close_modal = FALSE, notification = "Settings saved.") {
    roi_changed <- !identical(config$roi_folder, input$cfg_roi_folder)
    csv_changed <- !identical(config$csv_folder, input$cfg_csv_folder)
    data_source_changed <- !identical(config$data_source, input$cfg_data_source)
    dashboard_url_changed <- !identical(config$dashboard_url, input$cfg_dashboard_url)
    paths_changed <- roi_changed || csv_changed || data_source_changed || dashboard_url_changed

    old_db_folder <- config$db_folder

    config$csv_folder <- input$cfg_csv_folder
    config$roi_folder <- input$cfg_roi_folder
    config$output_folder <- input$cfg_output_folder
    config$png_output_folder <- input$cfg_png_output_folder
    config$db_folder <- input$cfg_db_folder
    config$use_threshold <- input$cfg_use_threshold
    config$pixels_per_micron <- input$cfg_pixels_per_micron
    config$auto_sync <- input$cfg_auto_sync
    config$save_format <- input$cfg_save_format
    config$export_statistics <- input$cfg_export_statistics
    config$skip_class_png <- input$cfg_skip_class_png
    config$data_source <- input$cfg_data_source
    config$dashboard_url <- input$cfg_dashboard_url
    config$dashboard_autoclass <- input$cfg_dashboard_autoclass
    config$dashboard_parallel_downloads <- if (!is.null(input$cfg_dashboard_parallel_downloads)) input$cfg_dashboard_parallel_downloads else 5
    config$dashboard_sleep_time <- if (!is.null(input$cfg_dashboard_sleep_time)) input$cfg_dashboard_sleep_time else 2
    config$dashboard_multi_timeout <- if (!is.null(input$cfg_dashboard_multi_timeout)) input$cfg_dashboard_multi_timeout else 120
    config$dashboard_max_retries <- if (!is.null(input$cfg_dashboard_max_retries)) input$cfg_dashboard_max_retries else 3
    config$gradio_url <- input$cfg_gradio_url
    config$prediction_model <- input$cfg_prediction_model

    if (!identical(old_db_folder, config$db_folder)) {
      rv$class_aphia_map <- load_worms_map(config$db_folder)
    }

    persist_settings(list(
      csv_folder = input$cfg_csv_folder,
      roi_folder = input$cfg_roi_folder,
      output_folder = input$cfg_output_folder,
      png_output_folder = input$cfg_png_output_folder,
      db_folder = input$cfg_db_folder,
      use_threshold = input$cfg_use_threshold,
      pixels_per_micron = input$cfg_pixels_per_micron,
      auto_sync = input$cfg_auto_sync,
      save_format = input$cfg_save_format,
      export_statistics = input$cfg_export_statistics,
      skip_class_png = input$cfg_skip_class_png,
      class2use_path = rv$class2use_path,
      python_venv_path = config$python_venv_path,
      data_source = input$cfg_data_source,
      dashboard_url = input$cfg_dashboard_url,
      dashboard_autoclass = input$cfg_dashboard_autoclass,
      dashboard_parallel_downloads = config$dashboard_parallel_downloads,
      dashboard_sleep_time = config$dashboard_sleep_time,
      dashboard_multi_timeout = config$dashboard_multi_timeout,
      dashboard_max_retries = config$dashboard_max_retries,
      gradio_url = input$cfg_gradio_url,
      prediction_model = input$cfg_prediction_model,
      zip_readme_author = config$zip_readme_author,
      zip_readme_contact_email = config$zip_readme_contact_email,
      zip_readme_doi = config$zip_readme_doi,
      zip_readme_license = config$zip_readme_license,
      zip_readme_version = config$zip_readme_version,
      zip_readme_citation = config$zip_readme_citation,
      zip_readme_institute = config$zip_readme_institute,
      zip_split_zip = config$zip_split_zip,
      zip_max_size = config$zip_max_size
    ))

    if (close_modal) removeModal()
    showNotification(notification, type = "message")

    if (paths_changed) {
      cache_path <- get_file_index_path()
      if (file.exists(cache_path)) file.remove(cache_path)
      rescan_trigger(rescan_trigger() + 1)
    }
  }

  observeEvent(input$apply_settings, {
    apply_settings(close_modal = FALSE, notification = "Settings applied.")
  })

  observeEvent(input$save_settings, {
    apply_settings(close_modal = TRUE, notification = "Settings saved.")
  })
}
