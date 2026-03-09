# MAT<->SQLite, ZIP/PNG import/export

setup_import_export_server <- function(input, output, session, rv, config,
                                       persist_settings, get_browse_volumes,
                                       build_zip_readme, roi_path_map,
                                       rescan_trigger) {
  # Import .mat -> SQLite bulk handler
  observeEvent(input$import_mat_to_db_btn, {
    if (is.null(config$output_folder) || config$output_folder == "") {
      showNotification("Output folder is not configured. Set it in Settings first.",
                       type = "error")
      return()
    }
    db_path <- get_db_path(config$db_folder)
    annotator <- if (!is.null(input$annotator_name) && nzchar(input$annotator_name)) {
      input$annotator_name
    } else {
      "imported"
    }

    withProgress(message = "Importing .mat files to SQLite...", {
      result <- import_all_mat_to_db(config$output_folder, db_path, annotator)
    })

    showNotification(
      sprintf("Import complete: %d imported, %d failed, %d skipped (already in DB).",
              result$success, result$failed, result$skipped),
      type = if (result$failed > 0) "warning" else "message",
      duration = 8
    )

    if (result$success > 0) {
      rescan_trigger(rescan_trigger() + 1)
    }
  })

  # Export SQLite -> .mat bulk handler: show confirmation dialog first
  observeEvent(input$export_db_to_mat_btn, {
    if (is.null(config$output_folder) || config$output_folder == "") {
      showNotification("Output folder is not configured. Set it in Settings first.",
                       type = "error")
      return()
    }
    if (!python_available) {
      showNotification("Python is not available. Export to .mat requires Python with scipy.",
                       type = "error")
      return()
    }

    showModal(modalDialog(
      title = "Confirm .mat export",
      p("This will export all annotated samples from the SQLite database as",
        tags$strong(".mat files"), "into:"),
      tags$code(config$output_folder),
      tags$br(), tags$br(),
      p(tags$strong("Existing .mat files in this folder will be overwritten"),
        "and cannot be recovered. Make sure you have a backup if needed."),
      p("Do you want to continue?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_export_mat_btn", "Export", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_export_mat_btn, {
    removeModal()
    db_path <- get_db_path(config$db_folder)

    withProgress(message = "Exporting SQLite to .mat files...", {
      result <- export_all_db_to_mat(db_path, config$output_folder)
    })

    showNotification(
      sprintf("Export complete: %d exported, %d failed.", result$success, result$failed),
      type = if (result$failed > 0) "warning" else "message",
      duration = 8
    )
  })

  observeEvent(input$export_db_to_zip_btn, {
    default_dir <- if (!is.null(config$output_folder) && nzchar(config$output_folder)) {
      config$output_folder
    } else {
      getwd()
    }
    default_zip <- file.path(
      default_dir,
      paste0("ifcb_ecotaxa_export_", format(Sys.Date(), "%Y%m%d"), ".zip")
    )

    # Get available instruments for filter dropdown
    zip_instruments <- tryCatch({
      meta <- list_annotation_metadata_db(get_db_path(config$db_folder))
      meta$instruments
    }, error = function(e) character())

    showModal(modalDialog(
      title = "Export SQLite -> ZIP",
      size = "l",
      textInput("zip_export_path", "ZIP file path", value = default_zip, width = "100%"),
      if (length(zip_instruments) > 1) {
        div(
          style = "margin-bottom: 10px;",
          selectizeInput("zip_instrument_filter", "Filter by IFCB",
                         choices = zip_instruments, selected = zip_instruments,
                         multiple = TRUE, width = "100%"),
          tags$small(class = "text-muted", "Remove instruments to exclude their samples from the export.")
        )
      },
      tags$small(class = "text-muted", "Optional fields used only when generating the README file."),
      div(
        style = "display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 10px;",
        textInput("zip_readme_author", "Author", value = config$zip_readme_author),
        textInput("zip_readme_contact_email", "Contact e-mail", value = config$zip_readme_contact_email),
        textInput("zip_readme_doi", "DOI", value = config$zip_readme_doi),
        textInput("zip_readme_license", "Licence", value = config$zip_readme_license),
        textInput("zip_readme_version", "Version", value = config$zip_readme_version),
        textInput("zip_readme_institute", "Institute", value = config$zip_readme_institute)
      ),
      textAreaInput("zip_readme_citation", "Citation", value = config$zip_readme_citation, width = "100%", rows = 3),
      checkboxInput("zip_split_zip", "Split ZIP archive", value = isTRUE(config$zip_split_zip)),
      conditionalPanel(
        condition = "input.zip_split_zip == true",
        numericInput(
          "zip_max_size",
          "Max part size (MB)",
          value = if (isTRUE(config$zip_split_zip)) config$zip_max_size else 500,
          min = 1, step = 50
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_export_zip_btn", "Export", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_export_zip_btn, {
    removeModal()
    zip_path <- trimws(input$zip_export_path)
    readme_fields <- list(
      author = trimws(input$zip_readme_author),
      contact_email = trimws(input$zip_readme_contact_email),
      doi = trimws(input$zip_readme_doi),
      license = trimws(input$zip_readme_license),
      version = trimws(input$zip_readme_version),
      citation = trimws(input$zip_readme_citation),
      institute = trimws(input$zip_readme_institute)
    )
    split_zip <- isTRUE(input$zip_split_zip)
    max_size <- if (split_zip) {
      as.numeric(if (!is.null(input$zip_max_size)) input$zip_max_size else 500)
    } else {
      config$zip_max_size
    }
    if (is.na(max_size) || max_size <= 0) max_size <- 500

    instrument_filter <- input$zip_instrument_filter

    if (!nzchar(zip_path)) {
      showNotification("ZIP file path is empty.", type = "error")
      return()
    }

    db_path <- get_db_path(config$db_folder)
    if (!file.exists(db_path)) {
      showNotification("Database not found. Save annotations first.", type = "error")
      return()
    }

    zip_dir <- dirname(zip_path)
    if (!dir.exists(zip_dir)) {
      dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Persist ZIP README metadata
    config$zip_readme_author <- readme_fields$author
    config$zip_readme_contact_email <- readme_fields$contact_email
    config$zip_readme_doi <- readme_fields$doi
    config$zip_readme_license <- readme_fields$license
    config$zip_readme_version <- readme_fields$version
    config$zip_readme_citation <- readme_fields$citation
    config$zip_readme_institute <- readme_fields$institute
    config$zip_split_zip <- split_zip
    config$zip_max_size <- max_size
    persist_settings(list(
      csv_folder = config$csv_folder, roi_folder = config$roi_folder,
      output_folder = config$output_folder, png_output_folder = config$png_output_folder,
      db_folder = config$db_folder, use_threshold = config$use_threshold,
      pixels_per_micron = config$pixels_per_micron, auto_sync = config$auto_sync,
      save_format = config$save_format, export_statistics = config$export_statistics,
      skip_class_png = config$skip_class_png, class2use_path = rv$class2use_path,
      python_venv_path = config$python_venv_path, data_source = config$data_source,
      dashboard_url = config$dashboard_url, dashboard_autoclass = config$dashboard_autoclass,
      dashboard_parallel_downloads = config$dashboard_parallel_downloads,
      dashboard_sleep_time = config$dashboard_sleep_time,
      dashboard_multi_timeout = config$dashboard_multi_timeout,
      dashboard_max_retries = config$dashboard_max_retries,
      gradio_url = config$gradio_url, prediction_model = config$prediction_model,
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

    temp_png <- tempfile("zip_export_png_")
    dir.create(temp_png, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_png, recursive = TRUE, force = TRUE), add = TRUE)

    skip <- if (!is.null(config$skip_class_png) && nzchar(config$skip_class_png)) {
      config$skip_class_png
    } else {
      NULL
    }

    is_dashboard <- identical(config$data_source, "dashboard")
    export_counts <- list(success = 0L, failed = 0L, skipped = 0L)

    # Get samples and apply instrument filter
    all_samples <- list_annotated_samples_db(db_path)
    if (length(instrument_filter) > 0) {
      instrument_pattern <- paste0("_(", paste(instrument_filter, collapse = "|"), ")$")
      all_samples <- all_samples[grepl(instrument_pattern, all_samples)]
    }
    if (length(all_samples) == 0) {
      showNotification("No annotated samples match the selected filter.", type = "warning")
      return()
    }

    if (is_dashboard) {
      samples <- all_samples

      cache_dir <- get_dashboard_cache_dir()
      parsed <- parse_dashboard_url(config$dashboard_url)

      withProgress(message = "Downloading images from dashboard...", value = 0, {
        cached_samples <- download_dashboard_images_bulk(
          parsed$base_url, samples, cache_dir,
          parallel_downloads = config$dashboard_parallel_downloads,
          sleep_time = config$dashboard_sleep_time,
          multi_timeout = config$dashboard_multi_timeout,
          max_retries = config$dashboard_max_retries
        )
      })

      withProgress(message = "Copying PNGs to class folders...", value = 0, {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)

        for (sn in samples) {
          rows <- dbGetQuery(con,
            "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
            params = list(sn))

          if (nrow(rows) == 0) {
            export_counts$skipped <- export_counts$skipped + 1L
            next
          }

          if (!is.null(skip)) {
            rows <- rows[!rows$class_name %in% skip, ]
            if (nrow(rows) == 0) next
          }

          src_dir <- file.path(cache_dir, sn, sn)
          if (!sn %in% cached_samples || !dir.exists(src_dir)) {
            export_counts$skipped <- export_counts$skipped + 1L
            next
          }

          ok <- tryCatch({
            for (cls in unique(rows$class_name)) {
              cls_rois <- rows$roi_number[rows$class_name == cls]
              dest <- file.path(temp_png, cls)
              dir.create(dest, recursive = TRUE, showWarnings = FALSE)
              for (rn in cls_rois) {
                fname <- sprintf("%s_%05d.png", sn, rn)
                src <- file.path(src_dir, fname)
                if (file.exists(src)) {
                  file.copy(src, file.path(dest, fname), overwrite = TRUE)
                }
              }
            }
            TRUE
          }, error = function(e) FALSE)

          if (isTRUE(ok)) {
            export_counts$success <- export_counts$success + 1L
          } else {
            export_counts$failed <- export_counts$failed + 1L
          }

          incProgress(1 / length(samples))
        }
      })
    } else {
      current_roi_map <- roi_path_map()

      if (length(current_roi_map) == 0) {
        showNotification("No ROI file index available. Click Sync first.", type = "error")
        return()
      }

      withProgress(message = "Exporting PNGs from SQLite...", value = 0, {
        export_counts <- export_all_db_to_png(db_path, temp_png, current_roi_map,
                                               skip_class = skip, samples = all_samples)
      })
    }

    png_count <- length(list.files(temp_png, pattern = "\\.png$", recursive = TRUE))
    if (png_count == 0) {
      showNotification("No PNG files were exported. ZIP not created.", type = "warning")
      return()
    }

    inventory_files <- 0L
    withProgress(message = "Creating inventory text files...", value = 0, {
      inventory_files <- ClassiPyR:::create_ecotaxa_inventory_txt(temp_png, db_path)
      incProgress(1)
    })

    readme_template <- system.file("exdata/README-template.md", package = "ClassiPyR")
    if (!nzchar(readme_template)) {
      readme_template <- system.file("exdata/README-template.md", package = "iRfcb")
    }
    readme_path <- build_zip_readme(
      template_path = readme_template,
      png_folder = temp_png,
      zip_path = zip_path,
      fields = readme_fields
    )

    zip_ok <- tryCatch({
      withProgress(message = "Creating ZIP archive...", value = 0, {
        iRfcb::ifcb_zip_pngs(
          png_folder = temp_png,
          zip_filename = zip_path,
          readme_file = if (!is.null(readme_path) && nzchar(readme_path)) readme_path else NULL,
          include_txt = TRUE,
          split_zip = split_zip,
          max_size = max_size,
          quiet = TRUE
        )
        incProgress(1)
      })
      TRUE
    }, error = function(e) {
      showNotification(paste("ZIP export failed:", e$message), type = "error", duration = 8)
      FALSE
    })

    if (!isTRUE(zip_ok)) return()

    showNotification(
      sprintf(
        "ZIP export complete: %d samples exported, %d failed, %d skipped, %d inventory files. ZIP: %s",
        export_counts$success, export_counts$failed, export_counts$skipped,
        inventory_files, zip_path
      ),
      type = if (export_counts$failed > 0) "warning" else "message",
      duration = 10
    )
  })

  # Export SQLite -> MATLAB ZIP handler: show dialog
  observeEvent(input$export_db_to_matlab_zip_btn, {
    default_dir <- if (!is.null(config$output_folder) && nzchar(config$output_folder)) {
      config$output_folder
    } else {
      getwd()
    }
    default_zip <- file.path(
      default_dir,
      paste0("ifcb_matlab_export_", format(Sys.Date(), "%Y%m%d"), ".zip")
    )

    # Get available instruments for filter dropdown
    matlab_zip_instruments <- tryCatch({
      meta <- list_annotation_metadata_db(get_db_path(config$db_folder))
      meta$instruments
    }, error = function(e) character())

    showModal(modalDialog(
      title = "Export SQLite \u2192 MATLAB ZIP",
      size = "l",
      textInput("matlab_zip_export_path", "ZIP file path", value = default_zip, width = "100%"),
      if (length(matlab_zip_instruments) > 1) {
        div(
          style = "margin-bottom: 10px;",
          selectizeInput("matlab_zip_instrument_filter", "Filter by IFCB",
                         choices = matlab_zip_instruments, selected = matlab_zip_instruments,
                         multiple = TRUE, width = "100%"),
          tags$small(class = "text-muted", "Remove instruments to exclude their samples from the export.")
        )
      },
      textInput("matlab_zip_features_folder", "Features folder (required)", value = "", width = "100%"),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
                 "Top-level features folder (e.g. /path/to/features). CSV files in subdirectories are included when recursive search is enabled."),
      textInput("matlab_zip_data_folder", "Data folder (optional)",
                value = if (!is.null(config$roi_folder) && nzchar(config$roi_folder)) config$roi_folder else "",
                width = "100%"),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
                 "Folder with raw IFCB data files (.roi, .adc, .hdr). Clear to omit from ZIP."),
      div(
        style = "display: flex; gap: 20px; margin-bottom: 10px;",
        checkboxInput("matlab_zip_feature_recursive", "Search features recursively", value = TRUE),
        checkboxInput("matlab_zip_data_recursive", "Search data recursively", value = TRUE)
      ),
      tags$small(class = "text-muted", "Optional fields used for the README file."),
      div(
        style = "display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 10px;",
        textInput("matlab_zip_readme_author", "Author", value = config$zip_readme_author),
        textInput("matlab_zip_readme_contact_email", "Contact e-mail", value = config$zip_readme_contact_email),
        textInput("matlab_zip_readme_doi", "DOI", value = config$zip_readme_doi),
        textInput("matlab_zip_readme_license", "Licence", value = config$zip_readme_license),
        textInput("matlab_zip_readme_version", "Version", value = config$zip_readme_version),
        textInput("matlab_zip_readme_institute", "Institute", value = config$zip_readme_institute)
      ),
      textAreaInput("matlab_zip_readme_citation", "Citation", value = config$zip_readme_citation, width = "100%", rows = 3),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_export_matlab_zip_btn", "Export", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_export_matlab_zip_btn, {
    removeModal()

    zip_path <- trimws(input$matlab_zip_export_path)
    features_folder <- trimws(input$matlab_zip_features_folder)
    data_folder <- trimws(input$matlab_zip_data_folder)

    readme_fields <- list(
      author = trimws(input$matlab_zip_readme_author),
      contact_email = trimws(input$matlab_zip_readme_contact_email),
      doi = trimws(input$matlab_zip_readme_doi),
      license = trimws(input$matlab_zip_readme_license),
      version = trimws(input$matlab_zip_readme_version),
      citation = trimws(input$matlab_zip_readme_citation),
      institute = trimws(input$matlab_zip_readme_institute)
    )
    feature_recursive <- isTRUE(input$matlab_zip_feature_recursive)
    data_recursive <- isTRUE(input$matlab_zip_data_recursive)

    instrument_filter <- input$matlab_zip_instrument_filter

    if (!nzchar(zip_path)) {
      showNotification("ZIP file path is empty.", type = "error")
      return()
    }
    if (!nzchar(features_folder) || !dir.exists(features_folder)) {
      showNotification("Features folder is missing or does not exist.", type = "error")
      return()
    }

    db_path <- get_db_path(config$db_folder)
    if (!file.exists(db_path)) {
      showNotification("Database not found. Save annotations first.", type = "error")
      return()
    }

    # Get filtered sample list
    filtered_samples <- list_annotated_samples_db(db_path)
    if (length(instrument_filter) > 0) {
      instrument_pattern <- paste0("_(", paste(instrument_filter, collapse = "|"), ")$")
      filtered_samples <- filtered_samples[grepl(instrument_pattern, filtered_samples)]
    }
    if (length(filtered_samples) == 0) {
      showNotification("No annotated samples match the selected filter.", type = "warning")
      return()
    }

    # Determine manual_folder
    use_temp_mat <- FALSE
    if (config$save_format %in% c("mat", "both")) {
      manual_folder <- config$output_folder
    } else {
      if (!python_available) {
        showNotification("Python with scipy is required to convert SQLite annotations to .mat files.",
                         type = "error")
        return()
      }
      temp_mat_dir <- tempfile("matlab_zip_manual_")
      dir.create(temp_mat_dir, recursive = TRUE, showWarnings = FALSE)
      use_temp_mat <- TRUE

      withProgress(message = "Exporting SQLite to temporary .mat files...", {
        mat_result <- export_all_db_to_mat(db_path, temp_mat_dir, samples = filtered_samples)
      })

      if (mat_result$success == 0) {
        showNotification("No .mat files were exported. Check that annotations exist.", type = "error")
        unlink(temp_mat_dir, recursive = TRUE, force = TRUE)
        return()
      }
      manual_folder <- temp_mat_dir
    }
    if (use_temp_mat) {
      on.exit(unlink(temp_mat_dir, recursive = TRUE, force = TRUE), add = TRUE)
    }

    # Create class2use.mat in a temp location
    temp_config_dir <- tempfile("matlab_zip_config_")
    dir.create(temp_config_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_config_dir, recursive = TRUE, force = TRUE), add = TRUE)

    class_list <- rv$class2use
    if (is.null(class_list) || length(class_list) == 0) {
      class_list <- load_global_class_list_db(db_path)
    }
    if (is.null(class_list) || length(class_list) == 0) {
      showNotification("No class list available. Load or create a class list first.", type = "error")
      return()
    }

    class2use_file <- file.path(temp_config_dir, "class2use.mat")
    tryCatch({
      iRfcb::ifcb_create_class2use(class_list, class2use_file)
    }, error = function(e) {
      showNotification(paste("Failed to create class2use.mat:", e$message), type = "error")
      return()
    })

    if (!file.exists(class2use_file)) {
      showNotification("Failed to create class2use.mat.", type = "error")
      return()
    }

    # Build README
    readme_template <- system.file("exdata/README-template.md", package = "ClassiPyR")
    if (!nzchar(readme_template)) {
      readme_template <- system.file("exdata/README-template.md", package = "iRfcb")
    }
    readme_path <- build_zip_readme(
      template_path = readme_template,
      png_folder = manual_folder,
      zip_path = zip_path,
      fields = readme_fields
    )

    # MATLAB template
    matlab_readme <- system.file("exdata/MATLAB-template.md", package = "ClassiPyR")

    zip_dir <- dirname(zip_path)
    if (!dir.exists(zip_dir)) {
      dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Persist metadata settings
    config$zip_readme_author <- readme_fields$author
    config$zip_readme_contact_email <- readme_fields$contact_email
    config$zip_readme_doi <- readme_fields$doi
    config$zip_readme_license <- readme_fields$license
    config$zip_readme_version <- readme_fields$version
    config$zip_readme_citation <- readme_fields$citation
    config$zip_readme_institute <- readme_fields$institute
    persist_settings(list(
      csv_folder = config$csv_folder, roi_folder = config$roi_folder,
      output_folder = config$output_folder, png_output_folder = config$png_output_folder,
      db_folder = config$db_folder, use_threshold = config$use_threshold,
      pixels_per_micron = config$pixels_per_micron, auto_sync = config$auto_sync,
      save_format = config$save_format, export_statistics = config$export_statistics,
      skip_class_png = config$skip_class_png, class2use_path = rv$class2use_path,
      python_venv_path = config$python_venv_path, data_source = config$data_source,
      dashboard_url = config$dashboard_url, dashboard_autoclass = config$dashboard_autoclass,
      dashboard_parallel_downloads = config$dashboard_parallel_downloads,
      dashboard_sleep_time = config$dashboard_sleep_time,
      dashboard_multi_timeout = config$dashboard_multi_timeout,
      dashboard_max_retries = config$dashboard_max_retries,
      gradio_url = config$gradio_url, prediction_model = config$prediction_model,
      zip_readme_author = config$zip_readme_author,
      zip_readme_contact_email = config$zip_readme_contact_email,
      zip_readme_doi = config$zip_readme_doi,
      zip_readme_license = config$zip_readme_license,
      zip_readme_version = config$zip_readme_version,
      zip_readme_citation = config$zip_readme_citation,
      zip_readme_institute = config$zip_readme_institute
    ))

    # Call ifcb_zip_matlab
    zip_ok <- tryCatch({
      withProgress(message = "Creating MATLAB ZIP archive...", value = 0, {
        iRfcb::ifcb_zip_matlab(
          manual_folder = manual_folder,
          features_folder = features_folder,
          class2use_file = class2use_file,
          zip_filename = zip_path,
          data_folder = if (nzchar(data_folder) && dir.exists(data_folder)) data_folder else NULL,
          readme_file = if (!is.null(readme_path) && nzchar(readme_path)) readme_path else NULL,
          matlab_readme_file = if (nzchar(matlab_readme)) matlab_readme else NULL,
          email_address = readme_fields$contact_email,
          version = readme_fields$version,
          feature_recursive = feature_recursive,
          data_recursive = data_recursive,
          quiet = TRUE
        )
        incProgress(1)
      })
      TRUE
    }, error = function(e) {
      showNotification(paste("MATLAB ZIP export failed:", e$message), type = "error", duration = 8)
      FALSE
    })

    if (!isTRUE(zip_ok)) return()

    showNotification(
      sprintf("MATLAB ZIP export complete: %s", basename(zip_path)),
      type = "message",
      duration = 10
    )
  })

  # Export SQLite -> PNG bulk handler
  observeEvent(input$export_db_to_png_btn, {
    if (is.null(config$png_output_folder) || config$png_output_folder == "") {
      showNotification("PNG Output Folder is not configured. Set it in Settings first.",
                       type = "error")
      return()
    }

    db_path <- get_db_path(config$db_folder)
    is_dashboard <- identical(config$data_source, "dashboard")

    if (is_dashboard) {
      samples <- list_annotated_samples_db(db_path)
      if (length(samples) == 0) {
        showNotification("No annotated samples in database.", type = "warning")
        return()
      }

      cache_dir <- get_dashboard_cache_dir()
      parsed <- parse_dashboard_url(config$dashboard_url)

      skip <- if (!is.null(config$skip_class_png) && nzchar(config$skip_class_png)) {
        config$skip_class_png
      } else {
        NULL
      }

      counts <- list(success = 0L, failed = 0L, skipped = 0L)

      withProgress(message = "Downloading images from dashboard...", value = 0, {
        cached_samples <- download_dashboard_images_bulk(
          parsed$base_url, samples, cache_dir,
          parallel_downloads = config$dashboard_parallel_downloads,
          sleep_time = config$dashboard_sleep_time,
          multi_timeout = config$dashboard_multi_timeout,
          max_retries = config$dashboard_max_retries)
      })

      withProgress(message = "Copying PNGs to class folders...", value = 0, {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)

        for (sn in samples) {
          rows <- dbGetQuery(con,
            "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
            params = list(sn))

          if (nrow(rows) == 0) {
            counts$skipped <- counts$skipped + 1L
            next
          }

          if (!is.null(skip)) {
            rows <- rows[!rows$class_name %in% skip, ]
            if (nrow(rows) == 0) next
          }

          src_dir <- file.path(cache_dir, sn, sn)
          if (!sn %in% cached_samples || !dir.exists(src_dir)) {
            counts$skipped <- counts$skipped + 1L
            next
          }

          ok <- tryCatch({
            for (cls in unique(rows$class_name)) {
              cls_rois <- rows$roi_number[rows$class_name == cls]
              dest <- file.path(config$png_output_folder, cls)
              dir.create(dest, recursive = TRUE, showWarnings = FALSE)
              for (rn in cls_rois) {
                fname <- sprintf("%s_%05d.png", sn, rn)
                src <- file.path(src_dir, fname)
                if (file.exists(src)) {
                  file.copy(src, file.path(dest, fname), overwrite = TRUE)
                }
              }
            }
            TRUE
          }, error = function(e) FALSE)

          if (isTRUE(ok)) {
            counts$success <- counts$success + 1L
          } else {
            counts$failed <- counts$failed + 1L
          }

          incProgress(1 / length(samples))
        }
      })

      showNotification(
        sprintf("PNG export complete: %d exported, %d failed, %d skipped.",
                counts$success, counts$failed, counts$skipped),
        type = if (counts$failed > 0) "warning" else "message",
        duration = 8
      )
    } else {
      if (is.null(config$output_folder) || config$output_folder == "") {
        showNotification("Output folder is not configured. Set it in Settings first.",
                         type = "error")
        return()
      }

      current_roi_map <- roi_path_map()

      if (length(current_roi_map) == 0) {
        showNotification("No ROI file index available. Click Sync first.",
                         type = "error")
        return()
      }

      skip <- if (!is.null(config$skip_class_png) && nzchar(config$skip_class_png)) {
        config$skip_class_png
      } else {
        NULL
      }

      withProgress(message = "Exporting PNGs from SQLite...", {
        result <- export_all_db_to_png(db_path, config$png_output_folder,
                                       current_roi_map, skip_class = skip)
      })

      showNotification(
        sprintf("PNG export complete: %d exported, %d failed, %d skipped (ROI not found).",
                result$success, result$failed, result$skipped),
        type = if (result$failed > 0) "warning" else "message",
        duration = 8
      )
    }
  })

  # Import PNG -> SQLite (multi-step flow)
  png_import_state <- reactiveValues(
    scan_result = NULL,
    class_mapping = NULL,
    png_folder = NULL
  )

  observeEvent(input$import_png_to_db_btn, {
    showModal(modalDialog(
      title = "Import PNG \u2192 SQLite",
      size = "m",
      easyClose = TRUE,
      div(
        style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 15px;",
        div(style = "flex: 1;",
            textInput("cfg_png_import_folder", "PNG Import Folder",
                      value = config$png_output_folder, width = "100%")),
        shinyDirButton("browse_png_import_folder", "Browse", "Select PNG Import Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),
      tags$small(class = "text-muted",
                 "Select a folder containing PNG images organized in class-name subfolders.",
                 "Folder names follow iRfcb convention (trailing _NNN suffix is stripped)."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("scan_png_folder_btn", "Scan Folder", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$browse_png_import_folder, {
    if (!is.integer(input$browse_png_import_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_png_import_folder),
                             input$browse_png_import_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_png_import_folder", value = as.character(folder))
      }
    }
  })

  observeEvent(input$scan_png_folder_btn, {
    png_folder <- input$cfg_png_import_folder
    if (is.null(png_folder) || !nzchar(png_folder) || !dir.exists(png_folder)) {
      showNotification("Please select a valid folder.", type = "error")
      return()
    }

    withProgress(message = "Scanning PNG folder...", value = 0, {
      incProgress(0.1, detail = "Reading folder structure...")
      scan_result <- tryCatch(
        scan_png_class_folder(png_folder),
        error = function(e) {
          showNotification(paste("Scan failed:", e$message), type = "error")
          NULL
        }
      )
      incProgress(0.9, detail = "Scan complete")
    })

    if (is.null(scan_result) || nrow(scan_result$annotations) == 0) {
      showNotification("No valid PNG images found in the selected folder.", type = "error")
      return()
    }

    png_import_state$scan_result <- scan_result
    png_import_state$png_folder <- png_folder
    png_import_state$class_mapping <- NULL

    unmatched <- setdiff(scan_result$classes_found, rv$class2use)

    if (length(unmatched) > 0) {
      mapping_inputs <- lapply(unmatched, function(cls) {
        choices <- c("Add as new" = "__add_new__", setNames(rv$class2use, rv$class2use))
        div(
          style = "display: flex; gap: 10px; align-items: center; margin-bottom: 5px;",
          tags$span(style = "flex: 0 0 200px; font-weight: bold;", cls),
          div(style = "flex: 1;",
              selectInput(paste0("png_map_", gsub("[^a-zA-Z0-9]", "_", cls)),
                          label = NULL, choices = choices, width = "100%"))
        )
      })

      showModal(modalDialog(
        title = "Map Unmatched Classes",
        size = "l",
        easyClose = FALSE,
        p(sprintf("Found %d class(es) not in your current class list. Map them to existing classes or add as new:",
                  length(unmatched))),
        p(sprintf("Scanned: %d images across %d samples in %d classes.",
                  nrow(scan_result$annotations),
                  length(scan_result$sample_names),
                  length(scan_result$classes_found))),
        hr(),
        tagList(mapping_inputs),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_png_class_mapping_btn", "Continue", class = "btn-primary")
        )
      ))
    } else {
      check_png_import_overwrite()
    }
  })

  observeEvent(input$confirm_png_class_mapping_btn, {
    scan_result <- png_import_state$scan_result
    if (is.null(scan_result)) return()

    unmatched <- setdiff(scan_result$classes_found, rv$class2use)
    class_mapping <- character()
    new_classes <- character()

    for (cls in unmatched) {
      input_id <- paste0("png_map_", gsub("[^a-zA-Z0-9]", "_", cls))
      mapped_to <- input[[input_id]]
      if (!is.null(mapped_to) && mapped_to != "__add_new__") {
        class_mapping[cls] <- mapped_to
      } else {
        new_classes <- c(new_classes, cls)
      }
    }

    png_import_state$class_mapping <- if (length(class_mapping) > 0) class_mapping else NULL

    if (length(new_classes) > 0) {
      rv$class2use <- c(rv$class2use, new_classes)
      showNotification(
        sprintf("Added %d new class(es): %s", length(new_classes),
                paste(new_classes, collapse = ", ")),
        type = "message", duration = 5
      )
    }

    check_png_import_overwrite()
  })

  check_png_import_overwrite <- function() {
    scan_result <- png_import_state$scan_result
    if (is.null(scan_result)) return()

    db_path <- get_db_path(config$db_folder)
    existing <- list_annotated_samples_db(db_path)
    overlapping <- intersect(scan_result$sample_names, existing)

    if (length(overlapping) > 0) {
      showModal(modalDialog(
        title = "Overwrite Existing Samples?",
        size = "m",
        easyClose = FALSE,
        p(sprintf("%d of %d samples already have annotations in the database and will be overwritten:",
                  length(overlapping), length(scan_result$sample_names))),
        div(
          style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px;",
          tags$ul(lapply(overlapping, tags$li))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_png_overwrite_btn", "Overwrite & Import",
                       class = "btn-warning")
        )
      ))
    } else {
      execute_png_import()
    }
  }

  observeEvent(input$confirm_png_overwrite_btn, {
    execute_png_import()
  })

  execute_png_import <- function() {
    removeModal()

    scan_result <- png_import_state$scan_result
    png_folder <- png_import_state$png_folder
    if (is.null(scan_result) || is.null(png_folder)) return()

    db_path <- get_db_path(config$db_folder)
    annotator <- if (!is.null(input$annotator_name) && nzchar(input$annotator_name)) {
      input$annotator_name
    } else {
      "imported"
    }

    withProgress(message = "Importing PNG annotations to SQLite...", value = 0, {
      incProgress(0.1, detail = "Preparing import...")
      result <- import_png_folder_to_db(
        png_folder, db_path, rv$class2use,
        class_mapping = png_import_state$class_mapping,
        annotator = annotator
      )
      incProgress(0.9, detail = "Import complete")
    })

    showNotification(
      sprintf("PNG import complete: %d samples imported, %d failed.",
              result$success, result$failed),
      type = if (result$failed > 0) "warning" else "message",
      duration = 8
    )

    if (result$success > 0) {
      rescan_trigger(rescan_trigger() + 1)
    }

    png_import_state$scan_result <- NULL
    png_import_state$class_mapping <- NULL
    png_import_state$png_folder <- NULL
  }
}
