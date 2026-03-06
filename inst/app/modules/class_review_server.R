# Cross-sample class review mode

setup_class_review_server <- function(input, output, session, rv, config,
                                      roi_path_map, save_to_cache,
                                      disable_nav_buttons, enable_nav_buttons) {
  # Helper: update class review class list based on current filter values
  update_cr_class_list <- function() {
    db_path <- get_db_path(config$db_folder)
    year <- input$cr_year_select
    month <- input$cr_month_select
    instrument <- input$cr_instrument_select

    annotator <- input$cr_annotator_select

    classes_df <- list_classes_db(db_path, year = year, month = month,
                                  instrument = instrument,
                                  annotator = annotator)

    if (nrow(classes_df) == 0) {
      updateSelectizeInput(session, "class_review_select",
                           choices = c("No classes found" = ""))
      return()
    }

    choices <- setNames(
      classes_df$class_name,
      sprintf("%s (%d)", classes_df$class_name, classes_df$count)
    )
    updateSelectizeInput(session, "class_review_select", choices = choices)
  }

  # Helper: update month/instrument choices for class review filters
  update_cr_month_choices <- function() {
    db_path <- get_db_path(config$db_folder)
    meta <- list_annotation_metadata_db(db_path)

    year_val <- input$cr_year_select

    if (!is.null(year_val) && year_val != "all") {
      # Get samples for this year to extract available months/instruments
      con <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(config$db_folder))
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      year_samples <- DBI::dbGetQuery(con,
        "SELECT DISTINCT sample_name FROM annotations WHERE sample_name LIKE ?",
        params = list(paste0("D", year_val, "%"))
      )$sample_name

      months <- sort(unique(substr(year_samples, 6, 7)))
      month_labels <- MONTH_NAMES[months]
      updateSelectInput(session, "cr_month_select",
                        choices = c("All" = "all", setNames(months, month_labels)),
                        selected = "all")

      instruments <- sort(unique(sub(".*_", "", year_samples)))
      current_instrument <- input$cr_instrument_select
      selected_instrument <- if (!is.null(current_instrument) && current_instrument %in% instruments) {
        current_instrument
      } else {
        "all"
      }
      updateSelectInput(session, "cr_instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = selected_instrument)
    } else {
      months <- meta$months
      month_labels <- MONTH_NAMES[months]
      updateSelectInput(session, "cr_month_select",
                        choices = c("All" = "all", setNames(months, month_labels)),
                        selected = "all")

      instruments <- meta$instruments
      updateSelectInput(session, "cr_instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = "all")
    }
  }

  # Helper: populate year/month/instrument/annotator dropdowns from DB metadata
  populate_cr_database_filters <- function() {
    db_path <- get_db_path(config$db_folder)
    meta <- list_annotation_metadata_db(db_path)

    updateSelectInput(session, "cr_year_select",
                      choices = c("All" = "all", setNames(meta$years, meta$years)),
                      selected = "all")
    month_labels <- MONTH_NAMES[meta$months]
    updateSelectInput(session, "cr_month_select",
                      choices = c("All" = "all", setNames(meta$months, month_labels)),
                      selected = "all")
    updateSelectInput(session, "cr_instrument_select",
                      choices = c("All" = "all", setNames(meta$instruments, meta$instruments)),
                      selected = "all")
    updateSelectInput(session, "cr_annotator_select",
                      choices = c("All" = "all", setNames(meta$annotators, meta$annotators)),
                      selected = "all")

    update_cr_class_list()
  }

  # When entering class review mode, populate filters and class dropdown
  observeEvent(input$app_mode, {
    if (input$app_mode == "class_review") {
      rv$class_review_source <- if (!is.null(input$class_review_source)) input$class_review_source else "database"
      if (identical(rv$class_review_source, "database")) {
        populate_cr_database_filters()
      }
    } else {
      # Leaving class review mode - clear state
      rv$class_review_mode <- FALSE
      rv$class_review_source <- "database"
      rv$class_review_class <- NULL
      rv$class_review_samples <- character()
      rv$class_review_original <- NULL
      rv$class_review_external_files <- NULL
      rv$select_all_state <- "first"
    }
  }, ignoreInit = TRUE)

  observeEvent(input$class_review_source, {
    rv$class_review_source <- input$class_review_source
    if (input$app_mode == "class_review") {
      rv$class_review_mode <- FALSE
      rv$class_review_class <- NULL
      rv$class_review_samples <- character()
      rv$class_review_original <- NULL
      rv$class_review_external_files <- NULL
      rv$current_sample <- NULL
      rv$classifications <- NULL
      rv$original_classifications <- NULL
      rv$selected_images <- character()
      rv$current_page <- 1
      rv$changes_log <- create_empty_changes_log()
      updateSelectInput(session, "class_filter", choices = c("All" = "all"), selected = "all")
    }
    if (input$app_mode == "class_review" && identical(input$class_review_source, "database")) {
      populate_cr_database_filters()
    }
  }, ignoreInit = TRUE)

  # Cascading filter updates for class review
  observeEvent(input$cr_year_select, {
    req(input$app_mode == "class_review")
    req(identical(input$class_review_source, "database"))
    update_cr_month_choices()
    update_cr_class_list()
  }, ignoreInit = TRUE)

  observeEvent(input$cr_month_select, {
    req(input$app_mode == "class_review")
    req(identical(input$class_review_source, "database"))
    update_cr_class_list()
  }, ignoreInit = TRUE)

  observeEvent(input$cr_instrument_select, {
    req(input$app_mode == "class_review")
    req(identical(input$class_review_source, "database"))
    update_cr_class_list()
  }, ignoreInit = TRUE)

  observeEvent(input$cr_annotator_select, {
    req(input$app_mode == "class_review")
    req(identical(input$class_review_source, "database"))
    update_cr_class_list()
  }, ignoreInit = TRUE)

  # Class review info output
  cr_info_content <- reactive({
    if (!rv$class_review_mode || is.null(rv$classifications)) return(NULL)

    n_images <- nrow(rv$classifications)
    n_samples <- length(rv$class_review_samples)
    n_changed <- 0L

    if (!is.null(rv$class_review_original)) {
      n_changed <- sum(rv$classifications$class_name != rv$class_review_original$class_name)
    }

    div(
      style = "font-size: 12px; color: #666; margin-bottom: 8px;",
      if (identical(rv$class_review_source, "external")) {
        sprintf("%d images from external folder", n_images)
      } else {
        sprintf("%d images from %d samples", n_images, n_samples)
      },
      if (n_changed > 0) tags$span(
        style = "color: #dc3545; font-weight: bold; margin-left: 5px;",
        sprintf("(%d changed)", n_changed)
      )
    )
  })

  output$class_review_info <- renderUI({ cr_info_content() })
  output$class_review_info_ext <- renderUI({ cr_info_content() })

  # Load class for review
  observeEvent(input$load_class_review, {
    req(input$class_review_source == "database")
    req(input$class_review_select, input$class_review_select != "")

    class_name <- input$class_review_select
    db_path <- get_db_path(config$db_folder)

    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })

    # Save current sample work if in sample mode
    if (!rv$class_review_mode && !is.null(rv$current_sample)) {
      save_to_cache()
    }

    # Query all annotations for this class (with filters)
    annotations <- load_class_annotations_db(db_path, class_name,
                                              year = input$cr_year_select,
                                              month = input$cr_month_select,
                                              instrument = input$cr_instrument_select,
                                              annotator = input$cr_annotator_select)

    if (is.null(annotations) || nrow(annotations) == 0) {
      showNotification(paste("No annotations found for class:", class_name),
                       type = "warning")
      return()
    }

    # Get unique samples and their ROI paths
    unique_samples <- unique(annotations$sample_name)
    current_roi_map <- roi_path_map()
    is_dashboard <- identical(config$data_source, "dashboard")

    # Create temp folder for extracted PNGs
    if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder)) {
      if (isTRUE(rv$temp_png_is_managed) &&
          (!is_dashboard || !startsWith(rv$temp_png_folder, get_dashboard_cache_dir()))) {
        unlink(rv$temp_png_folder, recursive = TRUE)
      }
    }
    rv$temp_png_folder <- tempfile(pattern = "ifcb_class_review_")
    dir.create(rv$temp_png_folder, recursive = TRUE)
    rv$temp_png_is_managed <- TRUE

    # Extract PNGs per sample
    missing_samples <- character()
    extracted_files <- character()

    withProgress(message = paste("Loading", class_name, "images..."),
                 value = 0, {

      if (is_dashboard) {
        # Dashboard mode: download individual PNGs directly (much faster than zip)
        parsed <- parse_dashboard_url(config$dashboard_url)
        all_file_names <- annotations$file_name

        downloaded <- download_dashboard_images_individual(
          base_url = parsed$base_url,
          file_names = all_file_names,
          dest_dir = rv$temp_png_folder,
          max_retries = config$dashboard_max_retries
        )

        extracted_files <- downloaded
        downloaded_samples <- unique(gsub("_\\d+\\.png$", "", downloaded))
        missing_samples <- setdiff(unique_samples, downloaded_samples)
      }

      for (idx in seq_along(unique_samples)) {
        sn <- unique_samples[idx]

        if (is_dashboard) {
          # Already handled above in batch
          incProgress(1 / length(unique_samples))
          next
        } else {
          # Local mode: extract from ROI file
          roi_path <- current_roi_map[[sn]]

          if (is.null(roi_path) || !file.exists(roi_path)) {
            missing_samples <- c(missing_samples, sn)
            next
          }

          sample_rois <- annotations$roi_number[annotations$sample_name == sn]

          tryCatch({
            ifcb_extract_pngs(
              roi_file = roi_path,
              out_folder = rv$temp_png_folder,
              ROInumbers = sample_rois,
              verbose = FALSE
            )

            # Check which files were actually extracted
            sample_dir <- file.path(rv$temp_png_folder, sn)
            if (dir.exists(sample_dir)) {
              files <- list.files(sample_dir, pattern = "\\.png$")
              extracted_files <- c(extracted_files, files)
            }
          }, error = function(e) {
            missing_samples <<- c(missing_samples, sn)
          })
        }

        incProgress(1 / length(unique_samples))
      }
    })

    # Filter annotations to only those with extracted images
    annotations <- annotations[annotations$file_name %in% extracted_files, ]

    if (nrow(annotations) == 0) {
      showNotification("No images could be extracted. Check ROI file paths.",
                       type = "error")
      return()
    }

    # Build classifications data frame (compatible with gallery)
    classifications <- data.frame(
      file_name = annotations$file_name,
      class_name = annotations$class_name,
      score = NA_real_,
      width = NA_real_,
      height = NA_real_,
      roi_area = NA_real_,
      stringsAsFactors = FALSE
    )

    # Set class review state
    rv$class_review_mode <- TRUE
    rv$class_review_source <- "database"
    rv$class_review_class <- class_name
    rv$class_review_samples <- setdiff(unique_samples, missing_samples)
    rv$class_review_external_files <- NULL
    rv$current_sample <- NULL
    rv$classifications <- classifications
    rv$original_classifications <- classifications
    rv$class_review_original <- classifications
    rv$is_annotation_mode <- TRUE
    rv$has_both_modes <- FALSE
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    # Update class filter dropdown
    available_classes <- sort(unique(classifications$class_name))
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, available_classes)),
                      selected = "all")

    # Notify
    n_extracted <- nrow(classifications)
    n_samples <- length(rv$class_review_samples)
    msg <- sprintf("Loaded %d %s images from %d samples", n_extracted, class_name, n_samples)
    if (length(missing_samples) > 0) {
      msg <- paste0(msg, sprintf(" (%d samples skipped - ROI not found)", length(missing_samples)))
    }
    showNotification(msg, type = "message", duration = 8)
  })

  observeEvent(input$cr_external_folder, {
    req(input$app_mode == "class_review")
    req(identical(input$class_review_source, "external"))
    if (!nzchar(input$cr_external_initial_class) &&
        !is.null(input$cr_external_folder) &&
        nzchar(input$cr_external_folder) &&
        dir.exists(input$cr_external_folder)) {
      updateTextInput(session, "cr_external_initial_class",
                      value = basename(normalizePath(input$cr_external_folder)))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$load_external_class_review, {
    req(input$app_mode == "class_review")
    req(input$class_review_source == "external")
    req(input$cr_external_folder, nzchar(input$cr_external_folder))

    source_folder <- input$cr_external_folder
    if (!dir.exists(source_folder)) {
      showNotification("Input PNG folder does not exist.", type = "error")
      return()
    }

    png_paths <- list.files(source_folder, pattern = "\\.png$", ignore.case = TRUE, full.names = TRUE)
    if (length(png_paths) == 0) {
      showNotification("No PNG files found in selected folder.", type = "warning")
      return()
    }

    class_name <- sanitize_string(trimws(input$cr_external_initial_class))
    if (!nzchar(class_name)) {
      class_name <- sanitize_string(basename(normalizePath(source_folder)))
    }
    if (!nzchar(class_name)) class_name <- "unclassified"

    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })

    if (!rv$class_review_mode && !is.null(rv$current_sample)) {
      save_to_cache()
    }

    new_temp_folder <- tempfile(pattern = "ifcb_class_review_external_")
    sample_name <- "__external_review__"
    sample_folder <- file.path(new_temp_folder, sample_name)
    dir.create(sample_folder, recursive = TRUE)

    n_input <- length(png_paths)
    copied_paths <- character(n_input)
    copied_files <- character(n_input)
    ok <- logical(n_input)

    withProgress(message = "Loading external PNG folder...", value = 0, {
      for (i in seq_along(png_paths)) {
        src <- png_paths[i]
        dest_name <- basename(src)
        # Deduplicate filenames to prevent silent overwrites
        if (dest_name %in% copied_files[ok]) {
          base <- tools::file_path_sans_ext(dest_name)
          ext <- tools::file_ext(dest_name)
          counter <- 1L
          while (paste0(base, "_", counter, ".", ext) %in% copied_files[ok]) {
            counter <- counter + 1L
          }
          dest_name <- paste0(base, "_", counter, ".", ext)
        }
        dst <- file.path(sample_folder, dest_name)
        if (isTRUE(file.copy(src, dst, overwrite = FALSE))) {
          copied_paths[i] <- src
          copied_files[i] <- dest_name
          ok[i] <- TRUE
        }
        incProgress(0.7 / n_input)
      }

      copied_paths <- copied_paths[ok]
      copied_files <- copied_files[ok]

      dims <- vector("list", length(copied_files))
      for (i in seq_along(copied_files)) {
        dims[[i]] <- ClassiPyR:::read_png_dimensions(file.path(sample_folder, copied_files[i]))
        if (length(copied_files) > 0) incProgress(0.3 / length(copied_files))
      }
    })

    if (length(copied_files) == 0) {
      unlink(new_temp_folder, recursive = TRUE)
      showNotification("Could not prepare PNG files for preview.", type = "error")
      return()
    }

    # Success - now safe to replace old temp folder
    if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder) &&
        isTRUE(rv$temp_png_is_managed)) {
      unlink(rv$temp_png_folder, recursive = TRUE)
    }
    rv$temp_png_folder <- new_temp_folder
    rv$temp_png_is_managed <- TRUE

    classifications <- data.frame(
      file_name = copied_files,
      class_name = rep(class_name, length(copied_files)),
      score = NA_real_,
      width = vapply(dims, `[[`, numeric(1), "width"),
      height = vapply(dims, `[[`, numeric(1), "height"),
      roi_area = vapply(dims, function(x) x$width * x$height, numeric(1)),
      stringsAsFactors = FALSE
    )

    rv$class_review_mode <- TRUE
    rv$class_review_source <- "external"
    rv$class_review_class <- class_name
    rv$class_review_samples <- sample_name
    rv$class_review_external_files <- data.frame(
      file_name = copied_files,
      source_path = copied_paths,
      stringsAsFactors = FALSE
    )
    rv$current_sample <- sample_name
    rv$classifications <- classifications
    rv$original_classifications <- classifications
    rv$class_review_original <- classifications
    rv$is_annotation_mode <- TRUE
    rv$has_both_modes <- FALSE
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    available_classes <- sort(unique(classifications$class_name))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices(available_classes),
                      selected = "all")

    showNotification(
      sprintf("Loaded %d images from external folder for relabeling", nrow(classifications)),
      type = "message", duration = 8
    )
  })

  # Save class review changes
  observeEvent(input$save_class_review_btn, {
    req(rv$class_review_mode)
    req(identical(rv$class_review_source, "database"))
    req(rv$classifications)
    req(rv$class_review_original)

    rv$is_loading <- TRUE
    on.exit({ rv$is_loading <- FALSE })

    # Find changed rows
    current <- rv$classifications
    original <- rv$class_review_original
    changed_mask <- current$class_name != original$class_name
    n_changed <- sum(changed_mask)

    if (n_changed == 0) {
      showNotification("No changes to save", type = "warning")
      return()
    }

    # Parse sample_name and roi_number from file_name
    changed_files <- current$file_name[changed_mask]
    changes_df <- data.frame(
      sample_name = sub("_(\\d{5})\\.png$", "", changed_files),
      roi_number = as.integer(sub(".*_(\\d{5})\\.png$", "\\1", changed_files)),
      new_class_name = current$class_name[changed_mask],
      stringsAsFactors = FALSE
    )

    db_path <- get_db_path(config$db_folder)
    annotator <- if (!is.null(input$annotator_name) && nzchar(input$annotator_name)) {
      input$annotator_name
    } else {
      "Unknown"
    }

    tryCatch({
      withProgress(message = "Saving class review changes...", {
        updated <- save_class_review_changes_db(db_path, changes_df, annotator)
      })

      # Update original to reflect saved state
      rv$class_review_original <- rv$classifications

      showNotification(
        sprintf("Saved %d changes across %d samples",
                updated, length(unique(changes_df$sample_name))),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
    })
  })

  observeEvent(input$export_external_class_review_btn, {
    req(rv$class_review_mode)
    req(identical(rv$class_review_source, "external"))
    req(rv$classifications)
    req(rv$class_review_external_files)
    req(input$cr_external_export_folder, nzchar(input$cr_external_export_folder))

    export_folder <- input$cr_external_export_folder
    if (!dir.exists(export_folder)) {
      ok <- tryCatch({
        dir.create(export_folder, recursive = TRUE, showWarnings = FALSE)
      }, error = function(e) FALSE)
      if (!isTRUE(ok) && !dir.exists(export_folder)) {
        showNotification("Could not create export folder.", type = "error")
        return()
      }
    }

    rv$is_loading <- TRUE
    on.exit({ rv$is_loading <- FALSE })

    files_map <- rv$class_review_external_files
    idx <- match(rv$classifications$file_name, files_map$file_name)
    source_paths <- files_map$source_path[idx]

    copied <- 0L
    withProgress(message = "Exporting relabeled images...", value = 0, {
      n_total <- nrow(rv$classifications)
      for (i in seq_len(n_total)) {
        src <- source_paths[i]
        if (!is.na(src) && file.exists(src)) {
          class_name <- sanitize_string(rv$classifications$class_name[i])
          if (!nzchar(class_name)) class_name <- "unclassified"
          class_dir <- file.path(export_folder, class_name)
          if (!dir.exists(class_dir)) {
            dir.create(class_dir, recursive = TRUE, showWarnings = FALSE)
          }
          dst <- file.path(class_dir, basename(src))
          if (isTRUE(file.copy(src, dst, overwrite = TRUE))) {
            copied <- copied + 1L
          }
        }
        incProgress(1 / n_total,
                    detail = sprintf("Exporting (%d/%d)", i, n_total))
      }
    })

    skipped <- nrow(rv$classifications) - copied
    n_classes <- length(unique(rv$classifications$class_name))
    if (skipped > 0) {
      showNotification(
        sprintf("Exported %d images into %d class folders (%d skipped - source missing)", copied, n_classes, skipped),
        type = "warning", duration = 10
      )
    } else {
      showNotification(
        sprintf("Exported %d images into %d class folders", copied, n_classes),
        type = "message", duration = 8
      )
    }
  })
}
