# Load samples from various sources, navigation, caching

setup_sample_loading_server <- function(input, output, session, rv, config,
                                        roi_path_map, png_sample_path_map,
                                        csv_path_map, classifier_mat_files,
                                        classifier_h5_files, annotated_samples,
                                        classified_samples,
                                        get_filtered_samples,
                                        update_current_sample_status_fn,
                                        update_sample_list) {
  # Helper functions for classification file discovery
  find_csv_file <- function(sample_name) {
    csv_map <- csv_path_map()
    path <- csv_map[[sample_name]]
    if (!is.null(path) && file.exists(path)) return(path)
    NULL
  }

  find_classifier_mat <- function(sample_name) {
    mat_map <- classifier_mat_files()
    if (sample_name %in% names(mat_map)) return(mat_map[[sample_name]])
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
    NULL
  }

  list_sample_png_files <- function(sample_name, sample_png_dir) {
    if (is.null(sample_png_dir) || !dir.exists(sample_png_dir)) return(character())
    pattern <- paste0("^", sample_name, "_\\d+\\.png$")
    list.files(sample_png_dir, pattern = pattern, full.names = FALSE)
  }

  infer_roi_dims_from_png <- function(sample_name, sample_png_dir) {
    ClassiPyR:::infer_roi_dims_from_png(sample_name, sample_png_dir)
  }

  apply_roi_dims_to_classifications <- function(classifications, roi_dims) {
    if (is.null(classifications) || is.null(roi_dims) || nrow(classifications) == 0) {
      return(classifications)
    }
    dims <- ClassiPyR:::match_roi_dims(
      ClassiPyR:::roi_number_from_file_name(classifications$file_name), roi_dims
    )
    classifications$width <- dims$width
    classifications$height <- dims$height
    classifications$roi_area <- dims$roi_area
    classifications[order(-classifications$width, na.last = TRUE), ]
  }

  cleanup_temp_png_folder <- function() {
    if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder) &&
        isTRUE(rv$temp_png_is_managed)) {
      unlink(rv$temp_png_folder, recursive = TRUE)
    }
  }

  set_active_png_folder <- function(path, managed = TRUE) {
    cleanup_temp_png_folder()
    rv$temp_png_folder <- path
    rv$temp_png_is_managed <- isTRUE(managed)
  }

  # Folder holding the .adc file of a sample (needed for MAT saves), or the
  # sample's PNG folder for PNG-only samples; NULL when neither is known
  find_adc_folder <- function(sample_name) {
    roi_path <- roi_path_map()[[sample_name]]
    if (!is.null(roi_path)) return(dirname(roi_path))
    find_sample_png_dir(sample_name)
  }

  # Record that the current sample is stored exactly as it is shown now
  mark_current_saved <- function() {
    rv$saved_state <- saved_state_of(rv$current_sample, rv$classifications)
  }

  # Save current sample to cache with LRU eviction
  save_to_cache <- function() {
    # Never cache/auto-save class review data: it is not a real sample
    # (external review uses the synthetic name "__external_review__")
    if (isTRUE(rv$class_review_mode) ||
        identical(rv$current_sample, "__external_review__")) {
      return(invisible(NULL))
    }
    if (!is.null(rv$current_sample) && !is.null(rv$classifications)) {
      if (length(rv$session_cache) >= MAX_CACHED_SAMPLES &&
          !(rv$current_sample %in% names(rv$session_cache))) {
        oldest_sample <- names(rv$session_cache)[1]
        rv$session_cache[[oldest_sample]] <- NULL
      }

      adc_folder_for_save <- find_adc_folder(rv$current_sample)
      # Nothing changed since the last save (manual or automatic): writing
      # the sample again would only copy every image a second time
      already_saved <- is_saved_state(rv$saved_state, rv$current_sample,
                                      rv$classifications)

      cache_entry <- list(
        classifications = rv$classifications,
        original_classifications = rv$original_classifications,
        changes_log = rv$changes_log,
        is_annotation_mode = rv$is_annotation_mode,
        has_classification = rv$has_classification,
        # Stored so the session-end autosave can find this sample's images
        # and ADC file even after navigation switched the active sample
        temp_png_folder = rv$temp_png_folder,
        adc_folder = adc_folder_for_save,
        # TRUE once the entry is known to be in storage, so the session-end
        # autosave does not write it again
        saved = already_saved
      )
      rv$session_cache[[rv$current_sample]] <- cache_entry

      if (already_saved) {
        return(invisible(NULL))
      }

      tryCatch({
        save_fmt_for_autosave <- config$save_format
        if (identical(config$data_source, "dashboard") && is.null(adc_folder_for_save)) {
          if (save_fmt_for_autosave %in% c("mat", "both")) {
            save_fmt_for_autosave <- "sqlite"
          }
        }

        saved <- save_sample_annotations(
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
          annotator = annotator_or_unknown(input$annotator_name),
          adc_folder = adc_folder_for_save,
          save_format = save_fmt_for_autosave,
          db_folder = config$db_folder,
          export_statistics = config$export_statistics
        )
        if (isTRUE(saved)) {
          mark_current_saved()
          cache_entry$saved <- TRUE
          rv$session_cache[[rv$current_sample]] <- cache_entry
          current_annotated <- annotated_samples()
          if (!rv$current_sample %in% current_annotated) {
            annotated_samples(c(current_annotated, rv$current_sample))
            update_current_sample_status_fn(rv$current_sample)
          }
        }
      }, error = function(e) {
        showNotification(paste("Auto-save failed:", e$message), type = "error")
      })
    }
  }

  # Refresh the class filter dropdown from the current classifications
  update_class_filter <- function() {
    available_classes <- sort(unique(rv$classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices(available_classes, unmatched))
  }

  # Main sample loading function
  load_sample_data <- function(sample_name) {
    req(rv$class2use)
    if (identical(config$data_source, "dashboard")) {
      load_sample_dashboard(sample_name)
    } else {
      load_sample_local(sample_name)
    }
  }

  finalize_sample_load <- function(classifications, sample_name, mode_message) {
    rv$original_classifications <- classifications
    rv$classifications <- classifications
    rv$cached_validation_classifications <- NULL
    rv$current_sample <- sample_name
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    update_class_filter()

    if (!is.null(mode_message)) {
      actual_count <- nrow(rv$classifications)
      showNotification(paste0(mode_message, ": ", actual_count, " images"), type = "message")
    }
  }

  # Auto-classifications of a sample from the dashboard, with ROI dimensions
  # attached; NULL (after a notification) when the dashboard has none
  load_dashboard_autoclass <- function(sample_name, roi_dims) {
    parsed <- parse_dashboard_url(config$dashboard_url)
    autoclass_warning <- NULL
    autoclass <- withProgress(message = "Downloading auto-classifications...", value = 0, {
      incProgress(0.1, detail = "Requesting classifier output...")
      out <- tryCatch(
        withCallingHandlers(
          download_dashboard_autoclass(parsed$base_url, sample_name,
                                       get_dashboard_cache_dir(),
                                       dataset_name = parsed$dataset_name,
                                       parallel_downloads = config$dashboard_parallel_downloads,
                                       sleep_time = config$dashboard_sleep_time,
                                       multi_timeout = config$dashboard_multi_timeout,
                                       max_retries = config$dashboard_max_retries),
          warning = function(w) {
            autoclass_warning <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) NULL
      )
      incProgress(0.9, detail = "Download complete")
      out
    })

    if (is.null(autoclass) || nrow(autoclass) == 0) {
      if (!is.null(autoclass_warning)) {
        showNotification(
          "No auto-classifications available for this sample on the dashboard.",
          type = "warning", duration = 6
        )
      }
      return(NULL)
    }

    dims <- ClassiPyR:::match_roi_dims(
      ClassiPyR:::roi_number_from_file_name(autoclass$file_name), roi_dims
    )
    autoclass$width <- dims$width
    autoclass$height <- dims$height
    autoclass$roi_area <- dims$roi_area
    autoclass
  }

  # Dashboard mode sample loading
  load_sample_dashboard <- function(sample_name) {
    if (sample_name %in% names(rv$session_cache)) {
      return(load_from_cache(sample_name, NULL))
    }

    tryCatch({
      parsed <- parse_dashboard_url(config$dashboard_url)
      cache_dir <- get_dashboard_cache_dir()
      db_path <- get_db_path(config$db_folder)
      has_db_annotation <- ClassiPyR:::sample_has_annotations_db(db_path, sample_name)

      png_folder <- withProgress(message = "Downloading images...", value = 0, {
        incProgress(0.1, detail = "Requesting image bundle...")
        out <- download_dashboard_images(parsed$base_url, sample_name, cache_dir,
                                         parallel_downloads = config$dashboard_parallel_downloads,
                                         sleep_time = config$dashboard_sleep_time,
                                         multi_timeout = config$dashboard_multi_timeout,
                                         max_retries = config$dashboard_max_retries)
        incProgress(0.9, detail = "Download complete")
        out
      })

      if (is.null(png_folder)) {
        showNotification(paste("Failed to download images for:", sample_name), type = "error")
        return(FALSE)
      }

      adc_path <- download_dashboard_adc(parsed$base_url, sample_name, cache_dir,
                                         parallel_downloads = config$dashboard_parallel_downloads,
                                         sleep_time = config$dashboard_sleep_time,
                                         multi_timeout = config$dashboard_multi_timeout,
                                         max_retries = config$dashboard_max_retries)
      roi_dims <- if (!is.null(adc_path) && file.exists(adc_path)) {
        tryCatch(read_roi_dimensions(adc_path), error = function(e) NULL)
      } else {
        NULL
      }

      mode_message <- NULL
      classifications <- NULL

      if (has_db_annotation) {
        if (is.null(roi_dims)) {
          png_files <- list.files(file.path(png_folder, sample_name), pattern = "\\.png$")
          roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", png_files))
          roi_dims <- data.frame(roi_number = roi_numbers, width = NA_real_, height = NA_real_, area = NA_real_)
        }
        classifications <- load_from_db(db_path, sample_name, roi_dims)
        rv$is_annotation_mode <- TRUE
        # Local classifier output counts as well, so the switch to
        # validation mode is offered for it too
        rv$has_classification <- isTRUE(config$dashboard_autoclass) ||
          !is.null(find_csv_file(sample_name)) ||
          !is.null(find_classifier_h5(sample_name)) ||
          !is.null(find_classifier_mat(sample_name))
        mode_message <- if (rv$has_classification) "Manual mode (switch available)" else "Resumed"
      }

      if (is.null(classifications) && !isTRUE(config$dashboard_autoclass)) {
        # Local classifier output for dashboard samples comes from the file
        # index, which already scanned the classification folder. Listing
        # the folder recursively for every sample made each load slow.
        local_csv <- find_csv_file(sample_name)
        local_h5 <- find_classifier_h5(sample_name)
        local_mat <- find_classifier_mat(sample_name)
        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"

        if (!is.null(local_csv)) {
          classifications <- load_from_csv(local_csv, use_threshold = config$use_threshold)
          classifications <- apply_roi_dims_to_classifications(classifications, roi_dims)
          mode_message <- paste0("Validation mode (Local CSV, ", threshold_text, ")")
        } else if (!is.null(local_h5) && file.exists(local_h5)) {
          classifications <- load_from_h5(local_h5, sample_name, roi_dims, use_threshold = config$use_threshold)
          mode_message <- paste0("Validation mode (Local H5, ", threshold_text, ")")
        } else if (!is.null(local_mat) && file.exists(local_mat)) {
          classifications <- load_from_classifier_mat(local_mat, sample_name, rv$class2use, roi_dims, use_threshold = config$use_threshold)
          mode_message <- paste0("Validation mode (Local MAT, ", threshold_text, ")")
        }
        if (!is.null(classifications)) {
          rv$is_annotation_mode <- FALSE
          rv$has_classification <- TRUE
        }
      }

      if (is.null(classifications) && isTRUE(config$dashboard_autoclass)) {
        autoclass <- load_dashboard_autoclass(sample_name, roi_dims)
        if (!is.null(autoclass)) {
          classifications <- autoclass
          rv$is_annotation_mode <- FALSE
          rv$has_classification <- TRUE
          mode_message <- "Validation mode (Dashboard autoclass)"
        }
      }

      if (is.null(classifications)) {
        png_files <- list.files(file.path(png_folder, sample_name), pattern = "\\.png$")
        if (length(png_files) == 0) {
          showNotification(paste("No images found for:", sample_name), type = "error")
          return(FALSE)
        }

        roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", png_files))
        if (is.null(roi_dims)) {
          roi_dims <- data.frame(roi_number = roi_numbers, width = NA_real_, height = NA_real_, area = NA_real_)
        }

        classifications <- create_new_classifications(sample_name, roi_dims)
        rv$is_annotation_mode <- TRUE
        rv$has_classification <- FALSE
        mode_message <- "New annotation"
      }

      finalize_sample_load(classifications, sample_name, mode_message)

      if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder) &&
          isTRUE(rv$temp_png_is_managed) &&
          !startsWith(rv$temp_png_folder, cache_dir)) {
        unlink(rv$temp_png_folder, recursive = TRUE)
      }
      rv$temp_png_folder <- png_folder
      rv$temp_png_is_managed <- TRUE

      extracted_folder <- file.path(png_folder, sample_name)
      if (dir.exists(extracted_folder)) {
        extracted_files <- list.files(extracted_folder, pattern = "\\.png$")
        rv$classifications <- rv$classifications[rv$classifications$file_name %in% extracted_files, ]
        rv$original_classifications <- rv$original_classifications[
          rv$original_classifications$file_name %in% extracted_files, ]
      }

      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading sample:", e$message), type = "error")
      return(FALSE)
    })
  }

  # Local mode sample loading
  load_sample_local <- function(sample_name) {
    csv_path <- find_csv_file(sample_name)
    classifier_h5_path <- find_classifier_h5(sample_name)
    classifier_mat_path <- find_classifier_mat(sample_name)
    has_csv <- !is.null(csv_path)
    has_classifier_h5 <- !is.null(classifier_h5_path)
    has_classifier_mat <- !is.null(classifier_mat_path)

    roi_path <- roi_path_map()[[sample_name]]
    if (!is.null(roi_path) && !file.exists(roi_path)) roi_path <- NULL
    sample_png_dir <- find_sample_png_dir(sample_name)
    has_direct_png <- !is.null(sample_png_dir)
    if (is.null(roi_path) && !has_direct_png) {
      showNotification(paste("No ROI file or extracted PNG folder found for:", sample_name), type = "error")
      return(FALSE)
    }
    adc_path <- if (!is.null(roi_path)) sub("\\.roi$", ".adc", roi_path) else NULL

    if (sample_name %in% names(rv$session_cache)) {
      return(load_from_cache(sample_name, roi_path))
    }

    tryCatch({
      annotation_mat_path <- file.path(config$output_folder, paste0(sample_name, ".mat"))
      db_path <- get_db_path(config$db_folder)
      has_db_annotation <- ClassiPyR:::sample_has_annotations_db(db_path, sample_name)
      has_mat_annotation <- file.exists(annotation_mat_path)
      has_existing_annotation <- has_db_annotation || has_mat_annotation
      has_classification <- has_csv || has_classifier_h5 || has_classifier_mat

      rv$has_classification <- has_classification

      mode_message <- NULL
      roi_dims <- NULL

      if (!is.null(adc_path) && file.exists(adc_path)) {
        roi_dims <- read_roi_dimensions(adc_path)
      } else if (has_direct_png) {
        roi_dims <- infer_roi_dims_from_png(sample_name, sample_png_dir)
      }

      if (has_existing_annotation) {
        if (is.null(roi_dims)) {
          showNotification(paste("No ROI dimensions found for:", sample_name), type = "error")
          return(FALSE)
        }
        if (has_db_annotation) {
          classifications <- load_from_db(db_path, sample_name, roi_dims)
        } else {
          classifications <- load_from_mat(annotation_mat_path, sample_name, rv$class2use, roi_dims)
        }
        rv$is_annotation_mode <- TRUE
        mode_message <- if (rv$has_classification) "Manual mode (switch available)" else "Resumed"
      } else if (has_csv) {
        classifications <- load_from_csv(csv_path, use_threshold = config$use_threshold)
        classifications <- apply_roi_dims_to_classifications(classifications, roi_dims)
        rv$is_annotation_mode <- FALSE
        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
        mode_message <- paste0("Validation mode (CSV, ", threshold_text, ")")
      } else if (has_classifier_h5) {
        if (is.null(roi_dims)) {
          showNotification(paste("No ROI dimensions found for:", sample_name), type = "error")
          return(FALSE)
        }
        classifications <- load_from_h5(classifier_h5_path, sample_name, roi_dims, use_threshold = config$use_threshold)
        rv$is_annotation_mode <- FALSE
        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
        mode_message <- paste0("Validation mode (H5, ", threshold_text, ")")
      } else if (has_classifier_mat) {
        if (is.null(roi_dims)) {
          showNotification(paste("No ROI dimensions found for:", sample_name), type = "error")
          return(FALSE)
        }
        classifications <- load_from_classifier_mat(classifier_mat_path, sample_name, rv$class2use, roi_dims, use_threshold = config$use_threshold)
        rv$is_annotation_mode <- FALSE
        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
        mode_message <- paste0("Validation mode (MAT, ", threshold_text, ")")
      } else {
        if (is.null(roi_dims)) {
          showNotification(paste("No ROI dimensions found for:", sample_name), type = "error")
          return(FALSE)
        }
        classifications <- create_new_classifications(sample_name, roi_dims)
        rv$is_annotation_mode <- TRUE
        mode_message <- "New annotation"
      }

      # Suppress finalize's notification here: both branches below notify with
      # the post-filter image count, and two toasts with differing counts is
      # confusing (dashboard loads notify once, via finalize)
      finalize_sample_load(classifications, sample_name, mode_message = NULL)

      if (!is.null(roi_path)) {
        extract_sample_images(sample_name, roi_path, classifications, mode_message = mode_message)
      } else {
        set_active_png_folder(dirname(sample_png_dir), managed = FALSE)
        extracted_files <- list_sample_png_files(sample_name, sample_png_dir)
        rv$classifications <- rv$classifications[rv$classifications$file_name %in% extracted_files, ]
        rv$original_classifications <- rv$original_classifications[
          rv$original_classifications$file_name %in% extracted_files, ]

        update_class_filter()

        if (!is.null(mode_message)) {
          showNotification(paste0(mode_message, ": ", nrow(rv$classifications), " images"), type = "message")
        }
      }

      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading sample:", e$message), type = "error")
      return(FALSE)
    })
  }

  # Load from session cache
  load_from_cache <- function(sample_name, roi_path) {
    is_dashboard <- identical(config$data_source, "dashboard")

    # Validate the image source before mutating any state, so a failed cache
    # load leaves the currently displayed sample intact
    sample_png_dir <- NULL
    if (!is_dashboard) {
      sample_png_dir <- find_sample_png_dir(sample_name)
      if (!(is.null(roi_path) && !is.null(sample_png_dir)) &&
          (is.null(roi_path) || !file.exists(roi_path))) {
        showNotification(paste("ROI file not found for:", sample_name), type = "error")
        return(FALSE)
      }
    }

    cached <- rv$session_cache[[sample_name]]
    rv$classifications <- cached$classifications
    rv$original_classifications <- cached$original_classifications
    rv$changes_log <- cached$changes_log
    # Clear the stale validation snapshot from the previously loaded sample,
    # like finalize_sample_load() does; otherwise switching to validation
    # mode can restore another sample's classifications into this one
    rv$cached_validation_classifications <- NULL
    rv$current_sample <- sample_name
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$is_annotation_mode <- cached$is_annotation_mode
    rv$has_classification <- cached$has_classification %||% FALSE
    # The cached state is what storage holds when its auto-save succeeded
    rv$saved_state <- if (isTRUE(cached$saved)) {
      saved_state_of(sample_name, cached$classifications)
    }

    update_class_filter()

    if (is_dashboard) {
      cache_dir <- get_dashboard_cache_dir()
      png_folder <- file.path(cache_dir, sample_name)

      if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder) &&
          isTRUE(rv$temp_png_is_managed) &&
          !startsWith(rv$temp_png_folder, cache_dir)) {
        unlink(rv$temp_png_folder, recursive = TRUE)
      }
      rv$temp_png_folder <- png_folder
      rv$temp_png_is_managed <- TRUE
    } else {
      if (is.null(roi_path) && !is.null(sample_png_dir)) {
        set_active_png_folder(dirname(sample_png_dir), managed = FALSE)
      } else {
        cleanup_temp_png_folder()

        rv$temp_png_folder <- tempfile(pattern = "ifcb_validator_")
        dir.create(rv$temp_png_folder, recursive = TRUE)
        rv$temp_png_is_managed <- TRUE

        roi_numbers <- as.numeric(gsub(".*_(\\d+)\\.png$", "\\1", rv$classifications$file_name))

        withProgress(message = "Extracting images...", value = 0, {
          incProgress(0.1, detail = "Preparing extraction...")
          ifcb_extract_pngs(
            roi_file = roi_path,
            out_folder = rv$temp_png_folder,
            ROInumbers = roi_numbers,
            verbose = FALSE
          )
          incProgress(0.9, detail = "Extraction complete")
        })
      }
    }

    n_changes <- nrow(rv$changes_log)
    showNotification(paste("Restored from cache:", n_changes, "changes"), type = "message")
    return(TRUE)
  }

  # Extract images from ROI file
  extract_sample_images <- function(sample_name, roi_path, classifications, mode_message = NULL) {
    set_active_png_folder(tempfile(pattern = "ifcb_validator_"), managed = TRUE)
    dir.create(rv$temp_png_folder, recursive = TRUE)

    roi_numbers <- as.numeric(gsub(".*_(\\d+)\\.png$", "\\1", classifications$file_name))

    withProgress(message = "Extracting images...", value = 0, {
      incProgress(0.1, detail = "Preparing extraction...")
      ifcb_extract_pngs(
        roi_file = roi_path,
        out_folder = rv$temp_png_folder,
        ROInumbers = roi_numbers,
        verbose = FALSE
      )
      incProgress(0.9, detail = "Extraction complete")
    })

    extracted_folder <- file.path(rv$temp_png_folder, sample_name)
    if (dir.exists(extracted_folder)) {
      extracted_files <- list.files(extracted_folder, pattern = "\\.png$")
      rv$classifications <- rv$classifications[rv$classifications$file_name %in% extracted_files, ]
      rv$original_classifications <- rv$original_classifications[
        rv$original_classifications$file_name %in% extracted_files, ]

      update_class_filter()
    }

    if (!is.null(mode_message)) {
      actual_count <- nrow(rv$classifications)
      showNotification(paste0(mode_message, ": ", actual_count, " images"), type = "message")
    }
  }

  disable_nav_buttons <- function() {
    shinyjs::disable("load_sample")
    shinyjs::disable("prev_sample")
    shinyjs::disable("next_sample")
    shinyjs::disable("random_sample")
    shinyjs::disable("save_btn")
  }

  enable_nav_buttons <- function() {
    shinyjs::enable("load_sample")
    shinyjs::enable("prev_sample")
    shinyjs::enable("next_sample")
    shinyjs::enable("random_sample")
    shinyjs::enable("save_btn")
  }

  # Load sample button
  observeEvent(input$load_sample, {
    req(input$sample_select, input$sample_select != "")
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })

    rv$class_review_mode <- FALSE
    rv$class_review_source <- "database"
    rv$class_review_class <- NULL
    rv$class_review_samples <- character()
    rv$class_review_original <- NULL
    rv$class_review_external_files <- NULL

    save_to_cache()
    rv$pending_sample_select <- input$sample_select
    load_sample_data(input$sample_select)
  })

  # Reset to home
  observeEvent(input$reset_to_home, {
    if (!is.null(rv$current_sample)) save_to_cache()

    rv$current_sample <- NULL
    rv$classifications <- NULL
    rv$original_classifications <- NULL
    rv$changes_log <- create_empty_changes_log()
    rv$selected_images <- character(0)
    rv$is_annotation_mode <- FALSE
    rv$has_classification <- FALSE

    rv$class_review_mode <- FALSE
    rv$class_review_source <- "database"
    rv$class_review_class <- NULL
    rv$class_review_samples <- character()
    rv$class_review_original <- NULL
    rv$class_review_external_files <- NULL

    updateSelectizeInput(session, "sample_select", selected = "")
    updateRadioButtons(session, "app_mode", selected = "sample")
    shinyjs::runjs("$('.image-card').remove();")
  })

  # Previous sample button
  observeEvent(input$prev_sample, {
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })
    save_to_cache()
    samples <- get_filtered_samples()
    current_idx <- which(samples == rv$current_sample)

    if (length(current_idx) == 0) {
      if (length(samples) > 0) {
        prev_sample <- samples[length(samples)]
        rv$pending_sample_select <- prev_sample
        update_sample_list()
        load_sample_data(prev_sample)
      }
    } else if (current_idx > 1) {
      prev_sample <- samples[current_idx - 1]
      rv$pending_sample_select <- prev_sample
      update_sample_list()
      load_sample_data(prev_sample)
    } else {
      showNotification("Already at first sample", type = "warning")
    }
  })

  # Next sample button
  observeEvent(input$next_sample, {
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })
    save_to_cache()
    samples <- get_filtered_samples()
    current_idx <- which(samples == rv$current_sample)

    if (length(current_idx) == 0) {
      if (length(samples) > 0) {
        next_sample <- samples[1]
        rv$pending_sample_select <- next_sample
        update_sample_list()
        load_sample_data(next_sample)
      }
    } else if (current_idx < length(samples)) {
      next_sample <- samples[current_idx + 1]
      rv$pending_sample_select <- next_sample
      update_sample_list()
      load_sample_data(next_sample)
    } else {
      showNotification("No more samples in list", type = "warning")
    }
  })

  list(
    save_to_cache = save_to_cache,
    mark_current_saved = mark_current_saved,
    find_adc_folder = find_adc_folder,
    load_dashboard_autoclass = load_dashboard_autoclass,
    load_from_cache = load_from_cache,
    disable_nav_buttons = disable_nav_buttons,
    enable_nav_buttons = enable_nav_buttons,
    find_sample_png_dir = find_sample_png_dir
  )
}
