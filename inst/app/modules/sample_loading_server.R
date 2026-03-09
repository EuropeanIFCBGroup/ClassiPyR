# Load samples from various sources, navigation, caching

setup_sample_loading_server <- function(input, output, session, rv, config,
                                        roi_path_map, png_sample_path_map,
                                        csv_path_map, classifier_mat_files,
                                        classifier_h5_files, annotated_samples,
                                        classified_samples,
                                        get_filtered_samples,
                                        update_current_sample_status_fn) {
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
    png_files <- list_sample_png_files(sample_name, sample_png_dir)
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

  apply_roi_dims_to_classifications <- function(classifications, roi_dims) {
    if (is.null(classifications) || is.null(roi_dims) || nrow(classifications) == 0) {
      return(classifications)
    }
    roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", classifications$file_name))
    idx <- match(roi_numbers, roi_dims$roi_number)
    classifications$width <- ifelse(!is.na(idx), roi_dims$width[idx], NA_real_)
    classifications$height <- ifelse(!is.na(idx), roi_dims$height[idx], NA_real_)
    classifications$roi_area <- ifelse(!is.na(idx), roi_dims$area[idx], NA_real_)
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

  # Save current sample to cache with LRU eviction
  save_to_cache <- function() {
    if (!is.null(rv$current_sample) && !is.null(rv$classifications)) {
      if (length(rv$session_cache) >= MAX_CACHED_SAMPLES &&
          !(rv$current_sample %in% names(rv$session_cache))) {
        oldest_sample <- names(rv$session_cache)[1]
        rv$session_cache[[oldest_sample]] <- NULL
      }

      rv$session_cache[[rv$current_sample]] <- list(
        classifications = rv$classifications,
        original_classifications = rv$original_classifications,
        changes_log = rv$changes_log,
        is_annotation_mode = rv$is_annotation_mode,
        has_classification = rv$has_classification
      )

      tryCatch({
        roi_path_for_save <- roi_path_map()[[rv$current_sample]]
        sample_png_dir_for_save <- find_sample_png_dir(rv$current_sample)
        adc_folder_for_save <- if (!is.null(roi_path_for_save)) {
          dirname(roi_path_for_save)
        } else if (!is.null(sample_png_dir_for_save)) {
          sample_png_dir_for_save
        } else {
          NULL
        }

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
          annotator = input$annotator_name,
          adc_folder = adc_folder_for_save,
          save_format = save_fmt_for_autosave,
          db_folder = config$db_folder,
          export_statistics = config$export_statistics
        )
        if (isTRUE(saved)) {
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

    available_classes <- sort(unique(classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)))

    if (!is.null(mode_message)) {
      actual_count <- nrow(rv$classifications)
      showNotification(paste0(mode_message, ": ", actual_count, " images"), type = "message")
    }
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
      has_db_annotation <- sample_name %in% list_annotated_samples_db(db_path)

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
        rv$has_classification <- isTRUE(config$dashboard_autoclass)
        mode_message <- if (rv$has_classification) "Manual mode (switch available)" else "Resumed"
      }

      if (is.null(classifications) && !isTRUE(config$dashboard_autoclass)) {
        csv_folder <- config$csv_folder
        has_csv_folder <- !is.null(csv_folder) && nzchar(csv_folder) && dir.exists(csv_folder)

        if (has_csv_folder) {
          local_csv <- list.files(csv_folder, pattern = paste0("^", sample_name, "\\.csv$"),
                                  recursive = TRUE, full.names = TRUE)
          local_h5 <- list.files(csv_folder, pattern = paste0("^", sample_name, ".*\\.h5$"),
                                 recursive = TRUE, full.names = TRUE)
          local_mat <- list.files(csv_folder, pattern = paste0("^", sample_name, ".*\\.mat$"),
                                  recursive = TRUE, full.names = TRUE)

          if (length(local_csv) > 0) {
            classifications <- load_from_csv(local_csv[1], use_threshold = config$use_threshold)
            rv$is_annotation_mode <- FALSE
            rv$has_classification <- TRUE
            threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
            mode_message <- paste0("Validation mode (Local CSV, ", threshold_text, ")")
          } else if (length(local_h5) > 0) {
            classifications <- load_from_h5(local_h5[1], sample_name, roi_dims, use_threshold = config$use_threshold)
            rv$is_annotation_mode <- FALSE
            rv$has_classification <- TRUE
            threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
            mode_message <- paste0("Validation mode (Local H5, ", threshold_text, ")")
          } else if (length(local_mat) > 0) {
            classifications <- load_from_classifier_mat(local_mat[1], sample_name, rv$class2use, roi_dims, use_threshold = config$use_threshold)
            rv$is_annotation_mode <- FALSE
            rv$has_classification <- TRUE
            threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
            mode_message <- paste0("Validation mode (Local MAT, ", threshold_text, ")")
          }
        }
      }

      if (is.null(classifications) && isTRUE(config$dashboard_autoclass)) {
        autoclass_warning <- NULL
        autoclass <- withProgress(message = "Downloading auto-classifications...", value = 0, {
          incProgress(0.1, detail = "Requesting classifier output...")
          out <- tryCatch(
            withCallingHandlers(
              download_dashboard_autoclass(parsed$base_url, sample_name, cache_dir,
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

        if ((is.null(autoclass) || nrow(autoclass) == 0) && !is.null(autoclass_warning)) {
          showNotification(
            paste("No auto-classifications available for this sample on the dashboard."),
            type = "warning", duration = 6
          )
        }

        if (!is.null(autoclass) && nrow(autoclass) > 0) {
          if (!is.null(roi_dims)) {
            roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", autoclass$file_name))
            dim_data <- lapply(roi_numbers, function(rn) {
              idx <- which(roi_dims$roi_number == rn)
              if (length(idx) > 0) {
                list(width = roi_dims$width[idx], height = roi_dims$height[idx], area = roi_dims$area[idx])
              } else {
                list(width = NA_real_, height = NA_real_, area = NA_real_)
              }
            })
            autoclass$width <- vapply(dim_data, `[[`, numeric(1), "width")
            autoclass$height <- vapply(dim_data, `[[`, numeric(1), "height")
            autoclass$roi_area <- vapply(dim_data, `[[`, numeric(1), "area")
          } else {
            autoclass$width <- NA_real_
            autoclass$height <- NA_real_
            autoclass$roi_area <- NA_real_
          }

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
      has_db_annotation <- sample_name %in% list_annotated_samples_db(db_path)
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

      finalize_sample_load(classifications, sample_name, mode_message)

      if (!is.null(roi_path)) {
        extract_sample_images(sample_name, roi_path, classifications, mode_message = mode_message)
      } else {
        set_active_png_folder(dirname(sample_png_dir), managed = FALSE)
        extracted_files <- list_sample_png_files(sample_name, sample_png_dir)
        rv$classifications <- rv$classifications[rv$classifications$file_name %in% extracted_files, ]
        rv$original_classifications <- rv$original_classifications[
          rv$original_classifications$file_name %in% extracted_files, ]

        available_classes <- sort(unique(rv$classifications$class_name))
        unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
        display_names <- sapply(available_classes, function(cls) {
          if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
        })
        updateSelectInput(session, "class_filter",
                          choices = c("All" = "all", setNames(available_classes, display_names)))

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
    cached <- rv$session_cache[[sample_name]]
    rv$classifications <- cached$classifications
    rv$original_classifications <- cached$original_classifications
    rv$changes_log <- cached$changes_log
    rv$current_sample <- sample_name
    rv$selected_images <- character()
    rv$is_annotation_mode <- cached$is_annotation_mode
    rv$has_classification <- cached$has_classification %||% FALSE

    available_classes <- sort(unique(rv$classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)))

    is_dashboard <- identical(config$data_source, "dashboard")

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
      sample_png_dir <- find_sample_png_dir(sample_name)
      if (is.null(roi_path) && !is.null(sample_png_dir)) {
        set_active_png_folder(dirname(sample_png_dir), managed = FALSE)
      } else {
        if (is.null(roi_path) || !file.exists(roi_path)) {
          showNotification(paste("ROI file not found for:", sample_name), type = "error")
          return(FALSE)
        }
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

      available_classes <- sort(unique(rv$classifications$class_name))
      unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
      display_names <- sapply(available_classes, function(cls) {
        if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
      })
      updateSelectInput(session, "class_filter",
                        choices = c("All" = "all", setNames(available_classes, display_names)))
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
        updateSelectizeInput(session, "sample_select", selected = prev_sample)
        load_sample_data(prev_sample)
      }
    } else if (current_idx > 1) {
      prev_sample <- samples[current_idx - 1]
      rv$pending_sample_select <- prev_sample
      updateSelectizeInput(session, "sample_select", selected = prev_sample)
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
        updateSelectizeInput(session, "sample_select", selected = next_sample)
        load_sample_data(next_sample)
      }
    } else if (current_idx < length(samples)) {
      next_sample <- samples[current_idx + 1]
      rv$pending_sample_select <- next_sample
      updateSelectizeInput(session, "sample_select", selected = next_sample)
      load_sample_data(next_sample)
    } else {
      showNotification("No more samples in list", type = "warning")
    }
  })

  list(
    save_to_cache = save_to_cache,
    load_from_cache = load_from_cache,
    disable_nav_buttons = disable_nav_buttons,
    enable_nav_buttons = enable_nav_buttons
  )
}
