# Folder scanning, filter dropdowns, sample list

setup_sample_discovery_server <- function(input, output, session, rv, config,
                                          all_samples, classified_samples,
                                          annotated_samples, roi_path_map,
                                          png_sample_path_map, csv_path_map,
                                          classifier_mat_files, classifier_h5_files,
                                          rescan_trigger, last_sync_time) {
  # Helper: populate reactive values from file index data
  populate_from_index <- function(index_data) {
    sample_names <- as.character(index_data$sample_names)
    if (length(sample_names) == 0) return(FALSE)

    safe_char <- function(x) as.character(if (is.null(x)) character() else x)
    safe_list <- function(x) as.list(if (is.null(x)) list() else x)

    all_samples(sample_names)
    classified_samples(safe_char(index_data$classified_samples))
    annotated_samples(safe_char(index_data$annotated_samples))
    roi_path_map(safe_list(index_data$roi_path_map))
    png_sample_path_map(safe_list(index_data$png_sample_path_map))
    csv_path_map(safe_list(index_data$csv_path_map))
    classifier_mat_files(safe_list(index_data$classifier_mat_files))
    classifier_h5_files(safe_list(index_data$classifier_h5_files))

    years <- unique(substr(sample_names, 2, 5))
    years <- sort(years)
    first_year <- if (length(years) > 0) years[1] else "all"
    updateSelectInput(session, "year_select",
                      choices = c("All" = "all", setNames(years, years)),
                      selected = first_year)

    instruments <- unique(sub(".*_", "", sample_names))
    instruments <- sort(instruments)
    updateSelectInput(session, "instrument_select",
                      choices = c("All" = "all", setNames(instruments, instruments)),
                      selected = "all")

    last_sync_time(index_data$timestamp)
    TRUE
  }

  # Scan for available ROI files and classification files
  observe({
    rescan_trigger()
    data_source <- config$data_source

    if (identical(data_source, "dashboard")) {
      dashboard_url <- config$dashboard_url
      if (is.null(dashboard_url) || !nzchar(dashboard_url)) return()

      cached <- load_file_index()
      cache_valid <- !is.null(cached) &&
        identical(cached$data_source, "dashboard") &&
        identical(cached$dashboard_url, dashboard_url) &&
        identical(cached$csv_folder, config$csv_folder) &&
        identical(cached$dashboard_autoclass, isTRUE(config$dashboard_autoclass))

      if (cache_valid) {
        populate_from_index(cached)
        return()
      }

      auto_sync <- config$auto_sync
      if (!isTRUE(auto_sync) && !is.null(cached) &&
          identical(cached$data_source, "dashboard")) {
        populate_from_index(cached)
        return()
      }

      withProgress(message = "Fetching dashboard samples...", value = 0, {
        result <- rescan_file_index(
          data_source = "dashboard",
          dashboard_url = dashboard_url,
          csv_folder = config$csv_folder,
          db_folder = config$db_folder,
          dashboard_autoclass = isTRUE(config$dashboard_autoclass),
          verbose = FALSE,
          progress = function(value = NULL, detail = NULL) {
            if (!is.null(value)) {
              shiny::setProgress(value = value, detail = detail)
            } else if (!is.null(detail)) {
              shiny::setProgress(detail = detail)
            }
          }
        )
      })

      if (!is.null(result)) populate_from_index(result)
    } else {
      roi_folder <- config$roi_folder
      csv_folder <- config$csv_folder
      output_folder <- config$output_folder

      roi_valid <- !is.null(roi_folder) && length(roi_folder) == 1 && !isTRUE(is.na(roi_folder)) && nzchar(roi_folder) && dir.exists(roi_folder)
      if (!roi_valid) return()

      cached <- load_file_index()
      cache_valid <- !is.null(cached) &&
        identical(cached$roi_folder, roi_folder) &&
        identical(cached$csv_folder, csv_folder) &&
        identical(cached$output_folder, output_folder)

      if (cache_valid) {
        populate_from_index(cached)
        return()
      }

      auto_sync <- config$auto_sync
      if (!isTRUE(auto_sync) && !is.null(cached)) {
        populate_from_index(cached)
        return()
      }

      withProgress(message = "Syncing folders...", value = 0, {
        result <- rescan_file_index(
          roi_folder = roi_folder,
          csv_folder = csv_folder,
          output_folder = output_folder,
          verbose = FALSE,
          progress = function(value = NULL, detail = NULL) {
            if (!is.null(value)) {
              shiny::setProgress(value = value, detail = detail)
            } else if (!is.null(detail)) {
              shiny::setProgress(detail = detail)
            }
          }
        )
      })

      if (!is.null(result)) populate_from_index(result)
    }
  })

  # Update cache when annotations are saved
  observe({
    annotated <- annotated_samples()
    cached <- load_file_index()
    if (!is.null(cached) && !identical(as.character(cached$annotated_samples), annotated)) {
      cached$annotated_samples <- annotated
      cached$timestamp <- as.character(Sys.time())
      save_file_index(cached)
    }
  })

  # Rescan button
  observeEvent(input$rescan_folders, {
    cache_path <- get_file_index_path()
    if (file.exists(cache_path)) file.remove(cache_path)
    rescan_trigger(rescan_trigger() + 1)
  })

  # Helper: update month and instrument choices
  update_month_choices <- function() {
    samples <- all_samples()
    if (length(samples) == 0) return()

    year_val <- input$year_select

    if (!is.null(year_val) && year_val != "all") {
      year_pattern <- paste0("^D", year_val)
      year_samples <- samples[grepl(year_pattern, samples)]

      months <- unique(substr(year_samples, 6, 7))
      months <- sort(months)
      month_labels <- MONTH_NAMES[months]

      first_month <- if (length(months) > 0) months[1] else "all"
      updateSelectInput(session, "month_select",
                        choices = c("All" = "all", setNames(months, month_labels)),
                        selected = first_month)

      instruments <- unique(sub(".*_", "", year_samples))
      instruments <- sort(instruments)
      current_instrument <- input$instrument_select
      selected_instrument <- if (!is.null(current_instrument) && current_instrument %in% instruments) {
        current_instrument
      } else {
        "all"
      }
      updateSelectInput(session, "instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = selected_instrument)
    } else {
      updateSelectInput(session, "month_select",
                        choices = c("All" = "all"),
                        selected = "all")

      instruments <- unique(sub(".*_", "", samples))
      instruments <- sort(instruments)
      updateSelectInput(session, "instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = "all")
    }
  }

  # Helper: update sample list based on filters
  update_sample_list <- function() {
    samples <- all_samples()
    if (length(samples) == 0) return()

    year_val <- input$year_select
    month_val <- input$month_select
    status_val <- input$sample_status_filter
    classified <- classified_samples()
    annotated <- annotated_samples()

    if (!is.null(year_val) && year_val != "all") {
      year_pattern <- paste0("^D", year_val)
      samples <- samples[grepl(year_pattern, samples)]
    }

    if (!is.null(month_val) && month_val != "all") {
      month_pattern <- paste0("^D\\d{4}", month_val)
      samples <- samples[grepl(month_pattern, samples)]
    }

    instrument_val <- input$instrument_select
    if (!is.null(instrument_val) && instrument_val != "all") {
      instrument_pattern <- paste0("_", instrument_val, "$")
      samples <- samples[grepl(instrument_pattern, samples)]
    }

    if (!is.null(status_val)) {
      if (status_val == "classified") {
        samples <- samples[samples %in% classified & !samples %in% annotated]
      } else if (status_val == "unclassified") {
        samples <- samples[!samples %in% classified & !samples %in% annotated]
      } else if (status_val == "annotated") {
        samples <- samples[samples %in% annotated]
      }
    }

    samples <- sort(samples)

    if (length(samples) > 0) {
      display_names <- sapply(samples, function(s) {
        has_manual <- s %in% annotated
        has_classified <- s %in% classified
        if (has_manual && has_classified) {
          paste0(s, "\u270E\u2713")
        } else if (has_manual) {
          paste0(s, "\u270E")
        } else if (has_classified) {
          paste0(s, "\u2713")
        } else {
          paste0(s, "*")
        }
      })
      choices <- setNames(samples, display_names)
    } else {
      choices <- character(0)
    }

    current_selection <- if (!is.null(rv$pending_sample_select)) {
      rv$pending_sample_select
    } else {
      rv$current_sample
    }

    selected_value <- if (!is.null(current_selection) && current_selection %in% samples) {
      current_selection
    } else {
      character(0)
    }

    rv$pending_sample_select <- NULL

    updateSelectizeInput(session, "sample_select", choices = choices,
                         selected = selected_value,
                         options = list(placeholder = "Select sample..."),
                         server = TRUE)
  }

  # Filter change observers
  observeEvent(input$year_select, {
    update_month_choices()
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$month_select, {
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$instrument_select, {
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$sample_status_filter, {
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(all_samples(), {
    update_month_choices()
    update_sample_list()
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  # Helper: get filtered sample list
  get_filtered_samples <- function() {
    samples <- all_samples()
    classified <- classified_samples()
    annotated <- annotated_samples()

    if (!is.null(input$year_select) && input$year_select != "all") {
      year_pattern <- paste0("^D", input$year_select)
      samples <- samples[grepl(year_pattern, samples)]
    }

    if (!is.null(input$month_select) && input$month_select != "all") {
      month_pattern <- paste0("^D\\d{4}", input$month_select)
      samples <- samples[grepl(month_pattern, samples)]
    }

    if (!is.null(input$instrument_select) && input$instrument_select != "all") {
      instrument_pattern <- paste0("_", input$instrument_select, "$")
      samples <- samples[grepl(instrument_pattern, samples)]
    }

    if (!is.null(input$sample_status_filter)) {
      if (input$sample_status_filter == "classified") {
        samples <- samples[samples %in% classified & !samples %in% annotated]
      } else if (input$sample_status_filter == "unclassified") {
        samples <- samples[!samples %in% classified & !samples %in% annotated]
      } else if (input$sample_status_filter == "annotated") {
        samples <- samples[samples %in% annotated]
      }
    }

    sort(samples)
  }

  # Random sample selection
  observeEvent(input$random_sample, {
    samples <- get_filtered_samples()
    if (length(samples) > 0) {
      random_sample <- sample(samples, 1)
      rv$pending_sample_select <- random_sample
      updateSelectizeInput(session, "sample_select", selected = random_sample)
    } else {
      showNotification("No samples match current filters", type = "warning")
    }
  })

  list(
    update_month_choices = update_month_choices,
    update_sample_list = update_sample_list,
    get_filtered_samples = get_filtered_samples
  )
}
