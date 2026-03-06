# Startup class loading, class2use file upload

setup_class_list_loading_server <- function(input, output, session, rv, config,
                                            saved_settings, persist_settings,
                                            update_month_choices, update_sample_list) {
  # Try to load class2use on startup: SQLite first, then file fallback
  observe({
    if (!is.null(rv$class2use_path)) return()

    if (grepl("sqlite", config$save_format, fixed = TRUE)) {
      db_path <- get_db_path(config$db_folder)
      if (file.exists(db_path)) {
        db_classes <- load_global_class_list_db(db_path)
        if (!is.null(db_classes) && length(db_classes) > 0) {
          if (!"unclassified" %in% db_classes) {
            db_classes <- c("unclassified", db_classes)
          }
          rv$class2use <- db_classes

          sorted_classes <- sort(rv$class2use)
          updateSelectizeInput(session, "new_class_quick",
                               choices = sorted_classes,
                               selected = character(0))

          showNotification(
            paste("Restored", length(rv$class2use), "classes from database"),
            type = "message"
          )
          return()
        }
      }
    }

    class2use_path <- saved_settings$class2use_path
    if (is.null(class2use_path) || length(class2use_path) != 1 ||
        isTRUE(is.na(class2use_path)) || !nzchar(class2use_path)) {
      return()
    }

    path_to_try <- class2use_path
    if (file.exists(path_to_try)) {
      tryCatch({
        classes <- load_class_list(path_to_try)

        if (!"unclassified" %in% classes) {
          classes <- c("unclassified", classes)
        }

        rv$class2use <- classes
        rv$class2use_path <- path_to_try

        sorted_classes <- sort(rv$class2use)
        updateSelectizeInput(session, "new_class_quick",
                             choices = sorted_classes,
                             selected = character(0))

        showNotification(
          paste("Auto-loaded", length(rv$class2use), "classes from", basename(path_to_try)),
          type = "message"
        )
      }, error = function(e) {
        message("Could not load class list from saved path: ", e$message)
      })
    }
  })

  # Auto-save class list to SQLite whenever it changes
  observeEvent(rv$class2use, {
    if (!grepl("sqlite", config$save_format, fixed = TRUE)) return()
    classes <- rv$class2use
    if (is.null(classes) || length(classes) == 0) return()
    if (length(classes) == 1 && classes == "unclassified") return()

    db_path <- get_db_path(config$db_folder)
    save_global_class_list_db(db_path, classes)
  })

  # Load uploaded class2use file (from settings modal)
  observeEvent(input$class2use_file, {
    req(input$class2use_file)

    tryCatch({
      classes <- load_class_list(input$class2use_file$datapath)

      if (!"unclassified" %in% classes) {
        classes <- c("unclassified", classes)
      }

      rv$class2use <- classes

      ext <- tools::file_ext(input$class2use_file$name)
      persistent_path <- file.path(get_config_dir(), paste0("class2use_saved.", ext))
      file.copy(input$class2use_file$datapath, persistent_path, overwrite = TRUE)
      rv$class2use_path <- persistent_path

      persist_settings(list(
        csv_folder = config$csv_folder,
        roi_folder = config$roi_folder,
        output_folder = config$output_folder,
        png_output_folder = config$png_output_folder,
        db_folder = config$db_folder,
        use_threshold = config$use_threshold,
        class2use_path = persistent_path,
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

      sorted_classes <- sort(rv$class2use)
      updateSelectizeInput(session, "new_class_quick",
                           choices = sorted_classes,
                           selected = character(0))

      showNotification(paste("Loaded", length(rv$class2use), "classes"), type = "message")

      update_month_choices()
      update_sample_list()
    }, error = function(e) {
      showNotification(paste("Error loading class list:", e$message), type = "error")
    })
  })
}
