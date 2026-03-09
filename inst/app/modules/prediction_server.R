# Gradio live prediction

setup_prediction_server <- function(input, output, session, rv, config,
                                    build_class_filter_choices_fn) {
  output$predict_btn_ui <- renderUI({
    has_config <- nzchar(config$gradio_url) && nzchar(config$prediction_model)
    has_sample <- !is.null(rv$current_sample)
    is_enabled <- has_config && has_sample

    btn <- actionButton("predict_btn", "Predict",
                        icon = icon("robot"),
                        class = if (is_enabled) "btn-info" else "btn-outline-secondary",
                        width = "100%",
                        disabled = if (!is_enabled) "disabled" else NULL)

    if (!is_enabled) {
      # Wrap in a clickable div so we can intercept clicks on the disabled button
      tags$div(
        id = "predict_btn_wrapper",
        style = "cursor: pointer;",
        onclick = paste0(
          "Shiny.setInputValue('predict_btn_disabled_click',",
          " Math.random());"
        ),
        btn
      )
    } else {
      btn
    }
  })

  observeEvent(input$predict_btn_disabled_click, {
    has_config <- nzchar(config$gradio_url) && nzchar(config$prediction_model)
    if (!has_config) {
      showNotification(
        "Set a Gradio API URL and model name in Settings to enable predictions.",
        type = "warning",
        duration = 5
      )
    }
  })

  observeEvent(input$predict_btn, {
    req(rv$current_sample, rv$temp_png_folder, config$gradio_url, config$prediction_model)

    if (!grepl("^https?://", config$gradio_url)) {
      showNotification("Gradio URL must start with http:// or https://", type = "error")
      return()
    }

    png_folder <- file.path(rv$temp_png_folder, rv$current_sample)
    if (!dir.exists(png_folder)) {
      png_folder <- rv$temp_png_folder
    }
    png_files <- list.files(png_folder, pattern = "\\.png$", full.names = TRUE)
    if (length(png_files) == 0) {
      showNotification("No PNG images found in the loaded sample.", type = "warning")
      return()
    }

    manually_changed <- character()
    if (!is.null(rv$original_classifications) && !is.null(rv$classifications)) {
      merged <- merge(
        rv$classifications[, c("file_name", "class_name")],
        rv$original_classifications[, c("file_name", "class_name")],
        by = "file_name", suffixes = c("_current", "_original")
      )
      changed_rows <- merged$class_name_current != merged$class_name_original
      manually_changed <- merged$file_name[changed_rows]
    }

    all_filenames <- basename(png_files)
    files_to_predict <- png_files[!all_filenames %in% manually_changed]

    if (length(files_to_predict) == 0) {
      showNotification("All images have been manually reclassified. Nothing to predict.",
                       type = "message")
      return()
    }

    n_total <- length(files_to_predict)
    result_list <- vector("list", n_total)
    failed <- 0L

    withProgress(message = "Classifying images...", value = 0, {
      for (i in seq_len(n_total)) {
        setProgress(value = (i - 1) / n_total,
                    detail = paste0(i, " / ", n_total))
        tryCatch({
          result_list[[i]] <- iRfcb::ifcb_classify_images(
            png_file = files_to_predict[i],
            gradio_url = config$gradio_url,
            model_name = config$prediction_model,
            verbose = FALSE
          )
        }, error = function(e) {
          failed <<- failed + 1L
        })
      }
      setProgress(1, detail = "Done")
    })

    predictions <- do.call(rbind, Filter(Negate(is.null), result_list))

    if (is.null(predictions) || nrow(predictions) == 0) {
      showNotification("Prediction failed for all images.", type = "error")
      return()
    }
    if (failed > 0) {
      showNotification(paste("Warning:", failed, "images failed to classify."),
                       type = "warning")
    }

    if (!config$use_threshold && "class_name_auto" %in% names(predictions)) {
      predictions$class_name <- predictions$class_name_auto
    }

    cls <- rv$classifications
    for (i in seq_len(nrow(predictions))) {
      fname <- predictions$file_name[i]
      idx <- which(cls$file_name == fname)
      if (length(idx) == 1) {
        cls$class_name[idx] <- predictions$class_name[i]
        if ("score" %in% names(cls)) {
          cls$score[idx] <- predictions$score[i]
        }
      }
    }
    rv$classifications <- cls

    rv$original_classifications <- rv$classifications
    rv$is_annotation_mode <- FALSE

    new_classes <- setdiff(unique(predictions$class_name), rv$class2use)
    new_classes <- new_classes[!is.na(new_classes) & nzchar(new_classes)]
    if (length(new_classes) > 0) {
      rv$class2use <- c(rv$class2use, new_classes)
      sorted_classes <- sort(rv$class2use)
      updateSelectizeInput(session, "new_class_quick",
                           choices = sorted_classes,
                           selected = character(0))
    }

    available_classes <- sort(unique(rv$classifications$class_name))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices_fn(available_classes),
                      selected = "all")

    n_predicted <- nrow(predictions)
    n_skipped <- length(manually_changed)
    msg <- paste0("Predicted ", n_predicted, " images.")
    if (n_skipped > 0) {
      msg <- paste0(msg, " Skipped ", n_skipped, " manually reclassified images.")
    }
    showNotification(msg, type = "message", duration = 8)
  })
}
