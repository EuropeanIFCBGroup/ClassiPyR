# Image selection, relabeling

setup_selection_relabel_server <- function(input, output, session, rv,
                                           filtered_images, paginated_images) {
  observeEvent(input$toggle_image, {
    img <- input$toggle_image$img
    if (img %in% rv$selected_images) {
      rv$selected_images <- setdiff(rv$selected_images, img)
    } else {
      rv$selected_images <- c(rv$selected_images, img)
    }
  })

  observeEvent(input$drag_select, {
    imgs <- input$drag_select$images
    rv$selected_images <- unique(c(rv$selected_images, imgs))
  })

  observeEvent(input$select_all, {
    req(filtered_images(), paginated_images())

    if (rv$select_all_state == "first") {
      # "Select Page" button: select only images on current page (replaces selection)
      current_page_images <- paginated_images()$images$file_name
      rv$selected_images <- current_page_images
      # Change state to "second" for next click
      rv$select_all_state <- "second"
    } else {
      # "Select All" button: select all images across all pages (replaces selection)
      rv$selected_images <- filtered_images()$file_name
      # Toggle back to "first" so next click selects page only
      rv$select_all_state <- "first"
    }
  })

  observeEvent(input$deselect_all, {
    rv$selected_images <- character()
    rv$select_all_state <- "first"  # Reset select_all state
  })

  # Reset select_all_state when classifications change (new sample loaded)
  observeEvent(rv$classifications, {
    rv$select_all_state <- "first"
  }, priority = -1)  # Low priority so selection state is reset after classifications are loaded

  # Update Select All button text based on state
  observeEvent(rv$select_all_state, {
    if (rv$select_all_state == "first") {
      shinyjs::html("select_all", "Select Page")
    } else {
      shinyjs::html("select_all", "Select All")
    }
  })

  # Measure tool toggle
  observeEvent(input$measure_toggle, {
    rv$measure_mode <- !rv$measure_mode
    # Update button style via JavaScript
    if (rv$measure_mode) {
      shinyjs::addClass("measure_toggle", "btn-primary")
      shinyjs::removeClass("measure_toggle", "btn-outline-secondary")
      showNotification("Measure mode ON - Click and drag on images to measure", type = "message", duration = 3)
    } else {
      shinyjs::removeClass("measure_toggle", "btn-primary")
      shinyjs::addClass("measure_toggle", "btn-outline-secondary")
    }
    # Send measure mode state to JavaScript
    session$sendCustomMessage("measureMode", rv$measure_mode)
  })

  output$selected_count_inline <- renderText({
    n <- length(rv$selected_images)
    if (n > 0) paste0("(", n, " selected)")
  })

  # Relabel function (uses immutable pattern)
  do_relabel <- function(new_class) {
    req(rv$classifications)
    req(length(rv$selected_images) > 0)
    req(new_class, new_class != "")

    # Work with copies to avoid mutation issues with reactivity
    updated_classifications <- rv$classifications
    updated_changes_log <- rv$changes_log
    relabeled_count <- 0

    for (img in rv$selected_images) {
      idx <- which(updated_classifications$file_name == img)
      if (length(idx) > 0) {
        old_class <- updated_classifications$class_name[idx]

        if (old_class != new_class) {
          updated_changes_log <- rbind(updated_changes_log, data.frame(
            image = img,
            original_class = old_class,
            new_class = new_class,
            stringsAsFactors = FALSE
          ))

          updated_classifications$class_name[idx] <- new_class
          relabeled_count <- relabeled_count + 1
        }
      }
    }

    # Single assignment to reactive values
    rv$classifications <- updated_classifications
    rv$changes_log <- updated_changes_log

    available_classes <- sort(unique(rv$classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)),
                      selected = input$class_filter)

    showNotification(paste("Relabeled", relabeled_count, "images to", new_class), type = "message")
    rv$selected_images <- character()
  }

  observeEvent(input$relabel_quick, {
    do_relabel(input$new_class_quick)
  })
}
