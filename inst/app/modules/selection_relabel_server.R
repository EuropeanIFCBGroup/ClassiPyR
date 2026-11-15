# Image selection, relabeling

setup_selection_relabel_server <- function(input, output, session, rv,
                                           filtered_images, paginated_images) {
  # The gallery renderUI isolates rv$selected_images (a selection change must
  # not re-render every card), so server-initiated selection changes have to
  # push their styling to the client explicitly
  sync_selection_to_client <- function() {
    session$sendCustomMessage("setSelectedCards",
                              list(images = as.list(rv$selected_images)))
  }

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
    sync_selection_to_client()
  })

  observeEvent(input$deselect_all, {
    rv$selected_images <- character()
    rv$select_all_state <- "first"  # Reset select_all state
    sync_selection_to_client()
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

    # All selected images are relabeled in one vectorised step. (One
    # which() and one rbind() per image made "Select All" + Relabel on a
    # large sample take many seconds.)
    updated_classifications <- rv$classifications
    idx <- match(unique(rv$selected_images), updated_classifications$file_name)
    idx <- idx[!is.na(idx)]
    idx <- idx[updated_classifications$class_name[idx] != new_class]
    relabeled_count <- length(idx)

    if (relabeled_count > 0) {
      new_changes <- data.frame(
        image = updated_classifications$file_name[idx],
        original_class = updated_classifications$class_name[idx],
        new_class = new_class,
        stringsAsFactors = FALSE
      )
      updated_classifications$class_name[idx] <- new_class

      # Single assignment to reactive values
      rv$classifications <- updated_classifications
      rv$changes_log <- rbind(rv$changes_log, new_changes)
    }

    available_classes <- sort(unique(rv$classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices(available_classes, unmatched),
                      selected = input$class_filter)

    showNotification(paste("Relabeled", relabeled_count, "images to", new_class), type = "message")
    rv$selected_images <- character()
    # A relabel that changed something re-renders the gallery, which picks up
    # the cleared selection; one that changed nothing (images already in that
    # class) does not, so the cards are deselected explicitly
    if (relabeled_count == 0) sync_selection_to_client()
  }

  observeEvent(input$relabel_quick, {
    do_relabel(input$new_class_quick)
  })
}
