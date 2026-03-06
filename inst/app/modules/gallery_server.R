# Image gallery rendering, pagination, filtering

setup_gallery_server <- function(input, output, session, rv) {
  # Register temp folder as session-specific resource path
  observe({
    req(rv$temp_png_folder)
    if (dir.exists(rv$temp_png_folder)) {
      # Use session token for unique path to prevent cross-session data access
      path_name <- paste0("temp_images_", session$token)
      addResourcePath(path_name, rv$temp_png_folder)
      rv$resource_path_name <- path_name
    }
  })

  # Filter images by class (sorted appropriately for current mode)
  filtered_images <- reactive({
    req(rv$classifications)

    df <- rv$classifications

    if (input$class_filter == "all") {
      if (rv$is_annotation_mode) {
        # In annotation mode: sort unclassified by width (widest first),
        # then classified by class name
        unclassified <- df %>%
          filter(class_name == "unclassified") %>%
          arrange(desc(width))

        classified <- df %>%
          filter(class_name != "unclassified") %>%
          arrange(class_name, file_name)

        bind_rows(unclassified, classified)
      } else {
        # Validation mode: sort by class name, then file name
        df %>% arrange(class_name, file_name)
      }
    } else {
      # Single class filter
      filtered <- df %>% filter(class_name == input$class_filter)

      if (rv$is_annotation_mode) {
        # Sort by width in annotation mode
        filtered %>% arrange(desc(width))
      } else {
        filtered %>% arrange(file_name)
      }
    }
  })

  # Pagination
  paginated_images <- reactive({
    req(filtered_images())

    images <- filtered_images()
    per_page <- as.numeric(input$images_per_page)
    if (is.null(per_page)) per_page <- 100

    total_pages <- ceiling(nrow(images) / per_page)
    current_page <- min(rv$current_page, max(1, total_pages))

    start_idx <- (current_page - 1) * per_page + 1
    end_idx <- min(current_page * per_page, nrow(images))

    list(
      images = images[start_idx:end_idx, , drop = FALSE],
      current_page = current_page,
      total_pages = total_pages,
      total_images = nrow(images),
      start_idx = start_idx,
      end_idx = end_idx
    )
  })

  output$page_info <- renderText({
    req(paginated_images())
    p <- paginated_images()
    sprintf("Page %d/%d (%d-%d of %d)",
            p$current_page, max(1, p$total_pages),
            p$start_idx, p$end_idx, p$total_images)
  })

  observeEvent(input$prev_page, {
    if (rv$current_page > 1) {
      rv$current_page <- rv$current_page - 1
      rv$select_all_state <- "first"  # Reset select_all state when navigating
    }
  })

  observeEvent(input$next_page, {
    req(paginated_images())
    if (rv$current_page < paginated_images()$total_pages) {
      rv$current_page <- rv$current_page + 1
      rv$select_all_state <- "first"  # Reset select_all state when navigating
    }
  })

  observeEvent(input$class_filter, {
    rv$current_page <- 1
    rv$select_all_state <- "first"  # Reset select_all state when filter changes
  })
  observeEvent(input$images_per_page, {
    rv$current_page <- 1
    rv$select_all_state <- "first"  # Reset select_all state when page size changes
  })

  # Render gallery
  output$image_gallery <- renderUI({
    req(paginated_images())
    req(rv$temp_png_folder)
    # Allow gallery to render in class review mode (no current_sample)
    req(isTRUE(rv$class_review_mode) || !is.null(rv$current_sample))

    p <- paginated_images()
    images <- p$images

    if (nrow(images) == 0) {
      return(div(class = "alert alert-info", "No images to display"))
    }

    classes <- sort(unique(images$class_name))

    class_panels <- lapply(classes, function(cls) {
      class_images <- images %>% filter(class_name == cls)

      image_cards <- lapply(seq_len(nrow(class_images)), function(i) {
        img_row <- class_images[i, ]
        img_file <- img_row$file_name

        is_selected <- img_file %in% rv$selected_images

        was_relabeled <- FALSE
        original_class <- ""
        orig_idx <- which(rv$original_classifications$file_name == img_file)
        if (length(orig_idx) > 0) {
          original_class <- rv$original_classifications$class_name[orig_idx]
          was_relabeled <- (original_class != img_row$class_name)
        }

        border_style <- if (is_selected) {
          "border: 3px solid #007bff;"
        } else if (was_relabeled) {
          "border: 3px solid #ffc107;"
        } else {
          "border: 1px solid #ddd;"
        }

        card_class <- if (is_selected) "image-card selected" else "image-card"

        # Sanitize file names to prevent XSS
        safe_img_file <- htmltools::htmlEscape(img_file)
        resource_path <- if (!is.null(rv$resource_path_name)) rv$resource_path_name else "temp_images"

        # In DB class review mode, derive sample from IFCB file naming convention.
        # External class review uses a single synthetic sample folder.
        if (rv$class_review_mode && !identical(rv$class_review_source, "external")) {
          sample_for_img <- sub("_(\\d{5})\\.png$", "", img_file)
        } else {
          sample_for_img <- rv$current_sample
        }
        safe_sample <- htmltools::htmlEscape(sample_for_img)
        img_src <- sprintf("%s/%s/%s", resource_path, safe_sample, safe_img_file)

        div(
          class = card_class,
          `data-img` = safe_img_file,
          `data-relabeled` = tolower(as.character(was_relabeled)),
          style = paste0("display: inline-block; margin: 5px; padding: 5px; ",
                         border_style, " border-radius: 5px; cursor: pointer; ",
                         "background-color: ", if(is_selected) "#e7f1ff" else "white", ";"),

          tags$img(
            src = img_src,
            style = "display: block;",
            onerror = "this.style.display='none'; this.nextSibling.style.display='block';"
          ),
          div(style = "width: 100px; height: 80px; background: #f0f0f0; display: none;
                       line-height: 80px; text-align: center; font-size: 11px;",
              "Not found"),

          div(
            style = "font-size: 10px; text-align: center; margin-top: 3px; word-break: break-all;",
            if (isTRUE(rv$class_review_mode)) {
              sub("\\.png$", "", img_file)
            } else {
              gsub(".*_(\\d+)\\.png$", "ROI \\1", img_file)
            },
            if (was_relabeled) {
              tags$span(style = "color: #856404;",
                        paste0(" (was: ", gsub("_\\d+$", "", original_class), ")"))
            },
            if (!is.na(img_row$score)) {
              tagList(br(), tags$span(style = "color: #666;", sprintf("%.1f%%", img_row$score * 100)))
            }
          )
        )
      })

      total_in_class <- sum(filtered_images()$class_name == cls)

      # Check if this class is unmatched (not in class2use)
      is_unmatched <- !(cls %in% c(rv$class2use, "unclassified"))
      header_style <- if (is_unmatched) {
        "background: #fff3cd; padding: 10px; border-radius: 5px; border-left: 4px solid #ffc107;"
      } else {
        "background: #f8f9fa; padding: 10px; border-radius: 5px;"
      }
      class_display <- if (is_unmatched) {
        tagList(
          tags$span(style = "color: #856404;", "\u26A0 "),
          tags$span(style = "color: #856404;", cls)
        )
      } else {
        cls
      }

      div(
        style = "margin-bottom: 20px;",
        h5(style = header_style,
           class_display,
           tags$span(style = "color: #666; font-size: 14px;",
                     sprintf(" (%d on page, %d total)", nrow(class_images), total_in_class)),
           if (is_unmatched) tags$span(style = "color: #856404; font-size: 12px; margin-left: 10px;",
                                       "- Not in class list, needs relabeling")),
        div(style = "display: flex; flex-wrap: wrap;", image_cards)
      )
    })

    div(class_panels)
  })

  list(
    filtered_images = filtered_images,
    paginated_images = paginated_images
  )
}
