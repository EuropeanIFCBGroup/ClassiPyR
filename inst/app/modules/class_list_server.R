# Class list editor modal, WoRMS matching, download handlers

setup_class_list_server <- function(input, output, session, rv, config,
                                    save_worms_map) {
  # Class List Editor Modal
  observeEvent(input$open_class_editor, {
    showModal(modalDialog(
      title = "Class List Editor",
      size = "l",
      easyClose = TRUE,

      tags$head(tags$style(HTML(
        ".modal-dialog.modal-lg { max-width: 1200px; }"
      ))),

      tags$div(
        class = "alert alert-warning",
        style = "font-size: 12px; padding: 8px;",
        tags$strong("Note for ifcb-analysis users:"),
        " Class indices are used in .mat annotations. ",
        tags$strong("Do not remove or reorder classes"),
        " if using the ",
        tags$a(href = "https://github.com/hsosik/ifcb-analysis", target = "_blank", "ifcb-analysis"),
        " MATLAB toolbox, as this will break existing annotations. ",
        "You may rename classes or add new ones at the end."
      ),

      div(
        style = "display: flex; gap: 15px; align-items: stretch;",
        div(
          style = "flex: 1; display: flex; flex-direction: column;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
            tags$label(sprintf("Current classes (%d)", length(if (is.null(rv$class2use)) character(0) else rv$class2use)), style = "margin: 0;"),
            div(
              class = "btn-group btn-group-sm",
              id = "sort_btn_group",
              actionButton("sort_by_id", "By ID", class = "btn-outline-secondary active",
                           style = "padding: 2px 8px; font-size: 11px;"),
              actionButton("sort_alpha", "A-Z", class = "btn-outline-secondary",
                           style = "padding: 2px 8px; font-size: 11px;")
            ),
            tags$script(HTML("
              $(document).on('click', '#sort_by_id', function() {
                $('#sort_btn_group .btn').removeClass('active');
                $(this).addClass('active');
              });
              $(document).on('click', '#sort_alpha', function() {
                $('#sort_btn_group .btn').removeClass('active');
                $(this).addClass('active');
              });
            "))
          ),
          tags$div(
            style = "flex: 1; overflow-y: auto; border: 1px solid #ddd; padding: 8px; font-family: monospace; font-size: 12px; background: #f8f9fa; border-radius: 4px; min-height: 250px;",
            uiOutput("class_list_display")
          )
        ),
        div(
          style = "flex: 1; display: flex; flex-direction: column;",
          tags$label("Edit class list (one per line)", style = "margin-bottom: 5px;"),
          tags$textarea(
            id = "class_list_edit",
            class = "form-control",
            style = "flex: 1; font-family: monospace; font-size: 12px; min-height: 250px; resize: vertical;",
            if (is.null(rv$class2use)) "" else paste(rv$class2use, collapse = "\n")
          )
        )
      ),

      div(
        style = "margin-top: 10px;",
        div(
          textInput("new_class_name", "Add new class:", placeholder = "Enter new class name"),
          actionButton("add_class_btn", "Add to End", class = "btn-sm btn-outline-primary")
        ),
        div(
          style = "margin-top: 12px; padding-top: 10px; border-top: 1px solid #e5e5e5;",
          tags$label("WoRMS Matching", style = "margin-bottom: 6px; font-weight: 600;"),
          div(
            style = "display: flex; gap: 8px; align-items: center;",
            actionButton("match_worms_btn", "Match WoRMS AphiaID", class = "btn-sm btn-outline-info")
          ),
          tags$small(
            class = "text-muted",
            "Class names are sanitized before WoRMS lookup (e.g. separator handling and long-name skipping)."
          )
        )
      ),

      footer = tagList(
        div(
          style = "display: flex; gap: 10px; width: 100%; justify-content: space-between;",
          div(
            style = "display: flex; gap: 10px;",
            downloadButton("save_class2use_mat", "Save as .mat", class = "btn-sm btn-outline-secondary"),
            downloadButton("save_class2use_txt", "Save as .txt", class = "btn-sm btn-outline-secondary")
          ),
          div(
            style = "display: flex; gap: 10px;",
            actionButton("apply_class_changes", "Apply Changes", class = "btn-warning"),
            modalButton("Close")
          )
        )
      )
    ))
  })

  # Sort button handlers
  observeEvent(input$sort_by_id, { rv$class_sort_mode <- "id" })
  observeEvent(input$sort_alpha, { rv$class_sort_mode <- "alpha" })

  # Render class list with indices
  output$class_list_display <- renderUI({
    if (is.null(rv$class2use) || length(rv$class2use) == 0) {
      return(tags$div(
        style = "color: #666; font-style: italic;",
        "No classes defined yet. Add classes using the form below or edit the text area."
      ))
    }

    classes <- rv$class2use
    indices <- seq_along(classes)

    counts <- tryCatch({
      db_path <- get_db_path(config$db_folder)
      if (file.exists(db_path)) {
        classes_df <- list_classes_db(db_path)
        setNames(classes_df$count, classes_df$class_name)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    df <- data.frame(idx = indices, cls = classes, stringsAsFactors = FALSE)
    if (rv$class_sort_mode == "alpha") {
      df <- df[order(df$cls), ]
    }

    class_lines <- mapply(function(idx, cls) {
      count <- if (!is.null(counts) && cls %in% names(counts)) counts[[cls]] else 0L
      aphia <- if (!is.null(rv$class_aphia_map) && cls %in% names(rv$class_aphia_map)) {
        rv$class_aphia_map[[cls]]
      } else {
        NA_character_
      }
      aphia_txt <- if (!is.na(aphia) && nzchar(aphia)) paste0(" [AphiaID: ", aphia, "]") else ""
      tags$div(sprintf("%3d: %s%s (%d)", idx, cls, aphia_txt, count))
    }, df$idx, df$cls, SIMPLIFY = FALSE)

    tagList(class_lines)
  })

  # Add new class
  observeEvent(input$add_class_btn, {
    req(input$new_class_name)
    new_class <- trimws(input$new_class_name)

    if (new_class == "") {
      showNotification("Please enter a class name", type = "warning")
      return()
    }

    current_classes <- if (is.null(rv$class2use)) character(0) else rv$class2use

    if (new_class %in% current_classes) {
      showNotification("Class already exists", type = "warning")
      return()
    }

    rv$class2use <- c(current_classes, new_class)

    if (is.null(rv$class2use_path)) {
      temp_class_file <- file.path(tempdir(), "class2use_temp.txt")
      writeLines(rv$class2use, temp_class_file)
      rv$class2use_path <- temp_class_file
    } else {
      if (grepl("class2use_temp", rv$class2use_path)) {
        writeLines(rv$class2use, rv$class2use_path)
      }
    }

    updateTextAreaInput(session, "class_list_edit",
                        value = paste(rv$class2use, collapse = "\n"))
    updateTextInput(session, "new_class_name", value = "")

    sorted_classes <- sort(rv$class2use)
    updateSelectizeInput(session, "new_class_quick",
                         choices = sorted_classes,
                         selected = character(0))

    showNotification(paste("Added class:", new_class), type = "message")
  })

  # Apply class list changes from text area
  observeEvent(input$apply_class_changes, {
    text_content <- input$class_list_edit
    if (is.null(text_content)) text_content <- ""

    new_classes <- strsplit(text_content, "\n")[[1]]
    new_classes <- trimws(new_classes)
    new_classes <- new_classes[new_classes != ""]

    if (length(new_classes) == 0) {
      showNotification("Please enter at least one class name", type = "warning")
      return()
    }

    current_count <- if (is.null(rv$class2use)) 0 else length(rv$class2use)
    if (length(new_classes) < current_count) {
      showNotification(
        "Warning: Removing classes can break existing .mat annotations if using ifcb-analysis. Proceed with caution.",
        type = "warning", duration = 5
      )
    }

    rv$class2use <- new_classes

    if (is.null(rv$class2use_path)) {
      temp_class_file <- file.path(tempdir(), "class2use_temp.txt")
      writeLines(new_classes, temp_class_file)
      rv$class2use_path <- temp_class_file
      showNotification(
        "Class list created. Remember to save it using 'Save as .mat' or 'Save as .txt' for future use.",
        type = "message", duration = 8
      )
    } else if (grepl("class2use_temp", rv$class2use_path)) {
      writeLines(new_classes, rv$class2use_path)
    }

    sorted_classes <- sort(rv$class2use)
    updateSelectizeInput(session, "new_class_quick",
                         choices = sorted_classes,
                         selected = character(0))

    showNotification(paste("Applied", length(new_classes), "classes"), type = "message")
  })

  output$worms_match_table <- DT::renderDT({
    req(rv$worms_matches)
    DT::datatable(
      rv$worms_matches,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, order = list(list(0, "asc")))
    )
  })

  worms_query_input_id <- function(i) paste0("worms_query_", i)

  output$worms_unmatched_inputs <- renderUI({
    req(rv$worms_matches)
    matches <- rv$worms_matches
    unmatched_idx <- which(is.na(matches$aphia_id) | !nzchar(matches$aphia_id))

    if (!length(unmatched_idx)) {
      return(tags$div(class = "text-muted", "No unmatched classes to edit."))
    }

    rows <- lapply(seq_along(unmatched_idx), function(j) {
      i <- unmatched_idx[j]
      cls <- matches$class_name[i]
      qry <- matches$query_name[i]
      if (is.na(qry) || !nzchar(qry)) qry <- cls

      tags$div(
        style = "margin-bottom: 8px;",
        tags$div(style = "font-size: 12px; color: #555;", tags$strong(cls)),
        textInput(
          inputId = worms_query_input_id(j),
          label = NULL,
          value = qry,
          width = "100%"
        )
      )
    })
    do.call(tagList, rows)
  })

  observeEvent(input$match_worms_btn, {
    if (!requireNamespace("worrms", quietly = TRUE)) {
      showNotification(
        "Package 'worrms' is required. Install with install.packages('worrms')",
        type = "error", duration = 8
      )
      return()
    }
    req(rv$class2use)

    target_classes <- setdiff(rv$class2use, "unclassified")
    if (!length(target_classes)) {
      showNotification("No classes to match (only 'unclassified' found).", type = "warning")
      return()
    }

    initial_rows <- NULL
    withProgress(message = "Matching class names against WoRMS...", value = 0.2, {
      initial_rows <- ClassiPyR::build_worms_match_rows(target_classes, target_classes)
      incProgress(0.6)
    })

    if (is.null(initial_rows) || !nrow(initial_rows)) {
      showNotification("No results returned from WoRMS.", type = "warning")
      return()
    }

    rv$worms_matches <- initial_rows
    matched_n <- sum(!is.na(rv$worms_matches$aphia_id) & nzchar(rv$worms_matches$aphia_id))
    unmatched_n <- nrow(rv$worms_matches) - matched_n
    skipped_n <- sum(rv$worms_matches$status == "skipped", na.rm = TRUE)

    showModal(modalDialog(
      title = "WoRMS Match Results",
      size = "l",
      easyClose = TRUE,
      tags$p(sprintf(
        "Matched %d of %d classes. Unmatched: %d. Skipped (>80 chars): %d.",
        matched_n, nrow(rv$worms_matches), unmatched_n, skipped_n
      )),
      tags$label("Manual rematch for unmatched/skipped (edit query only)"),
      uiOutput("worms_unmatched_inputs"),
      checkboxInput("worms_apply_renames",
                    "Rename classes to WoRMS accepted_name when available",
                    value = FALSE),
      DTOutput("worms_match_table"),
      footer = tagList(
        actionButton("rematch_unmatched_btn", "Rematch Unmatched", class = "btn-outline-info"),
        actionButton("apply_worms_matches", "Apply AphiaID Matches", class = "btn-primary"),
        modalButton("Close")
      )
    ))
  })

  observeEvent(input$rematch_unmatched_btn, {
    req(rv$worms_matches)
    matches <- rv$worms_matches
    unmatched_idx <- which(is.na(matches$aphia_id) | !nzchar(matches$aphia_id))
    if (!length(unmatched_idx)) {
      showNotification("No unmatched classes to rematch.", type = "message")
      return()
    }

    target_classes <- matches$class_name[unmatched_idx]
    target_queries <- matches$query_name[unmatched_idx]
    target_queries[is.na(target_queries) | !nzchar(target_queries)] <- target_classes[is.na(target_queries) | !nzchar(target_queries)]
    for (j in seq_along(unmatched_idx)) {
      q <- input[[worms_query_input_id(j)]]
      if (!is.null(q) && nzchar(trimws(q))) {
        target_queries[j] <- trimws(q)
      }
    }

    updated_rows <- NULL
    withProgress(message = "Re-matching unmatched classes...", value = 0.2, {
      updated_rows <- ClassiPyR::build_worms_match_rows(target_classes, target_queries)
      incProgress(0.7)
    })
    if (is.null(updated_rows) || !nrow(updated_rows)) {
      showNotification("No updates from rematch.", type = "warning")
      return()
    }

    matches[unmatched_idx, c("class_name", "query_name", "matched_name", "accepted_name", "aphia_id", "status", "note")] <- updated_rows
    rv$worms_matches <- matches

    matched_n <- sum(!is.na(rv$worms_matches$aphia_id) & nzchar(rv$worms_matches$aphia_id))
    remaining_n <- nrow(rv$worms_matches) - matched_n
    showNotification(
      sprintf("Re-match complete. Matched: %d. Remaining unmatched/skipped: %d.", matched_n, remaining_n),
      type = "message"
    )
  })

  observeEvent(input$apply_worms_matches, {
    req(rv$worms_matches)
    matches <- rv$worms_matches
    matched <- matches[!is.na(matches$aphia_id) & nzchar(matches$aphia_id), , drop = FALSE]
    if (!nrow(matched)) {
      showNotification("No matched AphiaID values to apply.", type = "warning")
      return()
    }

    current_classes <- rv$class2use
    rename_requested <- isTRUE(input$worms_apply_renames)
    renamed <- 0L
    rename_conflicts <- 0L

    if (rename_requested) {
      for (i in seq_len(nrow(matched))) {
        old_name <- matched$class_name[i]
        new_name <- matched$accepted_name[i]
        if (is.na(new_name) || !nzchar(new_name) || identical(old_name, new_name)) next
        old_idx <- match(old_name, current_classes)
        if (is.na(old_idx)) next
        if (new_name %in% current_classes && !identical(new_name, old_name)) {
          rename_conflicts <- rename_conflicts + 1L
          next
        }
        current_classes[old_idx] <- new_name
        renamed <- renamed + 1L
      }
      rv$class2use <- current_classes

      if (!is.null(rv$class2use_path) && grepl("class2use_temp", rv$class2use_path)) {
        writeLines(rv$class2use, rv$class2use_path)
      }
    }

    updated_map <- rv$class_aphia_map
    for (i in seq_len(nrow(matched))) {
      key <- matched$class_name[i]
      if (rename_requested && !is.na(matched$accepted_name[i]) && nzchar(matched$accepted_name[i])) {
        if (matched$accepted_name[i] %in% rv$class2use) {
          key <- matched$accepted_name[i]
        }
      }
      updated_map[[key]] <- matched$aphia_id[i]
    }
    rv$class_aphia_map <- updated_map
    save_worms_map(updated_map, db_folder = config$db_folder, matches_df = matched)

    if (rename_requested) {
      updateTextAreaInput(session, "class_list_edit", value = paste(rv$class2use, collapse = "\n"))
      sorted_classes <- sort(rv$class2use)
      updateSelectizeInput(session, "new_class_quick",
                           choices = sorted_classes,
                           selected = character(0))
    }

    showNotification(
      sprintf(
        "Stored %d AphiaID matches%s%s.",
        nrow(matched),
        if (rename_requested) paste0("; renamed ", renamed, " classes") else "",
        if (rename_conflicts > 0) paste0("; skipped ", rename_conflicts, " rename conflicts") else ""
      ),
      type = "message", duration = 8
    )
    removeModal()
  })

  # Download handlers
  output$save_class2use_mat <- downloadHandler(
    filename = function() "class2use.mat",
    content = function(file) ifcb_create_class2use(rv$class2use, file)
  )

  output$save_class2use_txt <- downloadHandler(
    filename = function() "class2use.txt",
    content = function(file) writeLines(rv$class2use, file)
  )
}
