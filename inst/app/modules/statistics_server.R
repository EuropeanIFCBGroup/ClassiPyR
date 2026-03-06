# Summary table, validation stats

setup_statistics_server <- function(input, output, session, rv,
                                    do_switch_to_validation) {
  calculate_stats <- reactive({
    req(rv$classifications)
    req(rv$original_classifications)

    original <- rv$original_classifications
    current <- rv$classifications

    comparison <- merge(
      original %>% select(file_name, original_class = class_name),
      current %>% select(file_name, validated_class = class_name),
      by = "file_name"
    )

    comparison$correct <- comparison$original_class == comparison$validated_class

    total <- nrow(comparison)
    correct <- sum(comparison$correct)

    data.frame(
      sample = rv$current_sample,
      total_images = total,
      correct_classifications = correct,
      incorrect_classifications = total - correct,
      accuracy = if (total > 0) correct / total else NA
    )
  })

  output$summary_table <- renderDT({
    req(rv$classifications)

    has_scores <- !all(is.na(rv$classifications$score))

    if (has_scores) {
      summary_df <- rv$classifications %>%
        group_by(class_name) %>%
        summarise(
          count = n(),
          avg_score = mean(score, na.rm = TRUE),
          min_score = min(score, na.rm = TRUE),
          max_score = max(score, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(class_name)

      datatable(summary_df,
                options = list(pageLength = 25),
                colnames = c("Class", "Count", "Avg Score", "Min Score", "Max Score")) %>%
        formatPercentage(c("avg_score", "min_score", "max_score"), digits = 1)
    } else {
      summary_df <- rv$classifications %>%
        group_by(class_name) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(class_name)

      datatable(summary_df,
                options = list(pageLength = 25),
                colnames = c("Class", "Count"))
    }
  })

  # Conditional content for Validation Statistics tab

  output$validation_tab_content <- renderUI({
    if (is.null(rv$classifications)) {
      return(div(
        class = "alert alert-info",
        "Load a sample to see statistics."
      ))
    }

    if (rv$is_annotation_mode) {
      # In annotation mode, show a message that validation stats are not applicable
      div(
        div(
          class = "alert alert-info",
          tags$strong("Annotation Mode"),
          tags$p("Validation statistics compare auto-classifications against your corrections. ",
                 "In annotation mode, there are no auto-classifications to validate."),
          if (rv$has_both_modes) {
            tags$p("This sample also has auto-classifications available. ",
                   actionLink("switch_to_validation_from_tab", "Switch to Validation mode"),
                   " to see classifier performance statistics.")
          }
        ),
        # Side-by-side layout for annotation mode too
        div(
          style = "display: flex; gap: 20px; height: calc(100vh - 280px);",
          # Left panel: Annotation Progress (scrollable)
          div(
            style = "flex: 1; display: flex; flex-direction: column; min-width: 0;",
            h4("Annotation Progress"),
            div(
              style = "flex: 1; overflow-y: auto; background: #f8f9fa; border: 1px solid #ddd; border-radius: 4px; padding: 10px;",
              verbatimTextOutput("annotation_progress")
            )
          ),
          # Right panel: Changes Made (scrollable)
          div(
            style = "flex: 1; display: flex; flex-direction: column; min-width: 0;",
            h4("Changes Made"),
            div(
              style = "flex: 1; overflow-y: auto;",
              DTOutput("changes_table")
            )
          )
        )
      )
    } else {
      # In validation mode, show full statistics in a side-by-side layout
      div(
        style = "display: flex; gap: 20px; height: calc(100vh - 200px);",
        # Left panel: Classification Performance (scrollable)
        div(
          style = "flex: 1; display: flex; flex-direction: column; min-width: 0;",
          h4("Classification Performance"),
          div(
            style = "flex: 1; overflow-y: auto; background: #f8f9fa; border: 1px solid #ddd; border-radius: 4px; padding: 10px;",
            verbatimTextOutput("detailed_stats")
          )
        ),
        # Right panel: Changes Made (scrollable)
        div(
          style = "flex: 1; display: flex; flex-direction: column; min-width: 0;",
          h4("Changes Made"),
          div(
            style = "flex: 1; overflow-y: auto;",
            DTOutput("changes_table")
          )
        )
      )
    }
  })

  # Switch to validation mode from the tab link (reuse same logic as header button)
  observeEvent(input$switch_to_validation_from_tab, {
    do_switch_to_validation()
  }, ignoreInit = TRUE)

  # Annotation progress (shown in annotation mode)
  output$annotation_progress <- renderText({
    req(rv$classifications)
    req(rv$is_annotation_mode)

    current <- rv$classifications

    class_counts <- current %>%
      group_by(class_name) %>%
      summarise(count = n()) %>%
      arrange(desc(count))

    total <- nrow(current)
    classified <- sum(current$class_name != "unclassified")

    lines <- c(
      sprintf("Total images: %d", total),
      sprintf("Classified: %d (%.1f%%)", classified, (classified / total) * 100),
      sprintf("Remaining: %d (%.1f%%)", total - classified, ((total - classified) / total) * 100),
      "",
      "=== Classification Distribution ===",
      sprintf("%-40s %8s %10s", "Class", "Count", "Percent")
    )

    for (i in seq_len(nrow(class_counts))) {
      lines <- c(lines, sprintf("%-40s %8d %9.1f%%",
                                substr(class_counts$class_name[i], 1, 40),
                                class_counts$count[i],
                                (class_counts$count[i] / total) * 100))
    }

    paste(lines, collapse = "\n")
  })

  output$detailed_stats <- renderText({
    req(rv$classifications)
    req(rv$original_classifications)
    req(!rv$is_annotation_mode)  # Only show in validation mode

    stats <- calculate_stats()

    original <- rv$original_classifications
    current <- rv$classifications

    comparison <- merge(
      original %>% select(file_name, original_class = class_name),
      current %>% select(file_name, validated_class = class_name),
      by = "file_name"
    )

    comparison$correct <- comparison$original_class == comparison$validated_class

    class_stats <- comparison %>%
      group_by(original_class) %>%
      # Note: calculate accuracy BEFORE summing correct, otherwise mean() uses the summed value
      summarise(total = n(), accuracy = mean(correct), n_correct = sum(correct)) %>%
      arrange(desc(total))

    lines <- c(
      "=== Overall Statistics ===",
      sprintf("Total images: %d", stats$total_images),
      sprintf("Correct classifications: %d (%.1f%%)", stats$correct_classifications, stats$accuracy * 100),
      sprintf("Changed classifications: %d (%.1f%%)", stats$incorrect_classifications, (1 - stats$accuracy) * 100),
      "",
      "=== Per-Class Statistics ===",
      sprintf("%-40s %8s %8s %10s", "Class", "Total", "Correct", "Accuracy")
    )

    for (i in seq_len(nrow(class_stats))) {
      lines <- c(lines, sprintf("%-40s %8d %8d %9.1f%%",
                                substr(class_stats$original_class[i], 1, 40),
                                class_stats$total[i],
                                class_stats$n_correct[i],
                                class_stats$accuracy[i] * 100))
    }

    paste(lines, collapse = "\n")
  })

  output$changes_table <- renderDT({
    req(rv$changes_log)

    if (nrow(rv$changes_log) == 0) {
      return(datatable(data.frame(Message = "No changes made yet")))
    }

    datatable(rv$changes_log,
              options = list(pageLength = 25),
              colnames = c("Image", "Original Class", "New Class"))
  })

  list(calculate_stats = calculate_stats)
}
