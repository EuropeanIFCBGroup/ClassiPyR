# onSessionEnded cleanup

setup_session_cleanup_server <- function(input, session, rv, config,
                                         roi_path_map = function() list()) {
  session$onSessionEnded(function() {
    # Capture all values upfront while session context is still valid
    tryCatch({
      current_sample <- isolate(rv$current_sample)
      current_classifications <- isolate(rv$classifications)
      resource_path_name <- isolate(rv$resource_path_name)
      in_class_review <- isTRUE(isolate(rv$class_review_mode))

      if (!in_class_review &&
          !is.null(current_sample) && !is.null(current_classifications) &&
          !identical(current_sample, "__external_review__")) {
        isolate({
          roi_path <- roi_path_map()[[current_sample]]
          rv$session_cache[[current_sample]] <- list(
            classifications = rv$classifications,
            original_classifications = rv$original_classifications,
            changes_log = rv$changes_log,
            is_annotation_mode = rv$is_annotation_mode,
            has_classification = rv$has_classification,
            temp_png_folder = rv$temp_png_folder,
            adc_folder = if (!is.null(roi_path)) dirname(roi_path),
            saved = is_saved_state(rv$saved_state, current_sample,
                                   rv$classifications)
          )
        })
      }

      session_cache <- isolate(rv$session_cache)
      class2use <- isolate(rv$class2use)
      class2use_path <- isolate(rv$class2use_path)
      temp_png_folder <- isolate(rv$temp_png_folder)
      temp_png_is_managed <- isolate(rv$temp_png_is_managed)
      output_folder <- isolate(config$output_folder)
      png_output_folder <- isolate(config$png_output_folder)
      roi_folder <- isolate(config$roi_folder)
      annotator <- isolate(input$annotator_name)
      if (is.null(annotator) || !nzchar(annotator)) annotator <- "Unknown"

      # Save any unsaved samples with changes
      for (sample_name in names(session_cache)) {
        if (identical(sample_name, "__external_review__")) next
        cached <- session_cache[[sample_name]]
        # Samples already written by the Save button or the navigation
        # auto-save are skipped: their image folder is gone by now, so a
        # second MAT save could only fail, and a second database save would
        # only repeat the first.
        if (isTRUE(cached$saved)) next
        if (!is.null(cached$changes_log) && nrow(cached$changes_log) > 0) {
          tryCatch({
            save_sample_annotations(
              sample_name = sample_name,
              classifications = cached$classifications,
              original_classifications = cached$original_classifications,
              changes_log = cached$changes_log,
              # Each cached sample's own PNG folder; the session-level folder
              # only matches the sample that was loaded last
              temp_png_folder = cached$temp_png_folder %||% temp_png_folder,
              output_folder = output_folder,
              png_output_folder = png_output_folder,
              roi_folder = roi_folder,
              class2use_path = class2use_path,
              class2use = class2use,
              annotator = annotator,
              adc_folder = cached$adc_folder,
              save_format = isolate(config$save_format),
              db_folder = isolate(config$db_folder),
              export_statistics = isolate(config$export_statistics)
            )
          }, error = function(e) {
            message("Failed to auto-save ", sample_name, " on session end: ", e$message)
          })
        }
      }

      # Clean up session-specific resource path
      if (!is.null(resource_path_name)) {
        tryCatch({
          removeResourcePath(resource_path_name)
        }, error = function(e) {
          # Resource path may already be removed, ignore
        })
      }

      # Clean up temporary files, but never the persistent dashboard PNG
      # cache (in dashboard mode temp_png_folder points inside it, and every
      # other cleanup site excludes it the same way)
      if (!is.null(temp_png_folder) && dir.exists(temp_png_folder) &&
          isTRUE(temp_png_is_managed) &&
          !startsWith(temp_png_folder, get_dashboard_cache_dir())) {
        unlink(temp_png_folder, recursive = TRUE)
      }
    }, error = function(e) {
      message("Error during session cleanup: ", e$message)
    })
  })
}
