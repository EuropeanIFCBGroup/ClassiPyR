# onSessionEnded cleanup

setup_session_cleanup_server <- function(input, session, rv, config) {
  session$onSessionEnded(function() {
    # Capture all values upfront while session context is still valid
    tryCatch({
      current_sample <- isolate(rv$current_sample)
      current_classifications <- isolate(rv$classifications)
      resource_path_name <- isolate(rv$resource_path_name)

      if (!is.null(current_sample) && !is.null(current_classifications)) {
        isolate({
          rv$session_cache[[current_sample]] <- list(
            classifications = rv$classifications,
            original_classifications = rv$original_classifications,
            changes_log = rv$changes_log,
            is_annotation_mode = rv$is_annotation_mode
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
      annotator <- isolate(input$annotator_name) %||% "Unknown"

      # Save any unsaved samples with changes
      for (sample_name in names(session_cache)) {
        cached <- session_cache[[sample_name]]
        if (!is.null(cached$changes_log) && nrow(cached$changes_log) > 0) {
          tryCatch({
            save_sample_annotations(
              sample_name = sample_name,
              classifications = cached$classifications,
              original_classifications = cached$original_classifications,
              changes_log = cached$changes_log,
              temp_png_folder = temp_png_folder,
              output_folder = output_folder,
              png_output_folder = png_output_folder,
              roi_folder = roi_folder,
              class2use_path = class2use_path,
              class2use = class2use,
              annotator = annotator,
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

      # Clean up temporary files
      if (!is.null(temp_png_folder) && dir.exists(temp_png_folder) &&
          isTRUE(temp_png_is_managed)) {
        unlink(temp_png_folder, recursive = TRUE)
      }
    }, error = function(e) {
      message("Error during session cleanup: ", e$message)
    })
  })
}
