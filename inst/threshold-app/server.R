# Server logic for the ClassiPyR threshold review app
#
# This file orchestrates the modular server components. Each module is
# defined in inst/threshold-app/modules/ as a setup_thr_*_server() function
# that registers observers and renderers by side effect. The logic they call
# lives in the ClassiPyR package (R/threshold_*.R), where it is tested.
#
# Module files:
#   setup_server.R    - folders, index, model, pre-flight, freeze
#   review_server.R   - class picker, loading, confirming and undoing a page
#   gallery_server.R  - image folder, gallery rendering, session cleanup
#   results_server.R  - chosen thresholds, precision curve, manual thresholds
#   export_server.R   - writing the thresholds files
#
# Nothing is shared with the main app in inst/app.

# The review functions are internal to ClassiPyR. The modules and the server
# are therefore defined in an environment below the package namespace: they
# then find those functions in an installed package as well, where only the
# exported ones are attached.
thr_app_env <- new.env(parent = asNamespace("ClassiPyR"))

# Source all module files
modules_dir <- file.path(system.file("threshold-app", package = "ClassiPyR"),
                         "modules")
for (module_file in list.files(modules_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(module_file, local = thr_app_env)
}

# Build the server function. The reader of the class-score files and the
# image extractor can be replaced, which is how the tests run the whole app
# without HDF5 or raw IFCB files.
thr_app_server <- local({
  function(reader = read_class_scores_summary,
           extractor = iRfcb::ifcb_extract_pngs) {
    function(input, output, session) {
      # What the modules share. The *_version counters are bumped to make
      # everything that reads the index, the frame or the verdicts refresh.
      state <- reactiveValues(
        review_dir = NULL, h5_folder = NULL, roi_folder = NULL,
        settings_notice = NULL,
        index_version = 0, index_message = NULL,
        model_id = NULL, frozen = FALSE, frame_version = 0,
        annotations_folder = NULL, preflight = NULL,
        data_version = 0, page = NULL, page_token = NULL, page_class = NULL,
        undo_stack = list(), review_message = NULL,
        results_class = NULL, export_result = NULL,
        image_folder = NULL, resource_name = NULL
      )

      # 1. Setup: folders, index, model, pre-flight and freeze
      setup_thr_setup_server(input, output, session, state, reader = reader)

      # 2. Gallery: image folder, rendering, cleanup at session end
      setup_thr_gallery_server(input, output, session, state)

      # 3. Review: class picker, loading, confirming and undoing pages
      setup_thr_review_server(input, output, session, state,
                              extractor = extractor)

      # 4. Results: thresholds table, curve and manual thresholds
      thr_results <- setup_thr_results_server(input, output, session, state)

      # 5. Export
      setup_thr_export_server(input, output, session, state,
                              thr_results$resolved)
    }
  }
}, envir = thr_app_env)

server <- thr_app_server()
