# Server logic for ClassiPyR
#
# This file orchestrates the modular server components.
# Each module is defined in inst/app/modules/ as a setup_*_server() function
# that registers observers/renderers by side effect.
#
# Module files:
#   helpers_shared.R          - MONTH_NAMES, build_class_filter_choices, update_current_sample_status
#   init_server.R             - reactive values, config, settings I/O, WoRMS persistence
#   settings_server.R         - settings modal, file browsers, apply_settings
#   class_list_server.R       - class editor modal, WoRMS matching, download handlers
#   prediction_server.R       - Gradio live prediction
#   import_export_server.R    - MAT<->SQLite, ZIP/PNG import/export
#   ui_outputs_server.R       - title bar, mode indicators, mode switching
#   class_list_loading_server.R - startup class loading, class2use file upload
#   sample_discovery_server.R - folder scanning, filter dropdowns, sample list
#   sample_loading_server.R   - load samples, navigation, caching
#   gallery_server.R          - image gallery rendering, pagination, filtering
#   selection_relabel_server.R - image selection, relabeling
#   class_review_server.R     - cross-sample class review mode
#   manual_save_server.R      - save button handler
#   statistics_server.R       - summary table, validation stats
#   session_cleanup_server.R  - onSessionEnded cleanup

# Source all module files
modules_dir <- file.path(system.file("app", package = "ClassiPyR"), "modules")
for (module_file in list.files(modules_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(module_file, local = TRUE)
}

server <- function(input, output, session) {

  # 1. Init: creates rv, config, reactiveVals, utility functions
  init <- setup_init_server(input, output, session)

  rv              <- init$rv
  config          <- init$config
  saved_settings  <- init$saved_settings
  persist_settings <- init$persist_settings

  # ReactiveVal accessors
  all_samples          <- init$all_samples
  classified_samples   <- init$classified_samples
  annotated_samples    <- init$annotated_samples
  classifier_mat_files <- init$classifier_mat_files
  classifier_h5_files  <- init$classifier_h5_files
  roi_path_map         <- init$roi_path_map
  png_sample_path_map  <- init$png_sample_path_map
  csv_path_map         <- init$csv_path_map
  rescan_trigger       <- init$rescan_trigger
  last_sync_time       <- init$last_sync_time
  unmatched_classes    <- init$unmatched_classes

  # Utility closures from init
  get_browse_volumes    <- init$get_browse_volumes
  make_dynamic_roots    <- init$make_dynamic_roots
  setup_path_validation <- init$setup_path_validation
  build_zip_readme      <- init$build_zip_readme
  save_worms_map        <- init$save_worms_map
  load_worms_map        <- init$load_worms_map

  # Wrapper for build_class_filter_choices that captures unmatched_classes
  build_class_filter_choices_fn <- function(classes) {
    build_class_filter_choices(classes, unmatched = unmatched_classes())
  }

  # Wrapper for update_current_sample_status that captures session + reactiveVals
  update_current_sample_status_fn <- function(sample_name) {
    update_current_sample_status(session, sample_name,
                                 classified_samples, annotated_samples)
  }

  # 2. Sample discovery: folder scanning, filters, sample list
  discovery <- setup_sample_discovery_server(
    input, output, session, rv, config,
    all_samples, classified_samples, annotated_samples,
    roi_path_map, png_sample_path_map, csv_path_map,
    classifier_mat_files, classifier_h5_files,
    rescan_trigger, last_sync_time
  )

  # 3. Sample loading: load samples, navigation, caching
  loading <- setup_sample_loading_server(
    input, output, session, rv, config,
    roi_path_map, png_sample_path_map, csv_path_map,
    classifier_mat_files, classifier_h5_files,
    annotated_samples, classified_samples,
    discovery$get_filtered_samples,
    update_current_sample_status_fn
  )

  # 4. Gallery: image rendering, pagination
  gallery <- setup_gallery_server(input, output, session, rv)

  # 5. Selection and relabeling
  setup_selection_relabel_server(
    input, output, session, rv,
    gallery$filtered_images, gallery$paginated_images
  )

  # 6. Statistics (needs do_switch_to_validation from ui_outputs, but we create
  #    calculate_stats first since ui_outputs needs it — use a NULL placeholder
  #    for do_switch_to_validation and wire it after ui_outputs is set up)
  #
  #    Actually, statistics_server creates calculate_stats AND uses
  #    do_switch_to_validation. But ui_outputs_server needs calculate_stats.
  #    We resolve this circular dependency by passing do_switch_to_validation
  #    as a function that we'll define after ui_outputs is set up.
  do_switch_ref <- new.env(parent = emptyenv())
  do_switch_ref$fn <- function() NULL  # placeholder

  stats <- setup_statistics_server(
    input, output, session, rv,
    do_switch_to_validation = function() do_switch_ref$fn()
  )

  # 7. UI outputs: title bar, mode indicators, mode switching
  ui_out <- setup_ui_outputs_server(
    input, output, session, rv, config,
    stats$calculate_stats,
    roi_path_map, classifier_mat_files, classifier_h5_files,
    last_sync_time, csv_path_map, png_sample_path_map
  )

  # Wire the do_switch_to_validation reference
  do_switch_ref$fn <- ui_out$do_switch_to_validation

  # 8. Settings modal
  setup_settings_server(
    input, output, session, rv, config,
    get_browse_volumes, make_dynamic_roots, setup_path_validation,
    persist_settings, load_worms_map, rescan_trigger
  )

  # 9. Class list editor
  setup_class_list_server(input, output, session, rv, config, save_worms_map)

  # 10. Prediction
  setup_prediction_server(
    input, output, session, rv, config,
    build_class_filter_choices_fn
  )

  # 11. Import/Export
  setup_import_export_server(
    input, output, session, rv, config,
    persist_settings, get_browse_volumes, build_zip_readme,
    roi_path_map, rescan_trigger
  )

  # 12. Class list loading (startup)
  setup_class_list_loading_server(
    input, output, session, rv, config,
    saved_settings, persist_settings,
    discovery$update_month_choices, discovery$update_sample_list
  )

  # 13. Class review mode
  setup_class_review_server(
    input, output, session, rv, config,
    roi_path_map, loading$save_to_cache,
    loading$disable_nav_buttons, loading$enable_nav_buttons
  )

  # 14. Manual save
  setup_manual_save_server(
    input, output, session, rv, config,
    roi_path_map, annotated_samples,
    loading$disable_nav_buttons, loading$enable_nav_buttons,
    update_current_sample_status_fn
  )

  # 15. Session cleanup
  setup_session_cleanup_server(input, session, rv, config)
}
