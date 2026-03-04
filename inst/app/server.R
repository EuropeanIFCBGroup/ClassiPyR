# Server logic for ClassiPyR
#
# This file contains the main server-side logic for the Shiny application.
# It is organized into the following main sections:
#
# 1. REACTIVE VALUES - Core application state
# 2. SETTINGS MODAL - Configuration UI and persistence
# 3. CLASS LIST EDITOR - Class list management UI
# 4. SAMPLE DISCOVERY - Scanning for available samples
# 5. SAMPLE LOADING - Loading classifications and images
# 6. MODE SWITCHING - Switching between annotation/validation modes
# 7. CLASS LIST LOADING - Auto-loading class2use files
# 8. IMAGE SELECTION - Click and drag-select handlers
# 9. RELABELING - Changing image classifications
# 10. PAGINATION - Image gallery pagination
# 11. IMAGE GALLERY - Rendering the image display
# 12. SAVING - Persisting annotations to disk
# 13. STATISTICS - Validation statistics display
# 14. SESSION CLEANUP - Cleanup on session end
#
# For details on helper functions, see:
# - R/utils.R: Utility functions (validation, paths, class list loading)
# - R/sample_loading.R: Loading classifications from various sources
# - R/sample_saving.R: Saving annotations and statistics

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES
  # Core application state managed as reactive values
  # ============================================================================
  
  rv <- reactiveValues(
    # Class list (character vector of class names, order = indices for MAT files)
    # Default to "unclassified" so app works without loading a class list
    class2use = "unclassified",
    class2use_path = NULL,
    
    # Current sample data
    classifications = NULL,         # Current state of image classifications
    current_sample = NULL,          # Sample name (e.g., "D20220522T000439_IFCB134")
    temp_png_folder = NULL,         # Temporary folder with extracted PNG images
    original_classifications = NULL, # Original state for comparison/statistics
    
    # Selection and editing state
    selected_images = character(),  # Currently selected image filenames
    changes_log = create_empty_changes_log(), # Track all changes made
    
    # Session management
    session_cache = list(),         # Cache of loaded samples (for quick switching)
    
    # Mode tracking
    is_annotation_mode = FALSE,     # TRUE = annotation (no auto-class), FALSE = validation
    has_both_modes = FALSE,         # TRUE if sample has both manual AND auto-classification
    using_manual_mode = TRUE,       # When has_both_modes, which mode is active
    
    # UI state
    current_page = 1,               # Current pagination page
    class_sort_mode = "id",         # Class list sort: "id" (by index) or "alpha" (A-Z)
    resource_path_name = NULL,      # Session-specific Shiny resource path for images
    is_loading = FALSE,             # TRUE while loading/saving operations in progress
    measure_mode = FALSE,           # TRUE when measure tool is active
    pending_sample_select = NULL,   # Pending sample selection for dropdown update

    # Class review mode state
    class_review_mode = FALSE,      # TRUE when in class review mode
    class_review_class = NULL,      # Currently reviewed class name
    class_review_samples = character(), # Unique sample names in class review
    class_review_original = NULL    # Original classifications snapshot for diff
  )
  
  # Settings file for persistence (uses R_user_dir for CRAN compliance)
  settings_file <- get_settings_path()
  
  # Get working directory at app startup (for default paths)
  # Get user's working directory (captured by run_app() before Shiny changes it)
  startup_wd <- getOption("ClassiPyR.startup_wd", default = getwd())
  
  # Volumes for shinyFiles directory browser
  base_volumes <- c("Working Dir" = startup_wd, shinyFiles::getVolumes()())
  
  # Build volumes with optional "Current" root from a text input path
  get_browse_volumes <- function(current_path = NULL) {
    if (!is.null(current_path) && nzchar(current_path) && dir.exists(current_path)) {
      c("Current" = normalizePath(current_path), base_volumes)
    } else {
      base_volumes
    }
  }
  
  # Create a dynamic roots object for shinyDirChoose that reads the current
  
  # text input value each time the dialog opens or navigates
  make_dynamic_roots <- function(input_id) {
    f <- function() get_browse_volumes(input[[input_id]])
    structure(f, class = c("dynamic_roots", "function"))
  }
  
  # Load saved settings or use defaults
  load_settings <- function() {
    defaults <- list(
      csv_folder = startup_wd,
      roi_folder = startup_wd,
      output_folder = startup_wd,
      png_output_folder = startup_wd,
      db_folder = get_default_db_dir(),
      use_threshold = TRUE,
      pixels_per_micron = 3.4,  # IFCB default resolution
      auto_sync = TRUE,  # Automatically sync folders on startup
      class2use_path = NULL,  # Path to class2use file for auto-loading
      python_venv_path = NULL,  # NULL = use ./venv in working directory
      save_format = "sqlite",  # "sqlite" (default), "mat", or "both"
      export_statistics = TRUE,  # Write validation statistics CSV files
      skip_class_png = "",  # Class name to exclude from PNG export
      data_source = "local",  # "local" or "dashboard"
      dashboard_url = "",  # IFCB Dashboard URL
      dashboard_autoclass = FALSE,  # Use dashboard auto-classifications for validation
      gradio_url = "",  # Gradio API URL for CNN classification
      prediction_model = "",  # Model name for live prediction
      dashboard_parallel_downloads = 5,
      dashboard_sleep_time = 2,
      dashboard_multi_timeout = 120,
      dashboard_max_retries = 3
    )
    
    if (file.exists(settings_file)) {
      tryCatch({
        saved <- jsonlite::fromJSON(settings_file)
        # Merge saved with defaults (saved takes precedence, but only if valid)
        for (key in names(saved)) {
          if (key %in% names(defaults) || key == "class2use_path") {
            val <- saved[[key]]
            # Only use saved value if it's a valid non-empty string (for path settings)
            # or a valid value for other settings
            if (is.null(val) || length(val) == 0 ||
                (is.character(val) && (length(val) != 1 || isTRUE(is.na(val)) || !nzchar(val)))) {
              # Keep default for invalid/empty values
            } else {
              defaults[[key]] <- val
            }
          }
        }
      }, error = function(e) {
        message("Could not load settings: ", e$message)
      })
    }
    defaults
  }
  
  # Save settings to file
  persist_settings <- function(settings) {
    tryCatch({
      jsonlite::write_json(settings, settings_file, auto_unbox = TRUE, pretty = TRUE)
    }, error = function(e) {
      message("Could not save settings: ", e$message)
    })
  }
  
  # Initialize config from saved settings
  saved_settings <- load_settings()
  
  # run_app(venv_path=) takes precedence over saved settings
  run_app_venv <- getOption("ClassiPyR.venv_path", default = NULL)
  if (!is.null(run_app_venv) && nzchar(run_app_venv)) {
    saved_settings$python_venv_path <- run_app_venv
  }
  
  config <- reactiveValues(
    csv_folder = saved_settings$csv_folder,
    roi_folder = saved_settings$roi_folder,
    output_folder = saved_settings$output_folder,
    png_output_folder = saved_settings$png_output_folder,
    db_folder = saved_settings$db_folder,
    use_threshold = saved_settings$use_threshold,
    pixels_per_micron = saved_settings$pixels_per_micron,
    auto_sync = saved_settings$auto_sync,
    python_venv_path = saved_settings$python_venv_path,
    save_format = saved_settings$save_format,
    export_statistics = saved_settings$export_statistics,
    skip_class_png = saved_settings$skip_class_png,
    data_source = saved_settings$data_source,
    dashboard_url = saved_settings$dashboard_url,
    dashboard_autoclass = saved_settings$dashboard_autoclass,
    gradio_url = saved_settings$gradio_url,
    prediction_model = saved_settings$prediction_model,
    dashboard_parallel_downloads = saved_settings$dashboard_parallel_downloads,
    dashboard_sleep_time = saved_settings$dashboard_sleep_time,
    dashboard_multi_timeout = saved_settings$dashboard_multi_timeout,
    dashboard_max_retries = saved_settings$dashboard_max_retries
  )
  
  # Initialize class dropdown with default class list on startup
  observeEvent(once = TRUE, ignoreNULL = FALSE, session$clientData$url_protocol, {
    sorted_classes <- sort(rv$class2use)
    updateSelectizeInput(session, "new_class_quick",
                         choices = sorted_classes,
                         selected = character(0))
  })
  
  # Store all sample names and their classification status
  all_samples <- reactiveVal(character())
  classified_samples <- reactiveVal(character())  # Auto-classified (CSV, H5, or classifier MAT)
  annotated_samples <- reactiveVal(character())   # Manually annotated (has .mat in output folder)
  # Store mapping of sample names to classifier MAT/H5 file paths
  classifier_mat_files <- reactiveVal(list())
  classifier_h5_files <- reactiveVal(list())
  # Path maps: sample_name -> full file path (discovered during scan)
  roi_path_map <- reactiveVal(list())
  csv_path_map <- reactiveVal(list())
  # Trigger for forcing a folder rescan
  rescan_trigger <- reactiveVal(0)
  # Timestamp of last sync (updated after scan completes)
  last_sync_time <- reactiveVal(NULL)
  
  # Get classes in current classifications that are NOT in class2use
  unmatched_classes <- reactive({
    if (is.null(rv$classifications) || is.null(rv$class2use)) {
      return(character())
    }
    classification_classes <- unique(rv$classifications$class_name)
    unmatched <- setdiff(classification_classes, rv$class2use)
    # Also remove "unclassified" as it's always valid
    unmatched <- setdiff(unmatched, "unclassified")
    unmatched
  })
  
  # Build class filter choices with unmatched classes marked
  build_class_filter_choices <- function(classes) {
    unmatched <- unmatched_classes()
    # Create display names with warning for unmatched classes
    display_names <- sapply(classes, function(cls) {
      if (cls %in% unmatched) {
        paste0("\u26A0 ", cls)  # ⚠ Warning symbol for unmatched
      } else {
        cls
      }
    })
    c("All" = "all", setNames(classes, display_names))
  }
  
  # ============================================================================
  # Settings Modal
  # ============================================================================
  
  observeEvent(input$settings_btn, {
    showModal(modalDialog(
      title = "Settings",
      size = "l",
      easyClose = TRUE,

      # ── Data Source ─────────────────────────────────────────────
      h5("Data Source"),

      radioButtons("cfg_data_source", NULL,
                   choices = c("Local Folders" = "local",
                               "IFCB Dashboard" = "dashboard"),
                   selected = config$data_source, inline = TRUE),

      # Dashboard settings (visible when data source is "dashboard")
      conditionalPanel(
        condition = "input.cfg_data_source == 'dashboard'",
        textInput("cfg_dashboard_url", "Dashboard URL",
                  value = config$dashboard_url, width = "100%",
                  placeholder = "https://habon-ifcb.whoi.edu/timeline?dataset=tangosund"),
        tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
                   "Enter the IFCB Dashboard URL. Dataset can be specified via ?dataset=name."),
        checkboxInput("cfg_dashboard_autoclass", "Use dashboard auto-classifications",
                      value = config$dashboard_autoclass),
        tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                   "When enabled, downloads auto-classification scores from the dashboard for validation mode."),

        # Advanced download settings (collapsible)
        tags$details(
          tags$summary(style = "cursor: pointer; margin-bottom: 10px; color: #666;",
                       "Advanced Download Settings"),
          fluidRow(
            column(6, numericInput("cfg_dashboard_parallel_downloads",
                                   "Parallel Downloads",
                                   value = config$dashboard_parallel_downloads,
                                   min = 1, max = 20, step = 1)),
            column(6, numericInput("cfg_dashboard_sleep_time",
                                   "Sleep Time (seconds)",
                                   value = config$dashboard_sleep_time,
                                   min = 0, max = 30, step = 0.5))
          ),
          fluidRow(
            column(6, numericInput("cfg_dashboard_multi_timeout",
                                   "Download Timeout (seconds)",
                                   value = config$dashboard_multi_timeout,
                                   min = 10, max = 600, step = 10)),
            column(6, numericInput("cfg_dashboard_max_retries",
                                   "Max Retries",
                                   value = config$dashboard_max_retries,
                                   min = 1, max = 10, step = 1))
          ),
          tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                     "Settings for zip/ADC/autoclass downloads from the dashboard.")
        )
      ),

      # ── Classification Folder (both modes) ──────────────────────────
      h5("Classification"),

      div(
        style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 5px;",
        div(style = "flex: 1;",
            textInput("cfg_csv_folder", "Classification Folder (CSV/H5/MAT)",
                      value = config$csv_folder, width = "100%")),
        shinyDirButton("browse_csv_folder", "Browse", "Select Classification Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),

      conditionalPanel(
        condition = "input.cfg_data_source == 'dashboard'",
        tags$small(class = "text-muted", style = "display: block; margin-bottom: 5px;",
                   "Optional. Use local classification files instead of dashboard auto-classifications.")
      ),

      checkboxInput("cfg_use_threshold", "Apply classification threshold",
                    value = config$use_threshold),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                 "When enabled, classifications below the confidence threshold are marked as 'unclassified'."),

      # ── Folder Paths (local mode only) ──────────────────────────────
      conditionalPanel(
        condition = "input.cfg_data_source == 'local'",

        h5("ROI Data"),

        div(
          style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 15px;",
          div(style = "flex: 1;",
              textInput("cfg_roi_folder", "ROI Data Folder",
                        value = config$roi_folder, width = "100%")),
          shinyDirButton("browse_roi_folder", "Browse", "Select ROI Data Folder",
                         class = "btn-outline-secondary", style = "margin-bottom: 15px;")
        )
      ),

      div(
        style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 5px;",
        div(style = "flex: 1;",
            textInput("cfg_output_folder", "Output Folder (MAT/statistics)",
                      value = config$output_folder, width = "100%")),
        shinyDirButton("browse_output_folder", "Browse", "Select Output Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),

      checkboxInput("cfg_export_statistics", "Export validation statistics",
                    value = config$export_statistics),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                 "Write per-sample CSV files with classification accuracy to the output folder."),

      div(
        style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 15px;",
        div(style = "flex: 1;",
            textInput("cfg_png_output_folder", "PNG Output Folder",
                      value = config$png_output_folder, width = "100%")),
        shinyDirButton("browse_png_folder", "Browse", "Select PNG Output Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),

      div(
        style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 5px;",
        div(style = "flex: 1;",
            textInput("cfg_db_folder", "Database Folder (SQLite)",
                      value = config$db_folder, width = "100%")),
        shinyDirButton("browse_db_folder", "Browse", "Select Database Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                 "Must be a local drive. SQLite databases are",
                 tags$a(href = "https://www.sqlite.org/useovernet.html", target = "_blank",
                        "not safe on network filesystems"),
                 "due to unreliable file locking."),

      checkboxInput("cfg_auto_sync", "Sync folders automatically on startup",
                    value = config$auto_sync),
      tags$small(class = "text-muted",
                 "When disabled, the app loads from cache on startup. Use the sync button to update manually."),

      hr(),

      # ── Class List ────────────────────────────────────────────────
      h5("Class List"),

      fileInput("class2use_file", "Load class list file (.mat or .txt)",
                accept = c(".mat", ".txt")),

      div(
        style = "display: flex; align-items: center; gap: 10px;",
        actionButton("open_class_editor", "Edit Class List",
                     icon = icon("list"), class = "btn-outline-primary"),
        tags$span(class = "text-muted", style = "font-size: 12px;",
                  textOutput("class_count_text", inline = TRUE))
      ),

      hr(),

      # ── Annotation Storage ────────────────────────────────────────
      h5("Annotation Storage"),

      selectInput("cfg_save_format", "Storage Format",
                  choices = c(
                    "SQLite (recommended)" = "sqlite",
                    "MAT file (MATLAB compatible)" = "mat",
                    "Both SQLite and MAT" = "both"
                  ),
                  selected = config$save_format),
      tags$small(class = "text-muted",
                 "SQLite works out of the box. MAT files require Python and are only needed for ifcb-analysis compatibility."),

      hr(),

      # ── Import / Export ────────────────────────────────────────────
      h5("Import / Export"),

      tags$label("Import to SQLite", style = "font-weight: 600; display: block; margin-bottom: 5px;"),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 5px;",
        actionButton("import_mat_to_db_btn", ".mat \u2192 SQLite",
                     icon = icon("file-import"), class = "btn-outline-secondary btn-sm"),
        actionButton("import_png_to_db_btn", "PNG \u2192 SQLite",
                     icon = icon("file-import"), class = "btn-outline-secondary btn-sm")
      ),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
                 "Bulk import annotated samples from .mat files or PNG class folders."),

      tags$label("Export from SQLite", style = "font-weight: 600; display: block; margin-bottom: 5px;"),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 5px;",
        actionButton("export_db_to_mat_btn", "SQLite \u2192 .mat",
                     icon = icon("file-export"), class = "btn-outline-secondary btn-sm"),
        actionButton("export_db_to_png_btn", "SQLite \u2192 PNG",
                     icon = icon("file-export"), class = "btn-outline-secondary btn-sm")
      ),
      div(
        style = "margin-bottom: 5px;",
        textInput("cfg_skip_class_png", "Skip class in PNG export",
                  value = if (nzchar(config$skip_class_png)) config$skip_class_png
                          else if (!is.null(rv$class2use) && length(rv$class2use) > 0) rv$class2use[1]
                          else "",
                  width = "250px"),
        tags$small(class = "text-muted",
                   "Images with this class are excluded from PNG export.",
                   "Leave empty to export all classes.")
      ),

      hr(),

      # ── Live Prediction ─────────────────────────────────────────
      h5("Live Prediction"),

      textInput("cfg_gradio_url", "Gradio API URL",
                value = config$gradio_url, width = "100%",
                placeholder = "https://irfcb-classify.hf.space"),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
                 "Enter Gradio API URL for CNN classification. Example: https://irfcb-classify.hf.space"),

      selectInput("cfg_prediction_model", "Prediction Model",
                  choices = if (nzchar(config$prediction_model)) config$prediction_model else NULL,
                  selected = if (nzchar(config$prediction_model)) config$prediction_model else NULL,
                  width = "100%"),
      tags$small(class = "text-muted", style = "display: block; margin-bottom: 15px;",
                 "Select a CNN model for classification. Models are fetched from the Gradio API."),

      hr(),

      # ── IFCB Options ──────────────────────────────────────────────
      h5("IFCB Options"),

      div(
        style = "display: flex; gap: 10px; align-items: center; margin-top: 10px;",
        numericInput("cfg_pixels_per_micron", "Pixels per micron",
                     value = config$pixels_per_micron, min = 0.1, max = 20, step = 0.1,
                     width = "150px"),
        tags$small(class = "text-muted", "Scale calibration for the measuring tool. IFCB default: 3.4 px/\u00b5m.")
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_settings", "Apply", class = "btn-outline-primary"),
        actionButton("save_settings", "Save Settings", class = "btn-primary")
      )
    ))
  })
  
  # shinyFiles directory browser setup - dynamic roots so the dialog
  # opens at the path currently typed in the corresponding text field
  shinyDirChoose(input, "browse_csv_folder",
                 roots = make_dynamic_roots("cfg_csv_folder"), session = session)
  shinyDirChoose(input, "browse_roi_folder",
                 roots = make_dynamic_roots("cfg_roi_folder"), session = session)
  shinyDirChoose(input, "browse_output_folder",
                 roots = make_dynamic_roots("cfg_output_folder"), session = session)
  shinyDirChoose(input, "browse_db_folder",
                 roots = make_dynamic_roots("cfg_db_folder"), session = session)
  shinyDirChoose(input, "browse_png_folder",
                 roots = make_dynamic_roots("cfg_png_output_folder"), session = session)
  
  # Browse button observers - parse selection and update text input
  observeEvent(input$browse_csv_folder, {
    if (!is.integer(input$browse_csv_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_csv_folder), input$browse_csv_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_csv_folder", value = as.character(folder))
      }
    }
  })
  
  observeEvent(input$browse_roi_folder, {
    if (!is.integer(input$browse_roi_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_roi_folder), input$browse_roi_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_roi_folder", value = as.character(folder))
      }
    }
  })
  
  observeEvent(input$browse_output_folder, {
    if (!is.integer(input$browse_output_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_output_folder), input$browse_output_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_output_folder", value = as.character(folder))
      }
    }
  })
  
  observeEvent(input$browse_db_folder, {
    if (!is.integer(input$browse_db_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_db_folder), input$browse_db_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_db_folder", value = as.character(folder))
      }
    }
  })

  observeEvent(input$browse_png_folder, {
    if (!is.integer(input$browse_png_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_png_output_folder), input$browse_png_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_png_output_folder", value = as.character(folder))
      }
    }
  })
  
  # Fetch available models when Gradio URL changes in settings (debounced)
  gradio_url_debounced <- debounce(reactive(input$cfg_gradio_url), 1500)

  observeEvent(gradio_url_debounced(), {
    url <- gradio_url_debounced()
    if (is.null(url) || !nzchar(url)) {
      updateSelectInput(session, "cfg_prediction_model", choices = character(0))
      return()
    }
    tryCatch({
      models <- iRfcb::ifcb_classify_models(url)
      if (length(models) > 0) {
        # Preserve current selection if still valid
        current <- config$prediction_model
        selected <- if (nzchar(current) && current %in% models) current else models[1]
        updateSelectInput(session, "cfg_prediction_model",
                          choices = models, selected = selected)
      } else {
        updateSelectInput(session, "cfg_prediction_model", choices = character(0))
        showNotification("No models found at the provided URL.", type = "warning")
      }
    }, error = function(e) {
      updateSelectInput(session, "cfg_prediction_model", choices = character(0))
      showNotification(paste("Could not fetch models:", e$message), type = "error")
    })
  })

  # Class count display

  output$class_count_text <- renderText({
    if (is.null(rv$class2use)) {
      "No class list loaded"
    } else {
      paste(length(rv$class2use), "classes loaded")
    }
  })
  
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
        textInput("new_class_name", "Add new class:", placeholder = "Enter new class name"),
        actionButton("add_class_btn", "Add to End", class = "btn-sm btn-outline-primary")
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
  observeEvent(input$sort_by_id, {
    rv$class_sort_mode <- "id"
  })
  
  observeEvent(input$sort_alpha, {
    rv$class_sort_mode <- "alpha"
  })
  
  # Render class list with indices
  output$class_list_display <- renderUI({
    # Handle empty/NULL class list
    if (is.null(rv$class2use) || length(rv$class2use) == 0) {
      return(tags$div(
        style = "color: #666; font-style: italic;",
        "No classes defined yet. Add classes using the form below or edit the text area."
      ))
    }

    classes <- rv$class2use
    indices <- seq_along(classes)

    # Get image counts per class from the database
    counts <- tryCatch({
      db_path <- get_db_path(config$db_folder)
      if (file.exists(db_path)) {
        classes_df <- list_classes_db(db_path)
        setNames(classes_df$count, classes_df$class_name)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    # Create data frame for sorting
    df <- data.frame(idx = indices, cls = classes, stringsAsFactors = FALSE)

    if (rv$class_sort_mode == "alpha") {
      df <- df[order(df$cls), ]
    }

    class_lines <- mapply(function(idx, cls) {
      count <- if (!is.null(counts) && cls %in% names(counts)) counts[[cls]] else 0L
      tags$div(sprintf("%3d: %s (%d)", idx, cls, count))
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
    
    # Handle NULL class list (starting from scratch)
    current_classes <- if (is.null(rv$class2use)) character(0) else rv$class2use
    
    if (new_class %in% current_classes) {
      showNotification("Class already exists", type = "warning")
      return()
    }
    
    # Add to class list
    rv$class2use <- c(current_classes, new_class)
    
    # If no class2use_path exists (created from scratch), create a temp file
    if (is.null(rv$class2use_path)) {
      temp_class_file <- file.path(tempdir(), "class2use_temp.txt")
      writeLines(rv$class2use, temp_class_file)
      rv$class2use_path <- temp_class_file
    } else {
      # Update the temp file if it exists
      if (grepl("class2use_temp", rv$class2use_path)) {
        writeLines(rv$class2use, rv$class2use_path)
      }
    }
    
    # Update the text area
    updateTextAreaInput(session, "class_list_edit",
                        value = paste(rv$class2use, collapse = "\n"))
    updateTextInput(session, "new_class_name", value = "")
    
    # Update relabel dropdown
    sorted_classes <- sort(rv$class2use)
    updateSelectizeInput(session, "new_class_quick",
                         choices = sorted_classes,
                         selected = character(0))
    
    showNotification(paste("Added class:", new_class), type = "message")
  })
  
  # Apply class list changes from text area
  observeEvent(input$apply_class_changes, {
    # Get text from textarea (may be empty when starting from scratch)
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
        type = "warning",
        duration = 5
      )
    }
    
    rv$class2use <- new_classes
    
    # If no class2use_path exists (created from scratch), create a temp file
    # This is needed for saving annotations
    if (is.null(rv$class2use_path)) {
      temp_class_file <- file.path(tempdir(), "class2use_temp.txt")
      writeLines(new_classes, temp_class_file)
      rv$class2use_path <- temp_class_file
      showNotification(
        "Class list created. Remember to save it using 'Save as .mat' or 'Save as .txt' for future use.",
        type = "message",
        duration = 8
      )
    } else if (grepl("class2use_temp", rv$class2use_path)) {
      # Update the temp file if it exists
      writeLines(new_classes, rv$class2use_path)
    }
    
    # Update relabel dropdown
    sorted_classes <- sort(rv$class2use)
    updateSelectizeInput(session, "new_class_quick",
                         choices = sorted_classes,
                         selected = character(0))
    
    showNotification(paste("Applied", length(new_classes), "classes"), type = "message")
  })
  
  # Download class2use as .mat file
  output$save_class2use_mat <- downloadHandler(
    filename = function() {
      "class2use.mat"
    },
    content = function(file) {
      ifcb_create_class2use(rv$class2use, file)
    }
  )
  
  # Download class2use as .txt file
  output$save_class2use_txt <- downloadHandler(
    filename = function() {
      "class2use.txt"
    },
    content = function(file) {
      writeLines(rv$class2use, file)
    }
  )
  
  apply_settings <- function(close_modal = FALSE, notification = "Settings saved.") {
    # Check if folder paths actually changed (to avoid spurious resets)
    roi_changed <- !identical(config$roi_folder, input$cfg_roi_folder)
    csv_changed <- !identical(config$csv_folder, input$cfg_csv_folder)
    data_source_changed <- !identical(config$data_source, input$cfg_data_source)
    dashboard_url_changed <- !identical(config$dashboard_url, input$cfg_dashboard_url)
    paths_changed <- roi_changed || csv_changed || data_source_changed || dashboard_url_changed

    config$csv_folder <- input$cfg_csv_folder
    config$roi_folder <- input$cfg_roi_folder
    config$output_folder <- input$cfg_output_folder
    config$png_output_folder <- input$cfg_png_output_folder
    config$db_folder <- input$cfg_db_folder
    config$use_threshold <- input$cfg_use_threshold
    config$pixels_per_micron <- input$cfg_pixels_per_micron
    config$auto_sync <- input$cfg_auto_sync
    config$save_format <- input$cfg_save_format
    config$export_statistics <- input$cfg_export_statistics
    config$skip_class_png <- input$cfg_skip_class_png
    config$data_source <- input$cfg_data_source
    config$dashboard_url <- input$cfg_dashboard_url
    config$dashboard_autoclass <- input$cfg_dashboard_autoclass
    config$dashboard_parallel_downloads <- if (!is.null(input$cfg_dashboard_parallel_downloads)) input$cfg_dashboard_parallel_downloads else 5
    config$dashboard_sleep_time <- if (!is.null(input$cfg_dashboard_sleep_time)) input$cfg_dashboard_sleep_time else 2
    config$dashboard_multi_timeout <- if (!is.null(input$cfg_dashboard_multi_timeout)) input$cfg_dashboard_multi_timeout else 120
    config$dashboard_max_retries <- if (!is.null(input$cfg_dashboard_max_retries)) input$cfg_dashboard_max_retries else 3
    config$gradio_url <- input$cfg_gradio_url
    config$prediction_model <- input$cfg_prediction_model

    # Persist settings to file for next session
    # python_venv_path is kept from config (set via run_app() or previous save)
    persist_settings(list(
      csv_folder = input$cfg_csv_folder,
      roi_folder = input$cfg_roi_folder,
      output_folder = input$cfg_output_folder,
      png_output_folder = input$cfg_png_output_folder,
      db_folder = input$cfg_db_folder,
      use_threshold = input$cfg_use_threshold,
      pixels_per_micron = input$cfg_pixels_per_micron,
      auto_sync = input$cfg_auto_sync,
      save_format = input$cfg_save_format,
      export_statistics = input$cfg_export_statistics,
      skip_class_png = input$cfg_skip_class_png,
      class2use_path = rv$class2use_path,
      python_venv_path = config$python_venv_path,
      data_source = input$cfg_data_source,
      dashboard_url = input$cfg_dashboard_url,
      dashboard_autoclass = input$cfg_dashboard_autoclass,
      dashboard_parallel_downloads = config$dashboard_parallel_downloads,
      dashboard_sleep_time = config$dashboard_sleep_time,
      dashboard_multi_timeout = config$dashboard_multi_timeout,
      dashboard_max_retries = config$dashboard_max_retries,
      gradio_url = input$cfg_gradio_url,
      prediction_model = input$cfg_prediction_model
    ))

    if (close_modal) {
      removeModal()
    }
    showNotification(notification, type = "message")

    # Only trigger sample rescan if folder paths actually changed
    if (paths_changed) {
      cache_path <- get_file_index_path()
      if (file.exists(cache_path)) {
        file.remove(cache_path)
      }
      rescan_trigger(rescan_trigger() + 1)
    }
  }

  observeEvent(input$apply_settings, {
    apply_settings(close_modal = FALSE, notification = "Settings applied.")
  })

  observeEvent(input$save_settings, {
    apply_settings(close_modal = TRUE, notification = "Settings saved.")
  })

  # ============================================================================
  # LIVE PREDICTION
  # ============================================================================

  # Render predict button conditionally
  output$predict_btn_ui <- renderUI({
    has_config <- nzchar(config$gradio_url) && nzchar(config$prediction_model)
    has_sample <- !is.null(rv$current_sample)

    if (!has_config) return(NULL)

    actionButton("predict_btn", "Predict",
                 icon = icon("robot"),
                 class = if (has_sample) "btn-info" else "btn-outline-secondary",
                 width = "100%",
                 disabled = if (!has_sample) "disabled" else NULL)
  })

  # Predict observer
  observeEvent(input$predict_btn, {
    req(rv$current_sample, rv$temp_png_folder, config$gradio_url, config$prediction_model)

    # Validate Gradio URL scheme
    if (!grepl("^https?://", config$gradio_url)) {
      showNotification("Gradio URL must start with http:// or https://", type = "error")
      return()
    }

    # Get PNG files from temp folder (PNGs live in a sample-name subfolder)
    png_folder <- file.path(rv$temp_png_folder, rv$current_sample)
    if (!dir.exists(png_folder)) {
      # Fallback: try the temp folder directly (e.g. dashboard cache)
      png_folder <- rv$temp_png_folder
    }
    png_files <- list.files(png_folder, pattern = "\\.png$", full.names = TRUE)
    if (length(png_files) == 0) {
      showNotification("No PNG images found in the loaded sample.", type = "warning")
      return()
    }

    # Identify images manually reclassified by the user (skip these)
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

    # Filter to only non-reclassified images
    all_filenames <- basename(png_files)
    files_to_predict <- png_files[!all_filenames %in% manually_changed]

    if (length(files_to_predict) == 0) {
      showNotification("All images have been manually reclassified. Nothing to predict.",
                       type = "message")
      return()
    }

    # Run classification with per-image progress
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

    # Apply threshold logic
    if (!config$use_threshold && "class_name_auto" %in% names(predictions)) {
      predictions$class_name <- predictions$class_name_auto
    }

    # Merge predictions into rv$classifications
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

    # Update original_classifications so predictions become the new baseline
    rv$original_classifications <- rv$classifications

    # Switch to validation mode since we now have auto-classifications
    rv$is_annotation_mode <- FALSE

    # Add any new classes from predictions to class2use
    new_classes <- setdiff(unique(predictions$class_name), rv$class2use)
    new_classes <- new_classes[!is.na(new_classes) & nzchar(new_classes)]
    if (length(new_classes) > 0) {
      rv$class2use <- c(rv$class2use, new_classes)
      # Update relabel dropdown
      sorted_classes <- sort(rv$class2use)
      updateSelectizeInput(session, "new_class_quick",
                           choices = sorted_classes,
                           selected = character(0))
    }

    # Update class filter dropdown
    available_classes <- sort(unique(rv$classifications$class_name))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices(available_classes),
                      selected = "all")

    n_predicted <- nrow(predictions)
    n_skipped <- length(manually_changed)
    msg <- paste0("Predicted ", n_predicted, " images.")
    if (n_skipped > 0) {
      msg <- paste0(msg, " Skipped ", n_skipped, " manually reclassified images.")
    }
    showNotification(msg, type = "message", duration = 8)
  })

  # Import .mat -> SQLite bulk handler
  observeEvent(input$import_mat_to_db_btn, {
    if (is.null(config$output_folder) || config$output_folder == "") {
      showNotification("Output folder is not configured. Set it in Settings first.",
                       type = "error")
      return()
    }
    db_path <- get_db_path(config$db_folder)
    annotator <- if (!is.null(input$annotator_name) && nzchar(input$annotator_name)) {
      input$annotator_name
    } else {
      "imported"
    }

    withProgress(message = "Importing .mat files to SQLite...", {
      result <- import_all_mat_to_db(config$output_folder, db_path, annotator)
    })

    showNotification(
      sprintf("Import complete: %d imported, %d failed, %d skipped (already in DB).",
              result$success, result$failed, result$skipped),
      type = if (result$failed > 0) "warning" else "message",
      duration = 8
    )

    # Trigger file index rescan to update sample list
    if (result$success > 0) {
      rescan_trigger(rescan_trigger() + 1)
    }
  })

  # Export SQLite -> .mat bulk handler: show confirmation dialog first
  observeEvent(input$export_db_to_mat_btn, {
    if (is.null(config$output_folder) || config$output_folder == "") {
      showNotification("Output folder is not configured. Set it in Settings first.",
                       type = "error")
      return()
    }
    if (!python_available) {
      showNotification("Python is not available. Export to .mat requires Python with scipy.",
                       type = "error")
      return()
    }

    showModal(modalDialog(
      title = "Confirm .mat export",
      p("This will export all annotated samples from the SQLite database as",
        tags$strong(".mat files"), "into:"),
      tags$code(config$output_folder),
      tags$br(), tags$br(),
      p(tags$strong("Existing .mat files in this folder will be overwritten"),
        "and cannot be recovered. Make sure you have a backup if needed."),
      p("Do you want to continue?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_export_mat_btn", "Export", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  # Confirmed: run the actual export
  observeEvent(input$confirm_export_mat_btn, {
    removeModal()

    db_path <- get_db_path(config$db_folder)

    withProgress(message = "Exporting SQLite to .mat files...", {
      result <- export_all_db_to_mat(db_path, config$output_folder)
    })

    showNotification(
      sprintf("Export complete: %d exported, %d failed.", result$success, result$failed),
      type = if (result$failed > 0) "warning" else "message",
      duration = 8
    )
  })

  # Export SQLite -> PNG bulk handler
  observeEvent(input$export_db_to_png_btn, {
    if (is.null(config$png_output_folder) || config$png_output_folder == "") {
      showNotification("PNG Output Folder is not configured. Set it in Settings first.",
                       type = "error")
      return()
    }

    db_path <- get_db_path(config$db_folder)
    is_dashboard <- identical(config$data_source, "dashboard")

    if (is_dashboard) {
      # Dashboard mode: copy PNGs from cache instead of extracting from ROI
      samples <- list_annotated_samples_db(db_path)
      if (length(samples) == 0) {
        showNotification("No annotated samples in database.", type = "warning")
        return()
      }

      cache_dir <- get_dashboard_cache_dir()
      parsed <- parse_dashboard_url(config$dashboard_url)

      skip <- if (!is.null(config$skip_class_png) && nzchar(config$skip_class_png)) {
        config$skip_class_png
      } else {
        NULL
      }

      counts <- list(success = 0L, failed = 0L, skipped = 0L)

      # Bulk download all needed zip archives in one batched call
      # (uses parallel_downloads internally, much faster than one-at-a-time)
      withProgress(message = "Downloading images from dashboard...", value = 0, {
        cached_samples <- download_dashboard_images_bulk(
          parsed$base_url, samples, cache_dir,
          parallel_downloads = config$dashboard_parallel_downloads,
          sleep_time = config$dashboard_sleep_time,
          multi_timeout = config$dashboard_multi_timeout,
          max_retries = config$dashboard_max_retries)
      })

      withProgress(message = "Copying PNGs to class folders...", value = 0, {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)

        for (sn in samples) {
          rows <- dbGetQuery(con,
            "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
            params = list(sn))

          if (nrow(rows) == 0) {
            counts$skipped <- counts$skipped + 1L
            next
          }

          if (!is.null(skip)) {
            rows <- rows[!rows$class_name %in% skip, ]
            if (nrow(rows) == 0) next
          }

          src_dir <- file.path(cache_dir, sn, sn)
          if (!sn %in% cached_samples || !dir.exists(src_dir)) {
            counts$skipped <- counts$skipped + 1L
            next
          }

          ok <- tryCatch({
            for (cls in unique(rows$class_name)) {
              cls_rois <- rows$roi_number[rows$class_name == cls]
              dest <- file.path(config$png_output_folder, cls)
              dir.create(dest, recursive = TRUE, showWarnings = FALSE)
              for (rn in cls_rois) {
                fname <- sprintf("%s_%05d.png", sn, rn)
                src <- file.path(src_dir, fname)
                if (file.exists(src)) {
                  file.copy(src, file.path(dest, fname), overwrite = TRUE)
                }
              }
            }
            TRUE
          }, error = function(e) FALSE)

          if (isTRUE(ok)) {
            counts$success <- counts$success + 1L
          } else {
            counts$failed <- counts$failed + 1L
          }

          incProgress(1 / length(samples))
        }
      })

      showNotification(
        sprintf("PNG export complete: %d exported, %d failed, %d skipped.",
                counts$success, counts$failed, counts$skipped),
        type = if (counts$failed > 0) "warning" else "message",
        duration = 8
      )
    } else {
      # Local mode: extract from ROI files
      if (is.null(config$output_folder) || config$output_folder == "") {
        showNotification("Output folder is not configured. Set it in Settings first.",
                         type = "error")
        return()
      }

      current_roi_map <- roi_path_map()

      if (length(current_roi_map) == 0) {
        showNotification("No ROI file index available. Click Sync first.",
                         type = "error")
        return()
      }

      skip <- if (!is.null(config$skip_class_png) && nzchar(config$skip_class_png)) {
        config$skip_class_png
      } else {
        NULL
      }

      withProgress(message = "Exporting PNGs from SQLite...", {
        result <- export_all_db_to_png(db_path, config$png_output_folder,
                                       current_roi_map, skip_class = skip)
      })

      showNotification(
        sprintf("PNG export complete: %d exported, %d failed, %d skipped (ROI not found).",
                result$success, result$failed, result$skipped),
        type = if (result$failed > 0) "warning" else "message",
        duration = 8
      )
    }
  })

  # ============================================================================
  # Import PNG -> SQLite (multi-step flow)
  # ============================================================================

  # Temporary storage for the PNG import flow
  png_import_state <- reactiveValues(
    scan_result = NULL,
    class_mapping = NULL,
    png_folder = NULL
  )

  # Step 1: Button click - show folder picker dialog
  observeEvent(input$import_png_to_db_btn, {
    showModal(modalDialog(
      title = "Import PNG \u2192 SQLite",
      size = "m",
      easyClose = TRUE,
      div(
        style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 15px;",
        div(style = "flex: 1;",
            textInput("cfg_png_import_folder", "PNG Import Folder",
                      value = config$png_output_folder, width = "100%")),
        shinyDirButton("browse_png_import_folder", "Browse", "Select PNG Import Folder",
                       class = "btn-outline-secondary", style = "margin-bottom: 15px;")
      ),
      tags$small(class = "text-muted",
                 "Select a folder containing PNG images organized in class-name subfolders.",
                 "Folder names follow iRfcb convention (trailing _NNN suffix is stripped)."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("scan_png_folder_btn", "Scan Folder", class = "btn-primary")
      )
    ))
  })

  # Set up shinyFiles dir chooser for PNG import folder
  shinyDirChoose(input, "browse_png_import_folder",
                 roots = make_dynamic_roots("cfg_png_import_folder"), session = session)

  observeEvent(input$browse_png_import_folder, {
    if (!is.integer(input$browse_png_import_folder)) {
      folder <- parseDirPath(get_browse_volumes(input$cfg_png_import_folder),
                             input$browse_png_import_folder)
      if (length(folder) > 0) {
        updateTextInput(session, "cfg_png_import_folder", value = as.character(folder))
      }
    }
  })

  # Step 1b: Scan the folder
  observeEvent(input$scan_png_folder_btn, {
    png_folder <- input$cfg_png_import_folder
    if (is.null(png_folder) || !nzchar(png_folder) || !dir.exists(png_folder)) {
      showNotification("Please select a valid folder.", type = "error")
      return()
    }

    withProgress(message = "Scanning PNG folder...", {
      scan_result <- tryCatch(
        scan_png_class_folder(png_folder),
        error = function(e) {
          showNotification(paste("Scan failed:", e$message), type = "error")
          NULL
        }
      )
    })

    if (is.null(scan_result) || nrow(scan_result$annotations) == 0) {
      showNotification("No valid PNG images found in the selected folder.", type = "error")
      return()
    }

    png_import_state$scan_result <- scan_result
    png_import_state$png_folder <- png_folder
    png_import_state$class_mapping <- NULL

    # Check for class mismatches
    unmatched <- setdiff(scan_result$classes_found, rv$class2use)

    if (length(unmatched) > 0) {
      # Step 2: Show class mapping dialog
      mapping_inputs <- lapply(unmatched, function(cls) {
        choices <- c("Add as new" = "__add_new__", setNames(rv$class2use, rv$class2use))
        div(
          style = "display: flex; gap: 10px; align-items: center; margin-bottom: 5px;",
          tags$span(style = "flex: 0 0 200px; font-weight: bold;", cls),
          div(style = "flex: 1;",
              selectInput(paste0("png_map_", gsub("[^a-zA-Z0-9]", "_", cls)),
                          label = NULL, choices = choices, width = "100%"))
        )
      })

      showModal(modalDialog(
        title = "Map Unmatched Classes",
        size = "l",
        easyClose = FALSE,
        p(sprintf("Found %d class(es) not in your current class list. Map them to existing classes or add as new:",
                  length(unmatched))),
        p(sprintf("Scanned: %d images across %d samples in %d classes.",
                  nrow(scan_result$annotations),
                  length(scan_result$sample_names),
                  length(scan_result$classes_found))),
        hr(),
        tagList(mapping_inputs),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_png_class_mapping_btn", "Continue", class = "btn-primary")
        )
      ))
    } else {
      # No mismatches - proceed to overwrite check (Step 3)
      check_png_import_overwrite()
    }
  })

  # Step 2b: Process class mapping
  observeEvent(input$confirm_png_class_mapping_btn, {
    scan_result <- png_import_state$scan_result
    if (is.null(scan_result)) return()

    unmatched <- setdiff(scan_result$classes_found, rv$class2use)
    class_mapping <- character()
    new_classes <- character()

    for (cls in unmatched) {
      input_id <- paste0("png_map_", gsub("[^a-zA-Z0-9]", "_", cls))
      mapped_to <- input[[input_id]]
      if (!is.null(mapped_to) && mapped_to != "__add_new__") {
        class_mapping[cls] <- mapped_to
      } else {
        new_classes <- c(new_classes, cls)
      }
    }

    png_import_state$class_mapping <- if (length(class_mapping) > 0) class_mapping else NULL

    # Add new classes to class2use
    if (length(new_classes) > 0) {
      rv$class2use <- c(rv$class2use, new_classes)
      showNotification(
        sprintf("Added %d new class(es): %s", length(new_classes),
                paste(new_classes, collapse = ", ")),
        type = "message", duration = 5
      )
    }

    check_png_import_overwrite()
  })

  # Step 3: Check for existing samples and show overwrite warning
  check_png_import_overwrite <- function() {
    scan_result <- png_import_state$scan_result
    if (is.null(scan_result)) return()

    db_path <- get_db_path(config$db_folder)
    existing <- list_annotated_samples_db(db_path)
    overlapping <- intersect(scan_result$sample_names, existing)

    if (length(overlapping) > 0) {
      showModal(modalDialog(
        title = "Overwrite Existing Samples?",
        size = "m",
        easyClose = FALSE,
        p(sprintf("%d of %d samples already have annotations in the database and will be overwritten:",
                  length(overlapping), length(scan_result$sample_names))),
        div(
          style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px;",
          tags$ul(lapply(overlapping, tags$li))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_png_overwrite_btn", "Overwrite & Import",
                       class = "btn-warning")
        )
      ))
    } else {
      # No overlap - proceed directly
      execute_png_import()
    }
  }

  observeEvent(input$confirm_png_overwrite_btn, {
    execute_png_import()
  })

  # Step 4: Execute the import
  execute_png_import <- function() {
    removeModal()

    scan_result <- png_import_state$scan_result
    png_folder <- png_import_state$png_folder
    if (is.null(scan_result) || is.null(png_folder)) return()

    db_path <- get_db_path(config$db_folder)
    annotator <- if (!is.null(input$annotator_name) && nzchar(input$annotator_name)) {
      input$annotator_name
    } else {
      "imported"
    }

    withProgress(message = "Importing PNG annotations to SQLite...", {
      result <- import_png_folder_to_db(
        png_folder, db_path, rv$class2use,
        class_mapping = png_import_state$class_mapping,
        annotator = annotator
      )
    })

    showNotification(
      sprintf("PNG import complete: %d samples imported, %d failed.",
              result$success, result$failed),
      type = if (result$failed > 0) "warning" else "message",
      duration = 8
    )

    # Trigger rescan to refresh sample list
    if (result$success > 0) {
      rescan_trigger(rescan_trigger() + 1)
    }

    # Clean up state
    png_import_state$scan_result <- NULL
    png_import_state$class_mapping <- NULL
    png_import_state$png_folder <- NULL
  }

  # ============================================================================
  # UI Outputs - Warnings and Indicators
  # ============================================================================
  
  output$cache_age_text <- renderUI({
    invalidateLater(60000)
    ts <- last_sync_time()
    if (!is.null(ts)) {
      cache_time <- as.POSIXct(ts)
      age_secs <- as.numeric(difftime(Sys.time(), cache_time, units = "secs"))
      age_text <- if (age_secs < 60) {
        "just now"
      } else if (age_secs < 3600) {
        paste0(round(age_secs / 60), " min ago")
      } else if (age_secs < 86400) {
        paste0(round(age_secs / 3600), " hours ago")
      } else {
        paste0(round(age_secs / 86400), " days ago")
      }
      div(
        style = "font-size: 11px; color: #999; margin-bottom: 5px;",
        icon("clock", style = "margin-right: 3px;"),
        paste0("Last synced ", age_text)
      )
    }
  })
  
  output$python_warning <- renderUI({
    needs_python <- config$save_format %in% c("mat", "both")
    if (!python_available && needs_python) {
      div(
        class = "alert alert-warning",
        style = "margin-top: 10px; padding: 8px; font-size: 12px;",
        "Python not available. Saving .mat files will not work. ",
        "Switch to SQLite storage format in Settings, or install Python: ",
        "run ifcb_py_install() in R console. ",
        "MAT files are only needed for ",
        tags$a(href = "https://github.com/hsosik/ifcb-analysis", target = "_blank", "ifcb-analysis"),
        " compatibility."
      )
    }
  })
  
  # Send pixels_per_micron to JavaScript for measure tool
  observe({
    session$sendCustomMessage("updatePixelsPerMicron", config$pixels_per_micron)
  })
  
  # Loading overlay (shown during load/save operations)
  output$loading_overlay <- renderUI({
    if (rv$is_loading) {
      div(
        class = "loading-overlay",
        div(
          style = "text-align: center;",
          div(class = "spinner-border text-primary", role = "status",
              style = "width: 3rem; height: 3rem;"),
          div(style = "margin-top: 10px; font-weight: bold;", "Loading...")
        )
      )
    }
  })
  
  # Dynamic title with mode-based navbar coloring
  output$dynamic_title <- renderUI({
    # Determine mode class for navbar styling
    mode_class <- if (rv$class_review_mode) {
      "navbar-mode-class-review"
    } else if (is.null(rv$current_sample)) {
      "navbar-mode-none"
    } else if (rv$is_annotation_mode) {
      "navbar-mode-annotation"
    } else {
      "navbar-mode-validation"
    }

    all_mode_classes <- "navbar-mode-none navbar-mode-annotation navbar-mode-validation navbar-mode-class-review"

    # Add JavaScript to apply class to navbar
    tagList(
      tags$script(HTML(sprintf("
        $(document).ready(function() {
          $('.navbar').removeClass('%s').addClass('%s');
        });
        $('.navbar').removeClass('%s').addClass('%s');
      ", all_mode_classes, mode_class, all_mode_classes, mode_class))),
      div(
        style = "display: flex; align-items: baseline; gap: 20px;",
        actionLink(
          "reset_to_home",
          label = span(paste("ClassiPyR", app_version), style = "color: white; font-size: 18px;"),
          style = "text-decoration: none;",
          title = "Click to unload sample and return to initial state"
        ),
        div(style = "display: inline;", uiOutput("mode_indicator_inline", inline = TRUE))
      )
    )
  })
  
  output$mode_indicator_inline <- renderUI({
    if (rv$class_review_mode) {
      n_images <- if (!is.null(rv$classifications)) nrow(rv$classifications) else 0
      n_samples <- length(rv$class_review_samples)
      n_changed <- 0L
      if (!is.null(rv$class_review_original) && !is.null(rv$classifications)) {
        n_changed <- sum(rv$classifications$class_name != rv$class_review_original$class_name)
      }
      change_text <- if (n_changed > 0) sprintf(", %d changed", n_changed) else ""

      span(
        style = "font-size: 14px; color: white;",
        tags$span(style = "font-weight: bold; margin-right: 8px;", "CLASS REVIEW"),
        tags$span(rv$class_review_class),
        tags$span(
          style = "margin-left: 10px; opacity: 0.9;",
          sprintf("(%d images, %d samples%s)", n_images, n_samples, change_text)
        )
      )
    } else if (is.null(rv$current_sample)) {
      span(
        style = "font-size: 14px; color: white; font-weight: 500;",
        "No sample loaded"
      )
    } else if (rv$is_annotation_mode) {
      # Show progress for annotation mode
      total <- nrow(rv$classifications)
      classified <- sum(rv$classifications$class_name != "unclassified")
      pct <- round((classified / total) * 100)
      
      # Build mode switch button if both modes available
      switch_btn <- if (rv$has_both_modes) {
        actionLink(
          "switch_to_validation",
          label = tags$span(style = "font-size: 12px; color: white;", "\u2192 Validation"),
          style = "margin-left: 10px;"
        )
      }
      
      span(
        style = "font-size: 14px; color: white;",
        tags$span(
          style = "font-weight: bold; margin-right: 8px;",
          "ANNOTATION"
        ),
        tags$span(rv$current_sample),
        tags$span(
          style = "margin-left: 10px; opacity: 0.9;",
          sprintf("(%d/%d - %d%%)", classified, total, pct)
        ),
        switch_btn
      )
    } else {
      # Show accuracy for validation mode
      stats <- calculate_stats()
      
      # Build mode switch button if both modes available
      switch_btn <- if (rv$has_both_modes) {
        actionLink(
          "switch_to_annotation",
          label = tags$span(style = "font-size: 12px; color: white;", "\u2192 Manual"),
          style = "margin-left: 10px;"
        )
      }
      
      span(
        style = "font-size: 14px; color: white;",
        tags$span(
          style = "font-weight: bold; margin-right: 8px;",
          "VALIDATION"
        ),
        tags$span(rv$current_sample),
        tags$span(
          style = "margin-left: 10px; opacity: 0.9;",
          sprintf("(%d changed - %.1f%% acc)", stats$incorrect_classifications, stats$accuracy * 100)
        ),
        switch_btn
      )
    }
  })
  
  # Shared helper: switch current sample to validation mode
  do_switch_to_validation <- function() {
    req(rv$current_sample, rv$has_both_modes)

    sample_name <- rv$current_sample
    roi_path <- roi_path_map()[[sample_name]]
    if (is.null(roi_path)) {
      showNotification("ROI file not found for this sample", type = "error")
      return()
    }
    adc_path <- sub("\\.roi$", ".adc", roi_path)

    # Find classification source (CSV > H5 > MAT)
    csv_path <- find_csv_file(sample_name)
    classifier_h5_path <- find_classifier_h5(sample_name)
    classifier_mat_path <- classifier_mat_files()[[sample_name]]

    if (!is.null(csv_path)) {
      classifications <- load_from_csv(csv_path, use_threshold = config$use_threshold)
      showNotification("Switched to Validation mode (CSV)", type = "message")
    } else if (!is.null(classifier_h5_path)) {
      roi_dims <- read_roi_dimensions(adc_path)
      classifications <- load_from_h5(
        classifier_h5_path, sample_name, roi_dims,
        use_threshold = config$use_threshold
      )
      showNotification("Switched to Validation mode (H5)", type = "message")
    } else if (!is.null(classifier_mat_path)) {
      roi_dims <- read_roi_dimensions(adc_path)
      classifications <- load_from_classifier_mat(
        classifier_mat_path, sample_name, rv$class2use, roi_dims,
        use_threshold = config$use_threshold
      )
      showNotification("Switched to Validation mode (MAT)", type = "message")
    } else {
      showNotification("No classification data available", type = "warning")
      return()
    }

    rv$original_classifications <- classifications
    rv$classifications <- classifications
    rv$is_annotation_mode <- FALSE
    rv$using_manual_mode <- FALSE
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    # Update class filter dropdown
    available_classes <- sort(unique(classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)),
                      selected = "all")
  }

  # Switch from annotation mode to validation mode
  observeEvent(input$switch_to_validation, {
    do_switch_to_validation()
  })
  
  # Switch from validation mode to annotation mode
  observeEvent(input$switch_to_annotation, {
    req(rv$current_sample, rv$has_both_modes)

    sample_name <- rv$current_sample
    roi_path <- roi_path_map()[[sample_name]]
    if (is.null(roi_path)) {
      showNotification("ROI file not found for this sample", type = "error")
      return()
    }
    adc_path <- sub("\\.roi$", ".adc", roi_path)

    # Try SQLite first, then .mat
    db_path <- get_db_path(config$db_folder)
    annotation_mat_path <- file.path(config$output_folder, paste0(sample_name, ".mat"))
    has_db <- sample_name %in% list_annotated_samples_db(db_path)
    has_mat <- file.exists(annotation_mat_path)

    if (has_db || has_mat) {
      roi_dims <- read_roi_dimensions(adc_path)
      if (has_db) {
        classifications <- load_from_db(db_path, sample_name, roi_dims)
      } else {
        classifications <- load_from_mat(annotation_mat_path, sample_name, rv$class2use, roi_dims)
      }

      rv$original_classifications <- classifications
      rv$classifications <- classifications
      rv$is_annotation_mode <- TRUE
      rv$using_manual_mode <- TRUE
      rv$selected_images <- character()
      rv$current_page <- 1
      rv$changes_log <- create_empty_changes_log()

      # Update class filter dropdown
      available_classes <- sort(unique(classifications$class_name))
      unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
      display_names <- sapply(available_classes, function(cls) {
        if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
      })
      updateSelectInput(session, "class_filter",
                        choices = c("All" = "all", setNames(available_classes, display_names)),
                        selected = "all")

      showNotification("Switched to Manual annotation mode", type = "message")
    } else {
      showNotification("No manual annotation file found", type = "warning")
    }
  })
  
  # ============================================================================
  # Class List Loading
  # ============================================================================
  
  # Try to load class2use file on startup (from persisted path or default locations)
  observe({
    # Skip if we've already loaded a class list from a file
    if (!is.null(rv$class2use_path)) return()
    
    # Only load from persisted settings path (no auto-loading from root directory)
    class2use_path <- saved_settings$class2use_path
    # Validate: must be non-null, non-NA, non-empty single string
    if (is.null(class2use_path) || length(class2use_path) != 1 ||
        isTRUE(is.na(class2use_path)) || !nzchar(class2use_path)) {
      return()  # Start with empty class list
    }
    
    path_to_try <- class2use_path
    if (file.exists(path_to_try)) {
      tryCatch({
        classes <- load_class_list(path_to_try)
        
        if (!"unclassified" %in% classes) {
          classes <- c("unclassified", classes)
        }
        
        rv$class2use <- classes
        rv$class2use_path <- path_to_try
        
        sorted_classes <- sort(rv$class2use)
        updateSelectizeInput(session, "new_class_quick",
                             choices = sorted_classes,
                             selected = character(0))
        
        showNotification(
          paste("Auto-loaded", length(rv$class2use), "classes from", basename(path_to_try)),
          type = "message"
        )
      }, error = function(e) {
        message("Could not load class list from saved path: ", e$message)
      })
    }
  })
  
  # Load uploaded class2use file (from settings modal)
  observeEvent(input$class2use_file, {
    req(input$class2use_file)
    
    tryCatch({
      classes <- load_class_list(input$class2use_file$datapath)
      
      if (!"unclassified" %in% classes) {
        classes <- c("unclassified", classes)
      }
      
      rv$class2use <- classes
      
      # Copy to user config directory so it survives package reinstalls
      ext <- tools::file_ext(input$class2use_file$name)
      persistent_path <- file.path(get_config_dir(), paste0("class2use_saved.", ext))
      file.copy(input$class2use_file$datapath, persistent_path, overwrite = TRUE)
      rv$class2use_path <- persistent_path
      
      # Persist settings immediately
      persist_settings(list(
        csv_folder = config$csv_folder,
        roi_folder = config$roi_folder,
        output_folder = config$output_folder,
        png_output_folder = config$png_output_folder,
        db_folder = config$db_folder,
        use_threshold = config$use_threshold,
        class2use_path = persistent_path
      ))
      
      sorted_classes <- sort(rv$class2use)
      updateSelectizeInput(session, "new_class_quick",
                           choices = sorted_classes,
                           selected = character(0))
      
      showNotification(paste("Loaded", length(rv$class2use), "classes"), type = "message")
      
      # Force filter update to work around Shiny reactivity quirk
      update_month_choices()
      update_sample_list()
    }, error = function(e) {
      showNotification(paste("Error loading class list:", e$message), type = "error")
    })
  })
  
  # ============================================================================
  # Sample Discovery and Selection
  # ============================================================================
  
  # Helper: populate reactive values from file index data
  populate_from_index <- function(index_data) {
    sample_names <- as.character(index_data$sample_names)
    if (length(sample_names) == 0) return(FALSE)
    
    safe_char <- function(x) as.character(if (is.null(x)) character() else x)
    safe_list <- function(x) as.list(if (is.null(x)) list() else x)
    
    all_samples(sample_names)
    classified_samples(safe_char(index_data$classified_samples))
    annotated_samples(safe_char(index_data$annotated_samples))
    roi_path_map(safe_list(index_data$roi_path_map))
    csv_path_map(safe_list(index_data$csv_path_map))
    classifier_mat_files(safe_list(index_data$classifier_mat_files))
    classifier_h5_files(safe_list(index_data$classifier_h5_files))

    years <- unique(substr(sample_names, 2, 5))
    years <- sort(years)
    first_year <- if (length(years) > 0) years[1] else "all"
    updateSelectInput(session, "year_select",
                      choices = c("All" = "all", setNames(years, years)),
                      selected = first_year)

    instruments <- unique(sub(".*_", "", sample_names))
    instruments <- sort(instruments)
    updateSelectInput(session, "instrument_select",
                      choices = c("All" = "all", setNames(instruments, instruments)),
                      selected = "all")
    
    last_sync_time(index_data$timestamp)
    TRUE
  }
  
  # Scan for available ROI files and classification files (CSV, H5, and MAT)
  # Uses disk cache for fast startup on subsequent launches
  observe({
    rescan_trigger()  # Force dependency on rescan trigger
    data_source <- config$data_source

    if (identical(data_source, "dashboard")) {
      # Dashboard mode: fetch sample list from remote API
      dashboard_url <- config$dashboard_url
      if (is.null(dashboard_url) || !nzchar(dashboard_url)) return()

      # Try loading from cache first
      cached <- load_file_index()
      cache_valid <- !is.null(cached) &&
        identical(cached$data_source, "dashboard") &&
        identical(cached$dashboard_url, dashboard_url)

      if (cache_valid) {
        populate_from_index(cached)
        return()
      }

      # When auto-sync is disabled, load stale cache if available
      auto_sync <- config$auto_sync
      if (!isTRUE(auto_sync) && !is.null(cached) &&
          identical(cached$data_source, "dashboard")) {
        populate_from_index(cached)
        return()
      }

      withProgress(message = "Fetching dashboard samples...", value = 0, {
        result <- rescan_file_index(
          data_source = "dashboard",
          dashboard_url = dashboard_url,
          db_folder = config$db_folder,
          verbose = FALSE
        )
      })

      if (!is.null(result)) {
        populate_from_index(result)
      }
    } else {
      # Local mode: scan folders
      roi_folder <- config$roi_folder
      csv_folder <- config$csv_folder
      output_folder <- config$output_folder

      # Validate folder paths before using them
      roi_valid <- !is.null(roi_folder) && length(roi_folder) == 1 && !isTRUE(is.na(roi_folder)) && nzchar(roi_folder) && dir.exists(roi_folder)

      if (!roi_valid) return()

      # Try loading from cache first
      cached <- load_file_index()
      cache_valid <- !is.null(cached) &&
        identical(cached$roi_folder, roi_folder) &&
        identical(cached$csv_folder, csv_folder) &&
        identical(cached$output_folder, output_folder)

      if (cache_valid) {
        populate_from_index(cached)
        return()
      }

      # When auto-sync is disabled, load stale cache if available
      auto_sync <- config$auto_sync
      if (!isTRUE(auto_sync) && !is.null(cached)) {
        populate_from_index(cached)
        return()
      }

      # Full scan with progress indicator (delegates to rescan_file_index)
      withProgress(message = "Syncing folders...", value = 0, {
        result <- rescan_file_index(
          roi_folder = roi_folder,
          csv_folder = csv_folder,
          output_folder = output_folder,
          verbose = FALSE
        )
      })

      if (!is.null(result)) {
        populate_from_index(result)
      }
    }
  })
  
  # Update cache when annotations are saved (so status is correct after restart)
  observe({
    annotated <- annotated_samples()
    cached <- load_file_index()
    if (!is.null(cached) && !identical(as.character(cached$annotated_samples), annotated)) {
      cached$annotated_samples <- annotated
      cached$timestamp <- as.character(Sys.time())
      save_file_index(cached)
    }
  })
  
  # Rescan button: invalidate cache and trigger fresh scan
  observeEvent(input$rescan_folders, {
    cache_path <- get_file_index_path()
    if (file.exists(cache_path)) {
      file.remove(cache_path)
    }
    rescan_trigger(rescan_trigger() + 1)
  })
  
  # Helper function to update month and instrument choices based on year selection
  update_month_choices <- function() {
    samples <- all_samples()
    if (length(samples) == 0) return()

    year_val <- input$year_select

    if (!is.null(year_val) && year_val != "all") {
      # Filter to selected year
      year_pattern <- paste0("^D", year_val)
      year_samples <- samples[grepl(year_pattern, samples)]

      # Extract months (characters 6-7 of sample name: DYYYYMMDD...)
      months <- unique(substr(year_samples, 6, 7))
      months <- sort(months)

      # Create month names
      month_names <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",
                       "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug",
                       "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
      month_labels <- month_names[months]

      # Auto-select first month for better UX with large sample lists
      first_month <- if (length(months) > 0) months[1] else "all"
      updateSelectInput(session, "month_select",
                        choices = c("All" = "all", setNames(months, month_labels)),
                        selected = first_month)

      # Update instrument choices for selected year
      instruments <- unique(sub(".*_", "", year_samples))
      instruments <- sort(instruments)
      current_instrument <- input$instrument_select
      selected_instrument <- if (!is.null(current_instrument) && current_instrument %in% instruments) {
        current_instrument
      } else {
        "all"
      }
      updateSelectInput(session, "instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = selected_instrument)
    } else {
      updateSelectInput(session, "month_select",
                        choices = c("All" = "all"),
                        selected = "all")

      # Show all instruments when no year filter
      instruments <- unique(sub(".*_", "", samples))
      instruments <- sort(instruments)
      updateSelectInput(session, "instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = "all")
    }
  }
  
  # Helper function to update sample list based on filters
  update_sample_list <- function() {
    samples <- all_samples()
    if (length(samples) == 0) return()
    
    year_val <- input$year_select
    month_val <- input$month_select
    status_val <- input$sample_status_filter
    classified <- classified_samples()
    annotated <- annotated_samples()
    
    # Filter by year
    if (!is.null(year_val) && year_val != "all") {
      year_pattern <- paste0("^D", year_val)
      samples <- samples[grepl(year_pattern, samples)]
    }
    
    # Filter by month
    if (!is.null(month_val) && month_val != "all") {
      month_pattern <- paste0("^D\\d{4}", month_val)
      samples <- samples[grepl(month_pattern, samples)]
    }

    # Filter by instrument
    instrument_val <- input$instrument_select
    if (!is.null(instrument_val) && instrument_val != "all") {
      instrument_pattern <- paste0("_", instrument_val, "$")
      samples <- samples[grepl(instrument_pattern, samples)]
    }

    # Filter by classification status
    if (!is.null(status_val)) {
      if (status_val == "classified") {
        # Show only auto-classified (not manually annotated)
        samples <- samples[samples %in% classified & !samples %in% annotated]
      } else if (status_val == "unclassified") {
        # Show only unannotated (neither classified nor manually annotated)
        samples <- samples[!samples %in% classified & !samples %in% annotated]
      } else if (status_val == "annotated") {
        # Show only manually annotated
        samples <- samples[samples %in% annotated]
      }
    }
    
    samples <- sort(samples)
    
    if (length(samples) > 0) {
      is_annotated <- samples %in% annotated
      is_classified <- samples %in% classified
      # Show both symbols if sample has both manual annotation AND classification results
      # Symbols: ✎ (pencil) for manual, ✓ (check) for auto-classified, * for unannotated
      display_names <- sapply(samples, function(s) {
        has_manual <- s %in% annotated
        has_classified <- s %in% classified
        if (has_manual && has_classified) {
          paste0(s, "\u270E\u2713")  # ✎✓ Both manual and classified
        } else if (has_manual) {
          paste0(s, "\u270E")  # ✎ Pencil - manually annotated only
        } else if (has_classified) {
          paste0(s, "\u2713")  # ✓ Checkmark - auto-classified only
        } else {
          paste0(s, "*")       # * Asterisk - unannotated
        }
      })
      choices <- setNames(samples, display_names)
    } else {
      choices <- character(0)
    }
    
    # Determine which sample should be selected:
    # 1. Use pending_sample_select if set (from navigation buttons)
    # 2. Otherwise use rv$current_sample (the loaded sample)
    # 3. Otherwise no selection
    current_selection <- if (!is.null(rv$pending_sample_select)) {
      rv$pending_sample_select
    } else {
      rv$current_sample
    }
    
    selected_value <- if (!is.null(current_selection) && current_selection %in% samples) {
      current_selection
    } else {
      character(0)  # No selection if current sample not in filtered list
    }
    
    # Clear the pending selection after using it
    rv$pending_sample_select <- NULL
    
    # Update sample dropdown with server-side processing for large datasets
    updateSelectizeInput(session, "sample_select", choices = choices,
                         selected = selected_value,
                         options = list(placeholder = "Select sample..."),
                         server = TRUE)
  }
  
  # Update the display text for current sample in dropdown to show pencil symbol
  # Uses JavaScript to modify just the displayed text without rebuilding dropdown
  update_current_sample_status <- function(sample_name) {
    classified <- classified_samples()
    annotated <- annotated_samples()
    
    has_manual <- sample_name %in% annotated
    has_classified <- sample_name %in% classified
    
    # Determine the new display suffix
    new_suffix <- if (has_manual && has_classified) {
      "\u270E\u2713"  # ✎✓ Both
      
    } else if (has_manual) {
      "\u270E"        # ✎ Pencil
    } else if (has_classified) {
      "\u2713"        # ✓ Checkmark
    } else {
      "*"             # Asterisk
    }
    
    new_display <- paste0(sample_name, new_suffix)

    # Escape backslashes and single quotes for safe JS string interpolation
    safe_js_string <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", x))
    safe_name <- safe_js_string(sample_name)
    safe_display <- safe_js_string(new_display)

    # Use JavaScript to update the selectize display
    shinyjs::runjs(sprintf(
      "var $select = $('#sample_select').selectize();
     if ($select.length && $select[0].selectize) {
       var selectize = $select[0].selectize;
       var currentVal = selectize.getValue();
       if (currentVal === '%s') {
         // Update the option's label
         var option = selectize.options[currentVal];
         if (option) {
           option.label = '%s';
           selectize.updateOption(currentVal, option);
           // Also update the displayed item
           selectize.$control.find('.item').text('%s');
         }
       }
     }",
      safe_name, safe_display, safe_display
    ))
  }
  
  # Simple observeEvent handlers that call the helper functions
  # These are more robust than using list() or reactive() in observeEvent
  observeEvent(input$year_select, {
    update_month_choices()
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$month_select, {
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$instrument_select, {
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$sample_status_filter, {
    update_sample_list()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Also trigger on sample list changes (when paths change)
  observeEvent(all_samples(), {
    update_month_choices()
    update_sample_list()
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  
  # Helper function to get filtered sample list
  get_filtered_samples <- function() {
    samples <- all_samples()
    classified <- classified_samples()
    annotated <- annotated_samples()
    
    if (!is.null(input$year_select) && input$year_select != "all") {
      year_pattern <- paste0("^D", input$year_select)
      samples <- samples[grepl(year_pattern, samples)]
    }
    
    if (!is.null(input$month_select) && input$month_select != "all") {
      month_pattern <- paste0("^D\\d{4}", input$month_select)
      samples <- samples[grepl(month_pattern, samples)]
    }

    if (!is.null(input$instrument_select) && input$instrument_select != "all") {
      instrument_pattern <- paste0("_", input$instrument_select, "$")
      samples <- samples[grepl(instrument_pattern, samples)]
    }

    if (!is.null(input$sample_status_filter)) {
      if (input$sample_status_filter == "classified") {
        samples <- samples[samples %in% classified & !samples %in% annotated]
      } else if (input$sample_status_filter == "unclassified") {
        samples <- samples[!samples %in% classified & !samples %in% annotated]
      } else if (input$sample_status_filter == "annotated") {
        samples <- samples[samples %in% annotated]
      }
    }
    
    sort(samples)
  }
  
  # Random sample selection
  observeEvent(input$random_sample, {
    samples <- get_filtered_samples()
    
    if (length(samples) > 0) {
      random_sample <- sample(samples, 1)
      rv$pending_sample_select <- random_sample
      updateSelectizeInput(session, "sample_select", selected = random_sample)
    } else {
      showNotification("No samples match current filters", type = "warning")
    }
  })
  
  # ============================================================================
  # Helper: Find classification file (CSV, H5, or classifier MAT)
  # ============================================================================
  
  find_csv_file <- function(sample_name) {
    csv_map <- csv_path_map()
    path <- csv_map[[sample_name]]
    if (!is.null(path) && file.exists(path)) {
      return(path)
    }
    return(NULL)
  }
  
  # Find classifier MAT file for a sample
  find_classifier_mat <- function(sample_name) {
    mat_map <- classifier_mat_files()
    if (sample_name %in% names(mat_map)) {
      return(mat_map[[sample_name]])
    }
    return(NULL)
  }

  # Find classifier H5 file for a sample
  find_classifier_h5 <- function(sample_name) {
    h5_map <- classifier_h5_files()
    if (sample_name %in% names(h5_map)) {
      return(h5_map[[sample_name]])
    }
    return(NULL)
  }
  
  # ============================================================================
  # Sample Loading
  # ============================================================================
  
  # Save current sample to cache with LRU eviction
  save_to_cache <- function() {
    if (!is.null(rv$current_sample) && !is.null(rv$classifications)) {
      # Enforce cache size limit (LRU eviction)
      if (length(rv$session_cache) >= MAX_CACHED_SAMPLES &&
          !(rv$current_sample %in% names(rv$session_cache))) {
        # Remove oldest entry (first in list)
        oldest_sample <- names(rv$session_cache)[1]
        rv$session_cache[[oldest_sample]] <- NULL
      }
      
      rv$session_cache[[rv$current_sample]] <- list(
        classifications = rv$classifications,
        original_classifications = rv$original_classifications,
        changes_log = rv$changes_log,
        is_annotation_mode = rv$is_annotation_mode
      )
      
      # Auto-save annotations
      tryCatch({
        roi_path_for_save <- roi_path_map()[[rv$current_sample]]
        adc_folder_for_save <- if (!is.null(roi_path_for_save)) dirname(roi_path_for_save) else NULL

        # In dashboard mode, fall back to SQLite if no ADC folder
        save_fmt_for_autosave <- config$save_format
        if (identical(config$data_source, "dashboard") && is.null(adc_folder_for_save)) {
          if (save_fmt_for_autosave %in% c("mat", "both")) {
            save_fmt_for_autosave <- "sqlite"
          }
        }

        saved <- save_sample_annotations(
          sample_name = rv$current_sample,
          classifications = rv$classifications,
          original_classifications = rv$original_classifications,
          changes_log = rv$changes_log,
          temp_png_folder = rv$temp_png_folder,
          output_folder = config$output_folder,
          png_output_folder = config$png_output_folder,
          roi_folder = config$roi_folder,
          class2use_path = rv$class2use_path,
          class2use = rv$class2use,
          annotator = input$annotator_name,
          adc_folder = adc_folder_for_save,
          save_format = save_fmt_for_autosave,
          db_folder = config$db_folder,
          export_statistics = config$export_statistics
        )
        # Only update annotated samples list if changes were actually saved
        if (isTRUE(saved)) {
          current_annotated <- annotated_samples()
          if (!rv$current_sample %in% current_annotated) {
            annotated_samples(c(current_annotated, rv$current_sample))
            update_current_sample_status(rv$current_sample)
          }
        }
      }, error = function(e) {
        showNotification(paste("Auto-save failed:", e$message), type = "error")
      })
    }
  }
  
  # Main sample loading function
  load_sample_data <- function(sample_name) {
    req(rv$class2use)

    is_dashboard <- identical(config$data_source, "dashboard")

    # --- Dashboard mode ---
    if (is_dashboard) {
      # Check session cache first
      if (sample_name %in% names(rv$session_cache)) {
        return(load_from_cache(sample_name, NULL))
      }

      tryCatch({
        parsed <- parse_dashboard_url(config$dashboard_url)
        cache_dir <- get_dashboard_cache_dir()
        db_path <- get_db_path(config$db_folder)
        has_db_annotation <- sample_name %in% list_annotated_samples_db(db_path)

        # Download PNG images from dashboard
        png_folder <- withProgress(message = "Downloading images...", {
          download_dashboard_images(parsed$base_url, sample_name, cache_dir,
                                    parallel_downloads = config$dashboard_parallel_downloads,
                                    sleep_time = config$dashboard_sleep_time,
                                    multi_timeout = config$dashboard_multi_timeout,
                                    max_retries = config$dashboard_max_retries)
        })

        if (is.null(png_folder)) {
          showNotification(paste("Failed to download images for:", sample_name), type = "error")
          return(FALSE)
        }

        # Download ADC on demand for dimensions (graceful fallback to NA)
        adc_path <- download_dashboard_adc(parsed$base_url, sample_name, cache_dir,
                                           parallel_downloads = config$dashboard_parallel_downloads,
                                           sleep_time = config$dashboard_sleep_time,
                                           multi_timeout = config$dashboard_multi_timeout,
                                           max_retries = config$dashboard_max_retries)
        roi_dims <- if (!is.null(adc_path) && file.exists(adc_path)) {
          tryCatch(read_roi_dimensions(adc_path), error = function(e) NULL)
        } else {
          NULL
        }

        mode_message <- NULL
        classifications <- NULL

        if (has_db_annotation) {
          # Load from existing DB annotations
          if (is.null(roi_dims)) {
            # Build minimal roi_dims from PNG files
            png_files <- list.files(file.path(png_folder, sample_name), pattern = "\\.png$")
            roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", png_files))
            roi_dims <- data.frame(
              roi_number = roi_numbers,
              width = NA_real_, height = NA_real_, area = NA_real_
            )
          }
          classifications <- load_from_db(db_path, sample_name, roi_dims)
          rv$is_annotation_mode <- TRUE
          rv$has_both_modes <- isTRUE(config$dashboard_autoclass)
          rv$using_manual_mode <- TRUE
          mode_message <- if (rv$has_both_modes) "Manual mode (switch available)" else "Resumed"

        }

        # Try local classification files if csv_folder is configured
        if (is.null(classifications)) {
          csv_folder <- config$csv_folder
          has_csv_folder <- !is.null(csv_folder) && nzchar(csv_folder) && dir.exists(csv_folder)

          if (has_csv_folder) {
            # Search for classification files in csv_folder (recursive)
            local_csv <- list.files(csv_folder, pattern = paste0("^", sample_name, "\\.csv$"),
                                    recursive = TRUE, full.names = TRUE)
            local_h5 <- list.files(csv_folder, pattern = paste0("^", sample_name, ".*\\.h5$"),
                                   recursive = TRUE, full.names = TRUE)
            local_mat <- list.files(csv_folder, pattern = paste0("^", sample_name, ".*\\.mat$"),
                                    recursive = TRUE, full.names = TRUE)

            if (length(local_csv) > 0) {
              classifications <- load_from_csv(local_csv[1], use_threshold = config$use_threshold)
              rv$is_annotation_mode <- FALSE
              rv$has_both_modes <- FALSE
              rv$using_manual_mode <- FALSE
              threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
              mode_message <- paste0("Validation mode (Local CSV, ", threshold_text, ")")
            } else if (length(local_h5) > 0) {
              classifications <- load_from_h5(
                local_h5[1], sample_name, roi_dims,
                use_threshold = config$use_threshold
              )
              rv$is_annotation_mode <- FALSE
              rv$has_both_modes <- FALSE
              rv$using_manual_mode <- FALSE
              threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
              mode_message <- paste0("Validation mode (Local H5, ", threshold_text, ")")
            } else if (length(local_mat) > 0) {
              classifications <- load_from_classifier_mat(
                local_mat[1], sample_name, rv$class2use, roi_dims,
                use_threshold = config$use_threshold
              )
              rv$is_annotation_mode <- FALSE
              rv$has_both_modes <- FALSE
              rv$using_manual_mode <- FALSE
              threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
              mode_message <- paste0("Validation mode (Local MAT, ", threshold_text, ")")
            }
          }
        }

        if (is.null(classifications) && isTRUE(config$dashboard_autoclass)) {
          # Try dashboard autoclass for validation mode (fallback)
          autoclass <- withProgress(message = "Downloading auto-classifications...", {
            download_dashboard_autoclass(parsed$base_url, sample_name, cache_dir,
                                        parallel_downloads = config$dashboard_parallel_downloads,
                                        sleep_time = config$dashboard_sleep_time,
                                        multi_timeout = config$dashboard_multi_timeout,
                                        max_retries = config$dashboard_max_retries)
          })

          if (!is.null(autoclass) && nrow(autoclass) > 0) {
            # Merge with ROI dimensions if available
            if (!is.null(roi_dims)) {
              roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", autoclass$file_name))
              dim_data <- lapply(roi_numbers, function(rn) {
                idx <- which(roi_dims$roi_number == rn)
                if (length(idx) > 0) {
                  list(width = roi_dims$width[idx], height = roi_dims$height[idx],
                       area = roi_dims$area[idx])
                } else {
                  list(width = NA_real_, height = NA_real_, area = NA_real_)
                }
              })
              autoclass$width <- vapply(dim_data, `[[`, numeric(1), "width")
              autoclass$height <- vapply(dim_data, `[[`, numeric(1), "height")
              autoclass$roi_area <- vapply(dim_data, `[[`, numeric(1), "area")
            } else {
              autoclass$width <- NA_real_
              autoclass$height <- NA_real_
              autoclass$roi_area <- NA_real_
            }

            classifications <- autoclass
            rv$is_annotation_mode <- FALSE
            rv$has_both_modes <- FALSE
            rv$using_manual_mode <- FALSE
            mode_message <- "Validation mode (Dashboard autoclass)"
          }
        }

        if (is.null(classifications)) {
          # NEW ANNOTATION - no existing annotations or autoclass
          png_files <- list.files(file.path(png_folder, sample_name), pattern = "\\.png$")
          if (length(png_files) == 0) {
            showNotification(paste("No images found for:", sample_name), type = "error")
            return(FALSE)
          }

          roi_numbers <- as.integer(gsub(".*_(\\d+)\\.png$", "\\1", png_files))

          if (is.null(roi_dims)) {
            roi_dims <- data.frame(
              roi_number = roi_numbers,
              width = NA_real_, height = NA_real_, area = NA_real_
            )
          }

          classifications <- create_new_classifications(sample_name, roi_dims)
          rv$is_annotation_mode <- TRUE
          rv$has_both_modes <- FALSE
          rv$using_manual_mode <- TRUE
          mode_message <- "New annotation"
        }

        # Store state
        rv$original_classifications <- classifications
        rv$classifications <- classifications
        rv$current_sample <- sample_name
        rv$selected_images <- character()
        rv$current_page <- 1
        rv$changes_log <- create_empty_changes_log()

        # Set temp_png_folder to the parent (preserves path structure for gallery)
        if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder) &&
            !startsWith(rv$temp_png_folder, cache_dir)) {
          unlink(rv$temp_png_folder, recursive = TRUE)
        }
        rv$temp_png_folder <- png_folder

        # Filter to actually extracted PNGs
        extracted_folder <- file.path(png_folder, sample_name)
        if (dir.exists(extracted_folder)) {
          extracted_files <- list.files(extracted_folder, pattern = "\\.png$")
          rv$classifications <- rv$classifications[rv$classifications$file_name %in% extracted_files, ]
          rv$original_classifications <- rv$original_classifications[
            rv$original_classifications$file_name %in% extracted_files, ]
        }

        # Update class filter
        available_classes <- sort(unique(rv$classifications$class_name))
        unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
        display_names <- sapply(available_classes, function(cls) {
          if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
        })
        updateSelectInput(session, "class_filter",
                          choices = c("All" = "all", setNames(available_classes, display_names)))

        if (!is.null(mode_message)) {
          actual_count <- nrow(rv$classifications)
          showNotification(paste0(mode_message, ": ", actual_count, " images"), type = "message")
        }

        return(TRUE)
      }, error = function(e) {
        showNotification(paste("Error loading sample:", e$message), type = "error")
        return(FALSE)
      })
    }

    # --- Local mode ---

    # Find classification files (priority: CSV > H5 > MAT)
    csv_path <- find_csv_file(sample_name)
    classifier_h5_path <- find_classifier_h5(sample_name)
    classifier_mat_path <- find_classifier_mat(sample_name)
    has_csv <- !is.null(csv_path)
    has_classifier_h5 <- !is.null(classifier_h5_path)
    has_classifier_mat <- !is.null(classifier_mat_path)

    # Use discovered paths from scan (supports any folder structure)
    roi_path <- roi_path_map()[[sample_name]]
    if (is.null(roi_path) || !file.exists(roi_path)) {
      showNotification(paste("ROI file not found for:", sample_name), type = "error")
      return(FALSE)
    }
    adc_path <- sub("\\.roi$", ".adc", roi_path)

    # Check session cache first
    if (sample_name %in% names(rv$session_cache)) {
      return(load_from_cache(sample_name, roi_path))
    }

    tryCatch({
      annotation_mat_path <- file.path(config$output_folder, paste0(sample_name, ".mat"))
      db_path <- get_db_path(config$db_folder)
      has_db_annotation <- sample_name %in% list_annotated_samples_db(db_path)
      has_mat_annotation <- file.exists(annotation_mat_path)
      has_existing_annotation <- has_db_annotation || has_mat_annotation
      has_classification <- has_csv || has_classifier_h5 || has_classifier_mat

      # Track if sample has both modes available
      rv$has_both_modes <- has_existing_annotation && has_classification
      rv$using_manual_mode <- has_existing_annotation  # Default to manual if available

      # Variable to hold mode message for notification (shown after filtering)
      mode_message <- NULL

      # Priority: Manual annotation > Classification > New annotation
      # Within manual annotations: SQLite first (faster), then .mat fallback
      if (has_existing_annotation) {
        # ANNOTATION MODE - from existing manual annotation (priority when both exist)
        if (!file.exists(adc_path)) {
          showNotification(paste("ADC file not found:", adc_path), type = "error")
          return(FALSE)
        }

        roi_dims <- read_roi_dimensions(adc_path)

        if (has_db_annotation) {
          classifications <- load_from_db(db_path, sample_name, roi_dims)
        } else {
          classifications <- load_from_mat(annotation_mat_path, sample_name, rv$class2use, roi_dims)
        }
        rv$is_annotation_mode <- TRUE

        mode_message <- if (rv$has_both_modes) "Manual mode (switch available)" else "Resumed"

      } else if (has_csv) {
        # VALIDATION MODE - from CSV
        classifications <- load_from_csv(csv_path, use_threshold = config$use_threshold)
        rv$is_annotation_mode <- FALSE
        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
        mode_message <- paste0("Validation mode (CSV, ", threshold_text, ")")

      } else if (has_classifier_h5) {
        # VALIDATION MODE - from classifier H5 file
        if (!file.exists(adc_path)) {
          showNotification(paste("ADC file not found:", adc_path), type = "error")
          return(FALSE)
        }

        roi_dims <- read_roi_dimensions(adc_path)
        classifications <- load_from_h5(
          classifier_h5_path, sample_name, roi_dims,
          use_threshold = config$use_threshold
        )
        rv$is_annotation_mode <- FALSE

        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
        mode_message <- paste0("Validation mode (H5, ", threshold_text, ")")

      } else if (has_classifier_mat) {
        # VALIDATION MODE - from classifier MAT file
        if (!file.exists(adc_path)) {
          showNotification(paste("ADC file not found:", adc_path), type = "error")
          return(FALSE)
        }

        roi_dims <- read_roi_dimensions(adc_path)
        classifications <- load_from_classifier_mat(
          classifier_mat_path, sample_name, rv$class2use, roi_dims,
          use_threshold = config$use_threshold
        )
        rv$is_annotation_mode <- FALSE

        threshold_text <- if (config$use_threshold) "with threshold" else "without threshold"
        mode_message <- paste0("Validation mode (MAT, ", threshold_text, ")")

      } else {
        # NEW ANNOTATION
        if (!file.exists(adc_path)) {
          showNotification(paste("ADC file not found:", adc_path), type = "error")
          return(FALSE)
        }

        roi_dims <- read_roi_dimensions(adc_path)
        classifications <- create_new_classifications(sample_name, roi_dims)
        rv$is_annotation_mode <- TRUE

        mode_message <- "New annotation"
      }

      # Store state
      rv$original_classifications <- classifications
      rv$classifications <- classifications
      rv$current_sample <- sample_name
      rv$selected_images <- character()
      rv$current_page <- 1
      rv$changes_log <- create_empty_changes_log()

      # Update class filter with warnings for unmatched classes
      available_classes <- sort(unique(classifications$class_name))
      unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
      display_names <- sapply(available_classes, function(cls) {
        if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
      })
      updateSelectInput(session, "class_filter",
                        choices = c("All" = "all", setNames(available_classes, display_names)))

      # Extract images (notification shown after filtering with correct count)
      extract_sample_images(sample_name, roi_path, classifications, mode_message = mode_message)

      return(TRUE)

    }, error = function(e) {
      showNotification(paste("Error loading sample:", e$message), type = "error")
      return(FALSE)
    })
  }
  
  # Load from session cache
  load_from_cache <- function(sample_name, roi_path) {
    cached <- rv$session_cache[[sample_name]]
    rv$classifications <- cached$classifications
    rv$original_classifications <- cached$original_classifications
    rv$changes_log <- cached$changes_log
    rv$current_sample <- sample_name
    rv$selected_images <- character()
    rv$is_annotation_mode <- cached$is_annotation_mode

    available_classes <- sort(unique(rv$classifications$class_name))
    unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
    display_names <- sapply(available_classes, function(cls) {
      if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
    })
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, display_names)))

    is_dashboard <- identical(config$data_source, "dashboard")

    if (is_dashboard) {
      # Dashboard mode: use cached download dir (PNGs already extracted)
      cache_dir <- get_dashboard_cache_dir()
      png_folder <- file.path(cache_dir, sample_name)

      if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder) &&
          !startsWith(rv$temp_png_folder, cache_dir)) {
        unlink(rv$temp_png_folder, recursive = TRUE)
      }
      rv$temp_png_folder <- png_folder
    } else {
      # Local mode: re-extract from ROI file
      if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder)) {
        unlink(rv$temp_png_folder, recursive = TRUE)
      }

      rv$temp_png_folder <- tempfile(pattern = "ifcb_validator_")
      dir.create(rv$temp_png_folder, recursive = TRUE)

      roi_numbers <- as.numeric(gsub(".*_(\\d+)\\.png$", "\\1", rv$classifications$file_name))

      withProgress(message = "Extracting images...", {
        ifcb_extract_pngs(
          roi_file = roi_path,
          out_folder = rv$temp_png_folder,
          ROInumbers = roi_numbers,
          verbose = FALSE
        )
      })
    }

    n_changes <- nrow(rv$changes_log)
    showNotification(paste("Restored from cache:", n_changes, "changes"), type = "message")
    return(TRUE)
  }
  
  # Extract images from ROI file
  extract_sample_images <- function(sample_name, roi_path, classifications, mode_message = NULL) {
    if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder)) {
      unlink(rv$temp_png_folder, recursive = TRUE)
    }
    
    rv$temp_png_folder <- tempfile(pattern = "ifcb_validator_")
    dir.create(rv$temp_png_folder, recursive = TRUE)
    
    roi_numbers <- as.numeric(gsub(".*_(\\d+)\\.png$", "\\1", classifications$file_name))
    
    withProgress(message = "Extracting images...", {
      ifcb_extract_pngs(
        roi_file = roi_path,
        out_folder = rv$temp_png_folder,
        ROInumbers = roi_numbers,
        verbose = FALSE
      )
    })
    
    # Filter out empty triggers
    extracted_folder <- file.path(rv$temp_png_folder, sample_name)
    if (dir.exists(extracted_folder)) {
      extracted_files <- list.files(extracted_folder, pattern = "\\.png$")
      
      rv$classifications <- rv$classifications[rv$classifications$file_name %in% extracted_files, ]
      rv$original_classifications <- rv$original_classifications[
        rv$original_classifications$file_name %in% extracted_files, ]
      
      available_classes <- sort(unique(rv$classifications$class_name))
      unmatched <- setdiff(available_classes, c(rv$class2use, "unclassified"))
      display_names <- sapply(available_classes, function(cls) {
        if (cls %in% unmatched) paste0("\u26A0 ", cls) else cls
      })
      updateSelectInput(session, "class_filter",
                        choices = c("All" = "all", setNames(available_classes, display_names)))
    }
    
    # Show notification with correct count AFTER filtering
    if (!is.null(mode_message)) {
      actual_count <- nrow(rv$classifications)
      showNotification(paste0(mode_message, ": ", actual_count, " images"), type = "message")
    }
  }
  
  # Helper to disable/enable navigation buttons during loading
  disable_nav_buttons <- function() {
    shinyjs::disable("load_sample")
    shinyjs::disable("prev_sample")
    shinyjs::disable("next_sample")
    shinyjs::disable("random_sample")
    shinyjs::disable("save_btn")
  }
  
  enable_nav_buttons <- function() {
    shinyjs::enable("load_sample")
    shinyjs::enable("prev_sample")
    shinyjs::enable("next_sample")
    shinyjs::enable("random_sample")
    shinyjs::enable("save_btn")
  }
  
  # Load sample button
  observeEvent(input$load_sample, {
    req(input$sample_select, input$sample_select != "")
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })

    # Clear class review state when loading a sample
    rv$class_review_mode <- FALSE
    rv$class_review_class <- NULL
    rv$class_review_samples <- character()
    rv$class_review_original <- NULL

    save_to_cache()
    rv$pending_sample_select <- input$sample_select
    load_sample_data(input$sample_select)
  })
  
  # Reset to home (click on title)
  observeEvent(input$reset_to_home, {
    # Save current work if there's a sample loaded
    if (!is.null(rv$current_sample)) {
      save_to_cache()
    }

    # Reset all sample-related state
    rv$current_sample <- NULL
    rv$classifications <- NULL
    rv$original_classifications <- NULL
    rv$changes_log <- create_empty_changes_log()
    rv$selected_images <- character(0)
    rv$is_annotation_mode <- FALSE
    rv$has_both_modes <- FALSE

    # Reset class review state
    rv$class_review_mode <- FALSE
    rv$class_review_class <- NULL
    rv$class_review_samples <- character()
    rv$class_review_original <- NULL

    # Clear sample selection
    updateSelectizeInput(session, "sample_select", selected = "")

    # Reset app mode to sample mode
    updateRadioButtons(session, "app_mode", selected = "sample")

    # Clear any displayed content via JavaScript
    shinyjs::runjs("$('.image-card').remove();")
  })
  
  # Previous sample button
  observeEvent(input$prev_sample, {
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })
    save_to_cache()
    samples <- get_filtered_samples()
    current_idx <- which(samples == rv$current_sample)
    
    if (length(current_idx) == 0) {
      if (length(samples) > 0) {
        prev_sample <- samples[length(samples)]
        rv$pending_sample_select <- prev_sample
        updateSelectizeInput(session, "sample_select", selected = prev_sample)
        load_sample_data(prev_sample)
      }
    } else if (current_idx > 1) {
      prev_sample <- samples[current_idx - 1]
      rv$pending_sample_select <- prev_sample
      updateSelectizeInput(session, "sample_select", selected = prev_sample)
      load_sample_data(prev_sample)
    } else {
      showNotification("Already at first sample", type = "warning")
    }
  })
  
  # Next sample button
  observeEvent(input$next_sample, {
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })
    save_to_cache()
    samples <- get_filtered_samples()
    current_idx <- which(samples == rv$current_sample)
    
    if (length(current_idx) == 0) {
      if (length(samples) > 0) {
        next_sample <- samples[1]
        rv$pending_sample_select <- next_sample
        updateSelectizeInput(session, "sample_select", selected = next_sample)
        load_sample_data(next_sample)
      }
    } else if (current_idx < length(samples)) {
      next_sample <- samples[current_idx + 1]
      rv$pending_sample_select <- next_sample
      updateSelectizeInput(session, "sample_select", selected = next_sample)
      load_sample_data(next_sample)
    } else {
      showNotification("No more samples in list", type = "warning")
    }
  })
  
  # ============================================================================
  # Image Gallery
  # ============================================================================
  
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
        # In annotation mode: sort unclassified by area (largest first),
        # then classified by class name
        unclassified <- df %>%
          filter(class_name == "unclassified") %>%
          arrange(desc(roi_area))
        
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
      
      if (rv$is_annotation_mode && input$class_filter == "unclassified") {
        # Sort unclassified by area in annotation mode
        filtered %>% arrange(desc(roi_area))
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
    if (rv$current_page > 1) rv$current_page <- rv$current_page - 1
  })
  
  observeEvent(input$next_page, {
    req(paginated_images())
    if (rv$current_page < paginated_images()$total_pages) {
      rv$current_page <- rv$current_page + 1
    }
  })
  
  observeEvent(input$class_filter, { rv$current_page <- 1 })
  observeEvent(input$images_per_page, { rv$current_page <- 1 })
  
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

        # In class review mode, derive sample from file_name since current_sample is NULL
        if (rv$class_review_mode) {
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
            style = "max-height: 120px; display: block;",
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
  
  # ============================================================================
  # Selection and Relabeling
  # ============================================================================
  
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
    req(filtered_images())
    rv$selected_images <- unique(c(rv$selected_images, filtered_images()$file_name))
  })
  
  observeEvent(input$deselect_all, {
    rv$selected_images <- character()
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

  # ============================================================================
  # Class Review Mode
  # ============================================================================

  # Helper: update class review class list based on current filter values
  update_cr_class_list <- function() {
    db_path <- get_db_path(config$db_folder)
    year <- input$cr_year_select
    month <- input$cr_month_select
    instrument <- input$cr_instrument_select

    annotator <- input$cr_annotator_select

    classes_df <- list_classes_db(db_path, year = year, month = month,
                                  instrument = instrument,
                                  annotator = annotator)

    if (nrow(classes_df) == 0) {
      updateSelectizeInput(session, "class_review_select",
                           choices = c("No classes found" = ""))
      return()
    }

    choices <- setNames(
      classes_df$class_name,
      sprintf("%s (%d)", classes_df$class_name, classes_df$count)
    )
    updateSelectizeInput(session, "class_review_select", choices = choices)
  }

  # Helper: update month/instrument choices for class review filters
  update_cr_month_choices <- function() {
    db_path <- get_db_path(config$db_folder)
    meta <- list_annotation_metadata_db(db_path)

    year_val <- input$cr_year_select

    month_names <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",
                     "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug",
                     "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")

    if (!is.null(year_val) && year_val != "all") {
      # Get samples for this year to extract available months/instruments
      con <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(config$db_folder))
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      year_samples <- DBI::dbGetQuery(con,
        "SELECT DISTINCT sample_name FROM annotations WHERE sample_name LIKE ?",
        params = list(paste0("D", year_val, "%"))
      )$sample_name

      months <- sort(unique(substr(year_samples, 6, 7)))
      month_labels <- month_names[months]
      updateSelectInput(session, "cr_month_select",
                        choices = c("All" = "all", setNames(months, month_labels)),
                        selected = "all")

      instruments <- sort(unique(sub(".*_", "", year_samples)))
      current_instrument <- input$cr_instrument_select
      selected_instrument <- if (!is.null(current_instrument) && current_instrument %in% instruments) {
        current_instrument
      } else {
        "all"
      }
      updateSelectInput(session, "cr_instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = selected_instrument)
    } else {
      months <- meta$months
      month_labels <- month_names[months]
      updateSelectInput(session, "cr_month_select",
                        choices = c("All" = "all", setNames(months, month_labels)),
                        selected = "all")

      instruments <- meta$instruments
      updateSelectInput(session, "cr_instrument_select",
                        choices = c("All" = "all", setNames(instruments, instruments)),
                        selected = "all")
    }
  }

  # When entering class review mode, populate filters and class dropdown
  observeEvent(input$app_mode, {
    if (input$app_mode == "class_review") {
      db_path <- get_db_path(config$db_folder)
      meta <- list_annotation_metadata_db(db_path)

      month_names <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",
                       "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug",
                       "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")

      updateSelectInput(session, "cr_year_select",
                        choices = c("All" = "all", setNames(meta$years, meta$years)),
                        selected = "all")
      month_labels <- month_names[meta$months]
      updateSelectInput(session, "cr_month_select",
                        choices = c("All" = "all", setNames(meta$months, month_labels)),
                        selected = "all")
      updateSelectInput(session, "cr_instrument_select",
                        choices = c("All" = "all", setNames(meta$instruments, meta$instruments)),
                        selected = "all")
      updateSelectInput(session, "cr_annotator_select",
                        choices = c("All" = "all", setNames(meta$annotators, meta$annotators)),
                        selected = "all")

      update_cr_class_list()
    } else {
      # Leaving class review mode — clear state
      rv$class_review_mode <- FALSE
      rv$class_review_class <- NULL
      rv$class_review_samples <- character()
      rv$class_review_original <- NULL
    }
  }, ignoreInit = TRUE)

  # Cascading filter updates for class review
  observeEvent(input$cr_year_select, {
    req(input$app_mode == "class_review")
    update_cr_month_choices()
    update_cr_class_list()
  }, ignoreInit = TRUE)

  observeEvent(input$cr_month_select, {
    req(input$app_mode == "class_review")
    update_cr_class_list()
  }, ignoreInit = TRUE)

  observeEvent(input$cr_instrument_select, {
    req(input$app_mode == "class_review")
    update_cr_class_list()
  }, ignoreInit = TRUE)

  observeEvent(input$cr_annotator_select, {
    req(input$app_mode == "class_review")
    update_cr_class_list()
  }, ignoreInit = TRUE)

  # Class review info output
  output$class_review_info <- renderUI({
    if (!rv$class_review_mode || is.null(rv$classifications)) return(NULL)

    n_images <- nrow(rv$classifications)
    n_samples <- length(rv$class_review_samples)
    n_changed <- 0L

    if (!is.null(rv$class_review_original)) {
      n_changed <- sum(rv$classifications$class_name != rv$class_review_original$class_name)
    }

    div(
      style = "font-size: 12px; color: #666; margin-bottom: 8px;",
      sprintf("%d images from %d samples", n_images, n_samples),
      if (n_changed > 0) tags$span(
        style = "color: #dc3545; font-weight: bold; margin-left: 5px;",
        sprintf("(%d changed)", n_changed)
      )
    )
  })

  # Load class for review
  observeEvent(input$load_class_review, {
    req(input$class_review_select, input$class_review_select != "")

    class_name <- input$class_review_select
    db_path <- get_db_path(config$db_folder)

    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })

    # Save current sample work if in sample mode
    if (!rv$class_review_mode && !is.null(rv$current_sample)) {
      save_to_cache()
    }

    # Query all annotations for this class (with filters)
    annotations <- load_class_annotations_db(db_path, class_name,
                                              year = input$cr_year_select,
                                              month = input$cr_month_select,
                                              instrument = input$cr_instrument_select,
                                              annotator = input$cr_annotator_select)

    if (is.null(annotations) || nrow(annotations) == 0) {
      showNotification(paste("No annotations found for class:", class_name),
                       type = "warning")
      return()
    }

    # Get unique samples and their ROI paths
    unique_samples <- unique(annotations$sample_name)
    current_roi_map <- roi_path_map()
    is_dashboard <- identical(config$data_source, "dashboard")

    # Create temp folder for extracted PNGs
    if (!is.null(rv$temp_png_folder) && dir.exists(rv$temp_png_folder)) {
      if (!is_dashboard || !startsWith(rv$temp_png_folder, get_dashboard_cache_dir())) {
        unlink(rv$temp_png_folder, recursive = TRUE)
      }
    }
    rv$temp_png_folder <- tempfile(pattern = "ifcb_class_review_")
    dir.create(rv$temp_png_folder, recursive = TRUE)

    # Extract PNGs per sample
    missing_samples <- character()
    extracted_files <- character()

    withProgress(message = paste("Loading", class_name, "images..."),
                 value = 0, {

      if (is_dashboard) {
        # Dashboard mode: download individual PNGs directly (much faster than zip)
        parsed <- parse_dashboard_url(config$dashboard_url)
        all_file_names <- annotations$file_name

        downloaded <- download_dashboard_images_individual(
          base_url = parsed$base_url,
          file_names = all_file_names,
          dest_dir = rv$temp_png_folder,
          max_retries = config$dashboard_max_retries
        )

        extracted_files <- downloaded
        downloaded_samples <- unique(gsub("_\\d+\\.png$", "", downloaded))
        missing_samples <- setdiff(unique_samples, downloaded_samples)
      }

      for (idx in seq_along(unique_samples)) {
        sn <- unique_samples[idx]

        if (is_dashboard) {
          # Already handled above in batch
          incProgress(1 / length(unique_samples))
          next
        } else {
          # Local mode: extract from ROI file
          roi_path <- current_roi_map[[sn]]

          if (is.null(roi_path) || !file.exists(roi_path)) {
            missing_samples <- c(missing_samples, sn)
            next
          }

          sample_rois <- annotations$roi_number[annotations$sample_name == sn]

          tryCatch({
            ifcb_extract_pngs(
              roi_file = roi_path,
              out_folder = rv$temp_png_folder,
              ROInumbers = sample_rois,
              verbose = FALSE
            )

            # Check which files were actually extracted
            sample_dir <- file.path(rv$temp_png_folder, sn)
            if (dir.exists(sample_dir)) {
              files <- list.files(sample_dir, pattern = "\\.png$")
              extracted_files <- c(extracted_files, files)
            }
          }, error = function(e) {
            missing_samples <<- c(missing_samples, sn)
          })
        }

        incProgress(1 / length(unique_samples))
      }
    })

    # Filter annotations to only those with extracted images
    annotations <- annotations[annotations$file_name %in% extracted_files, ]

    if (nrow(annotations) == 0) {
      showNotification("No images could be extracted. Check ROI file paths.",
                       type = "error")
      return()
    }

    # Build classifications data frame (compatible with gallery)
    classifications <- data.frame(
      file_name = annotations$file_name,
      class_name = annotations$class_name,
      score = NA_real_,
      width = NA_real_,
      height = NA_real_,
      roi_area = NA_real_,
      stringsAsFactors = FALSE
    )

    # Set class review state
    rv$class_review_mode <- TRUE
    rv$class_review_class <- class_name
    rv$class_review_samples <- setdiff(unique_samples, missing_samples)
    rv$current_sample <- NULL
    rv$classifications <- classifications
    rv$original_classifications <- classifications
    rv$class_review_original <- classifications
    rv$is_annotation_mode <- TRUE
    rv$has_both_modes <- FALSE
    rv$selected_images <- character()
    rv$current_page <- 1
    rv$changes_log <- create_empty_changes_log()

    # Update class filter dropdown
    available_classes <- sort(unique(classifications$class_name))
    updateSelectInput(session, "class_filter",
                      choices = c("All" = "all", setNames(available_classes, available_classes)),
                      selected = "all")

    # Notify
    n_extracted <- nrow(classifications)
    n_samples <- length(rv$class_review_samples)
    msg <- sprintf("Loaded %d %s images from %d samples", n_extracted, class_name, n_samples)
    if (length(missing_samples) > 0) {
      msg <- paste0(msg, sprintf(" (%d samples skipped - ROI not found)", length(missing_samples)))
    }
    showNotification(msg, type = "message", duration = 8)
  })

  # Save class review changes
  observeEvent(input$save_class_review_btn, {
    req(rv$class_review_mode)
    req(rv$classifications)
    req(rv$class_review_original)

    rv$is_loading <- TRUE
    on.exit({ rv$is_loading <- FALSE })

    # Find changed rows
    current <- rv$classifications
    original <- rv$class_review_original
    changed_mask <- current$class_name != original$class_name
    n_changed <- sum(changed_mask)

    if (n_changed == 0) {
      showNotification("No changes to save", type = "warning")
      return()
    }

    # Parse sample_name and roi_number from file_name
    changed_files <- current$file_name[changed_mask]
    changes_df <- data.frame(
      sample_name = sub("_(\\d{5})\\.png$", "", changed_files),
      roi_number = as.integer(sub(".*_(\\d{5})\\.png$", "\\1", changed_files)),
      new_class_name = current$class_name[changed_mask],
      stringsAsFactors = FALSE
    )

    db_path <- get_db_path(config$db_folder)
    annotator <- if (!is.null(input$annotator_name) && nzchar(input$annotator_name)) {
      input$annotator_name
    } else {
      "Unknown"
    }

    tryCatch({
      withProgress(message = "Saving class review changes...", {
        updated <- save_class_review_changes_db(db_path, changes_df, annotator)
      })

      # Update original to reflect saved state
      rv$class_review_original <- rv$classifications

      showNotification(
        sprintf("Saved %d changes across %d samples",
                updated, length(unique(changes_df$sample_name))),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
    })
  })

  # ============================================================================
  # Manual Save
  # ============================================================================
  
  observeEvent(input$save_btn, {
    req(rv$classifications)
    req(rv$class2use)
    req(rv$current_sample)
    req(rv$class2use_path)
    
    rv$is_loading <- TRUE
    disable_nav_buttons()
    on.exit({
      rv$is_loading <- FALSE
      enable_nav_buttons()
    })
    
    annotator <- input$annotator_name
    if (is.null(annotator) || annotator == "") annotator <- "Unknown"
    
    # Check for unmatched classes and warn
    current_classes <- unique(rv$classifications$class_name)
    unmatched <- setdiff(current_classes, c(rv$class2use, "unclassified"))
    if (length(unmatched) > 0) {
      showNotification(
        paste0("Warning: Some classes are not in the class list and may not be saved correctly: ",
               paste(unmatched, collapse = ", ")),
        type = "warning",
        duration = 10
      )
    }
    
    tryCatch({
      is_dashboard <- identical(config$data_source, "dashboard")

      roi_path <- roi_path_map()[[rv$current_sample]]
      adc_folder <- if (!is.null(roi_path)) dirname(roi_path) else NULL

      # In dashboard mode, adc_folder may be NULL since there are no local ROI files
      if (is.null(adc_folder) && is_dashboard) {
        # Try to get ADC from dashboard cache for MAT saving
        cache_dir <- get_dashboard_cache_dir()
        parsed <- parse_dashboard_url(config$dashboard_url)
        adc_path <- download_dashboard_adc(parsed$base_url, rv$current_sample, cache_dir,
                                           parallel_downloads = config$dashboard_parallel_downloads,
                                           sleep_time = config$dashboard_sleep_time,
                                           multi_timeout = config$dashboard_multi_timeout,
                                           max_retries = config$dashboard_max_retries)
        adc_folder <- if (!is.null(adc_path)) dirname(adc_path) else NULL
      }

      save_fmt <- config$save_format

      # In dashboard mode, if MAT save is requested but no ADC available, fall back to SQLite
      if (is_dashboard && is.null(adc_folder) && save_fmt %in% c("mat", "both")) {
        if (save_fmt == "mat") {
          showNotification("MAT saving requires ADC data (not available from dashboard). Saving to SQLite instead.",
                           type = "warning", duration = 8)
          save_fmt <- "sqlite"
        } else {
          showNotification("ADC data not available from dashboard. Skipping MAT save, saving to SQLite only.",
                           type = "warning", duration = 8)
          save_fmt <- "sqlite"
        }
      }

      if (is.null(adc_folder) && !is_dashboard) {
        showNotification("Cannot find ROI data folder for this sample", type = "error")
        return()
      }

      progress_msg <- switch(save_fmt,
        sqlite = "Saving to database...",
        mat = "Saving MAT file...",
        both = "Saving annotations...",
        "Saving..."
      )

      withProgress(message = progress_msg, {
        result <- save_sample_annotations(
          sample_name = rv$current_sample,
          classifications = rv$classifications,
          original_classifications = rv$original_classifications,
          changes_log = rv$changes_log,
          temp_png_folder = rv$temp_png_folder,
          output_folder = config$output_folder,
          png_output_folder = config$png_output_folder,
          roi_folder = config$roi_folder,
          class2use_path = rv$class2use_path,
          class2use = rv$class2use,
          annotator = annotator,
          adc_folder = adc_folder,
          save_format = save_fmt,
          db_folder = config$db_folder,
          export_statistics = config$export_statistics
        )
      })

      if (!isTRUE(result)) {
        showNotification("Save returned no changes", type = "warning")
        return()
      }

      # Update annotated samples list to reflect new manual annotation
      current_annotated <- annotated_samples()
      if (!rv$current_sample %in% current_annotated) {
        annotated_samples(c(current_annotated, rv$current_sample))
        update_current_sample_status(rv$current_sample)
      }

      save_msg <- switch(save_fmt,
        sqlite = paste("Saved to database in", config$db_folder),
        mat = paste("Saved to", config$output_folder),
        both = paste("Saved to database and", config$output_folder),
        paste("Saved to", config$output_folder)
      )
      showNotification(save_msg, type = "message")

    }, error = function(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
    })
  })
  
  # ============================================================================
  # Statistics
  # ============================================================================
  
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
  
  # ============================================================================
  # Session Cleanup
  # ============================================================================
  
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
      class2use_path <- isolate(rv$class2use_path)
      temp_png_folder <- isolate(rv$temp_png_folder)
      output_folder <- isolate(config$output_folder)
      png_output_folder <- isolate(config$png_output_folder)
      roi_folder <- isolate(config$roi_folder)
      annotator <- isolate(input$annotator_name)
      
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
      if (!is.null(temp_png_folder) && dir.exists(temp_png_folder)) {
        unlink(temp_png_folder, recursive = TRUE)
      }
    }, error = function(e) {
      message("Error during session cleanup: ", e$message)
    })
  })
}
