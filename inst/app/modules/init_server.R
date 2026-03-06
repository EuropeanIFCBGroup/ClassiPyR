# Initialization: reactive values, config, settings I/O, WoRMS persistence
#
# Returns a list of objects needed by other modules.

setup_init_server <- function(input, output, session) {
  rv <- reactiveValues(
    # Class list (character vector of class names, order = indices for MAT files)
    # Default to "unclassified" so app works without loading a class list
    class2use = "unclassified",
    class2use_path = NULL,

    # Current sample data
    classifications = NULL,         # Current state of image classifications
    current_sample = NULL,          # Sample name (e.g., "D20220522T000439_IFCB134")
    temp_png_folder = NULL,         # Temporary folder with extracted PNG images
    temp_png_is_managed = TRUE,     # TRUE only for app-owned temp/cache folders
    original_classifications = NULL, # Original state for comparison/statistics

    # Selection and editing state
    selected_images = character(),  # Currently selected image filenames
    changes_log = create_empty_changes_log(), # Track all changes made

    # Session management
    session_cache = list(),         # Cache of loaded samples (for quick switching)

    # Mode tracking
    is_annotation_mode = FALSE,     # TRUE = annotation (no auto-class), FALSE = validation
    has_classification = FALSE,     # TRUE if sample has auto-classification data available

    # UI state
    current_page = 1,               # Current pagination page
    class_sort_mode = "id",         # Class list sort: "id" (by index) or "alpha" (A-Z)
    resource_path_name = NULL,      # Session-specific Shiny resource path for images
    is_loading = FALSE,             # TRUE while loading/saving operations in progress
    measure_mode = FALSE,           # TRUE when measure tool is active
    pending_sample_select = NULL,   # Pending sample selection for dropdown update
    select_all_state = "first",     # State for two-click Select All

    # Class review mode state
    class_review_mode = FALSE,      # TRUE when in class review mode
    class_review_source = "database", # "database" or "external"
    class_review_class = NULL,      # Currently reviewed class name
    class_review_samples = character(), # Unique sample names in class review
    class_review_original = NULL,   # Original classifications snapshot for diff
    class_review_external_files = NULL, # file_name -> source_path map for export

    # WoRMS matching state
    class_aphia_map = setNames(character(0), character(0)),
    worms_matches = NULL
  )

  # Settings file for persistence (uses R_user_dir for CRAN compliance)
  settings_file <- get_settings_path()

  # Get working directory at app startup (for default paths)
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

  # Create a dynamic roots object for shinyDirChoose
  make_dynamic_roots <- function(input_id) {
    f <- function() get_browse_volumes(input[[input_id]])
    structure(f, class = c("dynamic_roots", "function"))
  }

  # Disable browse buttons and notify when paths are invalid
  setup_path_validation <- function(input_id, button_id, label, notify_invalid = TRUE) {
    last_invalid <- reactiveVal(NULL)
    observeEvent(input[[input_id]], {
      path <- input[[input_id]]
      has_path <- !is.null(path) && nzchar(path)
      is_valid <- !has_path || dir.exists(path)

      if (is_valid) {
        shinyjs::enable(button_id)
        last_invalid(NULL)
      } else {
        shinyjs::disable(button_id)
        if (notify_invalid && !identical(last_invalid(), path)) {
          showNotification(
            paste0(label, " does not exist. Please enter a valid folder."),
            type = "error"
          )
          last_invalid(path)
        }
      }
    }, ignoreInit = TRUE)
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
      pixels_per_micron = 3.4,
      auto_sync = TRUE,
      class2use_path = NULL,
      python_venv_path = NULL,
      save_format = "sqlite",
      export_statistics = TRUE,
      skip_class_png = "",
      data_source = "local",
      dashboard_url = "",
      dashboard_autoclass = FALSE,
      gradio_url = "",
      prediction_model = "",
      dashboard_parallel_downloads = 5,
      dashboard_sleep_time = 2,
      dashboard_multi_timeout = 120,
      dashboard_max_retries = 3,
      zip_readme_author = "",
      zip_readme_contact_email = "",
      zip_readme_doi = "",
      zip_readme_license = "",
      zip_readme_version = "",
      zip_readme_citation = "",
      zip_readme_institute = "",
      zip_split_zip = FALSE,
      zip_max_size = 500
    )

    if (file.exists(settings_file)) {
      tryCatch({
        saved <- jsonlite::fromJSON(settings_file)
        for (key in names(saved)) {
          if (key %in% names(defaults) || key == "class2use_path") {
            val <- saved[[key]]
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
    dashboard_max_retries = saved_settings$dashboard_max_retries,
    zip_readme_author = saved_settings$zip_readme_author,
    zip_readme_contact_email = saved_settings$zip_readme_contact_email,
    zip_readme_doi = saved_settings$zip_readme_doi,
    zip_readme_license = saved_settings$zip_readme_license,
    zip_readme_version = saved_settings$zip_readme_version,
    zip_readme_citation = saved_settings$zip_readme_citation,
    zip_readme_institute = saved_settings$zip_readme_institute,
    zip_split_zip = isTRUE(saved_settings$zip_split_zip),
    zip_max_size = if (!is.null(saved_settings$zip_max_size)) as.numeric(saved_settings$zip_max_size) else 500
  )

  get_classipyr_citation_text <- function() {
    tryCatch({
      cit <- utils::citation("ClassiPyR")
      txt <- paste(format(cit[1], style = "text"), collapse = " ")
      trimws(gsub("\\s+", " ", txt))
    }, error = function(e) {
      paste0(
        "Torstensson A. ClassiPyR (R package), version ",
        as.character(utils::packageVersion("ClassiPyR"))
      )
    })
  }

  build_zip_readme <- function(template_path, png_folder, zip_path, fields) {
    if (!file.exists(template_path)) {
      return(NULL)
    }

    lines <- readLines(template_path, warn = FALSE)

    png_files <- list.files(png_folder, pattern = "\\.png$", recursive = TRUE, full.names = FALSE)
    class_dirs <- list.dirs(png_folder, recursive = FALSE, full.names = FALSE)
    class_dirs <- class_dirs[class_dirs != ""]

    years <- character(0)
    if (length(png_files) > 0) {
      mm <- regexec("^D([0-9]{4})[0-9]{4}T[0-9]{6}_", basename(png_files))
      years <- vapply(regmatches(basename(png_files), mm), function(x) {
        if (length(x) >= 2) x[2] else NA_character_
      }, character(1))
      years <- years[!is.na(years) & nzchar(years)]
    }
    year_start <- if (length(years) > 0) min(years) else as.character(format(Sys.Date(), "%Y"))
    year_end <- if (length(years) > 0) max(years) else as.character(format(Sys.Date(), "%Y"))

    replacements <- c(
      "<E-MAIL>" = ifelse(nzchar(fields$contact_email), fields$contact_email, ""),
      "<VERSION>" = ifelse(nzchar(fields$version), fields$version, ""),
      "<DATE>" = as.character(Sys.Date()),
      "<YEAR>" = ifelse(nzchar(fields$citation), fields$citation, ""),
      "<YEAR_START>" = year_start,
      "<YEAR_END>" = year_end,
      "<N_IMAGES>" = as.character(length(png_files)),
      "<CLASSES>" = as.character(length(class_dirs)),
      "<ZIP_NAME>" = basename(zip_path),
      "XXXX" = ifelse(nzchar(fields$institute), fields$institute, "")
    )

    set_field <- function(pattern, value) {
      idx <- grep(pattern, lines)
      if (length(idx) > 0) {
        lines[idx[1]] <<- sub(":.*$", paste0(": ", value), lines[idx[1]])
      }
    }

    set_field("^- Author:", ifelse(nzchar(fields$author), fields$author, ""))
    set_field("^- DOI:", ifelse(nzchar(fields$doi), fields$doi, ""))
    set_field("^- License:", ifelse(nzchar(fields$license), fields$license, ""))

    for (ph in names(replacements)) {
      lines <- gsub(ph, replacements[[ph]], lines, fixed = TRUE)
    }

    drop_line <- function(pattern, keep) {
      if (!isTRUE(keep)) {
        lines <<- lines[!grepl(pattern, lines)]
      }
    }
    drop_line("^- Author:\\s*$", nzchar(fields$author))
    drop_line("^- Contact e-mail:\\s*$", nzchar(fields$contact_email))
    drop_line("^- DOI:\\s*$", nzchar(fields$doi))
    drop_line("^- Licen[cs]e:\\s*$", nzchar(fields$license))
    drop_line("^- Version:\\s*$", nzchar(fields$version))
    drop_line("^Please cite as:\\s*$", nzchar(fields$citation))

    if (!nzchar(fields$institute)) {
      lines <- gsub("\\s+at the\\s*\\.", ".", lines)
    }

    classipyr_version <- as.character(utils::packageVersion("ClassiPyR"))
    classipyr_citation <- get_classipyr_citation_text()
    classipyr_footer <- c(
      "",
      "## Archive creation metadata",
      "",
      paste0("This archive was created using ClassiPyR version ", classipyr_version, "."),
      paste0("Please cite ClassiPyR as: ", classipyr_citation)
    )

    out <- tempfile("classipyr_readme_", fileext = ".md")
    writeLines(c(lines, classipyr_footer), out)
    out
  }

  # Persist class -> AphiaID mappings in SQLite (project DB)
  worms_map_file <- file.path(get_config_dir(), "class_aphia_map.json")

  load_legacy_worms_map <- function() {
    if (!file.exists(worms_map_file)) {
      return(setNames(character(0), character(0)))
    }
    tryCatch({
      x <- jsonlite::fromJSON(worms_map_file, simplifyVector = TRUE)
      if (is.null(x) || length(x) == 0) return(setNames(character(0), character(0)))
      x <- unlist(x, use.names = TRUE)
      x <- as.character(x)
      x <- x[!is.na(x) & nzchar(x)]
      x
    }, error = function(e) {
      message("Could not load legacy WoRMS map: ", e$message)
      setNames(character(0), character(0))
    })
  }

  save_worms_map <- function(x, db_folder, matches_df = NULL) {
    tryCatch({
      db_path <- get_db_path(db_folder)
      if (length(x) == 0) return(invisible(TRUE))

      accepted_lookup <- setNames(character(0), character(0))
      scientific_lookup <- setNames(character(0), character(0))
      accepted_aphia_lookup <- setNames(character(0), character(0))
      if (!is.null(matches_df) && nrow(matches_df) > 0 &&
          all(c("class_name", "accepted_name") %in% names(matches_df))) {
        accepted_lookup <- setNames(
          as.character(matches_df$accepted_name),
          as.character(matches_df$class_name)
        )
      }
      if (!is.null(matches_df) && nrow(matches_df) > 0 &&
          all(c("class_name", "scientific_name") %in% names(matches_df))) {
        scientific_lookup <- setNames(
          as.character(matches_df$scientific_name),
          as.character(matches_df$class_name)
        )
      } else if (!is.null(matches_df) && nrow(matches_df) > 0 &&
                 all(c("class_name", "matched_name") %in% names(matches_df))) {
        scientific_lookup <- setNames(
          as.character(matches_df$matched_name),
          as.character(matches_df$class_name)
        )
      }
      if (!is.null(matches_df) && nrow(matches_df) > 0 &&
          all(c("class_name", "accepted_aphia_id") %in% names(matches_df))) {
        accepted_aphia_lookup <- setNames(
          as.character(matches_df$accepted_aphia_id),
          as.character(matches_df$class_name)
        )
      }

      ok <- ClassiPyR::save_class_taxonomy_db(
        db_path = db_path,
        class_aphia_map = x,
        accepted_name_map = accepted_lookup,
        scientific_name_map = scientific_lookup,
        accepted_aphia_map = accepted_aphia_lookup
      )
      if (!isTRUE(ok)) {
        message("Could not save WoRMS map to database")
      }
      invisible(TRUE)
    }, error = function(e) {
      message("Could not save WoRMS map to database: ", e$message)
      invisible(FALSE)
    })
  }

  load_worms_map <- function(db_folder) {
    db_map <- tryCatch({
      db_path <- get_db_path(db_folder)
      ClassiPyR::load_class_taxonomy_db(db_path)
    }, error = function(e) {
      message("Could not load WoRMS map from database: ", e$message)
      setNames(character(0), character(0))
    })

    if (length(db_map) > 0) return(db_map)

    legacy_map <- load_legacy_worms_map()
    if (length(legacy_map) > 0) {
      save_worms_map(legacy_map, db_folder = db_folder)
    }
    legacy_map
  }

  rv$class_aphia_map <- load_worms_map(saved_settings$db_folder)

  # Initialize class dropdown with default class list on startup
  observeEvent(once = TRUE, ignoreNULL = FALSE, session$clientData$url_protocol, {
    sorted_classes <- sort(rv$class2use)
    updateSelectizeInput(session, "new_class_quick",
                         choices = sorted_classes,
                         selected = character(0))
  })

  # Store all sample names and their classification status
  all_samples <- reactiveVal(character())
  classified_samples <- reactiveVal(character())
  annotated_samples <- reactiveVal(character())
  classifier_mat_files <- reactiveVal(list())
  classifier_h5_files <- reactiveVal(list())
  roi_path_map <- reactiveVal(list())
  png_sample_path_map <- reactiveVal(list())
  csv_path_map <- reactiveVal(list())
  rescan_trigger <- reactiveVal(0)
  last_sync_time <- reactiveVal(NULL)

  # Get classes in current classifications that are NOT in class2use
  unmatched_classes <- reactive({
    if (is.null(rv$classifications) || is.null(rv$class2use)) {
      return(character())
    }
    classification_classes <- unique(rv$classifications$class_name)
    unmatched <- setdiff(classification_classes, rv$class2use)
    setdiff(unmatched, "unclassified")
  })

  list(
    rv = rv,
    config = config,
    saved_settings = saved_settings,
    persist_settings = persist_settings,
    get_browse_volumes = get_browse_volumes,
    make_dynamic_roots = make_dynamic_roots,
    setup_path_validation = setup_path_validation,
    build_zip_readme = build_zip_readme,
    save_worms_map = save_worms_map,
    load_worms_map = load_worms_map,
    all_samples = all_samples,
    classified_samples = classified_samples,
    annotated_samples = annotated_samples,
    classifier_mat_files = classifier_mat_files,
    classifier_h5_files = classifier_h5_files,
    roi_path_map = roi_path_map,
    png_sample_path_map = png_sample_path_map,
    csv_path_map = csv_path_map,
    rescan_trigger = rescan_trigger,
    last_sync_time = last_sync_time,
    unmatched_classes = unmatched_classes
  )
}
