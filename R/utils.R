# Utility functions for ClassiPyR
#
# Note: The following imports are used by the Shiny app (server.R, ui.R, global.R)
# which are not part of the package build but are needed at runtime.
#' @importFrom iRfcb ifcb_get_mat_variable
#' @importFrom shiny shinyApp
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib bs_theme
#' @importFrom DT renderDT
#' @importFrom jsonlite fromJSON
#' @importFrom reticulate py_available
#' @importFrom dplyr filter
NULL

#' Get ClassiPyR configuration directory
#'
#' Returns the path to the configuration directory for storing settings.
#' Uses tools::R_user_dir() for CRAN compliance. During R CMD check,
#' uses a temporary directory to avoid writing to user directories.
#'
#' @return Path to the configuration directory
#' @export
#' @examples
#' # Get the configuration directory path
#' config_dir <- get_config_dir()
#' print(config_dir)
get_config_dir <- function() {
  # Check if running under R CMD check
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))) {
    return(file.path(tempdir(), "ClassiPyR"))
  }
  tools::R_user_dir("ClassiPyR", "config")
}

#' Get path to settings file
#'
#' Returns the path to the settings JSON file, creating the configuration
#' directory if it doesn't exist.
#'
#' @return Path to the settings JSON file
#' @export
#' @examples
#' # Get the settings file path
#' settings_path <- get_settings_path()
#' print(settings_path)
get_settings_path <- function() {
  config_dir <- get_config_dir()
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(config_dir, "settings.json")
}

# Constants
VALID_SAMPLE_NAME_PATTERN <- "^D\\d{8}T\\d{6}_IFCB\\d+$"

# Characters unsafe for class names (used in folder names and HTML display):
# - / and \ are path separators -> replaced with "_" to preserve meaning
#   (common in taxonomy for ambiguous IDs: "Snowella/Woronichinia" -> "Snowella_Woronichinia")
# - < > " ' & are HTML/XSS risks -> removed
# - : * ? | are Windows filesystem unsafe -> removed
# - .. is path traversal risk -> removed
PATH_SEPARATOR_CHARS <- "[/\\\\]"
UNSAFE_CLASS_CHARS <- "[<>\"'&:*?|]"

#' Validate IFCB sample name format
#'
#' Checks if a sample name matches the expected IFCB naming convention:
#' DYYYYMMDDTHHMMSS_IFCBNNN (e.g., D20230101T120000_IFCB134).
#'
#' @param sample_name Sample name to validate
#' @return TRUE if valid, FALSE otherwise
#' @export
#' @examples
#' # Valid sample names
#' is_valid_sample_name("D20230101T120000_IFCB134")
#' is_valid_sample_name("D20220522T000439_IFCB1")
#'
#' # Invalid sample names
#' is_valid_sample_name("invalid_name")
#' is_valid_sample_name("20230101T120000_IFCB134")  # Missing 'D' prefix
#' is_valid_sample_name(NULL)
is_valid_sample_name <- function(sample_name) {
  if (is.null(sample_name) || length(sample_name) != 1 || !is.character(sample_name)) {
    return(FALSE)
  }
  grepl(VALID_SAMPLE_NAME_PATTERN, sample_name)
}

#' Sanitize string for safe use in HTML/file paths
#'
#' Removes or replaces characters that could be dangerous in HTML contexts
#' or file paths, including XSS attack vectors and path traversal attempts.
#'
#' @param x String to sanitize
#' @return Sanitized string
#' @export
#' @examples
#' # Remove HTML special characters
#' sanitize_string("<script>alert('xss')</script>")
#'
#' # Remove path traversal attempts
#' sanitize_string("../../../etc/passwd")
#'
#' # Normal strings pass through
#' sanitize_string("Diatom_chain")
sanitize_string <- function(x) {
  # Remove or replace potentially dangerous characters
  x <- gsub("[<>\"'&]", "", x)
  # Prevent path traversal
  x <- gsub("\\.\\.", "", x)
  x <- gsub("[/\\\\]", "", x)
  x
}

#' Load class list from MAT or TXT file
#'
#' Reads a class list from either a MATLAB .mat file (class2use format)
#' or a plain text file with one class per line. Class names are sanitized
#' for safe use in file paths and HTML.
#'
#' @param file_path Path to class2use file (.mat or .txt)
#' @return Character vector of class names
#' @export
#' @examples
#' \dontrun{
#' # Load from MATLAB file
#' classes <- load_class_list("/path/to/class2use.mat")
#'
#' # Load from text file
#' classes <- load_class_list("/path/to/class2use.txt")
#' }
#'
#' # Create a temporary text file for demonstration
#' tmp_file <- tempfile(fileext = ".txt")
#' writeLines(c("Diatom", "Ciliate", "Dinoflagellate"), tmp_file)
#' classes <- load_class_list(tmp_file)
#' print(classes)
#' unlink(tmp_file)
load_class_list <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  if (ext == "mat") {
    # Load from MAT file
    classes <- ifcb_get_mat_variable(file_path)
  } else if (ext == "txt") {
    # Load from text file (one class per line)
    classes <- readLines(file_path, warn = FALSE)
    classes <- trimws(classes)
    classes <- classes[classes != ""]
  } else {
    stop("Unsupported file format. Use .mat or .txt")
  }


  # Check for characters that need sanitization for filesystem/HTML safety
  if (length(classes) > 0) {
    has_path_sep <- grepl(PATH_SEPARATOR_CHARS, classes)
    has_unsafe <- grepl(UNSAFE_CLASS_CHARS, classes) | grepl("\\.\\.", classes)

    if (any(has_path_sep)) {
      # Replace path separators with underscore (preserves meaning for taxonomy like "Snowella/Woronichinia")
      message("Note: Forward slashes in class names replaced with underscores for filesystem compatibility")
      classes <- gsub(PATH_SEPARATOR_CHARS, "_", classes)
    }

    if (any(has_unsafe)) {
      warning("Some class names contain unsafe characters (< > \" ' & : * ? | ..) and were sanitized")
      classes <- gsub(UNSAFE_CLASS_CHARS, "", classes)
      classes <- gsub("\\.\\.", "", classes)
    }
  }

  return(classes)
}

#' Get sample paths from sample name
#'
#' Constructs file paths to IFCB data files (ROI, ADC) based on the standard
#' IFCB folder structure: roi_folder/YYYY/DYYYYMMDD/sample_name.ext
#'
#' @param sample_name Sample name (e.g., "D20220522T000439_IFCB134")
#' @param roi_folder Base ROI folder path
#' @return List with components: year, date_part, roi_path, adc_path, adc_folder
#' @export
#' @examples
#' # Get paths for a sample
#' paths <- get_sample_paths("D20230101T120000_IFCB134", "/data/ifcb/raw")
#' print(paths$year)       # "2023"
#' print(paths$date_part)  # "D20230101"
#' print(paths$roi_path)   # "/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.roi"
get_sample_paths <- function(sample_name, roi_folder) {
  # Validate sample name format to prevent path traversal

  if (!is_valid_sample_name(sample_name)) {
    stop("Invalid sample name format. Expected pattern: DYYYYMMDDTHHMMSS_IFCBNNN")
  }

  year <- substr(sample_name, 2, 5)
  date_part <- substr(sample_name, 1, 9)

  list(
    year = year,
    date_part = date_part,
    roi_path = file.path(roi_folder, year, date_part, paste0(sample_name, ".roi")),
    adc_path = file.path(roi_folder, year, date_part, paste0(sample_name, ".adc")),
    adc_folder = file.path(roi_folder, year, date_part)
  )
}

#' Read ROI dimensions from ADC file
#'
#' Reads an IFCB ADC file and extracts ROI dimensions (width, height, area)
#' for each ROI in the sample.
#'
#' @param adc_path Path to ADC file
#' @return Data frame with columns: roi_number, width, height, area
#' @export
#' @examples
#' \dontrun{
#' # Read dimensions from an ADC file
#' dims <- read_roi_dimensions("/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
#' head(dims)
#' }
read_roi_dimensions <- function(adc_path) {
  tryCatch({
    if (!file.exists(adc_path)) {
      stop("ADC file not found: ", adc_path)
    }

    adc_data <- utils::read.csv(adc_path, header = FALSE)

    if (nrow(adc_data) == 0) {
      return(data.frame(
        roi_number = integer(),
        width = numeric(),
        height = numeric(),
        area = numeric()
      ))
    }

    n_rois <- nrow(adc_data)

    # IFCB ADC columns: V14=ROIx, V15=ROIy, V16=ROIwidth, V17=ROIheight
    if (ncol(adc_data) >= 17) {
      roi_width <- adc_data$V16
      roi_height <- adc_data$V17
    } else {
      warning("ADC file has fewer than 17 columns, using default dimensions")
      roi_width <- rep(1, n_rois)
      roi_height <- rep(1, n_rois)
    }

    data.frame(
      roi_number = seq_len(n_rois),
      width = roi_width,
      height = roi_height,
      area = roi_width * roi_height
    )
  }, error = function(e) {
    stop("Failed to read ADC file: ", e$message)
  })
}

#' Create empty changes log data frame
#'
#' Creates an empty data frame with the correct structure for tracking
#' annotation changes during a session.
#'
#' @return Empty data frame with columns: image, original_class, new_class
#' @export
#' @examples
#' # Create an empty changes log
#' changes <- create_empty_changes_log()
#' print(names(changes))
#' print(nrow(changes))
create_empty_changes_log <- function() {
  data.frame(
    image = character(),
    original_class = character(),
    new_class = character(),
    stringsAsFactors = FALSE
  )
}

#' Initialize Python environment for iRfcb
#'
#' Checks if Python is already available via reticulate, otherwise tries to
#' use or create a virtual environment. Required for reading and writing
#' MATLAB .mat files.
#'
#' The resolution order is:
#' 1. If Python is already configured via reticulate, use it directly
#'    (installs scipy if missing).
#' 2. If \code{venv_path} is provided and the virtual environment exists,
#'    activate it.
#' 3. If \code{venv_path} is provided but does not exist, create it via
#'    \code{\link[iRfcb]{ifcb_py_install}}.
#' 4. If \code{venv_path} is NULL, default to \code{./venv} in the current
#'    working directory for steps 2--3.
#'
#' @param venv_path Optional path to virtual environment. If NULL (default),
#'   uses a \code{venv} folder in the current working directory.
#' @return TRUE if Python is available, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' # Initialize with default venv path (./venv)
#' success <- init_python_env()
#'
#' # Initialize with custom venv path
#' success <- init_python_env("/path/to/my/venv")
#'
#' if (success) {
#'   message("Python ready for MAT file operations")
#' }
#' }
init_python_env <- function(venv_path = NULL) {

  tryCatch({
    # First check if Python is already configured via reticulate
    if (reticulate::py_available(initialize = TRUE)) {
      # Check if scipy is installed (required for MAT file writing)
      if (!reticulate::py_module_available("scipy")) {
        message("Installing scipy...")
        reticulate::py_install("scipy")
      }
      message("Python environment ready")
      return(TRUE)
    }

    # Determine venv path: use provided path, or working directory default
    if (is.null(venv_path) || venv_path == "") {
      venv_path <- file.path(getwd(), "venv")
    }

    # Try to use existing venv
    if (reticulate::virtualenv_exists(venv_path)) {
      reticulate::use_virtualenv(venv_path, required = TRUE)
      message("Using Python environment: ", venv_path)
      return(TRUE)
    }

    # Create venv via iRfcb
    message("Creating Python environment at: ", venv_path)
    iRfcb::ifcb_py_install(venv_path)
    reticulate::use_virtualenv(venv_path, required = TRUE)
    return(TRUE)

  }, error = function(e) {
    warning("Failed to initialize Python environment: ", e$message)
    return(FALSE)
  })
}
