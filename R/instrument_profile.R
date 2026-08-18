# Instrument profile helpers for ClassiPyR
#
# ClassiPyR was originally built around the Imaging FlowCytobot (IFCB), whose
# samples follow a strict naming convention (DYYYYMMDDTHHMMSS_IFCBNNN) and whose
# image files are 5-digit ROI exports (sample_00001.png). "Generic" mode relaxes
# these assumptions so images from any imaging instrument - or indeed any image
# labelling task - can be annotated.
#
# These helpers centralise the small number of behaviours that differ between
# the "IFCB" and "generic" instrument profiles, so the rest of the package can
# stay free of mode-specific branching.

#' Supported instrument profile types
#'
#' @return Character vector of valid \code{instrument_type} values.
#' @keywords internal
INSTRUMENT_TYPES <- c("IFCB", "generic")

#' Default image file extensions for generic mode
#'
#' @keywords internal
DEFAULT_IMAGE_EXTENSIONS <- c("png", "jpg", "jpeg")

#' Normalise an instrument type string
#'
#' Accepts case-insensitive input and falls back to \code{"IFCB"} for any
#' missing or unrecognised value, so callers never have to guard against
#' invalid settings.
#'
#' @param instrument_type Instrument type string (e.g. \code{"IFCB"} or
#'   \code{"generic"}).
#' @return One of \code{"IFCB"} or \code{"generic"}.
#' @export
#' @examples
#' normalize_instrument_type("generic")
#' normalize_instrument_type("IFCB")
#' normalize_instrument_type(NULL) # -> "IFCB"
normalize_instrument_type <- function(instrument_type = "IFCB") {
  if (is.null(instrument_type) || length(instrument_type) != 1 ||
      isTRUE(is.na(instrument_type)) || !nzchar(instrument_type)) {
    return("IFCB")
  }
  match <- INSTRUMENT_TYPES[tolower(INSTRUMENT_TYPES) == tolower(instrument_type)]
  if (length(match) == 1) match else "IFCB"
}

#' Parse and clean a list of image file extensions
#'
#' Accepts either a character vector or a single comma/space separated string
#' (as stored in the Settings JSON) and returns a clean, lower-case vector of
#' extensions without leading dots. Falls back to
#' \code{\link{DEFAULT_IMAGE_EXTENSIONS}} when nothing usable is supplied.
#'
#' @param extensions Character vector or comma-separated string of extensions.
#' @return Character vector of lower-case extensions without a leading dot.
#' @export
#' @examples
#' parse_image_extensions("png, jpg, .JPEG")
#' parse_image_extensions(c("tif", "tiff"))
#' parse_image_extensions(NULL)
parse_image_extensions <- function(extensions = NULL) {
  if (is.null(extensions) || length(extensions) == 0) {
    return(DEFAULT_IMAGE_EXTENSIONS)
  }
  # Allow a single "png, jpg" style string as well as a character vector
  parts <- unlist(strsplit(as.character(extensions), "[,[:space:]]+"))
  parts <- tolower(trimws(parts))
  parts <- sub("^\\.", "", parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) DEFAULT_IMAGE_EXTENSIONS else unique(parts)
}

#' Build a regular expression matching image files
#'
#' Produces a case-insensitive regex (for use with \code{list.files}) that
#' matches any file ending in one of the supplied extensions.
#'
#' @param extensions Character vector or comma-separated string of extensions.
#'   Defaults to \code{\link{DEFAULT_IMAGE_EXTENSIONS}}.
#' @return A regular expression string, e.g. \code{"\\.(png|jpg|jpeg)$"}.
#' @export
#' @examples
#' image_file_pattern(c("png", "jpg"))
#' grepl(image_file_pattern("png"), "cell.PNG", ignore.case = TRUE)
image_file_pattern <- function(extensions = DEFAULT_IMAGE_EXTENSIONS) {
  exts <- parse_image_extensions(extensions)
  # Keep only alphanumeric extensions so the pattern is always a safe regex
  # (image extensions are alphanumeric; this guards against odd user input).
  exts <- exts[grepl("^[[:alnum:]]+$", exts)]
  if (length(exts) == 0) exts <- DEFAULT_IMAGE_EXTENSIONS
  paste0("\\.(", paste(exts, collapse = "|"), ")$")
}

#' Assign ROI numbers to a set of image file names
#'
#' The annotation database keys each image by \code{(sample_name, roi_number)}.
#' For IFCB data the ROI number is parsed from the 5-digit suffix of the file
#' name (\code{sample_00042.png} -> 42). For generic images, which may have any
#' file name, a synthetic ROI number is assigned by stable alphabetical order of
#' the file names. Stable ordering guarantees the same image always receives the
#' same ROI number for a given folder, which keeps annotations attached to the
#' correct image across reloads.
#'
#' @param file_names Character vector of image file names (basenames).
#' @param instrument_type Instrument profile, \code{"IFCB"} (default) or
#'   \code{"generic"}.
#' @return Integer vector of ROI numbers, parallel to \code{file_names}.
#' @export
#' @examples
#' assign_roi_numbers(c("s_00001.png", "s_00012.png"))            # IFCB: 1, 12
#' assign_roi_numbers(c("beta.jpg", "alpha.jpg"), "generic")      # generic: 2, 1
assign_roi_numbers <- function(file_names, instrument_type = "IFCB") {
  instrument_type <- normalize_instrument_type(instrument_type)
  if (length(file_names) == 0) return(integer(0))

  if (instrument_type == "IFCB") {
    return(as.integer(gsub(".*_(\\d+)\\.[A-Za-z0-9]+$", "\\1", file_names)))
  }

  # Generic: stable sequential numbering by sorted file name.
  ord <- order(file_names)
  roi <- integer(length(file_names))
  roi[ord] <- seq_along(file_names)
  roi
}
