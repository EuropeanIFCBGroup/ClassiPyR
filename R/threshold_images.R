# Raw data and images for the threshold review app
#
# A review page shows ROIs drawn from many samples at once. Their images are
# cut out of each sample's raw .roi file on the server, a few ROIs per sample,
# into a folder the browser can read. An image that cannot be made is never
# an error here: the draw is reported as not extracted, together with the
# reason, and the verdict rules decide from the reason what becomes of it.

# Why a draw has no image (the page column `no_image_reason`):
#   no_raw           the sample's .roi file was not found, so nothing was
#                    tried. The folder may only be away (a disk that is not
#                    mounted, a renamed folder): the draw stays pending.
#   not_extractable  the .roi file was there and was read, and this ROI could
#                    not be made from it. Final: the draw becomes unavailable.
#   invalid          the draw has no valid sample name or ROI number, so no
#                    file name was ever built for it. Final as well.
THR_NO_IMAGE_RAW <- "no_raw"
THR_NO_IMAGE_EXTRACT <- "not_extractable"
THR_NO_IMAGE_INVALID <- "invalid"
THR_NO_IMAGE_REASONS <- c(THR_NO_IMAGE_RAW, THR_NO_IMAGE_EXTRACT,
                          THR_NO_IMAGE_INVALID)

#' List the raw .roi files under a folder
#'
#' A function of its own so that the one expensive step of
#' \code{thr_resolve_roi_paths()} can be seen, and counted, in tests.
#'
#' @param roi_folder Raw data folder.
#' @return Character vector of full paths.
#' @noRd
thr_scan_roi_files <- function(roi_folder) {
  list.files(roi_folder, pattern = "\\.roi$", recursive = TRUE,
             full.names = TRUE)
}

#' Find the raw .roi file of each sample
#'
#' Looks in the standard IFCB layout first
#' (\code{roi_folder/YYYY/DYYYYMMDD/<sample>.roi}). Only when some sample is
#' not there is the folder scanned, once, for \code{.roi} files anywhere below
#' it, which are then matched by file name.
#'
#' @param sample_names Character vector of sample names. Repeated names are
#'   looked up once.
#' @param roi_folder Raw data folder.
#' @return Named character vector, one element per distinct sample name: the
#'   path of its \code{.roi} file, or \code{NA} when there is none. A name
#'   that is not a valid IFCB sample name gives \code{NA}, as does every
#'   sample when \code{roi_folder} is not an existing folder.
#' @noRd
thr_resolve_roi_paths <- function(sample_names, roi_folder) {
  sample_names <- unique(as.character(sample_names))
  paths <- stats::setNames(rep(NA_character_, length(sample_names)),
                           sample_names)
  if (!thr_is_single_string(roi_folder) || !dir.exists(roi_folder)) {
    return(paths)
  }
  valid <- !is.na(sample_names) &
    grepl(VALID_SAMPLE_NAME_PATTERN, sample_names)
  standard <- vapply(sample_names[valid], function(sample_name) {
    get_sample_paths(sample_name, roi_folder)$roi_path
  }, character(1))
  found <- file.exists(standard)
  paths[valid][found] <- standard[found]

  missing <- valid & is.na(paths)
  if (any(missing)) {
    scanned <- thr_scan_roi_files(roi_folder)
    at <- match(paste0(sample_names[missing], ".roi"), basename(scanned))
    paths[missing] <- scanned[at]
  }
  paths
}

#' Find the raw files that an earlier lookup did not find
#'
#' A lookup is only ever final for a sample whose file was found. A sample
#' without one is looked up again, so that raw data that was away (a disk
#' that was not mounted, a folder that had been renamed) is found as soon as
#' it is back. Samples that have their file cost nothing here.
#'
#' @param known Named character vector from an earlier call, or \code{NULL}.
#' @inheritParams thr_resolve_roi_paths
#' @return Named character vector as from \code{thr_resolve_roi_paths()}, one
#'   element per distinct sample name.
#' @noRd
thr_refresh_roi_paths <- function(known, sample_names, roi_folder) {
  sample_names <- unique(as.character(sample_names))
  paths <- stats::setNames(rep(NA_character_, length(sample_names)),
                           sample_names)
  carried <- intersect(sample_names, names(known))
  paths[carried] <- as.character(known[carried])
  unresolved <- is.na(paths)
  if (any(unresolved)) {
    paths[unresolved] <- thr_resolve_roi_paths(sample_names[unresolved],
                                               roi_folder)
  }
  paths
}

#' File name of each draw's image, relative to the image folder
#'
#' @param sample_name Character vector of sample names.
#' @param roi_number Integer vector of ROI numbers.
#' @return Character vector such as
#'   \code{"<sample>/<sample>_00012.png"}, the name
#'   \code{iRfcb::ifcb_extract_pngs()} writes. \code{NA} where the sample name
#'   is not a valid IFCB sample name or the ROI number is not a positive whole
#'   number, so that a path is never built from anything else.
#' @noRd
thr_png_file_names <- function(sample_name, roi_number) {
  sample_name <- as.character(sample_name)
  roi_number <- suppressWarnings(as.integer(roi_number))
  usable <- !is.na(sample_name) & !is.na(roi_number) & roi_number >= 1 &
    grepl(VALID_SAMPLE_NAME_PATTERN, sample_name)
  files <- rep(NA_character_, length(sample_name))
  files[usable] <- sprintf("%s/%s_%05d.png", sample_name[usable],
                           sample_name[usable], roi_number[usable])
  files
}

#' Which image files exist and hold something?
#'
#' @param out_folder Image folder.
#' @param png_file File names relative to it; \code{NA} is never a file.
#' @return Logical vector aligned with \code{png_file}.
#' @noRd
thr_png_exists <- function(out_folder, png_file) {
  size <- file.size(file.path(out_folder, png_file))
  !is.na(png_file) & !is.na(size) & size > 0
}

#' Run the extractor for some ROIs of one sample
#'
#' @param extractor Function with the interface of
#'   \code{iRfcb::ifcb_extract_pngs()}.
#' @param roi_file Path to the sample's \code{.roi} file.
#' @param out_folder Image folder.
#' @param roi_numbers ROI numbers to extract.
#' @return \code{TRUE} when the call finished, \code{FALSE} when it failed.
#' @noRd
thr_try_extract <- function(extractor, roi_file, out_folder, roi_numbers) {
  tryCatch({
    extractor(roi_file = roi_file, out_folder = out_folder,
              ROInumbers = roi_numbers, verbose = FALSE)
    TRUE
  }, error = function(e) FALSE)
}

#' Extract the missing images of one sample
#'
#' All of the sample's ROIs are asked for in one call. iRfcb fails such a
#' call as a whole when a single ROI number does not exist in the raw file,
#' so after a failure the ROIs still missing are tried one at a time: the
#' images that can be made are made, and the others stay missing.
#'
#' @inheritParams thr_try_extract
#' @param png_file Image file names of the ROIs, relative to
#'   \code{out_folder} and aligned with \code{roi_numbers}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_extract_sample <- function(extractor, roi_file, out_folder, roi_numbers,
                               png_file) {
  if (thr_try_extract(extractor, roi_file, out_folder, roi_numbers)) {
    return(invisible(NULL))
  }
  for (roi_number in roi_numbers[!thr_png_exists(out_folder, png_file)]) {
    thr_try_extract(extractor, roi_file, out_folder, roi_number)
  }
  invisible(NULL)
}

#' Extract the images a page does not have yet
#'
#' One extractor call per sample, and none for a sample whose raw file is not
#' there.
#'
#' @inheritParams thr_extract_page_images
#' @param png_file Image file name of every draw, relative to
#'   \code{out_folder}; \code{NA} for a draw that cannot have one.
#' @return Logical vector aligned with the rows of \code{page}: was the
#'   extractor run for the draw against a raw file that is still there
#'   afterwards? Only then does a missing image say something about the ROI
#'   rather than about where the raw data is.
#' @noRd
thr_extract_missing_images <- function(page, png_file, roi_paths, out_folder,
                                       extractor) {
  todo <- !is.na(png_file) & !thr_png_exists(out_folder, png_file)
  attempted <- rep(FALSE, nrow(page))
  for (sample_name in unique(page$sample_name[todo])) {
    roi_file <- unname(roi_paths[sample_name])
    if (is.na(roi_file) || !file.exists(roi_file)) {
      next
    }
    rows <- todo & page$sample_name == sample_name
    thr_extract_sample(extractor, roi_file, out_folder,
                       as.integer(page$roi_number[rows]), png_file[rows])
    # A raw file that went away while it was read explains the failure
    attempted[rows] <- file.exists(roi_file)
  }
  attempted
}

#' Why each draw of a page has no image
#'
#' @param png_file Image file name of every draw; \code{NA} for a draw that
#'   cannot have one.
#' @param extracted Logical: the image file exists.
#' @param attempted Logical, from \code{thr_extract_missing_images()}.
#' @return Character vector: \code{NA} for a draw with an image, otherwise
#'   one of \code{THR_NO_IMAGE_REASONS}.
#' @noRd
thr_no_image_reason <- function(png_file, extracted, attempted) {
  reason <- rep(NA_character_, length(png_file))
  reason[!extracted] <- THR_NO_IMAGE_RAW
  reason[!extracted & attempted] <- THR_NO_IMAGE_EXTRACT
  reason[is.na(png_file)] <- THR_NO_IMAGE_INVALID
  reason
}

#' Extract the images of a review page
#'
#' Cuts the page's ROIs out of the raw files, one extractor call per sample.
#' Images that are already in the folder are not extracted again. A sample
#' without a raw file, or an extractor that fails, never stops the page: the
#' draws concerned come back with \code{extracted = FALSE} and the reason in
#' \code{no_image_reason}.
#'
#' @param page Data frame of the draws on the page, with \code{draw_id},
#'   \code{sample_name} and \code{roi_number} (see \code{thr_next_page()}).
#' @param roi_paths Named character vector from
#'   \code{thr_resolve_roi_paths()}: the \code{.roi} file of each sample, or
#'   \code{NA}.
#' @param out_folder Folder to write the images to. Created if needed.
#' @param extractor Function called as \code{extractor(roi_file = ,
#'   out_folder = , ROInumbers = , verbose = FALSE)} that writes
#'   \code{out_folder/<sample>/<sample>_<5-digit ROI>.png}.
#' @return \code{page} with three more columns: \code{png_file} (the image
#'   file relative to \code{out_folder}, \code{NA} for a draw whose sample
#'   name or ROI number is not valid), \code{extracted} (logical: the image
#'   file exists afterwards) and \code{no_image_reason} (\code{NA} for an
#'   extracted draw, otherwise \code{"no_raw"} when the sample's raw file was
#'   not found, \code{"not_extractable"} when the raw file was read and did
#'   not give the ROI, and \code{"invalid"} when the draw has no valid sample
#'   name or ROI number).
#' @noRd
thr_extract_page_images <- function(page, roi_paths, out_folder,
                                    extractor = iRfcb::ifcb_extract_pngs) {
  thr_check_columns(page, c("draw_id", "sample_name", "roi_number"), "page")
  if (!thr_is_single_string(out_folder)) {
    stop("`out_folder` must be a single folder path for the images.",
         call. = FALSE)
  }
  if (!is.function(extractor)) {
    stop("`extractor` must be a function such as iRfcb::ifcb_extract_pngs.",
         call. = FALSE)
  }
  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  png_file <- thr_png_file_names(page$sample_name, page$roi_number)
  attempted <- thr_extract_missing_images(page, png_file, roi_paths,
                                          out_folder, extractor)
  result <- page
  result$png_file <- png_file
  result$extracted <- thr_png_exists(out_folder, png_file)
  result$no_image_reason <- thr_no_image_reason(png_file, result$extracted,
                                                attempted)
  result
}

#' Pixel size of PNG files
#'
#' Reads the width and height from the header of each file, so that the
#' gallery can reserve the space of an image before the browser has loaded
#' it. Without that, every lazily loaded image starts out with no size, they
#' all fit on screen at once, and the browser fetches the whole page.
#'
#' @param paths Character vector of file paths.
#' @return Data frame with \code{width} and \code{height} (integer), aligned
#'   with \code{paths}; \code{NA} for a file that is absent, too short or not
#'   a PNG.
#' @noRd
thr_png_dimensions <- function(paths) {
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  read_one <- function(path) {
    header <- tryCatch(
      suppressWarnings(readBin(path, "raw", n = 24L)),
      error = function(e) raw()
    )
    if (length(header) < 24 || !identical(header[1:8], signature)) {
      return(c(NA_integer_, NA_integer_))
    }
    # Width and height are the first two big-endian integers of the header
    # chunk, which always comes first
    size <- readBin(header[17:24], "integer", n = 2L, size = 4L,
                    endian = "big")
    if (anyNA(size) || any(size < 1)) c(NA_integer_, NA_integer_) else size
  }
  sizes <- vapply(as.character(paths), read_one, integer(2), USE.NAMES = FALSE)
  sizes <- matrix(sizes, nrow = 2)
  data.frame(width = sizes[1, ], height = sizes[2, ])
}
