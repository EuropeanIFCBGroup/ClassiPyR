# Duplicate class-score files in the index cache (build_threshold_index())
#
# A model may have scored a sample more than once: a re-run kept next to the
# first one, or a copy of a folder. Within the folder a build is run on, one
# file per model and sample is used, the first by path, and the others are set
# aside with status `duplicate`.
#
# Files compete only with files of the same build. The index keeps the files
# of every folder it was built on, while a sampling frame is frozen on one
# folder: were a copy in another folder to win, the folder under review would
# be left without usable files, and a frame frozen on it without the ROIs it
# draws from. The same sample can therefore be usable in two folders, and
# whatever reads across folders takes one file per sample, the first by path
# (thr_frame_candidates(), thr_index_models()).

# The usable files of one sample. Run once per sample of every batch, so it
# has to be answered from the files_sample index, never by scanning the table.
THR_DUPLICATE_LOOKUP_SQL <- paste(
  "SELECT h5_path, model_id, sample_name FROM files",
  "WHERE status = 'ok' AND sample_name = ?"
)

#' Decide which file is kept when a model scored a sample more than once
#'
#' The first file by path is kept. That can be a file of this batch or one
#' already in the index, in which case whichever sorts later is set aside.
#' Only files of the folder being built take part.
#'
#' @param con A DBI connection to the index SQLite file.
#' @param files Data frame of \code{files} rows about to be written, sorted by
#'   path.
#' @param scope Paths of the class-score files found in the folder being
#'   built. Indexed files that are not among them neither win nor lose.
#' @return A list with \code{files} (the rows, with later duplicates given
#'   status \code{duplicate}) and \code{demote} (data frame of \code{h5_path}
#'   and \code{message} for indexed files that lose their place).
#' @noRd
thr_resolve_duplicates <- function(con, files, scope) {
  demote <- data.frame(h5_path = character(), message = character(),
                       stringsAsFactors = FALSE)
  candidates <- which(files$status == "ok")
  if (length(candidates) == 0) {
    return(list(files = files, demote = demote))
  }
  indexed <- DBI::dbGetQuery(
    con, THR_DUPLICATE_LOOKUP_SQL,
    params = list(unique(files$sample_name[candidates]))
  )
  # Rows of this batch are about to be replaced, so they do not compete
  competes <- indexed$h5_path %in% scope & !(indexed$h5_path %in% files$h5_path)
  indexed <- indexed[competes, , drop = FALSE]
  kept <- stats::setNames(indexed$h5_path,
                          thr_sample_key(indexed$model_id, indexed$sample_name))

  for (i in candidates) {
    key <- thr_sample_key(files$model_id[i], files$sample_name[i])
    holder <- unname(kept[key])
    if (is.na(holder)) {
      kept[key] <- files$h5_path[i]
    } else if (thr_path_sorts_first(holder, files$h5_path[i])) {
      files$status[i] <- "duplicate"
      files$message[i] <- thr_duplicate_message(holder)
    } else {
      demote <- rbind(demote, data.frame(
        h5_path = holder, message = thr_duplicate_message(files$h5_path[i]),
        stringsAsFactors = FALSE
      ))
      kept[key] <- files$h5_path[i]
    }
  }
  list(files = files, demote = demote)
}

#' Key for one model's score of one sample
#'
#' @param model_id Character vector of model identifiers.
#' @param sample_name Character vector of sample names.
#' @return Character vector of keys.
#' @noRd
thr_sample_key <- function(model_id, sample_name) {
  paste(model_id, sample_name, sep = "\r")
}

#' Does one path sort before another?
#'
#' @param a,b Single paths.
#' @return \code{TRUE} when \code{a} comes first in C-locale order.
#' @noRd
thr_path_sorts_first <- function(a, b) {
  a != b && order(c(a, b), method = "radix")[1] == 1L
}

#' Message for a file set aside as a duplicate
#'
#' @param kept_path Path of the file that is kept instead.
#' @return One string.
#' @noRd
thr_duplicate_message <- function(kept_path) {
  paste0("The same model scored this sample in another file of the folder ",
         "that was indexed, which sorts first and is used instead: ", kept_path)
}

#' Set indexed files aside as duplicates
#'
#' @param con A DBI connection to the index SQLite file.
#' @param demote Data frame with \code{h5_path} and \code{message}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_demote_files <- function(con, demote) {
  if (nrow(demote) == 0) {
    return(invisible(NULL))
  }
  DBI::dbExecute(
    con,
    "DELETE FROM rois WHERE file_id IN
       (SELECT file_id FROM files WHERE h5_path = ?)",
    params = list(demote$h5_path)
  )
  DBI::dbExecute(
    con, "UPDATE files SET status = 'duplicate', message = ? WHERE h5_path = ?",
    params = list(demote$message, demote$h5_path)
  )
  invisible(NULL)
}

#' Re-read duplicates left without a kept file
#'
#' When the file that was kept for a sample goes missing or stops being
#' usable, its duplicates have no ROI rows to fall back on, although their
#' size and modification time are unchanged. They are read again so the first
#' of them by path takes over.
#'
#' The kept file has to be one of the folder being built. A duplicate whose
#' kept file lies elsewhere is read again as well: it was set aside by a build
#' of an enclosing folder, or by a version of the index that resolved
#' duplicates across every folder it had seen.
#'
#' @param con A DBI connection to the index SQLite file.
#' @inheritParams build_threshold_index
#' @param scope Paths of the class-score files found in the folder being
#'   built.
#' @return Character vector of the paths that were read again.
#' @noRd
thr_promote_duplicates <- function(con, reader, batch_size, scope) {
  duplicates <- DBI::dbGetQuery(con, "
    SELECT h5_path, model_id, sample_name FROM files
    WHERE status = 'duplicate'")
  duplicates <- duplicates[duplicates$h5_path %in% scope, , drop = FALSE]
  if (nrow(duplicates) == 0) {
    return(character())
  }
  kept <- DBI::dbGetQuery(con, THR_DUPLICATE_LOOKUP_SQL,
                          params = list(unique(duplicates$sample_name)))
  kept <- kept[kept$h5_path %in% scope, , drop = FALSE]
  has_kept <- thr_sample_key(duplicates$model_id, duplicates$sample_name) %in%
    thr_sample_key(kept$model_id, kept$sample_name)
  orphans <- duplicates$h5_path[!has_kept]
  orphans <- orphans[file.exists(orphans)]
  if (length(orphans) == 0) {
    return(character())
  }
  todo <- thr_stat_files(orphans)
  thr_index_files(con, todo, reader, progress = NULL, batch_size = batch_size,
                  scope = scope)
  todo$h5_path
}
