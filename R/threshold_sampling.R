# Stratified draws for the threshold review app
#
# A class is reviewed by simple random sampling without replacement within
# each score bin. Draws are written to the verdict database before anybody
# sees them, so what was offered for review is on record whether or not a
# verdict follows, and a ROI is never offered twice.

#' Check a count column of a stratum table
#'
#' @param strata Data frame of strata.
#' @param column Name of the column.
#' @return The column as an integer vector.
#' @noRd
thr_check_stratum_column <- function(strata, column) {
  values <- strata[[column]]
  valid <- is.numeric(values) && !anyNA(values) &&
    all(values >= 0 & values == round(values))
  if (!valid) {
    stop("`strata$", column, "` must hold whole numbers of 0 or more without ",
         "missing values.", call. = FALSE)
  }
  as.integer(values)
}

#' Is a value a single whole number of at least a minimum?
#'
#' @param x Value to check.
#' @param minimum Smallest value allowed.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
thr_is_count <- function(x, minimum = 0) {
  is.numeric(x) && length(x) == 1 && is.finite(x) && x >= minimum &&
    x == round(x)
}

#' Stop unless a seed is usable
#'
#' @param seed \code{NULL} or a single number.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_seed <- function(seed) {
  valid <- is.null(seed) ||
    (is.numeric(seed) && length(seed) == 1 && is.finite(seed))
  if (!valid) {
    stop("`seed` must be NULL or a single number.", call. = FALSE)
  }
  invisible(NULL)
}

#' Share new draws among the score bins of a class
#'
#' Water-filling: one unit at a time goes to the stratum with the fewest
#' usable draws (confirmed or pending) that still has undrawn ROIs, the lower
#' bin first on a tie. Every page therefore tops up the least covered bins,
#' and a bin with few ROIs is taken whole.
#'
#' @param strata Data frame with one row per stratum of the class:
#'   \code{bin}, \code{n_population} (ROIs in the stratum), \code{n_drawn}
#'   (draws ever made in it, whatever became of them) and \code{n_have}
#'   (confirmed plus pending draws).
#' @param n_request Number of new draws wanted.
#' @return Integer vector aligned with the rows of \code{strata}: new draws
#'   per stratum. They sum to \code{n_request}, or to the number of undrawn
#'   ROIs when that is smaller.
#' @noRd
thr_allocate_draw <- function(strata, n_request) {
  columns <- c("bin", "n_population", "n_drawn", "n_have")
  thr_check_columns(strata, columns, "strata")
  if (!thr_is_count(n_request)) {
    stop("`n_request` must be a single whole number of 0 or more.",
         call. = FALSE)
  }
  counts <- lapply(stats::setNames(columns, columns), thr_check_stratum_column,
                   strata = strata)
  if (anyDuplicated(counts$bin) > 0) {
    stop("`strata$bin` lists a bin more than once; pass the strata of one ",
         "class.", call. = FALSE)
  }
  capacity <- pmax(counts$n_population - counts$n_drawn, 0L)
  level <- counts$n_have
  allocation <- integer(nrow(strata))
  by_bin <- order(counts$bin)
  for (unit in seq_len(min(n_request, sum(capacity)))) {
    open <- by_bin[allocation[by_bin] < capacity[by_bin]]
    # which.min() takes the first minimum, which is the lowest bin
    pick <- open[which.min(level[open])]
    allocation[pick] <- allocation[pick] + 1L
    level[pick] <- level[pick] + 1L
  }
  allocation
}

#' Draw units at random without replacement
#'
#' @param candidates Data frame with one row per unit that may be drawn.
#' @param n Number of units to draw; all of them when there are fewer.
#' @param seed \code{NULL} to draw from the session's random number stream,
#'   or a single number for a reproducible draw. A seed does not change the
#'   session's random number state.
#' @return The drawn rows of \code{candidates}, in the order drawn.
#' @noRd
thr_draw_units <- function(candidates, n, seed = NULL) {
  if (!is.data.frame(candidates)) {
    stop("`candidates` must be a data frame with one row per unit.",
         call. = FALSE)
  }
  if (!thr_is_count(n)) {
    stop("`n` must be a single whole number of 0 or more.", call. = FALSE)
  }
  thr_check_seed(seed)
  n_candidates <- nrow(candidates)
  # sample.int(), not sample(): a single candidate must not be read as a range
  rows <- thr_sim_with_seed(seed, function() {
    sample.int(n_candidates, min(n, n_candidates))
  })
  thr_reset_rownames(candidates[rows, , drop = FALSE])
}

#' The next page of ROIs to review for a class
#'
#' Returns the class's pending draws, oldest first, and tops the page up with
#' new draws shared among the score bins by \code{thr_allocate_draw()}. New
#' draws are stored as pending in the same transaction that selects them, so
#' a second call without a confirmation in between returns the same page.
#'
#' Only frame files that the index still holds exactly as they were frozen
#' are drawn from. A stratum whose remaining ROIs sit in files that changed
#' since gives what it can, and the rest of the page comes from the other
#' bins.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier of a frozen frame.
#' @param class_name One class label of the model.
#' @param page_size Number of ROIs on a page.
#' @param seed \code{NULL} for a random draw, or a single number for a
#'   reproducible one. A seed does not change the session's random number
#'   state.
#' @return Data frame with \code{draw_id}, \code{class_name}, \code{bin},
#'   \code{sample_name}, \code{roi_number}, \code{score} and \code{is_new}
#'   (drawn by this call), sorted by score, highest first. Zero rows when
#'   every ROI of the class has been dealt with.
#' @noRd
thr_next_page <- function(db_path, index_path, model_id, class_name,
                          page_size = 100, seed = NULL) {
  thr_check_model_id(model_id)
  if (!thr_is_single_string(class_name)) {
    stop("`class_name` must be a single class label of the model.",
         call. = FALSE)
  }
  if (!thr_is_count(page_size, minimum = 1)) {
    stop("`page_size` must be a single whole number of 1 or more.",
         call. = FALSE)
  }
  thr_check_seed(seed)
  class_name <- enc2utf8(class_name)
  con <- thr_connect_verdicts(db_path, index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_write_transaction(con, function() {
    model <- thr_require_model(con, model_id)
    thr_check_class_name(class_name, model$class_labels)
    before <- DBI::dbGetQuery(con, "
      SELECT (SELECT COALESCE(MAX(draw_id), 0) FROM draws) AS last_id,
             (SELECT COUNT(*) FROM draws WHERE model_id = ? AND class_name = ?
                AND status = 'pending') AS n_pending",
      params = list(model_id, class_name))
    if (before$n_pending < page_size) {
      thr_sim_with_seed(seed, function() {
        thr_top_up_class(con, model, class_name, page_size - before$n_pending)
      })
    }
    thr_read_page(con, model_id, class_name, page_size, before$last_id)
  })
}

#' Draw new units for a class, inside an open transaction
#'
#' @param con A DBI connection to the verdict SQLite file with the index
#'   attached and a write transaction open.
#' @param model A frozen model, see \code{thr_get_model()}.
#' @param class_name Class label.
#' @param n_request Number of new draws wanted.
#' @return Invisibly, the number of units drawn.
#' @noRd
thr_top_up_class <- function(con, model, class_name, n_request) {
  counts <- thr_stratum_query(con, model$model_id, model$class_labels)
  counts <- counts[counts$class_name == class_name, , drop = FALSE]
  strata <- data.frame(bin = counts$bin, n_population = counts$n_population,
                       n_drawn = counts$n_drawn,
                       n_have = counts$n_verdicts + counts$n_pending)
  remaining <- n_request
  # Each further round closes at least one stratum, which bounds the rounds
  for (round in seq_len(nrow(strata) + 1)) {
    allocation <- thr_allocate_draw(strata, remaining)
    if (sum(allocation) == 0) {
      break
    }
    drawn <- thr_draw_strata(con, model, class_name, strata$bin, allocation)
    strata$n_drawn <- strata$n_drawn + drawn
    strata$n_have <- strata$n_have + drawn
    remaining <- remaining - sum(drawn)
    short <- drawn < allocation
    if (!any(short)) {
      break
    }
    # The rest of such a stratum sits in files that changed since the freeze.
    # Close it so its share goes to the other bins instead of being lost.
    strata$n_population[short] <- strata$n_drawn[short]
  }
  invisible(n_request - remaining)
}

#' Draw and store the allocated units of each stratum
#'
#' @param con A DBI connection to the verdict SQLite file with the index
#'   attached and a write transaction open.
#' @param model A frozen model, see \code{thr_get_model()}.
#' @param class_name Class label.
#' @param bins Score bins of the class's strata.
#' @param allocation Units to draw per stratum, aligned with \code{bins}.
#' @return Integer vector aligned with \code{bins}: units actually drawn.
#' @noRd
thr_draw_strata <- function(con, model, class_name, bins, allocation) {
  drawn <- integer(length(bins))
  # Fixed order, so a seed gives the same draw every time
  for (i in order(bins)) {
    if (allocation[i] == 0) {
      next
    }
    candidates <- thr_stratum_candidates(con, model, class_name, bins[i])
    units <- thr_draw_units(candidates, allocation[i])
    thr_insert_draws(con, model$model_id, class_name, bins[i], units)
    drawn[i] <- nrow(units)
  }
  drawn
}

#' Undrawn ROIs of one stratum
#'
#' Only the keys of the one stratum being drawn from are read, never a whole
#' class.
#'
#' @param con A DBI connection to the verdict SQLite file with the index
#'   attached.
#' @param model A frozen model, see \code{thr_get_model()}.
#' @param class_name Class label.
#' @param bin Score bin.
#' @return Data frame with \code{sample_name}, \code{roi_number} and
#'   \code{score}, ordered by sample and ROI number.
#' @noRd
thr_stratum_candidates <- function(con, model, class_name, bin) {
  edges <- model$score_edges
  # Bin 1 has no lower bound and the last bin no upper one, as in
  # thr_score_bin(), which clamps a score that strays just outside [0, 1].
  bounds <- c(
    if (bin > 1) "AND r.score >= ?",
    if (bin < length(edges)) "AND r.score < ?"
  )
  limits <- c(if (bin > 1) edges[bin], if (bin < length(edges)) edges[bin + 1])
  sql <- paste("
    SELECT ff.sample_name, r.roi_number, r.score
    FROM frame_files ff
    JOIN", THR_FRAME_INTACT_JOIN, "
    JOIN idx.rois r ON r.file_id = f.file_id
    WHERE ff.model_id = ? AND r.class_idx = ?", paste(bounds, collapse = " "), "
      AND NOT EXISTS (
        SELECT 1 FROM draws d
        WHERE d.model_id = ff.model_id AND d.sample_name = ff.sample_name
          AND d.roi_number = r.roi_number)")
  class_idx <- match(class_name, model$class_labels)
  candidates <- DBI::dbGetQuery(
    con, sql, params = unname(c(list(model$model_id, class_idx), as.list(limits)))
  )
  candidates$roi_number <- as.integer(candidates$roi_number)
  if (!all(thr_score_bin(candidates$score, edges) == bin)) {
    stop("Internal error: the scores selected for bin ", bin, " of class '",
         class_name, "' do not all fall in that bin.", call. = FALSE)
  }
  ordered <- order(candidates$sample_name, candidates$roi_number, method = "radix")
  thr_reset_rownames(candidates[ordered, , drop = FALSE])
}

#' Store new draws as pending
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param class_name Class label.
#' @param bin Score bin of the units.
#' @param units Data frame with \code{sample_name}, \code{roi_number} and
#'   \code{score}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_insert_draws <- function(con, model_id, class_name, bin, units) {
  n_units <- nrow(units)
  if (n_units == 0) {
    return(invisible(NULL))
  }
  DBI::dbExecute(con, "
    INSERT INTO draws (model_id, class_name, bin, sample_name, roi_number, score,
                       status, drawn_at)
    VALUES (?, ?, ?, ?, ?, ?, 'pending', ?)",
    params = list(rep(model_id, n_units), rep(class_name, n_units),
                  rep(as.integer(bin), n_units), units$sample_name,
                  units$roi_number, units$score, rep(thr_now(), n_units)))
  invisible(NULL)
}

#' Read the pending draws that make up a page
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param class_name Class label.
#' @param page_size Number of ROIs on a page.
#' @param last_id Highest draw id before this page was topped up.
#' @return The data frame \code{thr_next_page()} returns.
#' @noRd
thr_read_page <- function(con, model_id, class_name, page_size, last_id) {
  # Oldest first, so the same pending draws come back until they are resolved
  page <- DBI::dbGetQuery(con, "
    SELECT draw_id, class_name, bin, sample_name, roi_number, score FROM draws
    WHERE model_id = ? AND class_name = ? AND status = 'pending'
    ORDER BY draw_id LIMIT ?",
    params = list(model_id, class_name, as.integer(page_size)))
  page <- data.frame(
    draw_id = as.integer(page$draw_id),
    class_name = enc2utf8(as.character(page$class_name)),
    bin = as.integer(page$bin),
    sample_name = as.character(page$sample_name),
    roi_number = as.integer(page$roi_number),
    score = as.numeric(page$score),
    stringsAsFactors = FALSE
  )
  page$is_new <- page$draw_id > last_id
  page <- page[order(-page$score, page$draw_id), , drop = FALSE]
  thr_reset_rownames(page)
}
