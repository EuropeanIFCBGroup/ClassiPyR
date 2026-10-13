# Results and Export pages of the threshold review app (run_threshold_app())
#
# The table of chosen thresholds, the precision curve of one class and the
# export. The export runs the same resolution as the table, with the same
# seed, so the file holds the numbers the reviewer looked at.

# Plain words for the status of a class
THR_STATUS_LABELS <- c(
  fitted = "Fitted",
  manual = "Set manually",
  not_reviewed = "Not reviewed",
  under_reviewed = "Too few verdicts",
  too_few_predictions = "Too few predictions",
  target_unreachable = "Target not reachable"
)

# Colours of the precision curve plot
THR_PLOT_COLOURS <- c(precision = "#2C3E50", band = "#D6DBDF",
                      kept = "#3498DB", target = "#E74C3C",
                      chosen = "#18BC9C", previous = "#7B8A8B")

#' Status of a class in plain words
#'
#' @param status Character vector of status codes from
#'   \code{thr_resolve_thresholds()}.
#' @return Character vector; a code that is not known is returned as it is.
#' @noRd
thr_status_label <- function(status) {
  status <- as.character(status)
  label <- as.character(unname(THR_STATUS_LABELS[status]))
  unknown <- is.na(label)
  label[unknown] <- status[unknown]
  label
}

#' Format an estimate with its interval
#'
#' @param precision,lower,upper Numeric vectors of shares between 0 and 1.
#' @return Character vector such as \code{"93.1\% (88.0\% to 96.5\%)"};
#'   \code{""} where there is no estimate.
#' @noRd
thr_format_interval <- function(precision, lower, upper) {
  text <- sprintf("%.1f%%", 100 * precision)
  bounded <- !is.na(lower) & !is.na(upper)
  text[bounded] <- sprintf("%s (%.1f%% to %.1f%%)", text[bounded],
                           100 * lower[bounded], 100 * upper[bounded])
  text[is.na(precision)] <- ""
  text
}

#' The resolved thresholds, for display
#'
#' @param resolved Data frame from \code{thr_resolve_thresholds_db()}.
#' @param conf_level Confidence level of its intervals, for the column name.
#' @return Data frame with one row per class, in the same order:
#'   \code{Class}, \code{Predicted}, \code{Verdicts}, \code{Status},
#'   \code{Source}, \code{Previous threshold}, \code{Chosen threshold},
#'   \code{Precision (95\% interval)} and \code{Share kept}.
#' @noRd
thr_results_table <- function(resolved, conf_level = 0.95) {
  thr_check_columns(
    resolved,
    c("class_name", "threshold", "source", "status", "n_predicted",
      "n_verdicts", "precision", "lower", "upper", "kept_share",
      "previous_threshold"),
    "resolved"
  )
  table <- data.frame(
    Class = resolved$class_name,
    Predicted = as.integer(resolved$n_predicted),
    Verdicts = as.integer(resolved$n_verdicts),
    Status = thr_status_label(resolved$status),
    Source = resolved$source,
    previous = as.numeric(resolved$previous_threshold),
    chosen = as.numeric(resolved$threshold),
    precision = thr_format_interval(resolved$precision, resolved$lower,
                                    resolved$upper),
    kept = ifelse(is.na(resolved$kept_share), "",
                  sprintf("%.1f%%", 100 * resolved$kept_share)),
    stringsAsFactors = FALSE
  )
  names(table)[6:9] <- c(
    "Previous threshold", "Chosen threshold",
    sprintf("Precision (%s%% interval)", format(100 * conf_level)),
    "Share kept"
  )
  table
}

#' Precision curve of one class of a frozen model
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param class_name Class label.
#' @param seed Seed of the interval simulation. The default is the one the
#'   app resolves thresholds with, so the curve shows the same numbers as
#'   the results table.
#' @return The data frame of \code{estimate_precision_curve()}, over the
#'   score edges the frame was frozen with.
#' @noRd
thr_class_curve <- function(db_path, model_id, class_name,
                            seed = THR_APP_SEED) {
  model <- thr_get_model(db_path, model_id)
  counts <- thr_stratum_counts(db_path, model_id, class_name)
  estimate_precision_curve(counts, edges = model$score_edges, seed = seed)
}

#' Position of a threshold on the bin axis of the curve plot
#'
#' The plot gives every score bin the same width, because the bins that
#' matter most are far too narrow to see on a linear score axis. A threshold
#' inside a bin is placed in proportion to its distance from the bin's edges.
#'
#' @param threshold Numeric vector of thresholds between 0 and 1.
#' @param edges Lower edges of the score bins.
#' @return Numeric vector of positions: bin \code{b} spans \code{b} to
#'   \code{b + 1}. \code{NA} for a missing threshold.
#' @noRd
thr_threshold_position <- function(threshold, edges) {
  threshold <- as.numeric(threshold)
  position <- rep(NA_real_, length(threshold))
  known <- !is.na(threshold)
  bin <- findInterval(threshold[known], edges)
  lower <- edges[bin]
  width <- c(edges[-1], 1)[bin] - lower
  position[known] <- bin + ifelse(width > 0,
                                  (threshold[known] - lower) / width, 0)
  position
}

#' Extend a curve by one point so its last step is drawn
#'
#' @param values Values at the edges.
#' @return \code{values} with the last one repeated.
#' @noRd
thr_step_values <- function(values) {
  c(values, values[length(values)])
}

#' Draw the interval band and the lines of the curve plot
#'
#' @param curve Data frame from \code{estimate_precision_curve()}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_plot_curve_lines <- function(curve) {
  x <- c(curve$bin, nrow(curve) + 1)
  for (i in which(!is.na(curve$lower) & !is.na(curve$upper))) {
    graphics::rect(i, curve$lower[i], i + 1, curve$upper[i],
                   col = THR_PLOT_COLOURS[["band"]], border = NA)
  }
  graphics::lines(x, thr_step_values(curve$kept_share), type = "s", lwd = 1.5,
                  col = THR_PLOT_COLOURS[["kept"]])
  graphics::lines(x, thr_step_values(curve$precision), type = "s", lwd = 2,
                  col = THR_PLOT_COLOURS[["precision"]])
  # A filled point is an edge the review supports; a hollow one is not yet
  graphics::points(curve$bin, curve$precision,
                   pch = ifelse(curve$supported, 19, 1),
                   col = THR_PLOT_COLOURS[["precision"]])
  invisible(NULL)
}

#' Draw the target and the threshold markers of the curve plot
#'
#' @param target Target precision.
#' @param chosen_x,previous_x Positions of the chosen and the previous
#'   threshold on the bin axis, or \code{NA}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_plot_markers <- function(target, chosen_x, previous_x) {
  graphics::abline(h = target, lty = 2, col = THR_PLOT_COLOURS[["target"]])
  if (!is.na(previous_x)) {
    graphics::abline(v = previous_x, lty = 3, lwd = 2,
                     col = THR_PLOT_COLOURS[["previous"]])
  }
  if (!is.na(chosen_x)) {
    graphics::abline(v = chosen_x, lwd = 2,
                     col = THR_PLOT_COLOURS[["chosen"]])
  }
  invisible(NULL)
}

#' Draw the legend of the curve plot in a panel of its own
#'
#' A separate panel keeps the legend clear of the axis labels at any plot
#' size.
#'
#' @return \code{NULL}, invisibly.
#' @noRd
thr_plot_legend <- function() {
  # xpd: a legend wider than a narrow plot is cut by the page, not the panel
  graphics::par(mar = c(0, 0, 0, 0), xpd = NA)
  graphics::plot.new()
  graphics::legend(
    "center", ncol = 3, bty = "n", cex = 0.85,
    legend = c("Precision above the threshold", "Its interval",
               "Share of predictions kept", "Target precision",
               "Chosen threshold", "Previous threshold"),
    col = THR_PLOT_COLOURS[c("precision", "band", "kept", "target", "chosen",
                             "previous")],
    lty = c(1, NA, 1, 2, 1, 3), lwd = c(2, NA, 1.5, 1, 2, 2),
    pch = c(19, 15, NA, NA, NA, NA), pt.cex = c(1, 1.8, 1, 1, 1, 1)
  )
  invisible(NULL)
}

#' Plot the precision curve of a class
#'
#' Base graphics. Every score bin has the same width on the horizontal axis,
#' labelled with its lower edge (a candidate threshold). The precision at or
#' above each candidate is a step line with its interval as a band; filled
#' points are candidates the review supports. The share of predictions kept
#' is a second step line. The target is a dashed horizontal line; the chosen
#' and the previous threshold are vertical lines.
#'
#' @param curve Data frame from \code{estimate_precision_curve()}.
#' @param target Target precision.
#' @param chosen The threshold chosen for the class, or \code{NA}.
#' @param previous The model's previous threshold, or \code{NA}.
#' @param main Plot title, or \code{NULL}.
#' @return Invisibly, a list with \code{x} (the position of each candidate),
#'   \code{chosen_x}, \code{previous_x} and \code{n_estimated} (candidates
#'   with an estimate).
#' @noRd
thr_plot_precision_curve <- function(curve, target, chosen = NA_real_,
                                     previous = NA_real_, main = NULL) {
  thr_check_columns(curve, c("threshold", "bin", "kept_share", "precision",
                             "lower", "upper", "supported"), "curve")
  thr_check_target(target)
  chosen_x <- thr_threshold_position(chosen, curve$threshold)[1]
  previous_x <- thr_threshold_position(previous, curve$threshold)[1]

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::layout(matrix(1:2), heights = c(5, 1))
  graphics::par(mar = c(5.6, 4.2, 2.6, 1))
  graphics::plot(
    NA, xlim = c(1, nrow(curve) + 1), ylim = c(0, 1), xaxt = "n", las = 1,
    xaxs = "i", xlab = "", ylab = "Share", main = main
  )
  graphics::axis(1, at = curve$bin, las = 2,
                 labels = thr_format_threshold(curve$threshold))
  graphics::mtext("Threshold (lower edge of each score bin)", side = 1,
                  line = 4.4)
  thr_plot_curve_lines(curve)
  thr_plot_markers(target, chosen_x, previous_x)
  if (all(is.na(curve$precision))) {
    graphics::text(mean(c(1, nrow(curve) + 1)), 0.5,
                   "No precision estimate yet: review this class first.")
  }
  thr_plot_legend()
  invisible(list(x = curve$bin, chosen_x = chosen_x, previous_x = previous_x,
                 n_estimated = sum(!is.na(curve$precision))))
}

#' Count the classes of an export by where their threshold comes from
#'
#' @param resolved Data frame from \code{thr_resolve_thresholds_db()}.
#' @return Data frame with \code{Source}, \code{Status} (in plain words) and
#'   \code{Classes}: fitted first, then manual, then fallback by status.
#' @noRd
thr_export_summary <- function(resolved) {
  thr_check_columns(resolved, c("source", "status"), "resolved")
  source_order <- c("fitted", "manual", "fallback")
  status_order <- names(THR_STATUS_LABELS)
  counts <- as.data.frame(table(source = resolved$source,
                                status = resolved$status),
                          stringsAsFactors = FALSE)
  counts <- counts[counts$Freq > 0, , drop = FALSE]
  counts <- counts[order(match(counts$source, source_order),
                         match(counts$status, status_order)), , drop = FALSE]
  data.frame(
    Source = as.character(counts$source),
    Status = thr_status_label(counts$status),
    Classes = as.integer(counts$Freq),
    stringsAsFactors = FALSE
  )
}

#' Paths of the three files of an export
#'
#' @param out_folder Folder to write to. It must exist.
#' @param file_name Name of the JSON file. It must be a bare file name
#'   ending in lowercase \code{.json}; the names of the two CSV files are
#'   derived from it.
#' @return A list with \code{json}, \code{csv} and \code{report}.
#' @noRd
thr_export_paths <- function(out_folder, file_name) {
  out_folder <- thr_clean_path_input(out_folder)
  if (is.null(out_folder)) {
    stop("Enter an export folder.", call. = FALSE)
  }
  if (!dir.exists(out_folder)) {
    stop("The export folder does not exist: ", out_folder, ". Create it ",
         "first or choose another folder.", call. = FALSE)
  }
  file_name <- thr_clean_path_input(file_name)
  # A name with a folder in it would write outside the chosen folder
  if (is.null(file_name) || grepl("[/\\\\]", file_name) ||
      file_name %in% c(".", "..")) {
    stop("Enter a file name for the export, without a folder, such as ",
         "my_model_holdout_thresholds.json.", call. = FALSE)
  }
  if (!grepl("\\.json$", file_name)) {
    stop("The export file name must end in lowercase \".json\": ",
         "ifcb-classify reads a thresholds file with any other ending as ",
         "YAML.", call. = FALSE)
  }
  out_folder <- thr_normalise_folder(out_folder)
  stem <- sub("\\.json$", "", file_name)
  list(
    json = file.path(out_folder, file_name),
    csv = file.path(out_folder, paste0(stem, ".csv")),
    report = file.path(out_folder, paste0(stem, "_report.csv"))
  )
}

#' How to hand an export to ifcb-classify
#'
#' @param json_path Path of the exported JSON file.
#' @return The command-line option, with the path in double quotes.
#' @noRd
thr_export_hint <- function(json_path) {
  paste0("--thresholds \"", json_path, "\"")
}

#' Export the thresholds of a review
#'
#' Resolves the threshold of every class, builds the export and writes three
#' files: the JSON ifcb-classify loads, a \code{class,threshold} CSV and a
#' per-class report CSV. All three paths are checked before anything is
#' written, so a refused export leaves no file behind.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param target Target precision.
#' @param criterion \code{"estimate"} or \code{"lower"}.
#' @param out_folder Folder to write to.
#' @param file_name Name of the JSON file; see \code{thr_export_paths()}.
#' @param overwrite Whether existing files may be replaced.
#' @param seed Seed of the interval simulation; the default is the one the
#'   Results page uses.
#' @param generated_at Time recorded in the JSON, or \code{NULL} for none.
#' @return A list with the paths \code{json}, \code{csv} and \code{report},
#'   \code{resolved} (the data frame the files were written from) and
#'   \code{hint} (the ifcb-classify option that loads the JSON).
#' @noRd
thr_export_review <- function(db_path, model_id, target, criterion,
                              out_folder, file_name, overwrite = FALSE,
                              seed = THR_APP_SEED, generated_at = Sys.time()) {
  paths <- thr_export_paths(out_folder, file_name)
  thr_check_json_name(paths$json)
  existing <- unlist(paths)[file.exists(unlist(paths))]
  if (length(existing) > 0 && !isTRUE(overwrite)) {
    stop("Nothing was written, because these files already exist: ",
         paste(basename(existing), collapse = ", "), ". Choose another file ",
         "name, or allow existing files to be replaced.", call. = FALSE)
  }
  model <- thr_get_model(db_path, model_id)
  resolved <- thr_resolve_thresholds_db(db_path, model_id, target = target,
                                        criterion = criterion, seed = seed)
  export <- build_thresholds_export(
    resolved, model, target = target, criterion = criterion,
    fallback_source = model$fallback_source, edges = model$score_edges,
    generated_at = generated_at
  )
  write_thresholds_json(export, paths$json, overwrite = isTRUE(overwrite))
  write_thresholds_csv(export, paths$csv, overwrite = isTRUE(overwrite))
  write_thresholds_report_csv(export, paths$report,
                              overwrite = isTRUE(overwrite))
  c(paths, list(resolved = resolved, hint = thr_export_hint(paths$json)))
}
