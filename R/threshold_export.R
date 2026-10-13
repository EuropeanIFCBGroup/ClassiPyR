# Thresholds export of the threshold review app (run_threshold_app()).
#
# The JSON is read unchanged by ifcb-classify (`load_thresholds_json()` and
# `thresholds_fitted_on_augmented()` in its thresholds.py). Its reader has a
# few traps the writer here is built around:
#   - a class absent from `class_metrics` silently gets no threshold;
#   - keys must equal the class labels byte for byte;
#   - a missing `validation_transform` marks the thresholds as outdated;
#   - a path not ending in ".json" is parsed as YAML;
#   - `thresholds.json` and `*_thresholds_and_metrics.json` next to a
#     checkpoint are picked up automatically;
#   - the file is opened with the platform's default encoding, which is not
#     UTF-8 on Windows, so the text must be plain ASCII.

# Fields of one class in the export, in the order they are written.
THR_EXPORT_METRIC_FIELDS <- c(
  "class_name", "threshold", "source", "status", "reason", "n_predicted",
  "n_verdicts", "n_correct", "precision", "precision_lower",
  "precision_upper", "kept_share", "previous_threshold"
)

#' Check the model description of an export
#'
#' @param model List with \code{model_id}, \code{classifier_name},
#'   \code{checkpoint_sha256}, \code{transform} and \code{class_labels}.
#' @return The model with its optional text fields normalised to a single
#'   string or \code{NA}.
#' @noRd
thr_check_export_model <- function(model) {
  if (!is.list(model) || is.data.frame(model)) {
    stop("`model` must be a list with model_id, classifier_name, ",
         "checkpoint_sha256, transform and class_labels.", call. = FALSE)
  }
  labels <- model$class_labels
  if (!is.character(labels) || length(labels) == 0 || anyNA(labels) ||
      any(!nzchar(labels))) {
    stop("`model$class_labels` must be a character vector with the model's ",
         "class labels.", call. = FALSE)
  }
  if (anyDuplicated(labels) > 0) {
    stop("`model$class_labels` lists a class more than once: ",
         paste(unique(labels[duplicated(labels)]), collapse = ", "), ".",
         call. = FALSE)
  }
  text_or_na <- function(x) {
    if (length(x) == 1 && !is.na(x)) as.character(x) else NA_character_
  }
  transform <- text_or_na(model$transform)
  if (is.na(transform) || !nzchar(transform)) {
    stop("`model$transform` is missing. The export records it as ",
         "`validation_transform`, and ifcb-classify treats a thresholds ",
         "file without one as fitted on augmented images. Class score ",
         "files without a transform attribute cannot be used for an export.",
         call. = FALSE)
  }
  list(
    model_id = text_or_na(model$model_id),
    classifier_name = text_or_na(model$classifier_name),
    checkpoint_sha256 = text_or_na(model$checkpoint_sha256),
    transform = transform,
    class_labels = labels
  )
}

#' Order resolved thresholds by the model's class labels
#'
#' @param resolved Data frame from \code{\link{thr_resolve_thresholds}}.
#' @param class_labels The model's class labels.
#' @return \code{resolved} with exactly one row per label, in model order.
#' @noRd
thr_align_resolved <- function(resolved, class_labels) {
  thr_check_columns(
    resolved,
    c("class_name", "threshold", "source", "status", "reason", "n_predicted",
      "n_verdicts", "n_correct", "precision", "lower", "upper", "kept_share",
      "previous_threshold"),
    "resolved"
  )
  class_name <- as.character(resolved$class_name)
  if (anyDuplicated(class_name) > 0) {
    stop("`resolved` lists a class more than once: ",
         paste(unique(class_name[duplicated(class_name)]), collapse = ", "),
         ".", call. = FALSE)
  }
  absent <- setdiff(class_labels, class_name)
  unknown <- setdiff(class_name, class_labels)
  if (length(absent) > 0 || length(unknown) > 0) {
    stop(
      "`resolved` must have exactly one row per class of the model. ",
      if (length(absent) > 0) {
        paste0("Classes missing (they would get no threshold): ",
               paste(utils::head(absent, 5), collapse = ", "), ". ")
      },
      if (length(unknown) > 0) {
        paste0("Classes the model does not have: ",
               paste(utils::head(unknown, 5), collapse = ", "), ".")
      },
      call. = FALSE
    )
  }
  aligned <- resolved[match(class_labels, class_name), , drop = FALSE]
  bad <- !is.numeric(aligned$threshold) | !is.finite(aligned$threshold) |
    aligned$threshold < 0 | aligned$threshold > 1
  if (any(bad)) {
    stop("Every class needs a threshold between 0 and 1, but `resolved` has ",
         "none for: ", paste(utils::head(class_labels[bad], 5), collapse = ", "),
         ".", call. = FALSE)
  }
  aligned
}

#' Build the export entry of one class
#'
#' @param row One-row data frame of resolved thresholds.
#' @return List with the fields of \code{THR_EXPORT_METRIC_FIELDS}.
#' @noRd
thr_export_class_entry <- function(row) {
  list(
    class_name = as.character(row$class_name),
    threshold = as.numeric(row$threshold),
    source = as.character(row$source),
    status = as.character(row$status),
    reason = as.character(row$reason),
    n_predicted = as.integer(row$n_predicted),
    n_verdicts = as.integer(row$n_verdicts),
    n_correct = as.integer(row$n_correct),
    precision = as.numeric(row$precision),
    precision_lower = as.numeric(row$lower),
    precision_upper = as.numeric(row$upper),
    kept_share = as.numeric(row$kept_share),
    previous_threshold = as.numeric(row$previous_threshold)
  )
}

#' Format the time an export was generated
#'
#' @param generated_at \code{NULL}, a single string, or a date-time.
#' @return \code{NULL} or a single string (ISO 8601 in UTC for a date-time).
#' @noRd
thr_format_generated_at <- function(generated_at) {
  if (is.null(generated_at)) {
    return(NULL)
  }
  if (inherits(generated_at, "POSIXt") && length(generated_at) == 1) {
    return(format(generated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  if (is.character(generated_at) && length(generated_at) == 1 &&
      !is.na(generated_at)) {
    return(generated_at)
  }
  stop("`generated_at` must be NULL, a single string or a date-time.",
       call. = FALSE)
}

#' Build the thresholds export of a model
#'
#' Combines the resolved thresholds with the model's identity into the list
#' that \code{\link{write_thresholds_json}} writes. Every class of the model
#' is included, because ifcb-classify applies no threshold at all to a class
#' that is absent from the file.
#'
#' @param resolved Data frame from \code{\link{thr_resolve_thresholds}}, one
#'   row per class of the model (in any order).
#' @param model List with \code{model_id}, \code{classifier_name},
#'   \code{checkpoint_sha256}, \code{transform} and \code{class_labels}.
#' @param target Target precision the thresholds were fitted for.
#' @param criterion \code{"estimate"} or \code{"lower"}; see
#'   \code{\link{choose_class_threshold}}.
#' @param fallback_source Where the fallback thresholds came from, or
#'   \code{NA}.
#' @param edges Score bin edges the review was stratified by.
#' @param generated_by Name and version of the writing software.
#' @param generated_at \code{NULL} to leave the time out (which keeps the
#'   output reproducible), a single string, or a date-time.
#' @return A list ready for \code{\link{write_thresholds_json}}:
#'   \code{model_name}, \code{num_classes}, \code{validation_transform},
#'   \code{checkpoint_sha256}, \code{model_id}, \code{threshold_method}
#'   (\code{"holdout_review"}), \code{target_precision}, \code{criterion},
#'   \code{fallback_source}, \code{score_edges}, \code{generated_by},
#'   \code{generated_at} (only when given) and \code{class_metrics}, a list
#'   named by the raw class labels in model order.
#' @noRd
build_thresholds_export <- function(resolved, model, target,
                                    criterion = "estimate",
                                    fallback_source = NA_character_,
                                    edges = THR_SCORE_EDGES,
                                    generated_by = paste(
                                      "ClassiPyR",
                                      utils::packageVersion("ClassiPyR")
                                    ),
                                    generated_at = NULL) {
  model <- thr_check_export_model(model)
  thr_check_target(target)
  criterion <- match.arg(criterion, c("estimate", "lower"))
  edges <- thr_validate_edges(edges)
  aligned <- thr_align_resolved(resolved, model$class_labels)

  class_metrics <- lapply(seq_len(nrow(aligned)), function(i) {
    thr_export_class_entry(aligned[i, , drop = FALSE])
  })
  names(class_metrics) <- model$class_labels

  header <- list(
    model_name = model$classifier_name,
    num_classes = length(model$class_labels),
    validation_transform = model$transform,
    checkpoint_sha256 = model$checkpoint_sha256,
    model_id = model$model_id,
    threshold_method = "holdout_review",
    target_precision = as.numeric(target),
    criterion = criterion,
    fallback_source = as.character(fallback_source)[1],
    score_edges = edges,
    generated_by = as.character(generated_by)[1]
  )
  generated_at <- thr_format_generated_at(generated_at)
  if (!is.null(generated_at)) {
    header$generated_at <- generated_at
  }
  c(header, list(class_metrics = class_metrics))
}

#' Check the per-class part of an export
#'
#' @param export Export list.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_export <- function(export) {
  metrics <- if (is.list(export) && !is.data.frame(export)) {
    export$class_metrics
  }
  if (!is.list(metrics) || length(metrics) == 0) {
    stop("`export` must be a list with a non-empty `class_metrics` list, ",
         "one entry per class of the model.", call. = FALSE)
  }
  labels <- names(metrics)
  if (is.null(labels) || anyNA(labels) || any(!nzchar(labels))) {
    stop("`export$class_metrics` must be named by class label.",
         call. = FALSE)
  }
  if (anyDuplicated(labels) > 0) {
    stop("`export$class_metrics` lists a class more than once: ",
         paste(unique(labels[duplicated(labels)]), collapse = ", "), ".",
         call. = FALSE)
  }
  usable <- vapply(metrics, function(entry) {
    value <- if (is.list(entry)) entry$threshold
    is.numeric(value) && length(value) == 1 && is.finite(value) &&
      value >= 0 && value <= 1
  }, logical(1))
  if (!all(usable)) {
    stop("Every class needs a single threshold between 0 and 1, but ",
         "`export$class_metrics` has none for: ",
         paste(utils::head(labels[!usable], 5), collapse = ", "),
         ". ifcb-classify applies no threshold to such a class.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Check that an export records the transform its scores were made with
#'
#' @param export Export list.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_export_transform <- function(export) {
  transform <- export$validation_transform
  ok <- is.character(transform) && length(transform) == 1 &&
    !is.na(transform) && nzchar(transform)
  if (!ok) {
    stop("`export$validation_transform` is missing. ifcb-classify treats a ",
         "thresholds file without it as fitted on augmented images and ",
         "warns that the thresholds are outdated. Set it to the transform ",
         "the class scores were made with.", call. = FALSE)
  }
  invisible(NULL)
}

#' Check an output path before writing
#'
#' @param path File path to write.
#' @param overwrite Whether an existing file may be replaced.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_export_path <- function(path, overwrite) {
  if (!is.character(path) || length(path) != 1 || is.na(path) ||
      !nzchar(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!dir.exists(dirname(path))) {
    stop("The folder ", dirname(path), " does not exist. Create it first or ",
         "choose another location.", call. = FALSE)
  }
  if (dir.exists(path)) {
    stop(path, " is a folder. `path` must name a file.", call. = FALSE)
  }
  if (file.exists(path) && !overwrite) {
    stop(path, " already exists. Choose another name, or pass ",
         "`overwrite = TRUE` to replace it.", call. = FALSE)
  }
  invisible(NULL)
}

#' Refuse JSON file names that ifcb-classify would misread or auto-detect
#'
#' @param path File path to write.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_json_name <- function(path) {
  # Case-sensitive on purpose: ifcb-classify tests `path.endswith(".json")`.
  if (!grepl("\\.json$", path)) {
    stop("`path` must end in lowercase \".json\", not ", basename(path),
         ": ifcb-classify reads a thresholds file with any other ending as ",
         "YAML and would fail on it.", call. = FALSE)
  }
  # Case-insensitive on purpose: the file systems of Windows and macOS are.
  name <- tolower(basename(path))
  if (name == "thresholds.json" ||
      grepl("_thresholds_and_metrics\\.json$", name)) {
    stop(basename(path), " is a name ifcb-classify looks for next to a ",
         "checkpoint and loads automatically, so the export would silently ",
         "take the place of the thresholds from training. Use another name, ",
         "such as <model>_holdout_thresholds.json, and pass it to ",
         "ifcb-classify with --thresholds.", call. = FALSE)
  }
  invisible(NULL)
}

#' Write text to a file as UTF-8 with Unix line endings
#'
#' @param text Single string.
#' @param path File path.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_write_text <- function(text, path) {
  tryCatch({
    # Binary mode: no newline translation and no re-encoding on Windows
    con <- suppressWarnings(file(path, open = "wb"))
    on.exit(close(con), add = TRUE)
    writeLines(enc2utf8(text), con, sep = "\n", useBytes = TRUE)
  }, error = function(e) {
    stop("Could not write ", path, ": ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

#' Escape every non-ASCII character of a JSON text
#'
#' jsonlite writes non-ASCII characters as they are. Its own escapes and the
#' JSON syntax are plain ASCII, so any character above 127 is part of a
#' string value and can be replaced by its \code{\\uXXXX} escape without
#' touching anything else. Characters above the Basic Multilingual Plane
#' become a surrogate pair, as JSON requires.
#'
#' @param text JSON text as a single string.
#' @return The same JSON as a pure ASCII string.
#' @noRd
thr_json_escape_non_ascii <- function(text) {
  text <- enc2utf8(text)
  code_points <- utf8ToInt(text)
  if (anyNA(code_points)) {
    stop("The export contains text that is not valid UTF-8, so it cannot be ",
         "written as JSON. Check the class labels.", call. = FALSE)
  }
  non_ascii <- code_points > 127L
  if (!any(non_ascii)) {
    return(text)
  }
  pieces <- intToUtf8(code_points, multiple = TRUE)
  wide <- code_points[non_ascii]
  offset <- pmax(wide - 65536L, 0L)
  pieces[non_ascii] <- ifelse(
    wide < 65536L,
    sprintf("\\u%04x", wide),
    sprintf("\\u%04x\\u%04x", 55296L + offset %/% 1024L,
            56320L + offset %% 1024L)
  )
  paste(pieces, collapse = "")
}

#' Render an export as JSON text
#'
#' \code{digits = I(17)} writes 17 significant digits, the fewest that bring
#' every double back exactly (jsonlite's \code{digits = NA} stops at 15 and
#' does not). \code{always_decimal} writes a whole-number threshold as
#' \code{0.0} rather than \code{0}, so it is read back as a double.
#'
#' @param export Export list.
#' @return Pure ASCII JSON text as a single string.
#' @noRd
thr_export_json_text <- function(export) {
  # I() keeps a single edge an array under auto_unbox
  if (!is.null(export$score_edges)) {
    export$score_edges <- I(as.numeric(export$score_edges))
  }
  export$class_metrics <- lapply(export$class_metrics, function(entry) {
    entry$threshold <- as.numeric(entry$threshold)
    entry
  })
  json <- jsonlite::toJSON(export, auto_unbox = TRUE, digits = I(17),
                           always_decimal = TRUE, na = "null", null = "null",
                           pretty = TRUE)
  thr_json_escape_non_ascii(as.character(json))
}

#' Write thresholds as a JSON file for ifcb-classify
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes per-class thresholds in the JSON layout that the Python pipeline
#' ifcb-classify loads with `--thresholds`.
#'
#' @details
#' The file is written so that ifcb-classify reads back exactly what was
#' exported:
#'
#' * Numbers are written with 17 significant digits and thresholds always
#'   with a decimal point, so every value is read back bit for bit.
#' * The text is plain ASCII: non-ASCII characters in class labels are
#'   written as `\uXXXX` escapes. ifcb-classify opens the file with the
#'   platform's default encoding, which is not UTF-8 on Windows.
#' * Missing values are written as `null`. A threshold is never missing.
#'
#' The function refuses to write a file that ifcb-classify would misread:
#'
#' * `path` must end in lowercase `.json`, because any other ending is read
#'   as YAML.
#' * `path` must not be named `thresholds.json` or end in
#'   `_thresholds_and_metrics.json`. ifcb-classify picks those names up
#'   automatically next to a model checkpoint, so the export would silently
#'   replace the thresholds from training.
#' * Every class needs a threshold between 0 and 1, because a class that is
#'   absent from the file gets no threshold at all.
#' * `validation_transform` must be set, because a file without it is taken
#'   as fitted on augmented images.
#'
#' @param export A list describing the export. It needs
#'   `validation_transform` (the transform the class scores were made with)
#'   and `class_metrics`, a list named by class label in which every entry
#'   has a `threshold` between 0 and 1. Other fields are written as they
#'   are. The threshold review app builds this list from the review results.
#' @param path File to write. Must end in `.json`.
#' @param overwrite Whether to replace an existing file. Default `FALSE`.
#' @return `path`, invisibly.
#' @export
#' @seealso [write_thresholds_csv()] and [write_thresholds_report_csv()] for
#'   tables to read or share.
#' @examples
#' export <- list(
#'   model_name = "my_model",
#'   num_classes = 2L,
#'   validation_transform = "dataset_squarepad",
#'   class_metrics = list(
#'     Skeletonema_spp = list(class_name = "Skeletonema_spp", threshold = 0.9),
#'     Debris = list(class_name = "Debris", threshold = 0)
#'   )
#' )
#'
#' path <- tempfile("my_model_holdout_thresholds_", fileext = ".json")
#' write_thresholds_json(export, path)
#'
#' jsonlite::fromJSON(path)$class_metrics$Skeletonema_spp$threshold
#' unlink(path)
#' @md
write_thresholds_json <- function(export, path, overwrite = FALSE) {
  thr_check_export(export)
  thr_check_export_transform(export)
  if (is.character(path) && length(path) == 1 && !is.na(path) &&
      nzchar(path)) {
    thr_check_json_name(path)
  }
  thr_check_export_path(path, overwrite)
  thr_write_text(thr_export_json_text(export), path)
  invisible(path)
}

#' Format numbers so that they read back exactly
#'
#' Uses the fewest of 15 to 17 significant digits that return the same
#' double, so \code{0.9999} stays \code{0.9999} while \code{0.1 + 0.2} keeps
#' all its digits.
#'
#' @param x Numeric vector.
#' @return Character vector; \code{""} for missing values.
#' @noRd
thr_format_number <- function(x) {
  x <- as.numeric(x)
  text <- sprintf("%.15g", x)
  for (digits in 16:17) {
    inexact <- !is.na(x) & as.numeric(text) != x
    text[inexact] <- sprintf(paste0("%.", digits, "g"), x[inexact])
  }
  text[is.na(x)] <- ""
  text
}

#' Quote CSV fields that need it
#'
#' @param x Character vector.
#' @return Character vector with fields holding a comma, quote, line break
#'   or outer whitespace wrapped in double quotes.
#' @noRd
thr_csv_field <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  needs_quotes <- grepl("[\",\r\n]|^\\s|\\s$", x)
  x[needs_quotes] <- paste0("\"", gsub("\"", "\"\"", x[needs_quotes]), "\"")
  x
}

#' Collect one field of every class of an export as CSV text
#'
#' @param metrics The \code{class_metrics} list of an export.
#' @param field Name of the field.
#' @return Character vector with one CSV field per class; empty where the
#'   field is absent or missing.
#' @noRd
thr_export_csv_column <- function(metrics, field) {
  vapply(metrics, function(entry) {
    value <- entry[[field]]
    if (length(value) != 1 || is.na(value)) {
      return("")
    }
    if (is.numeric(value)) thr_format_number(value) else thr_csv_field(value)
  }, character(1), USE.NAMES = FALSE)
}

#' Write the thresholds of an export as a two-column CSV file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes one row per class with the columns `class` and `threshold`, as
#' UTF-8. This is a plain table for reading, sharing or other tools; to apply
#' the thresholds in ifcb-classify, use [write_thresholds_json()].
#'
#' Thresholds are written with as many digits as it takes to read the same
#' number back, and no more.
#'
#' @inheritParams write_thresholds_json
#' @param path File to write.
#' @return `path`, invisibly.
#' @export
#' @seealso [write_thresholds_report_csv()] for the full review report.
#' @examples
#' export <- list(
#'   class_metrics = list(
#'     Skeletonema_spp = list(class_name = "Skeletonema_spp", threshold = 0.9),
#'     Debris = list(class_name = "Debris", threshold = 0)
#'   )
#' )
#'
#' path <- tempfile("my_model_thresholds_", fileext = ".csv")
#' write_thresholds_csv(export, path)
#'
#' read.csv(path)
#' unlink(path)
#' @md
write_thresholds_csv <- function(export, path, overwrite = FALSE) {
  thr_check_export(export)
  thr_check_export_path(path, overwrite)
  metrics <- export$class_metrics
  rows <- paste(thr_csv_field(names(metrics)),
                thr_export_csv_column(metrics, "threshold"), sep = ",")
  thr_write_text(paste(c("class,threshold", rows), collapse = "\n"), path)
  invisible(path)
}

#' Write the per-class review report of an export as a CSV file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes one row per class with everything the export records about it: the
#' threshold, where it came from (`source`, `status`, `reason`), the number
#' of predictions and verdicts, the estimated precision with its interval,
#' the share of predictions kept, and the model's previous threshold. The
#' file is UTF-8; missing values are left empty.
#'
#' @inheritParams write_thresholds_csv
#' @return `path`, invisibly.
#' @export
#' @seealso [write_thresholds_json()] for the file ifcb-classify loads.
#' @examples
#' export <- list(
#'   class_metrics = list(
#'     Skeletonema_spp = list(
#'       class_name = "Skeletonema_spp", threshold = 0.9, source = "fitted",
#'       status = "fitted", n_predicted = 400L, n_verdicts = 120L,
#'       n_correct = 110L, precision = 0.93, kept_share = 0.75
#'     ),
#'     Debris = list(
#'       class_name = "Debris", threshold = 0, source = "fallback",
#'       status = "not_reviewed", n_predicted = 60L, n_verdicts = 0L
#'     )
#'   )
#' )
#'
#' path <- tempfile("my_model_threshold_report_", fileext = ".csv")
#' write_thresholds_report_csv(export, path)
#'
#' read.csv(path)
#' unlink(path)
#' @md
write_thresholds_report_csv <- function(export, path, overwrite = FALSE) {
  thr_check_export(export)
  thr_check_export_path(path, overwrite)
  # The key is the label ifcb-classify matches on, so it names the row even
  # when an entry carries no class_name of its own.
  metrics <- Map(function(entry, label) {
    utils::modifyList(entry, list(class_name = label))
  }, export$class_metrics, names(export$class_metrics))
  columns <- lapply(THR_EXPORT_METRIC_FIELDS, thr_export_csv_column,
                    metrics = metrics)
  rows <- do.call(paste, c(columns, sep = ","))
  header <- paste(THR_EXPORT_METRIC_FIELDS, collapse = ",")
  thr_write_text(paste(c(header, rows), collapse = "\n"), path)
  invisible(path)
}

#' Default file name of a thresholds export
#'
#' The name ends in \code{_holdout_thresholds.json}, which ifcb-classify does
#' not pick up automatically next to a checkpoint.
#'
#' @param classifier_name Name of the classifier, used as the file name stem.
#'   Characters that are not allowed in file names are replaced.
#' @return A file name such as \code{"my_model_holdout_thresholds.json"}.
#' @noRd
thr_default_export_name <- function(classifier_name) {
  stem <- if (length(classifier_name) == 1 && !is.na(classifier_name)) {
    enc2utf8(as.character(classifier_name))
  } else {
    ""
  }
  stem <- gsub("[/\\\\:*?\"<>|[:cntrl:]\\s]+", "_", stem, perl = TRUE)
  stem <- sub("^\\.+", "", stem)
  if (!nzchar(stem)) {
    stem <- "model"
  }
  paste0(stem, "_holdout_thresholds.json")
}
