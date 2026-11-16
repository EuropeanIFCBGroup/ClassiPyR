# Shared helpers used across multiple server modules
#
# These functions are defined in the server function scope and
# referenced by several setup_*_server() modules.

# Shared constant for month abbreviation lookups
MONTH_NAMES <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",
                 "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug",
                 "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")

# Build class filter choices with unmatched classes marked
build_class_filter_choices <- function(classes, unmatched = character()) {
  # Create display names with warning for unmatched classes
  display_names <- ifelse(classes %in% unmatched,
                          paste0("\u26A0 ", classes),  # Warning symbol for unmatched
                          classes)
  c("All" = "all", setNames(classes, display_names))
}

# Suffix shown after a sample name in the sample dropdown: pencil for manual
# annotations, checkmark for auto-classifications, asterisk for neither
sample_status_suffix <- function(samples, classified, annotated) {
  has_manual <- samples %in% annotated
  has_classified <- samples %in% classified
  ifelse(has_manual & has_classified, "\u270E\u2713",
         ifelse(has_manual, "\u270E",
                ifelse(has_classified, "\u2713", "*")))
}

# The fingerprint of what was last written to storage for a sample. The
# navigation and session-end auto-saves compare against it, so a sample that
# has not changed since its last save is not written (and its images are not
# copied) a second time.
saved_state_of <- function(sample_name, classifications) {
  list(sample = sample_name,
       files = classifications$file_name,
       classes = classifications$class_name)
}

is_saved_state <- function(saved_state, sample_name, classifications) {
  !is.null(saved_state) && !is.null(classifications) &&
    identical(saved_state, saved_state_of(sample_name, classifications))
}

# Annotator name to store, never empty
annotator_or_unknown <- function(annotator) {
  if (is.null(annotator) || length(annotator) != 1 || is.na(annotator) ||
      !nzchar(trimws(annotator))) {
    "Unknown"
  } else {
    annotator
  }
}

# TRUE for an annotator name worth remembering in the settings file
is_storable_annotator <- function(annotator) {
  is.character(annotator) && length(annotator) == 1 && !is.na(annotator) &&
    nzchar(trimws(annotator))
}

# Annotator name the app starts with: the one saved by an earlier session,
# otherwise the system user
startup_annotator_name <- function(saved_settings) {
  saved <- as.character(saved_settings$annotator_name)
  if (is_storable_annotator(saved)) trimws(saved) else Sys.info()[["user"]]
}

# Filling a sample with "unclassified" ROIs reads its .adc file, which only
# exists locally: dashboard mode has no raw data folder
can_fill_unclassified <- function(data_source) {
  !identical(data_source, "dashboard")
}

# Text of the notification that ends a "PNG -> SQLite" import. `filled` is the
# result of fill_unclassified_db(), or NULL when the fill was not asked for.
png_import_summary <- function(import, filled = NULL) {
  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")
  text <- sprintf("PNG import complete: %s imported, %d failed.",
                  plural(import$success, "sample"), import$failed)
  if (is.null(filled)) return(text)

  text <- sprintf("%s %s added as unclassified in %s.", text,
                  plural(filled$added, "remaining ROI"),
                  plural(filled$samples, "sample"))
  if (filled$skipped > 0) {
    text <- sprintf("%s %s skipped (raw data not found).", text,
                    plural(filled$skipped, "sample"))
  }
  text
}

# Update the display text for current sample in dropdown to show pencil symbol
# Uses JavaScript to modify just the displayed text without rebuilding dropdown
update_current_sample_status <- function(session, sample_name,
                                         classified_samples_fn,
                                         annotated_samples_fn) {
  classified <- classified_samples_fn()
  annotated <- annotated_samples_fn()

  new_suffix <- sample_status_suffix(sample_name, classified, annotated)

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
