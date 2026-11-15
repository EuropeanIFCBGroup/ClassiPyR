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
