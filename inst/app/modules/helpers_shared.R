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
  display_names <- sapply(classes, function(cls) {
    if (cls %in% unmatched) {
      paste0("\u26A0 ", cls)  # Warning symbol for unmatched
    } else {
      cls
    }
  })
  c("All" = "all", setNames(classes, display_names))
}

# Update the display text for current sample in dropdown to show pencil symbol
# Uses JavaScript to modify just the displayed text without rebuilding dropdown
update_current_sample_status <- function(session, sample_name,
                                         classified_samples_fn,
                                         annotated_samples_fn) {
  classified <- classified_samples_fn()
  annotated <- annotated_samples_fn()

  has_manual <- sample_name %in% annotated
  has_classified <- sample_name %in% classified

  # Determine the new display suffix
  new_suffix <- if (has_manual && has_classified) {
    "\u270E\u2713"  # Both
  } else if (has_manual) {
    "\u270E"        # Pencil
  } else if (has_classified) {
    "\u2713"        # Checkmark
  } else {
    "*"             # Asterisk
  }

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
