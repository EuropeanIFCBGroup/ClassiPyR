# UI definition for ClassiPyR
#
# This file defines the user interface layout using bslib's page_sidebar.
# The UI consists of:
#
# - SIDEBAR: Annotator name, settings button, sample selection, save button
# - MAIN AREA: Tabbed interface with:
#   - Image Gallery: Paginated grid of images with toolbar
#   - Summary Table: Class distribution statistics
#   - Validation Statistics: Accuracy metrics and change log
#
# JavaScript functions defined here:
# - gallery_js(): Click selection and drag-select for images
# - MutationObserver: Dynamic styling for unmatched class warnings
#
# Custom CSS:
# - Styling for selected/relabeled images
# - Dropdown menu styling for unmatched classes (yellow warning)
# - Responsive toolbar layout

#' JavaScript code for image gallery interactions
gallery_js <- function() {
  "
  // Single click selection
  $(document).on('click', '.image-card', function(e) {
    if (window.wasDragging) {
      window.wasDragging = false;
      return;
    }
    var img = $(this).data('img');
    $(this).toggleClass('selected');
    updateCardStyle($(this));
    Shiny.setInputValue('toggle_image', {img: img, time: new Date()});
  });

  function updateCardStyle(card) {
    if (card.hasClass('selected')) {
      card.css({'border': '3px solid #007bff', 'background-color': '#e7f1ff'});
    } else {
      var wasRelabeled = card.data('relabeled') === 'true';
      if (wasRelabeled) {
        card.css({'border': '3px solid #ffc107', 'background-color': 'white'});
      } else {
        card.css({'border': '1px solid #ddd', 'background-color': 'white'});
      }
    }
  }

  // Drag-select functionality
  var isDragging = false;
  var startX, startY;
  var selectionBox = null;

  // Start drag from anywhere in the gallery container
  $(document).on('mousedown', '.gallery-drag-area', function(e) {
    if (e.button !== 0) return;
    if ($(e.target).closest('.image-card').length && !e.shiftKey) return;

    isDragging = true;
    window.wasDragging = false;
    startX = e.pageX;
    startY = e.pageY;

    selectionBox = $('#selection-box');
    selectionBox.css({
      left: startX + 'px',
      top: startY + 'px',
      width: '0px',
      height: '0px',
      display: 'block'
    });

    e.preventDefault();
  });

  $(document).on('mousemove', function(e) {
    if (!isDragging) return;

    var currentX = e.pageX;
    var currentY = e.pageY;

    var width = Math.abs(currentX - startX);
    var height = Math.abs(currentY - startY);
    var left = Math.min(startX, currentX);
    var top = Math.min(startY, currentY);

    selectionBox.css({
      left: left + 'px',
      top: top + 'px',
      width: width + 'px',
      height: height + 'px'
    });

    if (width > 5 || height > 5) {
      window.wasDragging = true;
    }
  });

  $(document).on('mouseup', function(e) {
    if (!isDragging) return;
    isDragging = false;

    var boxRect = selectionBox[0].getBoundingClientRect();
    selectionBox.css('display', 'none');

    if (boxRect.width < 5 && boxRect.height < 5) {
      window.wasDragging = false;
      return;
    }

    var selectedImages = [];
    $('.image-card').each(function() {
      var cardRect = this.getBoundingClientRect();

      if (cardRect.left < boxRect.right &&
          cardRect.right > boxRect.left &&
          cardRect.top < boxRect.bottom &&
          cardRect.bottom > boxRect.top) {

        var img = $(this).data('img');
        selectedImages.push(img);

        $(this).addClass('selected');
        updateCardStyle($(this));
      }
    });

    if (selectedImages.length > 0) {
      Shiny.setInputValue('drag_select', {images: selectedImages, time: new Date()});
    }

    window.wasDragging = false;
  });

  // ============================================================================
  // Measure Tool
  // ============================================================================
  var measureMode = false;
  var measureStart = null;
  var measureLine = null;
  var measureLabel = null;
  var pixelsPerMicron = 3.4; // Default, updated from server

  // Listen for measure mode toggle from server
  Shiny.addCustomMessageHandler('measureMode', function(enabled) {
    measureMode = enabled;
    if (!measureMode) {
      removeMeasureLine();
    }
    // Change cursor on gallery when measure mode is active
    if (measureMode) {
      $('.gallery-drag-area').css('cursor', 'crosshair');
    } else {
      $('.gallery-drag-area').css('cursor', 'default');
    }
  });

  // Listen for pixels per micron updates
  Shiny.addCustomMessageHandler('updatePixelsPerMicron', function(value) {
    pixelsPerMicron = value;
  });

  function removeMeasureLine() {
    if (measureLine) {
      measureLine.remove();
      measureLine = null;
    }
    if (measureLabel) {
      measureLabel.remove();
      measureLabel = null;
    }
    measureStart = null;
  }

  // Measure on image - mousedown
  $(document).on('mousedown', '.image-card img', function(e) {
    if (!measureMode) return;
    e.preventDefault();
    e.stopPropagation();

    removeMeasureLine();

    var img = $(this);
    var imgOffset = img.offset();

    measureStart = {
      x: e.pageX,
      y: e.pageY,
      imgX: e.pageX - imgOffset.left,
      imgY: e.pageY - imgOffset.top,
      img: img
    };

    // Create measure line SVG overlay
    measureLine = $('<svg class=\"measure-line-svg\" style=\"position:fixed;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:9999;\"><line class=\"measure-line\" stroke=\"#ff0000\" stroke-width=\"2\" stroke-dasharray=\"4,2\"/><circle class=\"measure-start\" r=\"4\" fill=\"#ff0000\"/><circle class=\"measure-end\" r=\"4\" fill=\"#ff0000\"/></svg>');
    $('body').append(measureLine);

    measureLine.find('.measure-start').attr('cx', measureStart.x).attr('cy', measureStart.y);
    measureLine.find('.measure-line').attr('x1', measureStart.x).attr('y1', measureStart.y)
                                     .attr('x2', measureStart.x).attr('y2', measureStart.y);
    measureLine.find('.measure-end').attr('cx', measureStart.x).attr('cy', measureStart.y);
  });

  // Measure on image - mousemove
  $(document).on('mousemove.measure', function(e) {
    if (!measureMode || !measureStart) return;

    var endX = e.pageX;
    var endY = e.pageY;

    measureLine.find('.measure-line').attr('x2', endX).attr('y2', endY);
    measureLine.find('.measure-end').attr('cx', endX).attr('cy', endY);

    // Calculate distance and show label
    var dx = endX - measureStart.x;
    var dy = endY - measureStart.y;
    var pixelDist = Math.sqrt(dx*dx + dy*dy);
    var microns = pixelDist / pixelsPerMicron;

    if (!measureLabel) {
      measureLabel = $('<div class=\"measure-label\" style=\"position:fixed;background:rgba(0,0,0,0.8);color:white;padding:4px 8px;border-radius:4px;font-size:12px;z-index:10000;pointer-events:none;\"></div>');
      $('body').append(measureLabel);
    }

    measureLabel.text(microns.toFixed(1) + ' µm (' + Math.round(pixelDist) + ' px)');
    measureLabel.css({left: (endX + 15) + 'px', top: (endY - 10) + 'px'});
  });

  // Measure on image - mouseup
  $(document).on('mouseup.measure', function(e) {
    if (!measureMode || !measureStart) return;

    // Keep the line and label visible until next measurement or mode toggle
    measureStart = null;
  });

  // Click anywhere else to clear measurement
  $(document).on('click.measure', function(e) {
    if (!measureMode) return;
    if (!$(e.target).closest('.image-card img').length && !$(e.target).closest('.measure-label').length) {
      // Don't remove if clicking on measure toggle button
      if (!$(e.target).closest('#measure_toggle').length) {
        removeMeasureLine();
      }
    }
  });
  "
}

# Custom CSS for warning styling in dropdowns
warning_css <- "
/* Style selectize options containing warning symbol (⚠) */
.selectize-dropdown-content .option:has-text('⚠'),
.selectize-dropdown .option[data-value*='⚠'] {
  color: #856404 !important;
  background-color: #fff3cd !important;
}
/* Fallback: style any option starting with warning symbol using attribute selector */
.selectize-dropdown-content .option {
  /* Default styling */
}
/* Target the selected item in the dropdown that contains warning */
.selectize-input .item {
  /* Check content dynamically via JS */
}
"

# UI object
ui <- page_sidebar(
  title = uiOutput("dynamic_title"),
  window_title = "ClassiPyR",
  theme = bs_theme(bootswatch = "flatly"),

  # Enable shinyjs for button disabling during loading

  useShinyjs(),

  # Custom CSS for mode-based titlebar colors and other styling
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML("
      /* Mode-based navbar/header colors */
      .navbar-mode-none {
        background-color: #6c757d !important;  /* Gray for no sample */
      }
      .navbar-mode-annotation {
        background-color: #17a2b8 !important;  /* Blue/cyan for annotation */
      }
      .navbar-mode-validation {
        background-color: #28a745 !important;  /* Green for validation */
      }

      /* Override navbar title color for visibility */
      .navbar .navbar-brand, .navbar .navbar-brand span {
        color: white !important;
      }

      /* Clickable title styling */
      #reset_to_home {
        text-decoration: none !important;
        transition: opacity 0.2s ease;
      }
      #reset_to_home:hover {
        opacity: 0.8;
        text-decoration: none !important;
      }

      /* Style for warning items - applied via JavaScript */
      .selectize-dropdown-content .option.unmatched-class,
      .selectize-input .item.unmatched-class {
        color: #856404 !important;
        background-color: #fff3cd !important;
      }

      /* Loading overlay styles */
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(255,255,255,0.7);
        z-index: 9999;
        display: flex;
        justify-content: center;
        align-items: center;
        font-size: 24px;
        color: #333;
      }
    ")),
    # JavaScript to style options containing warning symbol
    tags$script(HTML("
      // Function to style unmatched class options
      function styleUnmatchedOptions(container) {
        $(container).find('.option, .item').each(function() {
          if ($(this).text().indexOf('\\u26A0') !== -1) {
            $(this).addClass('unmatched-class');
          }
        });
      }

      // Use MutationObserver to detect when dropdown content changes
      $(document).ready(function() {
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            mutation.addedNodes.forEach(function(node) {
              if (node.nodeType === 1) {
                // Check if it's a selectize dropdown or contains one
                if ($(node).hasClass('selectize-dropdown-content')) {
                  styleUnmatchedOptions(node);
                } else if ($(node).hasClass('selectize-dropdown')) {
                  styleUnmatchedOptions($(node).find('.selectize-dropdown-content'));
                }
                // Also check for items in the input
                if ($(node).hasClass('item')) {
                  if ($(node).text().indexOf('\\u26A0') !== -1) {
                    $(node).addClass('unmatched-class');
                  }
                }
              }
            });
          });
        });

        observer.observe(document.body, {
          childList: true,
          subtree: true
        });

        // Also style on dropdown open (backup)
        $(document).on('click', '.selectize-input', function() {
          setTimeout(function() {
            styleUnmatchedOptions($('.selectize-dropdown-content'));
          }, 50);
        });
      });
    "))
  ),

  sidebar = sidebar(
    width = 320,

    # Annotator and settings at top
    div(
      style = "display: flex; gap: 5px; margin-bottom: 10px;",
      div(style = "flex: 1;",
          textInput("annotator_name", "Annotator",
                    value = Sys.info()[["user"]], width = "100%")),
      div(style = "flex: 0; display: flex; align-items: flex-end;",
          actionButton("settings_btn", label = icon("gear"),
                       class = "btn-outline-secondary",
                       title = "Settings",
                       style = "height: 38px; margin-bottom: 15px;"))
    ),

    hr(),

    # Sample selection
    h4("Sample Selection"),

    # Year and month filters in a row
    div(
      style = "display: flex; gap: 10px;",
      div(style = "flex: 1;",
          selectInput("year_select", "Year", choices = NULL, width = "100%")),
      div(style = "flex: 1;",
          selectInput("month_select", "Month", choices = c("All" = "all"), width = "100%"))
    ),

    selectInput("sample_status_filter", "Show",
                choices = c("All samples" = "all",
                           "Auto-classified (validation)" = "classified",
                           "Manually annotated" = "annotated",
                           "Unannotated" = "unclassified")),

    # Sample dropdown with CSS to prevent text wrapping and reduce spacing
    tags$style("
      .sample-dropdown .selectize-input { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .sample-dropdown .form-group { margin-bottom: 2px; }
    "),
    div(class = "sample-dropdown",
        selectizeInput("sample_select", "Sample", choices = NULL, width = "100%",
                       options = list(
                         placeholder = "Select sample..."
                       ))),

    # Legend for sample status symbols (compact, single line)
    div(
      style = "font-size: 12px; color: #666; margin-bottom: 8px; white-space: nowrap;",
      tags$span(style = "margin-right: 8px;", "\u270E Manual"),
      tags$span(style = "margin-right: 8px;", "\u2713 Classified"),
      tags$span("* Unannotated")
    ),

    # Navigation buttons
    div(
      style = "display: flex; gap: 5px; margin-bottom: 15px;",
      actionButton("load_sample", "Load",
                   class = "btn-primary", style = "flex: 1;"),
      actionButton("prev_sample", label = icon("arrow-left"),
                   class = "btn-outline-primary", style = "flex: 0;",
                   title = "Previous sample"),
      actionButton("next_sample", label = icon("arrow-right"),
                   class = "btn-outline-primary", style = "flex: 0;",
                   title = "Next sample"),
      actionButton("random_sample", label = icon("random"),
                   class = "btn-outline-secondary", style = "flex: 0;",
                   title = "Random sample")
    ),

    hr(),

    # Save button (prominent)
    actionButton("save_btn", "Save Annotations",
                 class = "btn-success", width = "100%"),

    uiOutput("python_warning"),

    # Help link at bottom of sidebar
    div(
      style = "margin-top: 20px; text-align: center;",
      tags$a(
        href = "https://europeanifcbgroup.github.io/ClassiPyR/",
        target = "_blank",
        style = "color: #6c757d; font-size: 12px; text-decoration: none;",
        icon("question-circle"), " Documentation & Help"
      )
    ),

    # Logo at bottom of sidebar
    div(
      style = "margin-top: 15px; text-align: center;",
      tags$a(
        href = "https://europeanifcbgroup.github.io/ClassiPyR/",
        target = "_blank",
        img(src = "logo.png", height = "138px", alt = "ClassiPyR")
      )
    )
  ),

  # Main content area with tabs
  navset_card_tab(
    # Image Gallery tab
    nav_panel(
      "Image Gallery",

      # Sticky toolbar
      div(
        style = "position: sticky; top: 0; background: white; z-index: 100; padding: 10px 15px; border-bottom: 1px solid #ddd; margin: -1rem -1rem 10px -1rem; box-shadow: 0 -50px 0 0 white;",

        # CSS to align buttons with inputs and prevent text wrapping in dropdowns
        tags$style("
          .toolbar-row .form-group { margin-bottom: 0; }
          .toolbar-btn { height: 38px; }
          .wide-dropdown .selectize-input,
          .wide-dropdown .selectize-dropdown,
          .wide-dropdown .selectize-dropdown-content .option {
            white-space: nowrap;
          }
          .wide-dropdown .selectize-dropdown {
            width: auto !important;
            min-width: 100%;
          }
        "),

        # Main toolbar row
        div(
          class = "toolbar-row",
          style = "display: flex; gap: 15px; align-items: flex-end; flex-wrap: wrap;",

          div(
            class = "wide-dropdown",
            style = "flex: 0 0 280px;",
            selectInput("class_filter", "Filter by Class",
                        choices = c("All" = "all"), width = "100%")
          ),

          div(
            class = "wide-dropdown",
            style = "flex: 0 0 280px;",
            selectizeInput("new_class_quick", "Relabel to:",
                           choices = NULL,
                           selected = NULL,
                           multiple = FALSE,
                           options = list(
                             placeholder = "Type class name...",
                             maxOptions = 250,
                             onInitialize = I('function() { this.clear(); }')
                           ),
                           width = "100%")
          ),

          div(
            style = "display: flex; gap: 5px; align-items: center;",
            actionButton("relabel_quick", "Relabel", class = "btn-warning toolbar-btn"),
            actionButton("select_all", "Select All", class = "btn-outline-primary toolbar-btn"),
            actionButton("deselect_all", "Deselect", class = "btn-outline-secondary toolbar-btn"),
            actionButton("measure_toggle", label = icon("ruler"),
                         class = "btn-outline-secondary toolbar-btn",
                         title = "Measure: click and drag on images"),
            span(style = "margin-left: 5px; white-space: nowrap;", textOutput("selected_count_inline", inline = TRUE))
          ),

          # Pagination controls (far right)
          div(
            style = "display: flex; gap: 5px; align-items: center; margin-left: auto;",
            actionButton("prev_page", label = icon("arrow-left"), class = "btn-outline-secondary toolbar-btn"),
            span(style = "white-space: nowrap;", textOutput("page_info", inline = TRUE)),
            actionButton("next_page", label = icon("arrow-right"), class = "btn-outline-secondary toolbar-btn"),
            div(style = "width: 80px;",
                selectInput("images_per_page", NULL,
                            choices = c("50" = 50, "100" = 100, "200" = 200, "500" = 500),
                            selected = 100, width = "100%"))
          )
        )
      ),

      # Gallery drag area - enables drag-select from anywhere in the gallery
      div(
        class = "gallery-drag-area",
        style = "min-height: 200px; padding: 10px; margin: -10px; cursor: crosshair;",
        uiOutput("image_gallery")
      ),

      # Selection box for drag-select
      div(id = "selection-box",
          style = "position: fixed; border: 2px dashed #007bff; background: rgba(0,123,255,0.1);
                   pointer-events: none; display: none; z-index: 1000;"),

      # JavaScript for selection handling
      tags$script(HTML(gallery_js()))
    ),

    # Summary Table tab
    nav_panel(
      "Summary Table",
      DTOutput("summary_table")
    ),

    # Validation Statistics tab (only meaningful in validation mode)
    nav_panel(
      "Validation Statistics",
      uiOutput("validation_tab_content")
    )
  ),

  # Loading overlay (shown during load/save operations)
  uiOutput("loading_overlay")
)
