# UI definition for the ClassiPyR threshold review app
#
# A navbar page with four panels:
#
# - Setup:   folders, the class-score index, the model, and the one-time
#            freeze of the sampling frame
# - Review:  the gallery of a predicted class, where the wrong images are
#            marked and the page is confirmed
# - Results: the chosen threshold of every class and its precision curve
# - Export:  the thresholds file for ifcb-classify
#
# The gallery's click and drag-select handling is in www/threshold-gallery.js
# and the styles are in www/threshold-app.css.

# Defaults that live in the package. They are not exported, so they are
# fetched from its namespace: that works from an installed package too.
thr_default <- function(name) get(name, envir = asNamespace("ClassiPyR"))

# A folder path field with a Browse button next to it
thr_folder_input <- function(text_id, button_id, label, title,
                             placeholder = NULL) {
  div(
    style = "display: flex; gap: 5px; align-items: flex-end; margin-bottom: 10px;",
    div(style = "flex: 1;",
        textInput(text_id, label, value = "", width = "100%",
                  placeholder = placeholder)),
    div(style = "margin-bottom: 15px;",
        shinyDirButton(button_id, "Browse", title, class = "btn-outline-secondary"))
  )
}

thr_setup_panel <- nav_panel(
  "Setup",
  card(
    card_header("Folders"),
    thr_folder_input("thr_review_dir", "thr_browse_review_dir",
                     "Review folder (required)", "Select review folder",
                     placeholder = "Choose a folder for the verdicts of this review"),
    helpText("The verdicts and the index of the class-score files are stored",
             "in the review folder. Use a folder of its own, on a local disk."),
    thr_folder_input("thr_h5_folder", "thr_browse_h5_folder",
                     "Class-score folder (*_class*.h5 from ifcb-classify)",
                     "Select class-score folder"),
    thr_folder_input("thr_roi_folder", "thr_browse_roi_folder",
                     "Raw data folder (.roi, .adc, .hdr)",
                     "Select raw data folder"),
    textInput("thr_annotator", "Reviewer name", value = "", width = "300px"),
    div(actionButton("thr_apply_settings", "Apply", class = "btn-primary")),
    textOutput("thr_settings_status")
  ),
  card(
    card_header("Class-score index"),
    div(actionButton("thr_build_index", "Build / update index",
                     class = "btn-primary")),
    textOutput("thr_index_status"),
    tableOutput("thr_models_table"),
    uiOutput("thr_failures_ui"),
    tags$details(
      tags$summary("Files that cannot be used"),
      DTOutput("thr_failures_table")
    )
  ),
  card(
    card_header("Model"),
    selectInput("thr_model", "Model to review", choices = character(),
                width = "100%"),
    helpText("Each model is reviewed on its own. Verdicts of different",
             "models are never pooled. The counts are over every folder that",
             "was indexed, each sample once; Check tells how many of them the",
             "class-score folder in use holds. A model marked \"review",
             "frozen\" can be opened for its results and its export even when",
             "its class-score files are no longer indexed.")
  ),
  conditionalPanel(
    condition = "output.thr_frame_state == 'open'",
    card(
      card_header("Start review"),
      tags$h6("1. Training samples to exclude"),
      helpText("Samples the model was trained on must be left out, or the",
               "estimated precision will be too high."),
      fileInput("thr_exclusion_file",
                "Text file with one sample name per line",
                accept = c(".txt", ".csv", "text/plain")),
      div(
        style = "display: flex; gap: 5px; align-items: flex-end; flex-wrap: wrap;",
        div(style = "flex: 1; min-width: 260px;",
            textInput("thr_annotations_folder",
                      "Folder of the main app's annotations.sqlite",
                      value = "", width = "100%")),
        div(style = "margin-bottom: 15px;",
            actionButton("thr_read_annotations",
                         "Read annotated samples from annotations.sqlite",
                         class = "btn-outline-primary")),
        div(style = "margin-bottom: 15px;",
            actionButton("thr_clear_annotations", "Do not use",
                         class = "btn-outline-secondary"))
      ),
      textOutput("thr_annotations_status"),
      tags$h6("2. Samples without raw data", style = "margin-top: 15px;"),
      helpText("Samples whose raw data is not in the raw data folder are",
               "counted and left out: their images could not be shown."),
      tags$h6("3. Fallback thresholds", style = "margin-top: 15px;"),
      helpText("A class the review cannot fit keeps the threshold the model",
               "has now."),
      radioButtons(
        "thr_fallback_mode", NULL,
        choices = c(
          "Use the values stored in the class-score files" = "h5",
          "Read the model's thresholds JSON file" = "json"
        ),
        selected = "h5"
      ),
      conditionalPanel(
        condition = "input.thr_fallback_mode == 'json'",
        div(
          style = "display: flex; gap: 5px; align-items: flex-end;",
          div(style = "flex: 1;",
              textInput("thr_fallback_json",
                        "Path of the model's *_thresholds_and_metrics.json",
                        value = "", width = "100%")),
          div(style = "margin-bottom: 15px;",
              shinyFilesButton("thr_browse_fallback_json", "Browse",
                               "Select thresholds JSON file", multiple = FALSE,
                               class = "btn-outline-secondary"))
        )
      ),
      tags$h6("4. Check and freeze", style = "margin-top: 15px;"),
      div(
        style = "display: flex; gap: 8px;",
        actionButton("thr_preflight", "Check", class = "btn-primary"),
        actionButton("thr_freeze", "Freeze and start", class = "btn-danger")
      ),
      uiOutput("thr_preflight_ui")
    )
  ),
  conditionalPanel(
    condition = "output.thr_frame_state == 'frozen'",
    card(
      card_header("Frozen frame"),
      uiOutput("thr_frozen_ui")
    )
  )
)

thr_review_panel <- nav_panel(
  "Review",
  div(
    style = "display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap;",
    div(style = "flex: 1; min-width: 300px;",
        selectizeInput("thr_class",
                       "Predicted class (verdicts / predictions)",
                       choices = NULL, width = "100%",
                       options = list(placeholder = "Choose a class"))),
    div(style = "width: 130px;",
        selectInput("thr_page_size", "Images per page",
                    choices = thr_default("THR_APP_PAGE_SIZES"),
                    selected = thr_default("THR_APP_DEFAULT_PAGE_SIZE"),
                    width = "100%")),
    div(style = "margin-bottom: 15px;",
        actionButton("thr_load_page", "Load page", class = "btn-primary"))
  ),
  tags$details(
    tags$summary("Verdicts per score bin for this class"),
    tableOutput("thr_coverage")
  ),
  div(
    class = "alert alert-info thr-instruction",
    tags$strong("Click every image that is NOT the class."),
    "Mark doubtful ones wrong. Unmarked images are recorded as correct when",
    "the page is confirmed. Images you have not scrolled to, or that did not",
    "load, are not recorded and come back later.",
    "Drag from the space between images to mark several at once."
  ),
  div(
    class = "thr-toolbar",
    tags$button(id = "thr_confirm_btn", type = "button",
                class = "btn btn-success", "Confirm page"),
    actionButton("thr_clear_marks", "Clear marks",
                 class = "btn-outline-secondary"),
    actionButton("thr_undo", "Undo last page", class = "btn-outline-warning"),
    textOutput("thr_page_caption", inline = TRUE),
    span(id = "thr-mark-count", class = "thr-mark-count")
  ),
  textOutput("thr_review_message"),
  # Drag area: drag-select can start anywhere around the cards
  div(class = "thr-drag-area", uiOutput("thr_gallery"))
)

thr_results_panel <- nav_panel(
  "Results",
  div(
    style = "display: flex; gap: 20px; align-items: flex-start; flex-wrap: wrap;",
    numericInput("thr_target", "Target precision",
                 value = thr_default("THR_DEFAULT_TARGET_PRECISION"),
                 min = 0.01, max = 1, step = 0.01, width = "160px"),
    radioButtons(
      "thr_criterion", "The target must be met by",
      choices = c("the estimate" = "estimate",
                  "the lower bound of its interval" = "lower"),
      selected = "estimate", inline = TRUE
    )
  ),
  div(
    class = "alert alert-secondary",
    "The interval covers sampling error within the reviewed score bins only.",
    "It says nothing about samples outside the frozen frame, about classes",
    "or bins that were not reviewed, or about mistakes in the verdicts."
  ),
  DTOutput("thr_results_table"),
  uiOutput("thr_selected_class"),
  plotOutput("thr_curve_plot", height = "440px"),
  div(
    style = "display: flex; gap: 8px; align-items: flex-end; flex-wrap: wrap;",
    numericInput("thr_override_value", "Manual threshold for the selected class",
                 value = NA, min = 0, max = 1, step = 0.01, width = "300px"),
    div(style = "margin-bottom: 15px;",
        actionButton("thr_override_set", "Set", class = "btn-primary")),
    div(style = "margin-bottom: 15px;",
        actionButton("thr_override_clear", "Clear",
                     class = "btn-outline-secondary"))
  )
)

thr_export_panel <- nav_panel(
  "Export",
  card(
    card_header("Export thresholds"),
    helpText("The thresholds are exported with the target precision and the",
             "criterion chosen under Results. Every class of the model is",
             "written."),
    tableOutput("thr_export_summary"),
    thr_folder_input("thr_export_folder", "thr_browse_export_folder",
                     "Export folder", "Select export folder"),
    textInput("thr_export_name", "File name", value = "", width = "100%"),
    checkboxInput("thr_export_overwrite", "Replace existing files",
                  value = FALSE),
    div(actionButton("thr_export_btn", "Export", class = "btn-primary")),
    uiOutput("thr_export_result")
  )
)

# UI object
ui <- page_navbar(
  title = "ClassiPyR threshold review",
  id = "thr_nav",
  window_title = "ClassiPyR threshold review",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,
  header = tagList(
    # Enable shinyjs for enabling and disabling buttons
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = "threshold-app.css"))
  ),
  thr_setup_panel,
  thr_review_panel,
  thr_results_panel,
  thr_export_panel,
  footer = tagList(
    # Selection box for drag-select
    div(id = "thr-selection-box"),
    # JavaScript for marking images and confirming a page
    tags$script(src = "threshold-gallery.js")
  )
)
