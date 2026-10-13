# Gallery: the session's image folder, rendering a page, and cleanup

setup_thr_gallery_server <- function(input, output, session, state) {
  # Images are cut out of the raw files into a folder of this session only,
  # which the browser reads through a resource path of its own
  resource_name <- paste0("thr_images_", session$token)
  image_folder <- file.path(tempdir(), resource_name)
  dir.create(image_folder, recursive = TRUE, showWarnings = FALSE)
  addResourcePath(resource_name, image_folder)
  state$image_folder <- image_folder
  state$resource_name <- resource_name

  # The cards are keyed by draw id and carry the page token; which of them
  # are marked wrong is known to the browser only, until the page is confirmed
  output$thr_gallery <- renderUI({
    thr_gallery_ui(state$page, resource_name, state$page_token,
                   notice = state$page_notice,
                   remaining = state$page_remaining)
  })

  output$thr_page_caption <- renderText({
    thr_page_caption(state$page, state$page_class)
  })

  observeEvent(input$thr_clear_marks, {
    session$sendCustomMessage("thrClearMarks", list())
  })

  # Nothing to confirm on a page without a draw a confirmation would settle:
  # no page, an empty one, or one whose raw data was not found. The gallery
  # script applies the same rule when it enables the button again.
  observe({
    has_images <- !is.null(state$page) && any(state$page$extracted)
    shinyjs::toggleState("thr_confirm_btn",
                         condition = thr_page_can_confirm(state$page))
    shinyjs::toggleState("thr_clear_marks", condition = has_images)
  })

  session$onSessionEnded(function() {
    tryCatch(removeResourcePath(resource_name), error = function(e) NULL)
    unlink(image_folder, recursive = TRUE)
  })

  invisible(NULL)
}
