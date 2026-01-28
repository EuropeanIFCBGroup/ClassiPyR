# ClassiPyR Shiny App
# This file enables running the app directly with shiny::runApp()

# Load global settings
source("global.R", local = TRUE)

# Load UI
source("ui.R", local = TRUE)

# Load server
source("server.R", local = TRUE)

# Create Shiny app
shinyApp(ui = ui, server = server)
