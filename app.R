# Source file that reads in data, misc. set ups, and other static things
source("source.R", local = TRUE)

# Call app
source('ui.R', local = TRUE)
source('server.R', local = TRUE)

shiny::shinyApp(
  ui = ui,
  server = server,
)