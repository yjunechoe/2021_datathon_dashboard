# app.R



# Source file that reads in data, misc. set ups, and other static things
source("source.R")
source("ui.R")
source("server.R")

# Run
shinyApp(ui, server)
