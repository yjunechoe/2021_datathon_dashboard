# Load pkgs ----

xfun::pkg_attach(
    "shiny", "shinyWidgets", "shinydashboard", "shinydashboardPlus",
    "ggplot2", "ragg", "thematic", "colorspace",
    "reactable", "reactablefmtr",
    "data.table", "rlang", "glue",
    message = FALSE
)

# Plot themes ----

options(shiny.useragg = TRUE)

thematic_shiny()

font_spec(families = c("Roboto", "Montserrat"))

theme_set(theme_classic())

theme_update(
  plot.title = element_text(family = "Roboto"),
  axis.title = element_text(family = "Montserrat"),
  plot.margin = margin(.05, .02, .05, .02, "npc"),
  plot.title.position = "plot"
)

# Read in data ----