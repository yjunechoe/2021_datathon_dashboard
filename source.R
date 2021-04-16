# Load pkgs ----

xfun::pkg_attach(
    "shiny", "shinyWidgets", "shinydashboard", "shinydashboardPlus",
    "ggplot2", "ragg", "thematic", "colorspace",
    "reactable", "reactablefmtr",
    "data.table", "rlang", "glue",
    "dplyr","forcats","lubridate",
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

# I performed most data cleaning separately and saved as Rds 
# I could do a better job here and also do this separately
# I saved these locally, but would want to save in a location for 
# the app to read from

# This is offenses & dispositions data that has been cleaned up
od_clean <- readRDS('./data/off_disp_clean.Rds')%>% 
  dplyr::rename(judge = disposing_authority__document_name) %>% 
  # dplyr::mutate(grade_desc = paste(grade,description_clean,sep="_")) %>% 
  dplyr::filter(!is.na(disposition)) # For my viz, removed lots of rows

# This is defendant and docket info table merged with defendant IDs
ddd <- readRDS('./data/defendant_docket.Rds')

# Again, could do this outside app
merged <- od_clean %>% 
  dplyr::left_join(ddd, by = "docket_id") %>% 
  dplyr::mutate(disposition_year = lubridate::year(disposition_date)) %>% 
  # We don't end up using most the data in the current app
  dplyr::select(judge, disposition_year, docket_id, grade, description_clean,
         gender, defendant_id, race, grade, sentence_type, min_period_days,
         max_period_days)

# Display options in order of most common
judge_options <- na.omit(dplyr::pull(dplyr::count(od_clean, judge, sort=T), judge))
description_options <- na.omit(dplyr::pull(dplyr::count(od_clean, description_clean, sort=T), description_clean))
