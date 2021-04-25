# Load pkgs ----

# xfun::pkg_attach(
#     "shiny", "shinyWidgets", "shinydashboard", "shinydashboardPlus",
#     "ggplot2", "ragg", "thematic", "colorspace",
#     "reactable", "reactablefmtr",
#     "data.table", "rlang", "glue",
#     "dplyr","forcats","lubridate",
#     message = FALSE
# )
# Above package loading is not compatible with shinyapps/.io dependency handling (packrat)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(ragg)
library(thematic)
library(colorspace)
library(reactable)
library(reactablefmtr)
library(data.table)
library(rlang)
library(stringr)
library(glue)
library(dplyr)
library(forcats)
library(lubridate)
library(ggiraph)

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

# We performed most data cleaning separately and saved as Rds 

# Alice data note:
# This is offenses & dispositions data that has been cleaned up
# And filtered for offenses with dispositions!!
# Then merged with docket/defendant data
merged <- readRDS('./data/merged_filt_offenses_shiny.Rds')


# Load Alison data - note that this processing could be done, then saved before loading
# Could save a bit of time for loading
merged.narrow <- readRDS('./data/Docket_Offenses_Merged_Narrowed.Rds') %>%
  dplyr::mutate(Confinement_Time = max_period_days_Confinement/365.25) %>%
  dplyr::mutate(Probation_Time = max_period_days_Probation/365.25) %>%
  # filter here for Confinement_Time NA - this was a lot of the data and this...
  # dataset is only used to plot Confinement_Time so these are dropped later
  # dplyr::filter(!is.na(Confinement_Time) | !is.na(Probation_Time)) %>%
  tidyr::pivot_longer(cols = all_of(c("Confinement_Time", "Probation_Time")), values_to = "Time",names_to = "Type") %>%
  dplyr::filter(!is.na(Time)) %>%
  dplyr::select(-min_period_days_Confinement, -min_period_days_Probation,
                -max_period_days_Confinement, -max_period_days_Probation)

# Load Roy data
bail_net_change_by_judge <- readRDS('./data/bailnetchangebyjudge.rds')

# Load Kulbir data
dispo_det <- readRDS('./data/kk_dispo_det.Rds')

# Load Sybil data
completerecords <- readRDS('./data/new_bail_data.rds') %>% 
  mutate(month = factor(month, levels = c("Spring","Summer","Fall","Winter")))

# Create some other variables that the app uses: ----
bail_judge_options <- unique(completerecords$Judge)

options <-
  c(
    #"Judge",
    "max_grade",
    "statute_description",
    "disposition",
    "disposition_method",
    "Chapter_Description",
    "gender",
    "race"
  )
# Display options in order of most common
judge_options <-
  na.omit(dplyr::pull(dplyr::count(merged, judge, sort = T), judge))
description_options <-
  na.omit(dplyr::pull(
    dplyr::count(merged, description_clean, sort = T),
    description_clean
  ))
