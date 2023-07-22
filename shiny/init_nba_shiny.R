

# Imports -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rlang)
library(dplyr)
library(tidyr)
library(tidyselect)
library(purrr)
library(ggplot2)
library(plotly)
library(stringr)
library(gt)


# Database Connections ----------------------------------------------------

source(here::here("data", "dataHub.R"))


# Project Constants -------------------------------------------------------

# Dataframe used to select NBA stats
stat_selection <- 
  tibble::tribble(
    ~formatted_name, ~overview_name, ~log_name,
     "Minutes", "minutes_totals", "minutes",
     "3-pointers", "fg3m_totals", "fg3m",
     "Points", "pts_totals", "pts",
     "Field Goal %", "pct_fg", "pct_fg",
     "Free Throw %", "pct_ft", "pct_ft",
     "Rebounds", "trb_totals", "treb",
     "Assists", "ast_totals", "ast",
     "Steals", "stl_totals", "stl",
     "Blocks", "blk_totals", "blk",
     "Turnovers", "tov_totals", "tov"
  )

# Custom Functions --------------------------------------------------------

# Reverse plotly legend labels
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}