country <- "South_Africa" # Jamaica, Philippines, South_Africa Malaysia
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
source("R Shiny Template.R")
source("Data Cleaning.R")
parenttext_shiny(country = country, date_from = default_date_from, date_to = default_date_to, include_archived_data = include_archived_data)
