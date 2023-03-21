library(shiny)
library(shinydashboard)
library(tidyverse)
library(rjson)
library(readxl)
library(rapidpror)

country <- "Jamaica" # Jamaica, Philippines, South Africa, Malaysia
type <- "KPI"
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
if (type == "KPI"){
  source("kpi_update.R")
  source("kpi_shiny.R")
  kpi_shiny()
} else {
  source("R Shiny Template.R")
  source("Data Cleaning.R")
  parenttext_shiny(country = country, date_from = default_date_from, date_to = default_date_to, include_archived_data = include_archived_data)
}
