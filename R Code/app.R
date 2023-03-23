library(shiny)
library(shinydashboard)
library(tidyverse)
library(rjson)
library(readxl)
library(httr)
library(jsonlite)
library(rapidpror)
library(plotly)

country <- "Jamaica" # Jamaica, Philippines, South Africa, Malaysia
type <- "SRH" # ParentText, KPI, SRH for Jamaica only.
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
if (country == "Jamaica"){
  if (type == "KPI"){
    source("kpi_update.R")
    source("kpi_shiny.R")
    kpi_shiny()
  } else if (type == "SRH") {
    source("srh_update.R")
    source("srh_shiny.R")
    srh_shiny()
  } else {
    source("R Shiny Template.R")
    source("Data Cleaning.R")
    parenttext_shiny(country = country, date_from = default_date_from, date_to = default_date_to, include_archived_data = include_archived_data)
  }
} else {
  source("R Shiny Template.R")
  source("Data Cleaning.R")
  parenttext_shiny(country = country, date_from = default_date_from, date_to = default_date_to, include_archived_data = include_archived_data)
}