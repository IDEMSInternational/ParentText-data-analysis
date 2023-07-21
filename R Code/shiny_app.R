library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(rjson)
library(readxl)
library(httr)
library(jsonlite)
library(rapidpror)
library(plotly)
library(DT)

country <- "Jamaica" # Jamaica, Philippines, South Africa, Malaysia
type <- "KPI" # ParentText, KPI, SRH for Jamaica only.
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
print(country)
print(type)
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
