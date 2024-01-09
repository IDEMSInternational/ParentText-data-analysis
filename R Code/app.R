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
library(rio)
library(plhR)
library(httr)
library(jsonlite)
library(rlang)

country <- "Malaysia_2" #South_Africa_2" # Jamaica, Philippines, South Africa, Malaysia
type <- "ParentText2" #ParentText2" # ParentText, KPI, SRH for Jamaica only.
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
if (type == "ParentText2"){
  if (country == "South_Africa_2"){
    data_l <- import_list("data/PT2_shiny.xlsx")
    source("shiny_cleaning_PT2.R") 
    title <- "South Africa: ParentText 2.0"
  } else {
    #data_l <- import_list("data/PT2_shiny_Malaysia.xlsx")
    data_l <- import_list("data/PT2_shiny_malaysia.xlsx")
    source("shiny_cleaning_PT2.R")
    title <- "Malaysia: ParentText 2.0"
  }
  PLH_shiny(title = title,
            data_list = data_l,
            data_frame = df,
            status = "primary",
            colour = "blue")
} else {
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
}
