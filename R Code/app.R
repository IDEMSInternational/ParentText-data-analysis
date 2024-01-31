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

country <- "Malaysia_2" #South_Africa_2" # Jamaica, Philippines, South Africa, Malaysia, Malaysia_2
type <- "ParentText2" #ParentText2" # ParentText, KPI, SRH for Jamaica only.
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
if (type == "ParentText2"){
  if (country == "South_Africa_2"){
    data_l <- import_list("data/PT2_shiny.xlsx")
    source("shiny_cleaning_PT2.R") 
    title <- "South Africa: ParentText 2.0"
  } else if (country == "Malaysia_2") {
    data_l <- import_list("data/PT2_shiny_malaysia1.xlsx")
    source("shiny_cleaning_PT2.R")
    #save(df, result_flow2, checkin_data, goal_transitions_table, transitions, flow_module_checkin_data, flow_safeguarding_data, post_goal_checkin_data, pre_goal_checkin_data, stress_df, relation_df, develop_df, learning_df, structure_df, behave_df, safety_df, ipv_df, budget_df, file = "malaysia_20240118.rds")
    #load("malaysia_20240118.rds")
    title <- "Malaysia: ParentText 2.0"
    df$uuid <- df$id
  } else {
    stop("Unknown country")
  }
  PLH_shiny(title = title,
            data_list = data_l,
            data_frame = df,
            status = "primary",
            colour = "blue",
            key = "uuid")
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


# either group by isn't working
# or we're just repeating the first plot on each tab?
