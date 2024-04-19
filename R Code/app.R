#devtools::install_github("IDEMSInternational/plhr", force = TRUE)
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
#library(rlang)
library(shinythemes)
library(shinyjs)
#library(rpivotTable)
#library(survminer)

# TODO: work for multiple group boxes.

# put changes from main_page_group up and on
# changes from plh_shiny up and on
# but remove chnages I made

country <- "Malaysia_2" #South_Africa_2" #Mexico" #South_Africa "" #South_Africa_2" # Jamaica, Philippines, South Africa, Malaysia, Malaysia_2
type <- "ParentText2" #ParentText2" # ParentText, KPI, SRH for Jamaica only.
source("Functions.R")
source(paste0(country, "_dashboard_settings.R"))
if (type == "ParentText2"){
  if (country == "South_Africa_2"){
    data_l <- import_list("data/PT2_shiny.xlsx")
    source("shiny_cleaning_PT2.R") 
    title <- "South Africa: ParentText 2.0"
    df$uuid <- df$id
    
    #save(df, result_flow21, transitions, flow_checkin_data, flow_module_checkin_data, flow_safeguarding_data, post_goal_checkin_data, pre_goal_checkin_data, file = "sa_20240118.rds")
    
  } else if (country == "Malaysia_2") {
    data_l <- import_list("data/PT2_shiny_malaysia.xlsx")
    # for (i in 1:length(data_l)){
    #   if (is.null(data_l[[i]]$name)){
    #     data_l[[i]]$name <- paste0("box", 1:nrow(data_l[[i]]))
    #     # provided that data_list[[i]] is not a 
    #   }
    # }
    source("shiny_cleaning_PT2.R")
    #save(df, result_flow2, checkin_data, goal_transitions_table, transitions, flow_module_checkin_data, flow_safeguarding_data, post_goal_checkin_data, pre_goal_checkin_data, file = "malaysia_20240229.rds")
    #save(df, checkin_data, goal_transitions_table, goal_transitions, transitions, flow_module_checkin_data, flow_safeguarding_data, post_goal_checkin_data, pre_goal_checkin_data, goals_accessed_size, file = "malaysia_20240229.rds")
    #load("malaysia_20240118.rds")
    title <- "Malaysia: ParentText 2.0"
  } else if (country == "Mexico") {
    #data_l$main_page <- NULL
    source("MX_shiny_cleaning_PT.R")                          # group_by_data
    data_l <- import_list("data/PT2_shiny_mexico.xlsx")
    title <- "Mexico: ParentText 2.0"
    # bug if there's no filtering, but there is multiple dfs
  } else {
    stop("Unknown country")
  }
  
  #data_l$main_page <- data_l$main_page[c(1:9, 11),]
  # todo: bug with data_manip filter for "bar_summary"
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
    if (country == "South_Africa"){
      source("SA_PT1_shiny.R")
      source("SA_PT1_cleaning.R") # same as Data Cleaning, essentially. But we call in a DF instead of generating the data.
      parenttext_shiny(country = country, date_from = default_date_from, date_to = default_date_to, include_archived_data = include_archived_data)
    } else {
      source("R Shiny Template.R")
      source("Data Cleaning.R")
      parenttext_shiny(country = country, date_from = default_date_from, date_to = default_date_to, include_archived_data = include_archived_data)
    }
  }
}


# either group by isn't working
# or we're just repeating the first plot on each tab?
