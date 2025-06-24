devtools::install_github("IDEMSInternational/ExcelToShiny")
devtools::install_github("IDEMSInternational/rapidpror")
{
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
  library(openappr)
  library(httr)
  library(jsonlite)
  #library(rlang)
  library(shinythemes)
  library(shinyjs)
  library(ExcelToShiny)
  library(rapidpror)
  #library(rpivotTable)
  #library(survminer)
}

# TODO: work for multiple group boxes.

# created a set of deprecated_functions.R - if there's issues with flow data, make sure that i'm running get_flow_data1 

# put changes from main_page_group up and on
# changes from build_shiny up and on
# but remove chnages I made

# todo: check SA and Rohingya for archiving.
country <- "SWIFT" #"Malaysia_2" #South_Africa_2" #Rohingya #Malaysia_3" #South_Africa_2" #Mexico" #South_Africa "" #South_Africa_2" # Jamaica, Philippines, South Africa, Malaysia, Malaysia_2
type <- "SWIFT" #facilitator" #ParentText2" # ParentText, KPI, SRH for Jamaica only.
source("Functions.R")
if (country == "Malaysia_3"){
  source(paste0("Malaysia_2", "_dashboard_settings.R"))
} else {
  source(paste0(country, "_dashboard_settings.R"))
}
if (type == "SWIFT"){
  source("SWIFT_cleaning.R")
  data_l <- import_list("data/SWIFT_shiny.xlsx")
  title <- "SWIFT Text"

#data_l$main_page <- data_l$main_page[c(1:9, 11),]
# todo: bug with data_manip filter for "bar_summary"
build_shiny(title = title,
            data_list = data_l,
            data_frame = full_data,
            status = "primary",
            colour = "blue",
            key = "uuid")
  
} else if (type == "ParentText2"){
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
  } else if (country == "Malaysia_3") {
    data_l <- import_list("data/PT2_shiny_malaysia_2.xlsx")
    source("MY3_shiny_cleaning_PT.R")
    title <- "Malaysia: Localised Data"
    df$uuid <- df$id
  } else if (country == "Mexico") {
    #data_l$main_page <- NULL
    source("MX_shiny_cleaning_PT.R")                          # group_by_data
    data_l <- import_list("data/PT2_shiny_mexico.xlsx")
    title <- "Mexico: ParentText 2.0"
    df$uuid <- df$id
    # bug if there's no filtering, but there is multiple dfs
  } else if (country == "Rohingya") {
    source("RH_shiny_cleaning.R")                          # group_by_data
    data_l <- import_list("data/PT2_shiny_rohingya.xlsx")
    title <- "Rohingya Text"
    df$uuid <- df$id
    # bug if there's no filtering, but there is multiple dfs
  } else {
    stop("Unknown country")
  }
  
  #data_l$main_page <- data_l$main_page[c(1:9, 11),]
  # todo: bug with data_manip filter for "bar_summary"
  build_shiny(title = title,
            data_list = data_l,
            data_frame = df,
            status = "primary",
            colour = "blue",
            key = "uuid")
} else if (type == "facilitator") {
  if (country == "Malaysia_2") {
    source("MY_faci_cleaning.R")
    data_l <- import_list("data/fac_shiny_malaysia1.xlsx")
    title <- "Facilitator Data: Malaysia"
    faci_main_df$uuid <- faci_main_df$facilitator
    x <- faci_main_df
  } else {
    source("MX_faci_cleaning.R")
    data_l <- import_list("data/fac_shiny_mexico.xlsx")
    title <- "Facilitator Data: Mexico"
    x$uuid <- x$id 
  }
  data_l$contents$icon <- "user"
  build_shiny(title = title,
            data_list = data_l,
            data_frame = x,
            status = "primary",
            colour = "blue",
            key = "id")
} else {
  source("deprecated_functions.R")
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
