library(rio)
library(plhR)
library(readxl)
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
library(httr)
library(jsonlite)

# data_l <- import_list("PT2_shiny.xlsx")
# country <- "South_Africa_2" # Jamaica, Philippines, South Africa, Malaysia
# source("Functions.R")
# source(paste0(country, "_dashboard_settings.R"))
# source("shiny_cleaning_PT2.R")

# Then actually running it!
status = "primary"
colour = "blue"
  data_list = data_l
  data_frame = df
  PLH_shiny(title = "Testing Shiny Dashboard",
            data_list = data_l,
            data_frame = df)
  
  # todo: add boxes at the top. is this already a feature I've put in? Tryt his:
  # 
  # 
  # $main_page
  # type        name value                                                      parameter_list            variable
  # 1 value_box myvaluebox1    NA                    text = "Total", colour = "yellow", icon = "user"        true_consent
  # 2 value_box myvaluebox2    NA             text = "Consented", colour = "aqua", icon = "clipboard"        true_consent
  # 3 value_box myvaluebox3    NA text = "Active in last 24 hours", colour = "purple", icon = "clock" active_users_24_hrs
  # 4 value_box myvaluebox4    NA text = "Active in last 7 days", colour = "green", icon = "calendar" active_users_7_days
  # 
  # variable_value
  # 1           <NA>
  #   2            Yes
  # 3            Yes
  # 4            Yes
  # 
  
  