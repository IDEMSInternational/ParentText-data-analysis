# call the packages into R
library(httr)
library(jsonlite)
library(tidyverse)

setwd("C:/Users/ATEAMATE/Downloads/ParentText-data-analysis-master")

# 1. Reading in from RapidPro --------------------------------------------------------------------------------------
uuid_data <- read.csv("./Files/flows_uuids.csv")
token <- read.table("token.txt", quote="\"", comment.char="")

user_data <- get_user_data(token)

flow_name <- "PLH - Supportive - Praise"
flow_interaction <- get_flow_data(uuid_data,token, flow_name, result_1)
response_rate_graphs(flow_interaction, flow_name)


