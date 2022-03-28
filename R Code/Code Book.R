# call the packages into R
library(httr)
library(jsonlite)
library(tidyverse)

# 1. Package environment ------------------------------------------------
# for this to work you need to change the directory to where the token key is stored.
key <- read.table("./tokens/PT_malaysia_key.txt", quote="\"", comment.char="")
set_rapidpro_key(key = key)
set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
set_rapidpro_uuid_names()
get_rapidpro_uuid_names()

# 2. Getting the data in -----------------------------------------------------
get_user_data()
#get_rapidpro_uuid_names() %>% dplyr::filter(grepl("PLH - Supportive", name))
flow_name <- "PLH - Supportive - Development"
flow_interaction <- get_flow_data(flow_name = flow_name)
# TODO: result1 is the name. Need to get the result names.
# At the moment the names are "result1", "result", or more descriptive relating to the flow name
# Would they always relate to the flow name?
# for "Supportive - Calm" we have "result_1" and "skill". What is the difference between them?

# 3. Information at user level -----------------------------------------------
# list of variable names
user_data <- get_user_data()
user_data_all_vars <- lapply(user_data, names)

# contact list values - get variables that contain a set word
user_data1 <- select(user_data, matches("survey"))

names(get_user_data())

# user data
get_user_group_data()

# 4. Information at flow level -----------------------------------------------
response_rate_graphs(flow_interaction = flow_interaction)
flow_interaction <- get_flow_data(flow_name = "PLH - Supportive - Development")
