# Philippines Configuration

# TODO: update key, site to give correct key and site.
# TODO: default_date_from

key <- read.table("tokens/PT_malaysia_key.txt", quote="\"", comment.char="")
site <- "https://app.rapidpro.io/api/v2/"
include_archived_data <- FALSE # for now FALSE
#if (include_archived_data){
#  archived_data <- TODO
#}
default_date_from <- "2021-10-14"
default_date_to <- NULL
prefix <- "PH"

# for region/states
state_title <- "Parish"
state_1 <- "Ilocos Region"
state_2 <- "Cagayan Valley"
state_3 <- "Central Luzon"
state_4 <- "CALABARZON"
state_5 <- "MIMAROPA Region" 
state_6 <- "Bicol Region" 
state_7 <- "Western Visayas" 
state_8 <- "Central Visayas"
state_9 <- "Eastern Visayas"
state_10 <- "Zamboanga Peninsula" 
state_11 <- "Northern Mindanao"
state_12 <- "Davao Region"
state_13 <- "SOCCSKSARGEN"
state_14 <- "Caraga"
state_15 <- "National Capital Region"
state_16 <- "Cordillera Administrative Region" 
state_17 <- "Bangsamoro Autonomous Region in Muslim Mindanao"


#shiny template
skin <- "green"
background <- "green" # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
status <- "success"
#flag?