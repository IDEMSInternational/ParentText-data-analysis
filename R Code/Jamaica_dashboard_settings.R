# Jamaica Configuration

# TODO: update key, site to give correct key and site.
# TODO: default_date_from

key <- read.table("tokens/PT_jamaica_key.txt", quote="\"", comment.char="")
site <- "http://rapidpro.ilhasoft.mobi/api/v2/"
include_archived_data <- FALSE # for now FALSE
#if (include_archived_data){
#  archived_data <- TODO
#}
default_date_from <- "2022-06-10"
default_date_to <- NULL
prefix <- "JM"    #PH #SA

# for region/states
state_title <- "Parish"
state_1 <- "Hanover"
state_2 <- "Saint Elizabeth" 
state_3 <- "Saint James"
state_4 <- "Trelawny"
state_5 <- "Westmoreland" 
state_6 <- "Clarendon"
state_7 <- "Manchester"
state_8 <- "Saint Ann"
state_9 <- "Saint Catherine" 
state_10 <- "Saint Mary"
state_11 <- "Kingston Parish" 
state_12 <- "Portland"
state_13 <- "Saint Andrew"
state_14 <- "Saint Thomas"
state_15 <- "NA"
state_16 <- "NA"
state_17 <- "NA"

#shiny template
skin <- "yellow"
background <- "yellow" # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
status <- "warning"
#flag?
