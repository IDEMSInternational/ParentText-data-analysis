# Philippines Configuration

# TODO: update key, site to give correct key and site.
# TODO: default_date_from

key <- read.table("tokens/PT_philippines_key.txt", quote="\"", comment.char="")
site <- "https://app.rapidpro.io/api/v2/"
include_archived_data <- FALSE # for now FALSE
#if (include_archived_data){
#  archived_data <- TODO
#}
default_date_from <- "2022-06-28"
default_date_to <- NULL
prefix <- "PH"
pt_name <- "MaPaText"
country_name <- "Philippines"

# for region/states
state_title <- "Region"
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

# for enrollment
enrollment_variables <- c("CPU or other NGO", "Social media - Facebook", "DSWD or other government agency", "Someone I know", "Other")
enrollment_recode <- c(`DSWD or other government agency` = "Government",
                       `CPU or other NGO` = "Ngo",
                       `Someone I know` = "Someone",
                       `Social media - Facebook` = "Social",
                       Other = "Other channel")
enrollment_order <- c("CPU or other NGO", "Social media - Facebook", "DSWD or other government agency", "Someone I know", "Other")

#shiny template
skin <- "green"
background <- "green" # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
status <- "success"
#flag?