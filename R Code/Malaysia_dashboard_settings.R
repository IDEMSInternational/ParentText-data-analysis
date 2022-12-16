# Malaysia Configuration
key <- read.table("tokens/PT_malaysia_key.txt", quote="\"", comment.char="")
site <- "https://app.rapidpro.io/api/v2/"
include_archived_data <- TRUE
default_date_from <- "2021-10-14"
default_date_to <- NULL
prefix <- "MY"
pt_name <- "ParentText"
country_name<- "Malaysia"

# for region/states
state_title <- "State/territory"
state_1 <- "Johor"
state_2 <- "Kedah"
state_3 <- "Kelantan" 
state_4 <- "Melaka"
state_5 <- "Negeri Sembilan"
state_6 <- "Pahang"
state_7 <- "Perak"
state_8 <- "Perlis"
state_9 <- "Penang"
state_10 <- "Sabah"
state_11 <- "Sarawak" 
state_12 <- "Selangor" 
state_13 <- "Terengganu" 
state_14 <- "Wilayah Persekutuan"
state_15 <- "Kuala Lumpur"
state_16 <- "Putrajaya dan Labuan"
state_17 <- "NA"

# for enrollment
enrollment_variables <- c("U-Report", "NGO", "Social media", "National Family & Population Development Board",
                          "Someone I know", "Other")
enrollment_recode <- c(`National Family & Population Development Board` = "LPPKN",
                            `NGO` = "Ngo",
                            `Someone I know` = "Someone",
                            `U-Report` = "U-report",
                            `Social media` = "Social",
                            Other = "Other channel")
enrollment_order <- c("U-Report", "NGO", "Social media", "National Family & Population Development Board",
                      "Someone I know", "Other")

#shiny template
skin <- "blue"
background <- "light-blue"
status <- "primary"
#flag?
