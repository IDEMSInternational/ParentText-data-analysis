# Jamaica Configuration
#set_rapidpro_key(file_path = "tokens/PT_jamaica_key.txt")
type <- "KPI"
if (type == "KPI"){
  key <- read.table("tokens/KPI_jamaica_key.txt", quote="\"", comment.char="")
  site <- "http://rapidpro.ilhasoft.mobi/api/v2/"
} else if (type == "SRH"){
  key <- read.table("tokens/SRH_jamaica_key.txt", quote="\"", comment.char="")
  site <- "https://rapidpro-next.idems.international/api/v2/"
} else {
  key <- read.table("tokens/PT_jamaica_key.txt", quote="\"", comment.char="")
  site <- "http://rapidpro.ilhasoft.mobi/api/v2/"
}
include_archived_data <- FALSE # for now FALSE
#if (include_archived_data){
#  archived_data <- TODO
#}
default_date_from <- "2022-06-10"
default_date_to <- NULL
prefix <- "JM"    #PH #SA
pt_name <- "ParentText"
country_name <- "Jamaica"

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

# for enrollment
enrollment_variables <- c("U-Report", "Government organisation", "NGO, community church, or charity worker", "Social media",
                          "Someone I know", "Door-to-door survery", "TV", "Radio", "Newspaper", "Other")
enrollment_recode <- c(`Government organisation` = "Government",
                       `NGO, community church, or charity worker` = "Ngo",
                       `Someone I know` = "Someone",
                       `U-Report` = "U-report",
                       `Social media` = "Social",
                       `TV` = "Tv",
                       `Radio` = "Radio",
                       `Newspaper` = "Newspaper",
                       Other = "Other channel")
enrollment_order <- c("U-Report", "Government organisation", "NGO, community church, or charity worker", "Social media",
                      "Someone I know", "Door-to-door survery", "TV", "Radio", "Newspaper", "Other")

#shiny template
skin <- "yellow"
background <- "yellow" # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
status <- "warning"
#flag?

