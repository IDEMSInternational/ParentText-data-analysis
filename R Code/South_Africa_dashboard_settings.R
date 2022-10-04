# South Africa Configuration

# TODO: update key, site to give correct key and site.
# TODO: default_date_from

key <- read.table("tokens/PT_south_africa_key.txt", quote="\"", comment.char="")
site <- "https://rapidpro.ona.io/api/v2/"
include_archived_data <- FALSE # for now FALSE
#if (include_archived_data){
#  archived_data <- TODO
#}
default_date_from <- "2022-09-29"
default_date_to <- NULL
prefix <- "SA"
pt_name <- "ParentText"

# for region/states
state_title <- "Province"
state_1 <- "Eastern Cape"
state_2 <- "Free State"
state_3 <- "Gauteng"
state_4 <- "KwaZulu-Natal"
state_5 <- "Limpopo"
state_6 <- "Mpumalanga"
state_7 <- "North West"
state_8 <- "Northern Cape"
state_9 <- "Western Cape"
state_10 <- "NA"
state_11 <- "NA" 
state_12 <- "NA"
state_13 <- "NA"
state_14 <- "NA"
state_15 <- "NA"
state_16 <- "NA"
state_17 <- "NA"

# for enrollment
enrollment_variables <- c("Government", "NGO", "Someone I know", "U-Report", "Social media", "Zlto", "Other")
enrollment_recode <- c(`Government` = "Government",
                       `NGO` = "Ngo",
                       `Someone I know` = "Someone",
                       `U-Report` = "U-report",
                       `Social media` = "Social",
                       `Zlto` = "Zlto",
                       Other = "Other channel")
enrollment_order <- c("U-Report", "NGO", "Social media", "Government", "Someone I know", "Zlto", "Other")

#shiny template
skin <- "red"
background <- "red" # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
status <- "danger"
#flag?