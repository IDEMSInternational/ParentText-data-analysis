library(tidyverse)

# Read in your data here
user_data <- readxl::read_excel("~/GitHub/ParentText-data-analysis/R Code/SA_data.20240327.xlsx")

####################################################################################

# Creating a function so that any NULL variables don't throw an error
add_na_variable <- function(data = user_data, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}

################## Creating Module Related Variables ###############################
# These are the different goals in your data: (it might be just "t" for yours, but it's ok to do it this way!)
goals <- c("relation_t", "develop_t", "learning_t", "structure_t",
           "behave_t", "wellbeing_t", "safety_t",
           "relation_c", "develop_c", "learning_c", "structure_c",
           "behave_c", "wellbeing_c", "safety_c", "ipv", "budget")


# create a vector containing the variables related to completing a module, and then total modules
n_mod_completed <- paste0("goal_", goals, "_n_mod_compl")
n_mod_total <- paste0("goal_", goals, "_n_mod")

# Adding a dummy variable if it does not exist
user_data <- add_na_variable(user_data, n_mod_completed)
user_data <- add_na_variable(user_data, n_mod_total)

user_data <- user_data %>%
  
  # modules they've completed: Set as numeric, replace NAs with a 0, then sum them up-
  dplyr::mutate(across(all_of(n_mod_completed), ~as.numeric(.))) %>%
  dplyr::mutate(across(all_of(n_mod_completed), ~replace_na(., 0))) %>%
  dplyr::mutate(n_modules_completed = rowSums(across(n_mod_completed))) %>%
  
  # total number in modules: Set as numeric, replace NAs with a 0, then sum them up-
  dplyr::mutate(across(all_of(n_mod_total), ~as.numeric(.))) %>%
  dplyr::mutate(across(all_of(n_mod_total), ~replace_na(., 0))) %>%
  dplyr::mutate(n_modules_total = rowSums(across(n_mod_total))) %>%
  
  # percentage completed
  dplyr::mutate(perc_modules_completed = round(n_modules_completed/n_modules_total * 100, 1))


################## Creating Time in Study Variable ###############################
# First we create an "End Time" variable:
# 1. If “completion_time” has a value, then we give the end time as completion_time
# 2. Elseif the last element of “hook_message” only has date_hook, date_hook
# 3. Elseif “leave_time” has a value, use leave_time
# 4. Otherwise, the final time is not yet given, so give "today"

user_data$time_hook_unreplied <- ifelse(!is.na(user_data$hook_message),
                                                     ifelse(grepl("\\|$", user_data$hook_message), "NA", # if it ends with | then set as 0,
                                                            # if it contains a | then take everything after |
                                                            sub(".*\\|", "", user_data$hook_message)), "NA")
user_data$time_hook_unreplied <- lubridate::as_date(user_data$time_hook_unreplied)

user_data$end_time <- if_else(!is.na(user_data$completion_time), lubridate::as_date(user_data$completion_time),
                                           if_else(!is.na(user_data$time_hook_unreplied), user_data$time_hook_unreplied,
                                                   if_else(!is.na(user_data$leave_time), lubridate::as_date(user_data$leave_time),
                                                           lubridate::as_date(Sys.Date()))))

# Calculate difference between our end_time variable, and when they created their data
user_data$time_in_study <- factor(user_data$end_time - lubridate::as_date(user_data$created_on))
user_data$time_in_study_n <- as.numeric(as.character(user_data$time_in_study))
