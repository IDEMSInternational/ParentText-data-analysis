# to install most recent
library(tidyverse)
library(readxl)
library(rapidpror)

# set-up
set_rapidpro_site(site = site)
set_rapidpro_key(key = key$V1)


replace_none <- function(input_string) {
  # Check if the string contains "kemas" or "csos"
  if(any(grepl("KEMAS", input_string))) {
    # Replace "none" with "kemas"
    return(gsub("None", "KEMAS", input_string))
  } else if(any(grepl("csos", input_string))) {
    # Replace "none" with "csos"
    return(gsub("None", "CSOS", input_string))
  } else {
    # If neither "KEMAS" nor "csos" is found, return the original string
    return(input_string)
  }
}


#### Get the data ##############################################################
contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL)
names(contacts_unflat$groups) <- contacts_unflat$uuid
groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
groups_data <- groups_data %>%
  dplyr::filter(name %in% c("mexicopilot")) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::select(-uuid)
groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)

valid_ids <- groups_data$id
contacts_unflat <- contacts_unflat %>% filter(uuid %in% valid_ids)

df <- data.frame(groups_data)
valid_ids <- df$id
contacts_unflat <- contacts_unflat %>% dplyr::filter(uuid %in% valid_ids)

#### user data #################################################################
df$enrolled <- ifelse(is.na(contacts_unflat$fields$research_id), "no", "yes")
df$research_id <- contacts_unflat$fields$research_id
df$created_on <- lubridate::as_date(contacts_unflat$created_on)

# registered - done, under "completed_onboarding"
df$last_online <- as.Date(contacts_unflat$last_seen_on)
df$active_in_last_24_hours <- ifelse(as.Date(Sys.Date()) - df$last_online <= 1, "yes", "no")
df$active_in_last_7_days <- ifelse(as.Date(Sys.Date()) - df$last_online <= 7, "yes", "no")

# DEMOGRAPHICS -------------------------------------------------
df$language <- contacts_unflat$language
df$gender <- contacts_unflat$fields$gender
df$child_age <- contacts_unflat$fields$child_age
df$marital_status <- contacts_unflat$fields$marital_status
df$child_gender <- contacts_unflat$fields$child_gender
df$has_disability <- contacts_unflat$fields$has_disability

df <- df %>%
  dplyr::mutate(language = recode_factor(language,
                                         eng = "English",
                                         spa = "Spanish",
                                         .missing = "NA"),
                language = if_else(!language %in% c("English", "Spanish", "NA"), "Other", language),
                language = fct_relevel(language, c("English", "Spanish", "NA", "Other"))) %>%
  dplyr::mutate(gender = recode_factor(gender,
                                       woman = "Female",
                                       man = "Male",
                                       no = "Prefer not to say",
                                       .missing = "NA")) %>%
  dplyr::mutate(child_age = as.numeric(child_age)) %>%
  dplyr::mutate(marital_status = recode_factor(marital_status,
                                               no = "Prefer not to say",
                                               .missing = "NA")) %>%
  dplyr::mutate(child_gender = recode_factor(child_gender,
                                             woman = "Female",
                                             man = "Male",
                                             no = "Prefer not to say",
                                             .missing = "NA")) %>%
  dplyr::mutate(has_disability = recode_factor(has_disability,
                                               no = "No",
                                               yes = "Yes"))


# DEMOGRAPHICS -------------------------------------------------
df$completed_onboarding <- contacts_unflat$fields$completed_onboarding
df$type_of_media <- contacts_unflat$fields$type_of_media

df <- df %>%
  dplyr::mutate(type_of_media = recode_factor(type_of_media,
                                              high = "High",
                                              medium = "Medium",
                                              low = "Low",
                                              .missing = "NA"),
                type_of_media = fct_relevel(type_of_media, c("High", "Medium", "Low", "NA")))


# ENGAGEMENT FROM VARIABLES ----------- GOALS COMPLETED
df$n_goals_completed_f <- factor(contacts_unflat$fields$n_goals_completed)
df$n_goals_completed <- as.numeric(contacts_unflat$fields$n_goals_completed)
df$n_goals_prog <- as.numeric(contacts_unflat$fields$n_goals_prog)
df$perc_goals_completed <- round(df$n_goals_completed / df$n_goals_prog * 100, 1)
df$perc_goals_completed_f <- df$perc_goals_completed # _f <- factor(df$perc_goals_completed)

# first goal
# df$first_goal <- gsub( " .*$", "", contacts_unflat$fields$goals_accessed)
# table(first_goal)
# ggplot(df, aes(x = first_goal)) + geom_bar()
# 
# table(contacts_unflat$fields$goals_accessed)

# goal ids
goals <- c("relation", "stress")
#              "develop_t", "learning_t", "structure_t",
#              "behave_t", "wellbeing_t", "safety_t",
#              "develop_c", "learning_c", "structure_c",
#              "behave_c", "wellbeing_c", "safety_c", "ipv", "budget")
n_mod_completed <- paste0("goal_", goals, "_n_mod_compl")
n_mod_total <- paste0("goal_", goals, "_n_mod")

contacts_unflat$fields <- add_na_variable(contacts_unflat$fields, n_mod_completed)
contacts_unflat$fields <- add_na_variable(contacts_unflat$fields, n_mod_total)

contacts_unflat$fields <- contacts_unflat$fields %>%
  # modules they've completed
  dplyr::mutate(across(all_of(n_mod_completed), ~as.numeric(.))) %>%
  dplyr::mutate(across(all_of(n_mod_completed), ~replace_na(., 0))) %>%
  dplyr::mutate(n_modules_completed = rowSums(across(n_mod_completed))) %>%
  
  # total number in modules
  dplyr::mutate(across(all_of(n_mod_total), ~as.numeric(.))) %>%
  dplyr::mutate(across(all_of(n_mod_total), ~replace_na(., 0))) %>%
  dplyr::mutate(n_modules_total = rowSums(across(n_mod_total))) %>%
  
  # percentage completed
  dplyr::mutate(perc_modules_completed = round(n_modules_completed/n_modules_total * 100, 1))

df$n_modules_completed_numeric <- contacts_unflat$fields$n_modules_completed
df$n_modules_completed <- as_factor(contacts_unflat$fields$n_modules_completed)
df$perc_modules_completed <- (contacts_unflat$fields$perc_modules_completed)

# 
# contacts_unflat$
# 
# If “completion_time” has a value, completion_time
# Elseif the last element of “hook_message” only has date_hook, date_hook
# Elseif “leave_time” has a value,
# leave_time
# Else
# today

#stringr::str_split(contacts_unflat$fields$hook_message, fixed("|"), simplify = TRUE)

contacts_unflat$fields$time_hook_unreplied <- ifelse(!is.na(contacts_unflat$fields$hook_message),
                                                     ifelse(grepl("\\|$", contacts_unflat$fields$hook_message), "NA", # if it ends with | then set as 0,
                                                            # if it contains a | then take everything after |
                                                            sub(".*\\|", "", contacts_unflat$fields$hook_message)), "NA")
contacts_unflat$fields$time_hook_unreplied <- lubridate::as_date(contacts_unflat$fields$time_hook_unreplied)

contacts_unflat$fields$end_time <- if_else(!is.na(contacts_unflat$fields$completion_time), lubridate::as_date(contacts_unflat$fields$completion_time),
                                           if_else(!is.na(contacts_unflat$fields$time_hook_unreplied), contacts_unflat$fields$time_hook_unreplied,
                                                   if_else(!is.na(contacts_unflat$fields$leave_time), lubridate::as_date(contacts_unflat$fields$leave_time),
                                                           lubridate::as_date(Sys.Date()))))
# df$cens <- if_else(contacts_unflat$fields$end_time == lubridate::as_date(Sys.Date()),
#                    0, # haven't met the event / haven't left the study
#                    1) # met the event
# 
df$time_in_study <- factor(contacts_unflat$fields$end_time - lubridate::as_date(contacts_unflat$created_on))
df$time_in_study_n <- as.numeric(as.character(df$time_in_study))
# drop out plot if they want it
# fit <- survfit(Surv(time_in_study, cens) ~ 1, data = df)
# ggsurvplot(fit, data = df)
# df %>% group_by(cens) %>% summarise(mean(time_in_study))
# if they've not left yet, make them censored?

# transitions on "flow" page #####################################################
goals_accessed <- data.frame(stringr::str_split(contacts_unflat$fields$goals_accessed, " ", simplify = TRUE))
goals_accessed <- bind_cols(ID = contacts_unflat$uuid, goals_accessed) %>%
  pivot_longer(cols = !ID) %>%
  mutate(name = as.numeric(as.factor(name)),
         value = as.factor(value))

goals_accessed_size <- goals_accessed %>%
  group_by(name, value) %>%
  summarise(n())

# Create a new data frame for transitions
transitions <- goals_accessed %>%
  arrange(ID, name) %>%
  group_by(ID) %>%
  mutate(value_end = lead(value), name_end = lead(name)) %>%
  filter(!is.na(value_end)) %>%
  ungroup()

transitions$uuid <- transitions$ID
transitions$ID <- NULL

valid_ids <- df$id
rm(contacts_unflat)

# ENGAGEMENT info FROM RUNS -------------------------------------------------------------
# module ID
#mexico_goals <- read_excel(path = "...", sheet = "Copy of module_data")
#modules <- mexico_goals$ID

#mod_home_activity_checkin <- paste0("home_activity_checkin - ", modules)
# 
# get_command <- paste(rapidpro_site, call_type, valid_ids[1], sep = "")
# response <- httr::GET(get_command, config = httr::add_headers(Authorization = paste("Token", 
#                                                                                     token)))
# raw <- httr::content(response, as = "text")
# # results <- jsonlite::fromJSON(raw)

uuid_data = get_rapidpro_uuid_names()
rapidpro_site = get_rapidpro_site()
token = get_rapidpro_key()
call_type <- "runs.json?contact="
result_flow2 <- NULL
for (i in 1:length(valid_ids)){ # get each individual
  get_command <- paste(rapidpro_site, call_type, valid_ids[i],
                       sep = "")
  result_flow2[[i]] <- rapidpror:::httr_get_call(get_command = get_command, token = token)
}
#result_flow2 <- result_flow21
result_flow2 <- bind_rows(result_flow2)

# for goals:
# result_flow <- result_flow2 %>% dplyr::filter(grepl("home_activity_checkin -", flow$name))
# flow_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "check_in_2")
# flow_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
# flow_checkin_data <- flow_checkin_data %>% dplyr::mutate(response = fct_recode(response, Yes = "yes", `Not yet` = "not yet"))
# flow_checkin_data <- flow_checkin_data %>% dplyr::mutate(response = fct_relevel(response, c("Yes", "Not yet", "No response")))

# For Module:
result_flow <- result_flow2 %>% dplyr::filter(grepl("module -", flow$name))
flow_module_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "will_complete")
flow_module_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
#flow_module_checkin_data$ID <- str_remove(flow_module_checkin_data$ID, "_yc")
flow_module_checkin_data <- flow_module_checkin_data %>% dplyr::mutate(response = fct_recode(response, Yes = "yes", `No` = "No"))
flow_module_checkin_data <- flow_module_checkin_data %>% dplyr::mutate(response = fct_relevel(response, c("Yes", "No", "No response")))

# safeguarding
result_flow <- result_flow2 %>% dplyr::filter(grepl("safeguarding_help", flow$name))
flow_safeguarding_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "emergency")
flow_safeguarding_data$ID <- sub(".* ", "", result_flow$flow$name)
flow_safeguarding_data$response <- factor(flow_safeguarding_data$response)
flow_safeguarding_data$interacted <- factor(flow_safeguarding_data$interacted)

# pre-goal
# pre-goal checkin
pre_goal_checkin <- paste0("pre_goal_checkin - ", goals)
result_flow <- result_flow2 %>% dplyr::filter(grepl("pre_goal_checkin - ", flow$name))
pre_goal_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "value", flow_handle_type_sub = "value")
pre_goal_checkin_data$response <- str_to_title(pre_goal_checkin_data$response)
#pre_goal_checkin_data <- pre_goal_checkin_data %>% dplyr::mutate(response = fct_relevel(response, c("Positive", "Negative", "No response")))
pre_goal_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)

# post-goal checkin - want IMPROVEMENT and VALUE
post_goal_checkin <- paste0("post_goal_checkin - ", goals)
result_flow <- result_flow2 %>% dplyr::filter(grepl("post_goal_checkin - ", flow$name))
post_goal_checkin_data_value <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "value", flow_handle_type_sub = "value")
post_goal_checkin_data_value$ID <- sub(".* ", "", result_flow$flow$name)
post_goal_checkin_data_improvement <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "improvement", flow_handle_type_sub = "category")
post_goal_checkin_data_improvement <- post_goal_checkin_data_improvement %>%
  dplyr::mutate(improvement = response) %>% dplyr::select(-response)
post_goal_checkin_data_improvement$ID <- sub(".* ", "", result_flow$flow$name)
post_goal_checkin_data <- full_join(post_goal_checkin_data_value, post_goal_checkin_data_improvement)
post_goal_checkin_data$response <- str_to_title(post_goal_checkin_data$response)
post_goal_checkin_data$improvement <- str_to_title(post_goal_checkin_data$improvement)
#post_goal_checkin_data <- post_goal_checkin_data %>% dplyr::mutate(response = fct_relevel(response, c("Positive", "Negative", "No response")))
#post_goal_checkin_data <- post_goal_checkin_data %>% dplyr::mutate(improvement = fct_relevel(improvement, c("Better", "Same", "Worse", "No response")))
rm(result_flow2)

# Transitions Code

# For goals - all checkin data:
pre_goal_checkin_data <- pre_goal_checkin_data %>% 
  dplyr::filter(interacted) %>%
  dplyr::select(uuid, response, ID) %>%
  dplyr::mutate(time = "Pre")

post_goal_checkin_data <- post_goal_checkin_data %>% 
  filter(interacted) %>%
  select(uuid, response, ID, improvement) %>%
  mutate(time = "Post")

# Merging and counting
checkin_data <- bind_rows(pre_goal_checkin_data, post_goal_checkin_data) %>%
  mutate(time = fct_relevel(time, c("Pre", "Post")))

df$uuid <- df$id

df <- df %>% dplyr::mutate(group = ifelse(mexicopilot == 1, "mexicopilot", "None"))

#Connect to Database to get original data
source("config/personal_setup_mx.R")
faciNK_data <- postgresr::get_user_data(site = plh_con, filter = FALSE)
names(faciNK_data) <- gsub(x = names(faciNK_data), pattern = "\\-", replacement = ".")  

faciNK_data <- faciNK_data %>%
  filter(rp.contact.field.current_package %in% c("cdmx_dif", "cdmx_bs", "bc_dif", "edomx_bs", 
                                                 "mich_dif", "chih_dif"))

parent_data <- purrr::map(faciNK_data$`rp.contact.field.parent_data`, jsonlite::fromJSON)
family_data <- purrr::map(faciNK_data$`rp.contact.field.family_data`, jsonlite::fromJSON)
parent_data_table <- NULL
family_data_table <- NULL
valid_research_id <- df$research_id
facilitators <- NULL
j <- 1
for (i in 1:length(parent_data)){
  parent_data_table_i <- bind_rows(parent_data[[i]])
  
  group <- faciNK_data$`rp.contact.field.current_package`[[i]]
  
  # get external if it is one of valid_research_id
  external_id <- parent_data_table_i$external_id 
  match_id <- match(external_id, valid_research_id)
  
  
  # [1]   9  30  51  54  70  78  84  92 115 127
  
  # or if no masw 5, 28, 46, 49, 52, 64, 68
  if (!is.na(match_id) && any(match_id)){
    facilitators[j] <- i
    my_list <- family_data[[i]]
    indices <- rep(seq_along(my_list), times = sapply(my_list, length))
    parent_data_table_i <- parent_data_table_i %>% mutate(group_name = group)
    parent_data_table_i$parent_group_no <- indices
    parent_data_table_i <- parent_data_table_i %>%
      mutate(duplicated_parent_group = as.integer(!duplicated(indices)))
    parent_data_table[[j]] <- parent_data_table_i
    j <- j+1
  }
}
names(parent_data_table) <- faciNK_data$app_user_id[facilitators]
parent_data_table <- bind_rows(parent_data_table, .id = "facilitator")

# ok so now we want to find people who are parent_group_no > 1 so that we can filter the children to the 
# unique children in the analysis. 
parent_data_table <- parent_data_table %>% dplyr::select(c(facilitator, parent_group_no, external_id, group_name, duplicated_parent_group))
parent_data_table <- unique(parent_data_table)

parent_data_table$group_name <- case_match(
  parent_data_table$group_name,
  "bc_dif" ~ 1,             # randomly generated
  "mich_dif" ~ 2,
  "cdmx_dif" ~ 3,
  "chih_dif" ~ 4,
  "cdmx_bs" ~ 5,
  "edomx_bs" ~ 6
)
# which IDs are repeated in parent_data_table?

# merge with df   #df_unjoin is a copy of df
df <- left_join(df, parent_data_table, by = c("research_id" = "external_id"))
df <- df %>%
  
  # replace missing "group" with KEMAS if they have "kemas" defined for them. 
  group_by(facilitator) %>%
  mutate(group = ifelse(!is.na(group_name), replace_none(group), group)) %>%
  
  mutate(duplicated_parent_group = ifelse(is.na(duplicated_parent_group) | duplicated_parent_group == 1, 1, 2)) %>%
  
  # remove the ungrouped individuals.
  #' if someone is Unknown and their kemas_group is NA then is it safe to assume these are not individuals who are on the Malaysia Chatbot 2.0? Since we're now reading in all individuals who are in the KEMAS, CSOS, or no group.
  mutate(indicator = ifelse(group == "None" & is.na(facilitator), 1, 0)) %>%
  filter(indicator != 1) %>%
  dplyr::filter(is.na(facilitator) | facilitator != "5a270565-b7bf-4695-8e5e-012b7b2d001d") # this fac has 6 rows of data, all repeated with another fac id

df <- df %>% ungroup()



# 
# metadata <- Hmisc::contents(df1)
# plyr::ldply(metadata$Levels)



# Data to send to Chiara - 15/02 --------------------
# df <- data.frame(name = contacts_unflat$fields$first_user_name,
#                  surname = contacts_unflat$fields$family_name,
#                  research_id = contacts_unflat$fields$research_id,
#                  start_time = lubridate::as_date(contacts_unflat$fields$start_time),
#                  phone_number = unlist(contacts_unflat$urns),
#                  completed_onboarding = contacts_unflat$fields$completed_onboarding,
#                  goal_relation_n_mod_compl = contacts_unflat$fields$goal_relation_n_mod_compl,
#                  goal_relation_n_mod_started = contacts_unflat$fields$goal_relation_n_mod_started,
#                  completed_language = contacts_unflat$fields$completed_language,
#                  completed_consent = contacts_unflat$fields$completed_consent,
#                  completed_first_name = contacts_unflat$fields$completed_first_name,
#                  completed_family_name = contacts_unflat$fields$completed_family_name,
#                  completed_gender = contacts_unflat$fields$completed_gender,
#                  completed_location = contacts_unflat$fields$completed_location,
#                  completed_orientation = contacts_unflat$fields$completed_orientation,
#                  completed_media = contacts_unflat$fields$completed_media,
#                  completed_marital_status = contacts_unflat$fields$completed_marital_status,
#                  completed_child_name = contacts_unflat$fields$completed_child_name,
#                  completed_child_gender = contacts_unflat$fields$completed_child_gender,
#                  completed_child_dob = contacts_unflat$fields$completed_child_dob,
#                  completed_pause = contacts_unflat$fields$completed_pause)
# 
# df_type_number <- data.frame(stringr::str_split(df$phone_number, ":", simplify = TRUE))
# names(df_type_number) <- c("device_type", "number")
# df <- cbind(df, df_type_number)
# 
# writexl::write_xlsx(df, path = "mexico_pilot_data_20240219.xlsx")
# 
# writexl::write_xlsx(contacts_unflat, path = "mexico_complete_data_20240219.xlsx")
