
#average goals and modules completed from a percentage of total to a M number and SD
country <- "Malaysia_2"

library(survminer)
library(survival)
add_na_variable <- function(data = contacts_unflat, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}

### ParentText 2.0 ###
set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])
#set_rapidpro_uuid_names()

contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL)

contacts_unflat <- contacts_unflat %>% filter(as.Date(created_on) >= as.Date("2023-08-17"))
## Set up of variables

# TOP BOXES ----------------------------------------------------
if (country == "Malaysia_2"){
  names(contacts_unflat$groups) <- contacts_unflat$uuid
  groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
  groups_data <- groups_data %>%
    filter(name %in% c("kemas", "csos")) %>%
    mutate(value = 1) %>%
    dplyr::select(-uuid)
  groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
} else {
  names(contacts_unflat$groups) <- contacts_unflat$uuid
  groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
  groups_data <- groups_data %>%
    filter(name %in% c("in program", "joined", "enrolled")) %>%
    mutate(value = 1) %>%
    dplyr::select(-uuid)
  groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
}

df <- data.frame(groups_data)
valid_ids <- df$id

contacts_unflat <- contacts_unflat %>% filter(uuid %in% valid_ids)
# enrolled
df$enrolled <- ifelse(is.na(contacts_unflat$fields$research_id), "no", "yes")

df$research_id <- contacts_unflat$fields$research_id

# registered - done, under "completed_onboarding"

df$last_online <- as.Date(contacts_unflat$last_seen_on)
df$active_in_last_24_hours <- ifelse(as.Date(Sys.Date()) - df$last_online <= 1, "yes", "no")
df$active_in_last_7_days <- ifelse(as.Date(Sys.Date()) - df$last_online <= 7, "yes", "no")

# active overall - TODO

# DEMOGRAPHICS -------------------------------------------------
df$language <- contacts_unflat$language
df$gender <- contacts_unflat$fields$gender
df$child_age <- contacts_unflat$fields$child_age
df$marital_status <- contacts_unflat$fields$marital_status
df$child_gender <- contacts_unflat$fields$child_gender
df$has_disability <- contacts_unflat$fields$has_disability

df <- df %>%
  mutate(language = recode_factor(language,
                                  eng = "English",
                                  hau = "Swati",
                                  zul = "Zulu",
                                  .missing = "NA"),
         language = if_else(!language %in% c("English", "Swati", "Zulu", "NA"), "Other", language),
         language = fct_relevel(language, c("English", "Swati", "Zulu", "NA", "Other"))) %>%
  mutate(gender = recode_factor(gender,
                                woman = "Female",
                                man = "Male",
                                no = "Prefer not to say",
                                .missing = "NA")) %>%
  mutate(child_age = as.numeric(child_age)) %>%
  mutate(marital_status = recode_factor(marital_status,
                                        no = "Prefer not to say",
                                        .missing = "NA")) %>%
  mutate(child_gender = recode_factor(child_gender,
                                      woman = "Female",
                                      man = "Male",
                                      no = "Prefer not to say",
                                      .missing = "NA")) %>%
  mutate(has_disability = recode_factor(has_disability,
                                        no = "No",
                                        yes = "Yes"))


# DEMOGRAPHICS -------------------------------------------------
df$completed_onboarding <- contacts_unflat$fields$completed_onboarding
df$type_of_media <- contacts_unflat$fields$type_of_media

df <- df %>%
  mutate(type_of_media = recode_factor(type_of_media,
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
goals <- c("relation_t", "develop_t", "learning_t", "structure_t",
           "behave_t", "wellbeing_t", "safety_t",
           "relation_c", "develop_c", "learning_c", "structure_c",
           "behave_c", "wellbeing_c", "safety_c", "ipv", "budget")
n_mod_completed <- paste0("goal_", goals, "_n_mod_compl")
n_mod_total <- paste0("goal_", goals, "_n_mod")

contacts_unflat$fields <- add_na_variable(contacts_unflat$fields, n_mod_completed)
contacts_unflat$fields <- add_na_variable(contacts_unflat$fields, n_mod_total)

contacts_unflat$fields <- contacts_unflat$fields %>%
  # modules they've completed
  mutate(across(all_of(n_mod_completed), ~as.numeric(.))) %>%
  mutate(across(all_of(n_mod_completed), ~replace_na(., 0))) %>%
  mutate(n_modules_completed = rowSums(across(n_mod_completed))) %>%
  
  # total number in modules
  mutate(across(all_of(n_mod_total), ~as.numeric(.))) %>%
  mutate(across(all_of(n_mod_total), ~replace_na(., 0))) %>%
  mutate(n_modules_total = rowSums(across(n_mod_total))) %>%
  
  # percentage completed
  mutate(perc_modules_completed = round(n_modules_completed/n_modules_total * 100, 1))

df$n_modules_completed_numeric <- contacts_unflat$fields$n_modules_completed
df$n_modules_completed <- as_factor(contacts_unflat$fields$n_modules_completed)
df$perc_modules_completed <- (contacts_unflat$fields$perc_modules_completed)
# days_in_study
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

# ENGAGEMENT info FROM RUNS -------------------------------------------------------------
# module ID
modules <- c("one_on_one_teen", "praise_teen", "talk_feelings_teen",
             "care_myself_teen", "mental_changes_teen", "social_changes_teen", "physical_changes_teen",
             "fun_learning_teen", "help_teen_learn_teen", "positive_learning_teen", "learning_mistakes_teen", "learning_with_people_teen",
             "routines_teen", "rules_teen", "online_safety_teen", "habits_online_teen",
             "budget_needs_teen", "budget_expenses_teen", "budget_monthly_teen", "budget_savings_teen",
             "kind_to_myself_teen", "stress_teen", "show_kindness_teen", "stress_signs_teen",
             "manage_stress_teen", "misbehave_teen", "solve_problems_teen", "consequences_teen", 
             "community_safety_teen", "self_defence_teen", "sex_violence_teen", "crises_teen",
             "ipv_equals", "ipv_supportive", "ipv_sharing", "ipv_conflicts", "ipv_listen")

mod_home_activity_checkin <- paste0("home_activity_checkin - ", modules)

if (country == "Malaysia_2"){
  uuid_data = get_rapidpro_uuid_names()
  rapidpro_site = get_rapidpro_site()
  token = get_rapidpro_key()
  call_type <- "runs.json?contact="
  result_flow2 <- NULL
  for (i in 1:length(valid_ids)){
    get_command <- paste(rapidpro_site, call_type, valid_ids[i],
                         sep = "")
    result_flow2[[i]] <- rapidpror:::httr_get_call(get_command = get_command, token = token)
  }
  #result_flow2 <- result_flow21
  result_flow2 <- bind_rows(result_flow2)
  
  # For Home Activity Checkin:
  result_flow <- result_flow2 %>% filter(grepl("home_activity_checkin", flow$name))
  flow_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "check_in_2")
  flow_checkin_data <- flow_checkin_data %>% mutate(response = fct_recode(response, Yes = "yes", `Not yet` = "not yet"))
  flow_checkin_data <- flow_checkin_data %>% mutate(response = fct_relevel(response, c("Yes", "Not yet", "No response")))
  flow_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
  flow_checkin_data$ID <- str_remove(flow_checkin_data$ID, "_yc")
  
  # Etc for module and bits.
  result_flow <- result_flow2 %>% filter(grepl("module", flow$name))
  flow_module_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "will_complete")
  flow_module_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
  flow_module_checkin_data$ID <- str_remove(flow_module_checkin_data$ID, "_yc")
  flow_module_checkin_data <- flow_module_checkin_data %>% mutate(response = fct_recode(response, Yes = "yes", `No` = "No"))
  flow_module_checkin_data <- flow_module_checkin_data %>% mutate(response = fct_relevel(response, c("Yes", "No", "No response")))

  # safeguarding
  result_flow <- result_flow2 %>% filter(grepl("safeguarding_help", flow$name))
  flow_safeguarding_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "emergency")
  flow_safeguarding_data$ID <- sub(".* ", "", result_flow$flow$name)
  flow_safeguarding_data$response <- factor(flow_safeguarding_data$response)
  flow_safeguarding_data$interacted <- factor(flow_safeguarding_data$interacted)
  
  # pre-goal
  # pre-goal checkin
  pre_goal_checkin <- paste0("pre_goal_checkin - ", goals)
  result_flow <- result_flow2 %>% filter(grepl("pre_goal_checkin - ", flow$name))
  pre_goal_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "value", flow_handle_type_sub = "value")
  pre_goal_checkin_data <- pre_goal_checkin_data %>% mutate(response = fct_recode(response, Negative = "negative", `Positive` = "positive"))
  pre_goal_checkin_data <- pre_goal_checkin_data %>% mutate(response = fct_relevel(response, c("Positive", "Negative", "No response")))
  pre_goal_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
  
  # post-goal checkin - want IMPROVEMENT and VALUE
  post_goal_checkin <- paste0("post_goal_checkin - ", goals)
  result_flow <- result_flow2 %>% filter(grepl("post_goal_checkin - ", flow$name))
  post_goal_checkin_data_value <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "value", flow_handle_type_sub = "value")
  post_goal_checkin_data_value$ID <- sub(".* ", "", result_flow$flow$name)
  post_goal_checkin_data_improvement <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "improvement", flow_handle_type_sub = "category")
  post_goal_checkin_data_improvement <- post_goal_checkin_data_improvement %>%
    dplyr::mutate(improvement = response) %>% dplyr::select(-response)
  post_goal_checkin_data_improvement$ID <- sub(".* ", "", result_flow$flow$name)
  post_goal_checkin_data <- full_join(post_goal_checkin_data_value, post_goal_checkin_data_improvement)
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(response = fct_recode(response, Negative = "negative", `Positive` = "positive"))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(response = fct_relevel(response, c("Positive", "Negative", "No response")))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(improvement = fct_recode(improvement, Better = "better", `Same` = "same", `Worse` = "worse"))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(improvement = fct_relevel(improvement, c("Better", "Same", "Worse", "No response")))
} else {
  # If it is South Africa:
  
  flow_checkin_data <- get_flow_data(flow_name = mod_home_activity_checkin, flow_type = "check_in_2")
  flow_checkin_data <- flow_checkin_data %>% mutate(ID =  sub(".* ", "", `.id`))
  flow_checkin_data <- flow_checkin_data %>% mutate(response = fct_recode(response, Yes = "yes", `Not yet` = "not yet"))
  flow_checkin_data <- flow_checkin_data %>% mutate(response = fct_relevel(response, c("Yes", "Not yet", "No response")))
  flow_checkin_data <- flow_checkin_data %>% filter(uuid %in% valid_ids)
  
  #flow_checkin_data1 <- flow_checkin_data
  # todo: get the data in this format 
  #13:17 start
  
  module_checkin <- paste0("module - ", modules)
  flow_module_checkin_data <- get_flow_data(flow_name = module_checkin, flow_type = "other", flow_handle_type = "will_complete")
  flow_module_checkin_data <- flow_module_checkin_data %>% mutate(ID =  sub(".* ", "", `.id`))
  flow_module_checkin_data <- flow_module_checkin_data %>% mutate(response = fct_recode(response, Yes = "yes", `No` = "No"))
  flow_module_checkin_data <- flow_module_checkin_data %>% mutate(response = fct_relevel(response, c("Yes", "No", "No response")))
  flow_module_checkin_data <- flow_module_checkin_data %>% filter(uuid %in% valid_ids)
  
  # safeguarding help
  # Number of times the user accessed the referrals/emergency information through the trigger HELP
  # or from the main menu = number of runs for the user of the flow “safeguarding_help”
  
  flow_safeguarding_data <- get_flow_data(flow_name = "safeguarding_help", flow_type = "other", flow_handle_type = "emergency")
  flow_safeguarding_data <- flow_safeguarding_data %>% filter(uuid %in% valid_ids)
  flow_safeguarding_data$response <- factor(flow_safeguarding_data$response)
  flow_safeguarding_data$interacted <- factor(flow_safeguarding_data$interacted)
  
  # pre-goal checkin
  pre_goal_checkin <- paste0("pre_goal_checkin - ", goals)
  pre_goal_checkin_data <- rapidpror::get_flow_data(flow_name = pre_goal_checkin, flow_type = "other", flow_handle_type = "value", flow_handle_type_sub = "value") # value$value not value$category
  pre_goal_checkin_data <- pre_goal_checkin_data %>% mutate(ID =  sub(".* ", "", `.id`))
  pre_goal_checkin_data <- pre_goal_checkin_data %>% mutate(response = fct_recode(response, Negative = "negative", `Positive` = "positive"))
  pre_goal_checkin_data <- pre_goal_checkin_data %>% mutate(response = fct_relevel(response, c("Positive", "Negative", "No response")))
  pre_goal_checkin_data <- pre_goal_checkin_data %>% filter(uuid %in% valid_ids)
  
  # post-goal checkin - want IMPROVEMENT and VALUE
  post_goal_checkin <- paste0("post_goal_checkin - ", goals)
  post_goal_checkin_data <- get_flow_data(flow_name = post_goal_checkin, return_all = TRUE)
  post_goal_checkin_data_value <- purrr::map(.x = post_goal_checkin_data,
                                             .f = ~rapidpror:::flow_data_calculation(.x, flow_type = "other", flow_handle_type = "value", flow_handle_type_sub = "value"))
  names(post_goal_checkin_data_value) <- post_goal_checkin
  post_goal_checkin_data_value <- plyr::ldply(post_goal_checkin_data_value)
  post_goal_checkin_data_improvement <- purrr::map(.x = post_goal_checkin_data,
                                                   .f = ~rapidpror:::flow_data_calculation(.x, flow_type = "other", flow_handle_type = "improvement", flow_handle_type_sub = "category"))
  names(post_goal_checkin_data_improvement) <- post_goal_checkin
  post_goal_checkin_data_improvement <- plyr::ldply(post_goal_checkin_data_improvement) %>%
    dplyr::mutate(improvement = response) %>% dplyr::select(-response)
  post_goal_checkin_data <- full_join(post_goal_checkin_data_value, post_goal_checkin_data_improvement)
  post_goal_checkin_data <- post_goal_checkin_data %>% filter(uuid %in% valid_ids)
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(ID =  sub(".* ", "", `.id`))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(response = fct_recode(response, Negative = "negative", `Positive` = "positive"))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(response = fct_relevel(response, c("Positive", "Negative", "No response")))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(improvement = fct_recode(improvement, Better = "better", `Same` = "same", `Worse` = "worse"))
  post_goal_checkin_data <- post_goal_checkin_data %>% mutate(improvement = fct_relevel(improvement, c("Better", "Same", "Worse", "No response")))
}





# 
# metadata <- Hmisc::contents(df1)
# plyr::ldply(metadata$Levels)



# Data to send --------------------

#df$n_goals_completed_f <- NULL
#df$perc_goals_completed_f <- NULL
#df$n_modules_completed_numeric <- NULL
#df$time_in_study_n <- NULL

# df$first_user_name <- contacts_unflat$fields$first_user_name
# df$family_name <- contacts_unflat$fields$family_name
# df$research_id
# writexl::write_xlsx(df, "parenttext_2_data_20231103.xlsx")


