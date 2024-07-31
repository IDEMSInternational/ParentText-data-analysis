
#average goals and modules completed from a percentage of total to a M number and SD
# library(survminer)
# library(survival)
add_na_variable <- function(data = contacts_unflat, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}

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

### ParentText 2.0 ###
set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])
set_rapidpro_uuid_names()

contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL, call_type = "contacts.json")

# no date filter required
#contacts_unflat <- contacts_unflat %>% dplyr::filter(as.Date(created_on) >= as.Date("2023-08-17"))

## Set up of variables

# TOP BOXES ----------------------------------------------------
names(contacts_unflat$groups) <- contacts_unflat$uuid
groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
groups_data <- groups_data %>%
  dplyr::filter(name %in% c("socialmedia", "share", "thsn")) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::select(-uuid)
groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)

df <- data.frame(groups_data) %>%
  mutate(id = as.character(id)) %>%
  arrange(id)

valid_ids <- df$id

contacts_unflat <- contacts_unflat %>%
  dplyr::filter(uuid %in% valid_ids) %>%
  arrange(uuid)

#contacts_unflat <- flatten(contacts_unflat)
# writexl::write_xlsx(contacts_unflat, path = "sa_users_snapshot_20240306.xlsx")
# saveRDS(contacts_unflat, file = "malaysia_users_snapshot_29022024.rds")

x <- data.frame(df = df$id, cu = contacts_unflat$uuid) %>%
  mutate(hi = ifelse(df == cu, 1, 0)) %>%
  filter(hi == 0)
if (nrow(x) > 0) stop("Check ID order")

# enrolled
df$enrolled <- "yes"

df$research_id <- contacts_unflat$fields$research_id

df$created_on <- lubridate::as_date(contacts_unflat$created_on)

df$start_time <- as.Date(contacts_unflat$fields$start_time)


if ("share" %in% names(df)){
  df <- df %>% mutate(group_name = ifelse(socialmedia == 1, "unicef",
                                          ifelse(share == 1, "share", 
                                                 ifelse(thsn == 1, "thsn", "other"))))
} else {
  df <- df %>% mutate(group_name = ifelse(socialmedia == 1, "unicef",
                                          ifelse(thsn == 1, "thsn", "other")))
}


# start time for KEMAS - fields$start_time
valid_ids <- df$id
contacts_unflat <- contacts_unflat %>%
  dplyr::filter(uuid %in% valid_ids) %>%
  arrange(uuid)
x <- data.frame(df = df$id, cu = contacts_unflat$uuid) %>%
  mutate(hi = ifelse(df == cu, 1, 0)) %>%
  filter(hi == 0)
if (nrow(x) > 0) stop("Check ID order")

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
df$ethnicity <- contacts_unflat$fields$ethnicity
df$ethnicity <- factor(ifelse(is.na(df$ethnicity), "NA", df$ethnicity))
df$education_level <- contacts_unflat$fields$education_level
df$education_level <- factor(ifelse(is.na(df$education_level), "NA", df$education_level))
df$parent_prog_exp <- contacts_unflat$fields$parent_prog_exp
df$location <- contacts_unflat$fields$location
df$location <- factor(ifelse(is.na(df$location), "NA", df$location))

df <- df %>%
  dplyr::mutate(parent_prog_exp = recode_factor(parent_prog_exp,
                                                no = "No",
                                                yes = "Yes",
                                                .missing = "NA")) %>%
  dplyr::mutate(language = recode_factor(language,
                                         eng = "English",
                                         msa = "Malay / Bahasa Melayu",
                                         .missing = "NA"),
                language = if_else(!language %in% c("English", "Malay / Bahasa Melayu", "NA"), "Other", language),
                language = fct_relevel(language, c("English", "Malay / Bahasa Melayu", "NA", "Other"))) %>%
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

# NEW BITS ------------------------------------------------------
# constraints to have:
# group_name - unicef, thsn, share

df$parent_age <- as.numeric(contacts_unflat$fields$parent_age)

df <- df %>%
  mutate(parent_age_group = ifelse(is.na(parent_age), "NA", 
                                   ifelse(parent_age <= 17, "17 and below",
                                   ifelse(parent_age <= 19, "18-19",
                                          ifelse(parent_age <= 29, "20-29",
                                                 ifelse(parent_age <= 39, "30-39",
                                                        ifelse(parent_age <= 49, "40-49",
                                                               ifelse(parent_age <= 59, "50-59",
                                                                      ifelse(parent_age <= 69, "60-69",
                                                                             ifelse(parent_age <= 79, "70-79",
                                                                                    ifelse(parent_age > 79, "80+",
                                                                                                  "Other")))))))))))
df$parent_age_group <- factor(df$parent_age_group)

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
df$helpful <- contacts_unflat$fields$helpful
df$useful_goal <- contacts_unflat$fields$useful_goal
df$recommend <- contacts_unflat$fields$recommend

# first goal
# df$first_goal <- gsub( " .*$", "", contacts_unflat$fields$goals_accessed)
# table(first_goal)
# ggplot(df, aes(x = first_goal)) + geom_bar()
# 
# table(contacts_unflat$fields$goals_accessed)

# goal ids
goals <- c("stress", "relation", "develop", "learning",
           "structure", "behave", "safety", "ipv", "budget")

n_mod_completed <- paste0("goal_", goals, "_n_mod_compl")
n_mod_total <- paste0("goal_", goals, "_n_mod")

# mught put in modules later. 


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

#  df <- df %>% dplyr::mutate(group = ifelse(kemas == 1, "KEMAS",
#                                            ifelse(csos == 1, "CSOS", "None")))

# transitions on "flow" page
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
modules <- c("stress", "relation", "develop", "learning", "structure", "behave", "safety", "ipv", "budget")

mod_home_activity_checkin <- paste0("home_activity_checkin - ", modules)

df$uuid <- df$id

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

#result_flow21 <- flatten(result_flow2)
#writexl::write_xlsx(result_flow21, path = "malaysia_flows_snapshot_29022024.xlsx")
#saveRDS(result_flow2, path = "malaysia_flows_snapshot_29022024.rds")

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

