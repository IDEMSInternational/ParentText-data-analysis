
#average goals and modules completed from a percentage of total to a M number and SD
country <- "Malaysia_2"# South_Africa_2" #Malaysia_2"

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

# 
#if (country == "Malaysia_2"){
#  contacts_unflat <- contacts_unflat %>% dplyr::filter(as.Date(created_on) >= as.Date("2024-01-01"))
#} else {
contacts_unflat <- contacts_unflat %>% dplyr::filter(as.Date(created_on) >= as.Date("2023-08-17"))

if (country == "South_Africa_2"){
  contacts_unflat <- contacts_unflat %>% dplyr::filter(as.Date(created_on) <= as.Date("2023-11-01"))
}
#}
## Set up of variables

# TOP BOXES ----------------------------------------------------
if (country == "Malaysia_2"){
  names(contacts_unflat$groups) <- contacts_unflat$uuid
  val <- NULL
  j <- 1
  for (i in 1:length(contacts_unflat$groups)){
    if (length(contacts_unflat$groups[[i]]) == 0){
      val[[j]] <- c(id = contacts_unflat$uuid[i])
      j <- j + 1
    }
  }
  vals <- bind_rows(val) %>% mutate(csos = 0, kemas = 0)
  groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
  groups_data <- groups_data %>%
    dplyr::filter(name %in% c("kemas", "csos", "joined")) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::select(-uuid)
  groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
  groups_data <- bind_rows(groups_data, vals)
} else {
  names(contacts_unflat$groups) <- contacts_unflat$uuid
  groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
  groups_data <- groups_data %>%
    dplyr::filter(name %in% c("in program", "joined", "enrolled")) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::select(-uuid)
  groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
}

df <- data.frame(groups_data) %>%
  mutate(id = as.character(id)) %>%
  arrange(id)

valid_ids <- df$id

length(valid_ids)

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
df$enrolled <- ifelse(is.na(contacts_unflat$fields$research_id), "no", "yes")

df$research_id <- contacts_unflat$fields$research_id

df$created_on <- lubridate::as_date(contacts_unflat$created_on)

df$start_time <- as.Date(contacts_unflat$fields$start_time)

# remove CSOS before 13th Jan
df <- df %>%
  mutate(indicator = ifelse(csos == 1 & start_time < as.Date("2024-01-13"), 1, 0)) %>%
  filter(indicator != 1)

# active overall - TODO
df <- df %>% mutate(indicator = ifelse(kemas == 1 && start_time < as.Date("2024-01-01"), 1, 0)) %>%
  filter(indicator != 1)

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

df <- df %>%
  dplyr::mutate(language = recode_factor(language,
                                  eng = "English",
                                  hau = "Swati",
                                  zul = "Zulu",
                                  .missing = "NA"),
         language = if_else(!language %in% c("English", "Swati", "Zulu", "NA"), "Other", language),
         language = fct_relevel(language, c("English", "Swati", "Zulu", "NA", "Other"))) %>%
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
if (country == "Malaysia_2"){
  goals <- c("stress", "relation", "develop", "learning",
             "structure", "behave", "safety")
} else {
  goals <- c("relation_t", "develop_t", "learning_t", "structure_t",
             "behave_t", "wellbeing_t", "safety_t",
             "relation_c", "develop_c", "learning_c", "structure_c",
             "behave_c", "wellbeing_c", "safety_c", "ipv", "budget")
}
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

if (country == "Malaysia_2"){
  df <- df %>% dplyr::mutate(group = ifelse(kemas == 1, "KEMAS",
                                            ifelse(csos == 1, "CSOS", "None")))
  
  #' The CSOs trainers used the wrong trigger to start their training yesterday,
  #' and instead of using the one for testing, they used the one that was for
  #' registering parents and adding them to the "csos" group
  #' (so that their data can be easily identified).
  #' This means that users in "csos" group are currently not parents, but facilitators,
  #' so we need to ignore ALL data coming in through the "csos" group until 13 Jan
  #' (when CSOs start rolling out to parents)
  #' i.e. only consider users in the "csos" group for which created_on is >= 13 Jan.
  df <- df %>%
    dplyr::mutate(filter_row = ifelse((created_on < as.Date("2024-01-13")) &
                                        group == "CSOS", 1, 0))
  
  # check
  #df %>% group_by(group, filter_row) %>% summarise(min(created_on), max(created_on), n())
  df <- df %>% dplyr::filter(filter_row == 0)
  valid_ids <- df$id
  
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
}

df <- df %>% dplyr::filter(id != "c6c2a981-24a8-45cc-a8b9-3ac9a9f39a38")

valid_ids <- df$id
rm(contacts_unflat)

# ENGAGEMENT info FROM RUNS -------------------------------------------------------------
# module ID
if (country == "Malaysia_2"){
  malaysia_goals <- read_excel(path = "data/malaysia goal module ids.xlsx", sheet = "Copy of module_data")
  modules <- malaysia_goals$ID
} else {
  modules <- c("one_on_one_teen", "praise_teen", "talk_feelings_teen",
               "care_myself_teen", "mental_changes_teen", "social_changes_teen", "physical_changes_teen",
               "fun_learning_teen", "help_teen_learn_teen", "positive_learning_teen", "learning_mistakes_teen", "learning_with_people_teen",
               "routines_teen", "rules_teen", "online_safety_teen", "habits_online_teen",
               "budget_needs_teen", "budget_expenses_teen", "budget_monthly_teen", "budget_savings_teen",
               "kind_to_myself_teen", "stress_teen", "show_kindness_teen", "stress_signs_teen",
               "manage_stress_teen", "misbehave_teen", "solve_problems_teen", "consequences_teen", 
               "community_safety_teen", "self_defence_teen", "sex_violence_teen", "crises_teen",
               "ipv_equals", "ipv_supportive", "ipv_sharing", "ipv_conflicts", "ipv_listen")
}


mod_home_activity_checkin <- paste0("home_activity_checkin - ", modules)
# 
# get_command <- paste(rapidpro_site, call_type, valid_ids[1], sep = "")
# response <- httr::GET(get_command, config = httr::add_headers(Authorization = paste("Token", 
#                                                                                     token)))
# raw <- httr::content(response, as = "text")
# # results <- jsonlite::fromJSON(raw)

df$uuid <- df$id

#Connect to Database to get original data
if (country == "Malaysia_2"){
  source("config/personal_setup.R")
  faciNK_data <- postgresr::get_user_data(site = plh_con, filter = FALSE)
  #faciNK_data <- faciNK_data1
  names(faciNK_data) <- gsub(x = names(faciNK_data), pattern = "\\-", replacement = ".")  
  
  # from email on WC 8th April
  if ("rp.contact.field.user_name" %in% names(faciNK_data)){
    faciNK_data <- faciNK_data %>% mutate(rp.contact.field.user_name = ifelse(app_user_id == "506e80691d2c74f9", "Nurshuhada", rp.contact.field.user_name))
  }
  
  faciNK_data <- faciNK_data %>%
    filter(rp.contact.field.current_package %in% c("kemas_group_d", "kemas_group_a", "kemas_group_c", "kemas_group_b", "masw"))
  
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
    if (any(!is.na(match_id)) && any(match_id)){
      if ("754971" %in% parent_data_table_i$external_id){
        parent_data_table_i <- parent_data_table_i %>%
          mutate(external_id = ifelse(external_id == "754971", "754791", external_id))
      }
      facilitators[j] <- i
      my_list <- family_data[[i]]
      indices <- rep(seq_along(my_list), times = sapply(my_list, length))
      parent_data_table_i <- parent_data_table_i %>% mutate(kemas_group = group)
      parent_data_table_i$parent_group_no <- indices
      parent_data_table_i <- parent_data_table_i %>%
        mutate(duplicated_parent_group = as.integer(!duplicated(indices)))
      parent_data_table[[j]] <- parent_data_table_i
      j <- j+1
    }
  }
  names(parent_data_table) <- faciNK_data$app_user_id[facilitators]
  parent_data_table <- bind_rows(parent_data_table, .id = "facilitator")
  
  parent_data_table <- parent_data_table %>%
    mutate(kemas_group = ifelse(facilitator == "bd38993f-c783-4d0a-a7c0-a17ae806bce0", "masw", kemas_group))
  
  # ok so now we want to find people who are parent_group_no > 1 so that we can filter the children to the 
  # unique children in the analysis. 
  parent_data_table <- parent_data_table %>% dplyr::select(c(facilitator, parent_group_no, external_id, kemas_group, duplicated_parent_group))
  parent_data_table <- unique(parent_data_table)
  
  parent_data_table$kemas_group <- case_match(
    parent_data_table$kemas_group,
    "kemas_group_a" ~ 1,             # randomly generated
    "kemas_group_b" ~ 3,
    "kemas_group_c" ~ 4,
    "kemas_group_d" ~ 2,
    "masw" ~ 5
  )
  
  # which IDs are repeated in parent_data_table?
  
  # merge with df   #df_unjoin is a copy of df
  df <- left_join(df, parent_data_table, by = c("research_id" = "external_id"))
  
  # check
  if (nrow(df %>% filter(group == "CSOS") %>% filter(kemas_group != 5)) > 0) stop("Individuals who are given CSOS by the rapidpro data, but KEMAS by faciNK data")
  
  # TODO: we have five individuals who have KEMAS in the RapidPro data, but NA in the kemas_group. Talk to Durgesh about these. 
  # who is their facilitator?
  
  # TODO2: the ones flagged - are they meant to be KEMAS or CSOS?
  
  df <- df %>%
    #' CSOS in group 2 - correct to be group 5.
    #mutate(kemas_group = ifelse(group == "CSOS" & kemas_group != 5, 5, kemas_group)) %>%
    
    # replace missing "group" (group = "None") with KEMAS if they have "kemas" defined for them. 
    group_by(facilitator) %>%
    mutate(group = ifelse(!is.na(kemas_group), replace_none(group), group)) %>%
    
    #' Remove group 5 / masw entirely from kemas_group variable
    mutate(kemas_group = ifelse(group == "CSOS", NA, kemas_group)) %>%
    
    mutate(duplicated_parent_group = ifelse(is.na(duplicated_parent_group) | duplicated_parent_group == 1, 1, 2)) %>%
    # remove the ungrouped individuals.
    #' if someone is Unknown and their kemas_group is NA then is it safe to assume these are not individuals who are on the Malaysia Chatbot 2.0? Since we're now reading in all individuals who are in the KEMAS, CSOS, or no group.
    mutate(indicator = ifelse(group == "None" & is.na(facilitator), 1, 0)) %>%
    filter(indicator != 1) %>%
    dplyr::filter(is.na(facilitator) | facilitator != "5a270565-b7bf-4695-8e5e-012b7b2d001d") # this fac has 6 rows of data, all repeated with another fac id
}

df <- df %>% ungroup()

if (country == "Malaysia_2"){
  
  # todo: check if this works now:
  #get_flow_names2(by = "id_name", id_names = valid_ids)
  
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
  result_flow2 <- bind_rows(result_flow2) %>%
    filter(
      grepl("module -", flow$name) | 
        grepl("safeguarding_help", flow$name) | 
        grepl("pre_goal_checkin - ", flow$name) | 
        grepl("post_goal_checkin - ", flow$name)
    )
  
  # get archived data for these flows - we have Jan and Feb stored in a sheet already.
  # quicker to read this in. 
  archived_flow_data_monthly_1 <- readRDS("data/MY_PT2_archived_data_20240101_20240331.RDS")

  archived_flow_data_monthly <- get_archived_data(date_from = "2024-04-01", period = "monthly")
  archived_flow_data_monthly <- bind_rows(archived_flow_data_monthly)
  if (nrow(archived_flow_data_monthly) > 0){
    archived_flow_data_monthly <- archived_flow_data_monthly %>% filter(contact$uuid %in% valid_ids)
    archived_flow_data_monthly <- archived_flow_data_monthly %>%
      filter(
        grepl("module -", flow$name) | 
          grepl("safeguarding_help", flow$name) | 
          grepl("pre_goal_checkin - ", flow$name) | 
          grepl("post_goal_checkin - ", flow$name)
      )
  }
  archived_flow_data_monthly <- bind_rows(archived_flow_data_monthly_1, archived_flow_data_monthly)
  #saveRDS(archived_flow_data_monthly, "data/MY_PT2_archived_data_20240101_20240331.RDS")
  
  archived_flow_data_daily <- get_archived_data(date_from = "2024-04-01", period = "daily")
  archived_flow_data_daily <- bind_rows(archived_flow_data_daily)
  archived_flow_data_daily <- archived_flow_data_daily %>% filter(contact$uuid %in% valid_ids)
  if (nrow(archived_flow_data_daily) > 0){
    archived_flow_data_daily <- archived_flow_data_daily %>%
    filter(
      grepl("module -", flow$name) | 
        grepl("safeguarding_help", flow$name) | 
        grepl("pre_goal_checkin - ", flow$name) | 
        grepl("post_goal_checkin - ", flow$name)
    )
  }
  result_flow2 <- bind_rows(result_flow2, archived_flow_data_daily, archived_flow_data_monthly)
  result_flow2 <- result_flow2 %>% filter(contact$uuid %in% valid_ids)
  
  #result_flow21 <- flatten(result_flow2)
  #writexl::write_xlsx(result_flow21, path = "malaysia_flows_snapshot_29022024.xlsx")
  #saveRDS(result_flow2, path = "malaysia_flows_snapshot_29022024.rds")
  
  # For Module:
  result_flow <- result_flow2 %>% dplyr::filter(grepl("module -", flow$name))
  flow_module_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "will_complete")
  #flow_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "completed", flow_handle_type_sub  = "category")
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
} else {
  # If it is South Africa:
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
  result_flow2 <- bind_rows(result_flow2) %>%
    filter(
      grepl("module -", flow$name) | 
        grepl("home_activity_checkin -", flow$name) |
        grepl("safeguarding_help", flow$name) | 
        grepl("pre_goal_checkin - ", flow$name) | 
        grepl("post_goal_checkin - ", flow$name)
    )
  
  # archived data - have saved before Apr 1st in SA_PT2_archived_data_20240101_20240331.RDS
  archived_flow_data_monthly_1 <- readRDS("data/SA_PT2_archived_data_20240101_20240331.RDS")
  archived_flow_data_monthly <- get_archived_data(date_from = "2024-04-01", period = "monthly")
  archived_flow_data_monthly <- bind_rows(archived_flow_data_monthly)
  if (!is.null(archived_flow_data_monthly)){
    archived_flow_data_monthly_1 <- archived_flow_data_monthly_1 %>% filter(
      grepl("module -", flow$name) | 
        grepl("home_activity_checkin -", flow$name) |
        grepl("safeguarding_help", flow$name) | 
        grepl("pre_goal_checkin - ", flow$name) | 
        grepl("post_goal_checkin - ", flow$name)
    )
  }
  archived_flow_data_monthly <- rbind(archived_flow_data_monthly_1, archived_flow_data_monthly)
  archived_flow_data_monthly <- archived_flow_data_monthly  %>% filter(contact$uuid %in% valid_ids)
  #saveRDS(archived_flow_data_monthly, "data/SA_PT2_archived_data_20240101_20240331.RDS")
  
  archived_flow_data_daily <- get_archived_data(date_from = "2024-04-01", period = "daily")
  archived_flow_data_daily <- bind_rows(archived_flow_data_daily) %>%
    filter(
      grepl("module -", flow$name) | 
        grepl("home_activity_checkin -", flow$name) |
        grepl("safeguarding_help", flow$name) | 
        grepl("pre_goal_checkin - ", flow$name) | 
        grepl("post_goal_checkin - ", flow$name)
    )
  archived_flow_data_daily <- archived_flow_data_daily %>% filter(contact$uuid %in% valid_ids)
  
  result_flow2 <- bind_rows(result_flow2, archived_flow_data_daily, archived_flow_data_monthly)
  
  result_flow <- result_flow2 %>% dplyr::filter(grepl("home_activity_checkin -", flow$name))
  #flow_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "check_in_2")
  # values$completed$category
  flow_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "completed", flow_handle_type_sub  = "category")
  flow_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
  flow_checkin_data <- flow_checkin_data %>% dplyr::mutate(response = fct_recode(response, Yes = "yes", `Not yet` = "not yet"))
  flow_checkin_data <- flow_checkin_data %>% dplyr::mutate(response = fct_relevel(response, c("Yes", "Not yet", "No response")))
  
  # For Module:
  result_flow <- result_flow2 %>% dplyr::filter(grepl("module -", flow$name))
  flow_module_checkin_data <- flow_data_calculation(result_flow = result_flow, flow_type = "other", flow_handle_type = "will_complete")
  flow_module_checkin_data$ID <- sub(".* ", "", result_flow$flow$name)
  #flow_module_checkin_data$ID <- str_remove(flow_module_checkin_data$ID, "_yc")
  flow_module_checkin_data <- flow_module_checkin_data %>% dplyr::mutate(response = fct_recode(response, Yes = "yes", `No` = "no"))
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
  
  rm(result_flow2)
}

# Malaysia Transitions Code

if (country == "Malaysia_2"){
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
}


# 126 people and we don't know their group

# research ID is NA for a lot of these.

#df %>% group_by(facilitator, group) %>% summarise(n())

#df %>% filter(is.na(facilitator)) %>% filter(group == "KEMAS") %>% pull(id)

# 
# metadata <- Hmisc::contents(df1)
# plyr::ldply(metadata$Levels)

# for downloading data:
#valid_ids <- df$id

# Data to send --------------------

#df$n_goals_completed_f <- NULL
#df$perc_goals_completed_f <- NULL
#df$n_modules_completed_numeric <- NULL
#df$time_in_study_n <- NULL

# df$first_user_name <- contacts_unflat$fields$first_user_name
# df$family_name <- contacts_unflat$fields$family_name
# df$research_id
# writexl::write_xlsx(df, "parenttext_2_data_20231103.xlsx")
# 
# goal_transitions_table(data = checkin_data, "learning")
# 
# 
# ggplot(learning_df %>% filter(uuid %in% valid_ids) %>%
#          group_by(time_old, time_new, response_old, response_new) %>%
#          summarise(n_transitions = sum(n), .groups = 'drop')) +
#   geom_segment(aes(x = time_old, xend = time_new, y = response_old, yend = response_new, color = n_transitions), size = 1) + geom_point(aes(x = time_new, y = response_new, size = n_transitions, colour = n_transitions)) + ggthemes::scale_colour_gradient_tableau(palette = "Green-Gold") + theme_minimal() + labs(x = "Survey Asked", y = "Response", size = "Number of Transitions", colour = "Number of Transitions") + scale_x_discrete(limits = c("Pre", "Post"))

