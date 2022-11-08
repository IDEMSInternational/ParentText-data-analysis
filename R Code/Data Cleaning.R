library(httr)
library(jsonlite)
library(tidyverse)
#source("Functions.R")
#source("R Shiny Template.R")
# source("Code Book.R")
#install_github("lilyclements/rapidpror")
#library(rapidpror)

# RapidPro set up --------------------------------------------------------------
update_data <- function(country = "Malaysia", date_from = "2021-10-14", date_to = NULL, include_archived_data = FALSE, consent = TRUE) {
  
  set_rapidpro_site(site = site)
  set_rapidpro_key(key = key[[1]])
  set_rapidpro_uuid_names()
  
  # temp code to account for 
  if (!country %in% c("Malaysia", "Philippines", "Jamaica", "South Africa", "South_Africa")){
    contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL, date_to = "1970-01-01")
  } else {
    contacts_unflat <- get_user_data(flatten = FALSE, date_from = date_from, date_to = date_to)
  }

  created_on <- contacts_unflat$fields$starting_date
  did_not_consent <- contacts_unflat$fields$did_not_consent
  ID <- contacts_unflat$uuid
  last_online <- as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%d", tz = "UTC")
  
  enrolled <- NULL
  true_consent <- NULL
  program <- NULL
  gamification <- NULL
  personalisation <- NULL
  n_messages <- NULL
  
  if (length(contacts_unflat$groups) > 0){
    for (i in 1:length(contacts_unflat$groups)){
      contact_name <- contacts_unflat$groups[[i]]
      if (length(contact_name)==0) {
        enrolled[i] <- NA
        true_consent[i] <- NA
        program[i] <- NA
        gamification[i] <- NA
        personalisation[i] <- NA
        n_messages[i] <- NA
      } else{
        enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
        true_consent[i] <- ifelse(any(contact_name$name %in% "consent"), "Yes", "No")
        program[i] <- ifelse(any(contact_name$name %in% "in program"), "Yes", "No")
        gamification[i] <- ifelse(any(contact_name$name %in% "ABTest_test-gamification_Default"),
                                  "default",
                                  ifelse(any(contact_name$name %in% "ABTest_test-gamification_alternative"),
                                         "alternative",
                                         "none"))
        personalisation[i] <- ifelse(any(contact_name$name %in% "ABTest_test-personalisation_Default"),
                                     "default",
                                     ifelse(any(contact_name$name %in% "ABTest_test-personalisation_alternative"),
                                            "alternative",
                                            "none"))
        n_messages[i] <- ifelse(any(contact_name$name %in% "ABTest_test-n-messages-per-day_Default"),
                                "default",
                                ifelse(any(contact_name$name %in% "ABTest_test-n-messages-per-day_alternative"),
                                       "alternative",
                                       "none"))
      }
    }
  }
  group <- NULL
  if (country == "Jamaica"){
    for (i in 1:length(contacts_unflat$groups)){
      contact_name <- contacts_unflat$groups[[i]]
      if (length(contact_name)==0) {
        group[i] <- NA
      } else{
        group[i] <- ifelse(any(contact_name$name %in% "ParentText_IPV_WC_urban"),
                                  "ParentText_IPV_WC_urban",
                                  ifelse(any(contact_name$name %in% "ParentText_IPV_WC_rural"),
                                         "ParentText_IPV_WC_rural",
                                         ifelse(any(contact_name$name %in% "ParentText_IPV_school_CC"),
                                                "ParentText_IPV_school_CC",
                                                ifelse(any(contact_name$name %in% "ParentText_IPV_school_PB"),
                                                       "ParentText_IPV_school_PB", "none"))))
      }
    }
  }
  
  enrolled <- factor(enrolled)
  true_consent <- factor(true_consent)
  program <- factor(program)
  gamification <- factor(gamification)
  personalisation <- factor(personalisation)
  n_messages <- factor(n_messages)
  group <- factor(group)
  enrolled <- forcats::fct_expand(enrolled, c("Yes", "No"))
  true_consent <- forcats::fct_expand(true_consent, c("Yes", "No"))
  program <- forcats::fct_expand(program, c("Yes", "No"))
  enrolled <- forcats::fct_relevel(enrolled, c("Yes", "No"))
  true_consent <- forcats::fct_relevel(true_consent, c("Yes", "No"))
  program <- forcats::fct_relevel(program, c("Yes", "No"))
  
  language <- as.character(contacts_unflat$language)
  #language <- fct_expand(language, "Did not respond", language_setting)
  language <- forcats::fct_recode(language,
                                  MSA = "msa",
                                  ENG = "eng",
                                  FIL = "fil")
  language[is.na(language)] <- "Did not respond"
  language <- forcats::fct_relevel(language, c("ENG", "MSA", "FIL", "Did not respond"))
  df_consent <- data.frame(ID, created_on, program, enrolled, true_consent, language)
  if (country %in% c("South Africa", "South_Africa", "Jamaica")){
    ipv_version <- contacts_unflat$fields$ipv_version
    if (country == "Jamaica"){
      df_consent <- data.frame(df_consent, ipv_version, group)
    } else {
      df_consent <- data.frame(df_consent, ipv_version)
    }
  }
  df_consent <- df_consent %>%
    mutate(consent = ifelse(is.na(true_consent) &  is.na(language), "Did not interact",
                            ifelse(is.na(true_consent) & !is.na(language), "Did not respond",
                                   ifelse(true_consent == "Yes", "Yes", "No"))))
  df_consent$true_consent <- NULL
  true_consent <- NULL
  
  df_consent <- df_consent %>%
    mutate(consent = forcats::fct_relevel(as.character(consent), c("Yes", "No", "Did not interact", "Did not respond")))
  
  consent <- df_consent$consent
  if (length(contacts_unflat$groups) > 0){
    row <- 1:length(ID)
  } else {
    row <- NULL
    row <- factor(row)
  }
  df_created_on <- data.frame(ID, created_on, consent, program, row = row)
  if (country %in% c("South Africa", "South_Africa")){
    df_created_on <- data.frame(df_created_on, ipv_version)
  }
  if (country %in% c("Jamaica")){
    df_created_on <- data.frame(df_created_on, ipv_version, group)
  }
  if (!is.null(date_from)){
    df_created_on <- df_created_on %>%
      filter(created_on >= as.Date(date_from))
  }
  if (!is.null(date_to)){
    df_created_on <- df_created_on %>%
      filter(created_on >= as.Date(date_to))
  }
  list_of_ids <- df_created_on %>%
    filter(consent == "Yes")
  list_of_ids <- list_of_ids$ID
  
  # demographics -----------------------------------------------------------------------------
  state_of_origin <- as.character(contacts_unflat$fields$state_of_origin)
  state_of_origin <- dplyr::recode(state_of_origin, "1" = state_1, "2" = state_2, "3" = state_3, "4" = state_4, "5" = state_5,
                                   "6" = state_6, "7" = state_7, "8" = state_8, "9" = state_9, "10" = state_10, "11" = state_11,
                                   "12" = state_12, "13" = state_13, "14" = state_14, "15" = state_15, "16" = state_16, "17" = state_17)
  next_tip_main <- as.numeric(as.character(contacts_unflat$fields$next_tip_main))
  next_tip_morning <- as.numeric(as.character(contacts_unflat$fields$next_tip_morning))
  next_tip_evening <- as.numeric(as.character(contacts_unflat$fields$next_tip_evening))
  parent_gender <- contacts_unflat$fields$gender
  parent_gender <- factor(ifelse(parent_gender %in% c("female", "f", "Female", "woman", "Woman"), "Woman",
                                 ifelse(parent_gender %in% c("male", "m", "Male", "man", "Man"), "Man",
                                        ifelse(parent_gender %in% c("no", "B"), NA, parent_gender))))
  parent_gender <- fct_expand(parent_gender, "Woman", "Man")
  parent_gender <- forcats::fct_relevel(parent_gender, c("Woman", "Man"))
  
  child_age_group <- contacts_unflat$fields$age_group_for_tips
  know_age_group <- contacts_unflat$fields$know_age_group_for_tips
  child_age_group <- ifelse(child_age_group == "child" & know_age_group == "no", "Default", child_age_group)
  child_age_group <- factor(child_age_group)
  child_age_group <- fct_expand(child_age_group, "Baby", "Child", "Teen", "Default")
  child_age_group <- forcats::fct_recode(child_age_group,
                                         Baby = "baby",
                                         Child = "child",
                                         Teen = "teen")
  child_age_group <- forcats::fct_relevel(child_age_group, c("Baby", "Child", "Teen", "Default"))
  
  child_gender <- factor(contacts_unflat$fields$survey_behave_sex)
  child_gender <- fct_expand(child_gender, "Girl", "Boy", "Prefer not to say")
  child_gender <-  forcats::fct_recode(child_gender,
                                       Boy = "male",
                                       Girl = "female", 
                                       `Prefer not to say` = "no")
  child_gender <- forcats::fct_relevel(child_gender, c("Girl", "Boy", "Prefer not to say"))
  
  parent_child_relationship <- factor(contacts_unflat$fields$survey_behave_relationship)
  parent_child_relationship <- fct_expand(parent_child_relationship, "Parent", "Grandparent", "Aunt/Uncle", "Foster Parent", "Other", "Prefer not to say")
  parent_child_relationship <- forcats::fct_recode(parent_child_relationship,
                                                   Parent = "parent",
                                                   Grandparent = "grandparent",
                                                   `Aunt/Uncle`= "uncle",
                                                   `Foster Parent` = "foster",
                                                   Other = "other",
                                                   `Prefer not to say`  = "no")
  parent_child_relationship <- forcats::fct_relevel(parent_child_relationship,
                                                    c("Parent", "Grandparent", "Aunt/Uncle", "Foster Parent", "Other", "Prefer not to say"))
  
  
  parent_relationship <- factor(contacts_unflat$fields$marital_status)
  parent_relationship <- forcats::fct_recode(parent_relationship,
                                             `Prefer not to say`  = "no")
  parent_relationship <- fct_expand(parent_relationship, "Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say")
  parent_relationship <- forcats::fct_relevel(parent_relationship, c("Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say"))
  
  child_disabilities <- factor(contacts_unflat$fields$has_disability)
  child_disabilities <- fct_expand(child_disabilities, "Yes", "No")
  child_disabilities <- forcats::fct_recode(child_disabilities,
                                            Yes = "yes",
                                            No = "no",
                                            `supp_disab`= "supp_disab")
  child_disabilities <- forcats::fct_relevel(child_disabilities,
                                             c("Yes", "No", "supp_disab"))
  
  
  recruitment_channel <- factor(contacts_unflat$fields$enrollment)
  recruitment_channel <- forcats::fct_expand(recruitment_channel, enrollment_variables)
  recruitment_channel <- forcats::fct_recode(recruitment_channel, !!!enrollment_recode)
  recruitment_channel <- forcats::fct_relevel(recruitment_channel, enrollment_order)
  
  parenting_goal <- factor(as.numeric(contacts_unflat$fields$parenting_goal))
  parenting_goal <- forcats::fct_expand(parenting_goal, c("Relationship","Behaviour", "School", "COVID-19", "Stress", "Finances", "Family conflict", "Safety", "Disabilities", "Other"))
  parenting_goal <- forcats::fct_recode(parenting_goal,
                                        `Relationship` = "1",
                                        `Behaviour` = "2",
                                        `School` = "3",
                                        `COVID-19` = "4",
                                        `Stress` = "5",
                                        `Finances` = "6",
                                        `Family conflict` = "7",
                                        `Safety`= "8",
                                        `Disabilities` = "9",
                                        `Other` = "0")
  parenting_goal <- forcats::fct_relevel(parenting_goal,
                                         c("Relationship","Behaviour",
                                           "School", "COVID-19",
                                           "Stress", "Finances",
                                           "Family conflict", "Safety",
                                           "Disabilities", "Other"))
  parenting_goal_wrap <- str_wrap_factor(parenting_goal, width = 15)
  
  # active users # N = contacts for which the time difference between the current time and the datetime variable "last_seen_on" is less than 24 h 
  active_users <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 24
  active_users <- factor(active_users)
  if (length(levels(active_users)) == 1){
    if (levels(active_users) == "FALSE"){
      levels(active_users) <- c(levels(active_users),"TRUE")
    } else if (levels(active_users) == "TRUE"){
      levels(active_users) <- c(levels(active_users),"FALSE")
    }
  }
  active_users <- forcats::fct_expand(active_users, c("Yes", "No"))
  active_users <- forcats::fct_recode(active_users,
                                      "No" = "FALSE",
                                      "Yes" = "TRUE")
  active_users <- forcats::fct_relevel(active_users, c("Yes", "No"))
  
  active_users_7_days <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 7*24
  active_users_7_days <- factor(active_users_7_days)
  if (length(levels(active_users_7_days)) == 1){
    if (levels(active_users_7_days) == "FALSE"){
      levels(active_users_7_days) <- c(levels(active_users_7_days),"TRUE")
    } else if (levels(active_users_7_days) == "TRUE"){
      levels(active_users_7_days) <- c(levels(active_users_7_days),"FALSE")
    }
  }
  active_users_7_days <- forcats::fct_expand(active_users_7_days, c("Yes", "No"))
  active_users_7_days <- forcats::fct_recode(active_users_7_days,
                                             "No" = "FALSE",
                                             "Yes" = "TRUE")
  active_users_7_days <- forcats::fct_relevel(active_users_7_days, c("Yes", "No"))
  
  # comp_prog_overall
  # TODO: only should be lookign at this for those who consented
  comp_prog_overall <- as.numeric(as.character(contacts_unflat$fields$n_skills))
  
  # Participant age etc
  # TODO: only should be lookign at this for those who consented
  parent_age <- as.numeric(as.character(contacts_unflat$fields$age))
  
  # completed surveys
  completed_welcome <- factor(contacts_unflat$fields$completed_welcome)
  completed_welcome <- forcats::fct_expand(completed_welcome, c("Yes", "No"))
  completed_welcome <- forcats::fct_recode(completed_welcome,
                                           "No" = "no",
                                           "Yes" = "yes")
  completed_welcome[is.na(completed_welcome)] <- "No"
  completed_welcome <- forcats::fct_relevel(completed_welcome, c("Yes", "No"))
  
  comp_survey_w1 <- str_count(contacts_unflat$fields$surveyparenting_completion, fixed("|"))
  if (length(comp_survey_w1) == 0){comp_survey_w1 <- rep(NA, length(enrolled))}
  
  comp_survey_w2 <- str_count(contacts_unflat$fields$surveyparentingbehave_completion, fixed("|")) + 1
  if (length(comp_survey_w2) == 0){comp_survey_w2 <- rep(NA, length(enrolled))}
  
  consent_survey_w1 <- c(data.frame(str_split(contacts_unflat$fields$surveyparenting_datestamps, ",", n = 2, simplify = TRUE))[1])$X1
  if (length(consent_survey_w1) == 0){consent_survey_w1 <- rep(NA, length(enrolled))}
  consent_survey_w1 <- factor(consent_survey_w1)
  consent_survey_w1 <- forcats::fct_expand(consent_survey_w1, c("Yes", "No"))
  consent_survey_w1 <- forcats::fct_recode(consent_survey_w1,
                                           "No" = "no",
                                           "Yes" = "yes",
                                           `NA` = "goback")
  #consent_survey_w1[is.na(consent_survey_w1)] <- "Did not interact"
  
  #consent_survey_w1[is.na(consent_survey_w1)] <- "Did not interact"
  consent_survey_w1 <- forcats::fct_relevel(consent_survey_w1, c("Yes", "No", "Did not interact"))
  
  survey_consented_wk2_plus <- str_split(contacts_unflat$fields$surveyparentingbehave_datestamps, fixed("|"))
  all_split_data <- NULL
  if (length(survey_consented_wk2_plus) > 0){
    for (j in 1:length(survey_consented_wk2_plus)){
      if (is.na(survey_consented_wk2_plus[[j]])){
        split_data <- data.frame(V1 = NA, V2 = NA, V3 = NA, row = j)
      } else {
        split_parenting_2 <- stringr::str_split(survey_consented_wk2_plus[[j]], ",")
        split_data <- plyr::ldply(split_parenting_2[1:(length(split_parenting_2)-1)])
        split_data <- split_data %>% mutate(row = j)
      }
      all_split_data[[j]] <- split_data
    }
  }
  
  survey_consented_wk2_plus_data <- plyr::ldply(all_split_data)
  survey_consented_wk2_plus_data$survey_response <- as.numeric(as.factor(survey_consented_wk2_plus_data$V1))
  survey_consented_wk2_plus_data$survey_number <- survey_consented_wk2_plus_data$V2
  survey_consented_wk2_plus_data$V1 <- NULL
  survey_consented_wk2_plus_data$V2 <- NULL
  survey_consented_wk2_plus_data$V3 <- NULL
  if (nrow(survey_consented_wk2_plus_data) > 0){
    survey_consented_wk2_plus_data <- merge(survey_consented_wk2_plus_data, df_created_on, by = "row") %>% arrange(row)
    survey_consented_wk2_plus_data_wider <- pivot_wider(survey_consented_wk2_plus_data, id_cols = ID, names_from = survey_number,
                                                        values_from = survey_response, names_prefix = "consent_survey_w",
                                                        values_fn = mean) # there are several responses for some. So find mean. If they ever consented they we say they did consent
    survey_consented_wk2_plus_data_wider <- survey_consented_wk2_plus_data_wider %>%
      mutate_at(vars(starts_with("consent_survey_")), round) %>%
      mutate_at(vars(starts_with("consent_survey_")), as.factor)
    survey_consented_wk2_plus_data_wider <- survey_consented_wk2_plus_data_wider %>%
      mutate_at(vars(starts_with("consent_survey_")), ~forcats::fct_recode(.,
                                                                           "No" = "1",
                                                                           "Yes" = "2"))
  } else {
    survey_consented_wk2_plus_data_wider <- NULL
  }
  
  challenge_behav <- contacts_unflat$fields$survey_behave_most_challenging
  challenge_behav <- dplyr::case_when(
    child_age_group == "Baby" & challenge_behav == "1" ~ "Crying",
    child_age_group == "Baby" & challenge_behav == "2" ~ "Problems sleeping",
    child_age_group == "Baby" & challenge_behav == "3" ~ "Acting clingy",
    child_age_group == "Baby" & challenge_behav == "4" ~ "Whining",
    child_age_group == "Baby" & challenge_behav == "5" ~ "Bad tempered",
    child_age_group == "Baby" & challenge_behav == "6" ~ "Problems eating",
    child_age_group == "Baby" & challenge_behav == "7" ~ "Stubborn/fussy",
    child_age_group == "Baby" & challenge_behav == "8" ~ "Naughty behaviour",
    child_age_group == "Baby" & challenge_behav == "9" ~ "Temper Tantrums",
    child_age_group %in% c("Child", "Default", "Teen") & challenge_behav == "1" ~ "Refuses to obey",
    child_age_group %in% c("Child", "Default") & challenge_behav == "2" ~ "Gets angry",
    child_age_group %in% c("Child", "Default", "Teen") & challenge_behav == "3" ~ "Rude behaviour",
    child_age_group %in% c("Child", "Default") & challenge_behav == "4" ~ "Mood swings",
    child_age_group %in% c("Child", "Default") & challenge_behav == "5" ~ "Does not follow rules",
    child_age_group %in% c("Child", "Default") & challenge_behav == "6" ~ "Stubbornness",
    child_age_group %in% c("Child", "Default" ~ "Teen") & challenge_behav == "7" ~ "Breaks things",
    child_age_group %in% c("Child", "Default" ~ "Teen") & challenge_behav == "8" ~ "Gets into fights",
    child_age_group %in% c("Child", "Default" ~ "Teen") & challenge_behav == "9" ~ "Teases others",
    child_age_group %in% c("Teen") & challenge_behav == "2" ~ "Temper Tantrums",
    child_age_group %in% c("Teen") & challenge_behav == "4" ~ "Whining",
    child_age_group %in% c("Teen") & challenge_behav == "5" ~ "Hyperactivity",
    child_age_group %in% c("Teen") & challenge_behav == "6" ~ "Hits others")
  
  challenge_behav <- forcats::fct_expand(challenge_behav, c("Crying", "Problems sleeping", "Acting clingy", "Whining", "Bad tempered", "Problems eating", "Stubborn/fussy", "Naughty behaviour", "Temper Tantrums", "Refuses to obey", "Gets angry", "Rude behaviour", "Mood swings", "Does not follow rules", "Stubbornness", "Breaks things", "Gets into fights", "Teases others", "Hyperactivity", "Hits others"))
  challenge_behav <- forcats::fct_relevel(challenge_behav, c("Crying", "Problems sleeping", "Acting clingy", "Whining", "Bad tempered", "Problems eating", "Stubborn/fussy", "Naughty behaviour", "Temper Tantrums", "Refuses to obey", "Gets angry", "Rude behaviour", "Mood swings", "Does not follow rules", "Stubbornness", "Breaks things", "Gets into fights", "Teases others", "Hyperactivity", "Hits others"))
  challenge_behav_wrap <- str_wrap_factor(challenge_behav, width = 10)
  
  df <- data.frame(ID, created_on, last_online, enrolled, consent, program, language, parent_gender, child_gender, child_age_group, parent_child_relationship,
                   state_of_origin, 
                   parent_relationship, child_disabilities, recruitment_channel, parenting_goal, parenting_goal_wrap,
                   active_users, active_users_7_days, comp_prog_overall, next_tip_main, next_tip_morning, next_tip_evening, parent_age, completed_welcome, comp_survey_w1, comp_survey_w2, consent_survey_w1,
                   challenge_behav, challenge_behav_wrap,
                   gamification, personalisation, n_messages)
  
  if (country %in% c("South Africa", "South_Africa")){
    df <- data.frame(df, ipv_version)
  }
  if (country %in% c("Jamaica")){
    df <- data.frame(df, ipv_version, group)
  }
  
  df <- df %>%
    mutate(length_in_programme = as.numeric(as.Date(last_online) - as.Date(created_on)) + 1)
  
  df <- df %>% 
    mutate(not_active_7_days = ifelse(active_users_7_days == "No",
                                      "Yes",
                                      "No"))
  if (nrow(df) > 0){
    df <- df %>% mutate(order = 1:nrow(df))
  }
  
  if (length(survey_consented_wk2_plus_data_wider) > 0){
    df <- merge(df, survey_consented_wk2_plus_data_wider, by = "ID") %>% arrange(order)
  }
  #df <- df %>%
  #  mutate(across(starts_with("consent_survey_"), ~replace_na(., 0)))
  
  df <- df %>%
    mutate(parent_child_relationship_2 = ifelse(is.na(parent_child_relationship), NA, paste(parent_child_relationship, parent_gender, sep = "_")))
  
  df <- df %>%
    mutate(parent_child_relationship_2 = plyr::revalue(x=as.character(parent_child_relationship_2), 
                                                       replace=c(`Parent_Woman` =  "Mother", `Grandparent_Woman` = "Grandmother", `Aunt/Uncle_Woman` = "Aunt", `Foster Parent_Woman` = "Foster Mother",
                                                                 `Other_Woman` = "Other (F)", `Prefer not to say_Woman` = "Prefer not to say (F)",
                                                                 `Parent_Man` =  "Father", `Grandparent_Man` = "Grandfather", `Aunt/Uncle_Man` = "Uncle", `Foster Parent_Man` = "Foster Father",
                                                                 `Other_Man` = "Other (M)", `Prefer not to say_Man` = "Prefer not to say (M)",
                                                                 `Parent_NA` =  "Parent", `Grandparent_NA` = "Grandparent", `Aunt/Uncle_NA` = "Aunt/Uncle", `Foster Parent_NA` = "Foster Parent",
                                                                 `Other_NA` = "Other (unknown)", `Prefer not to say_NA` = "Prefer not to say (unknown)")))
  # Hook messages
  hook_messages <- contacts_unflat$fields$hook_message
  hook_data_all <- NULL
  if (length(hook_messages) > 0){
    for (i in 1:length(hook_messages)){
      hook_messages_all <- hook_messages[[i]]
      ID_hook_all <- ID[i]
      hook_data_all[[i]] <- data.frame(hook_message_time_all = hook_messages_all, ID = ID_hook_all)
    }
    hook_message_all <- data.frame(plyr::ldply(hook_data_all))
    hook_message_all <- hook_message_all %>%
      mutate(hook_message_time_all = ifelse(stringi::stri_sub(hook_message_time_all,-1) == "|",
                                            NA,
                                            hook_message_time_all))
    hook_message_all$hook_message_time_all <- as.POSIXct(gsub(".*,","",hook_message_all$hook_message_time_all), format="%Y-%m-%dT%H:%M:%OS", tz = "EST") - lubridate::hm("6, 0")
    df <- dplyr::left_join(df, hook_message_all)
    df$created_on <- as.POSIXct(gsub(".*,","",df$created_on), format="%Y-%m-%dT%H:%M:%OS", tz = "EST") - lubridate::hm("6, 0")
    df$time_in_study <- df$hook_message_time_all - df$created_on
    df <- df %>%
      mutate(cens = ifelse(is.na(time_in_study),
                           0,
                           1),
             time_in_study = ifelse(is.na(time_in_study),
                                    difftime(Sys.time(), created_on, units = "hours"),
                                    time_in_study))
  }

  if (length(list_of_ids) > 0){
    if (consent){
      df <- df %>%
        filter(ID %in% list_of_ids) 
    }
  }

  df_consent <- data.frame(df_consent, parent_gender, child_gender, child_age_group)
  
  # for Jamaica Only: Parent Pals data cleaning --------------------
  if (country == "Jamaica"){
    women_centre <- contacts_unflat$fields$women_centre
    womens_centre_location <- as.character(contacts_unflat$fields$women_centre_location)
    womens_centre_location <- forcats::fct_expand(womens_centre_location, c("Kingston Centre", "Spanish Town Centre", "Denbigh Centre", "Mandeville Centre",
                                                                            "Junction Outreach", "Savanna-La-Mar Centre", "Montego Bay Centre", "St. Ann’s Bay Centre", "Port Antonio Centre", "Morant Bay Centre",
                                                                            "St. Margaret’s Outreach", "Jones Town Outreach", "Kellits Outreach", "Santa Cruz Outreach", "Lucea Outreach", "Duncan’s Outreach", "High Gate Outreach", "Ewarton Outreach"))
    womens_centre_location <- dplyr::recode(womens_centre_location, "1" = "Kingston Centre", "2" = "Spanish Town Centre", "3" = "Denbigh Centre", "4" = "Mandeville Centre",
                                            "5" = "Junction Outreach", "6" = "Savanna-La-Mar Centre", "7" = "Montego Bay Centre", "8" = "St. Ann’s Bay Centre",
                                            "9" = "Port Antonio Centre", "10" = "Morant Bay Centre", "11" = "St. Margaret’s Outreach", "12" = "Jones Town Outreach",
                                            "13" = "Kellits Outreach", "14" = "Santa Cruz Outreach", "15" = "Lucea Outreach", "16" = "Duncan’s Outreach",
                                            "17" = "High Gate Outreach", "18" = "Ewarton Outreach")
    womens_centre_data <- data.frame(ID, created_on, womens_centre_location, women_centre) %>%
      dplyr::filter(women_centre == "yes") %>%
      dplyr::filter(ID %in% list_of_ids)
  } else {
    womens_centre_data <- 1
  }
  #  pp_n_recruited <- df %>% group_by(child_age_group) %>% # group_by will be "recruited by" in time
  #    mutate(completed_welcome = ifelse(completed_welcome == "Yes", 1, 0)) %>% # reorder welcome survey
  #    summarise(`Number recruited` = n(),
  #              `Toolkit skills` = sum(comp_prog_overall, na.rm = TRUE),
  #              `Welcome survey completed` = sum(completed_welcome, na.rm = TRUE),
  #              `Week1 survey completed` = sum((consent_survey_w1 == "Yes"), na.rm = TRUE))
  #  pp_n_consent <- df %>% group_by(child_age_group, .drop = FALSE) %>% # group_by will be "recruited by" in time
  #    filter(consent == "Yes") %>%
  #    summarise(`Number consented` = n())
  #  pp_data_frame <- merge(pp_n_recruited, pp_n_consent)
  #  pp_data_frame <- pp_data_frame %>%
  #    mutate(`Recruited by` = 1:nrow(.)) %>%
  #    dplyr::select(`Recruited by`, `Number recruited`, `Number consented`, `Welcome survey completed`, `Week1 survey completed`,
  #                  `Toolkit skills`) %>%
  #    mutate(Total = `Number recruited` + `Number consented` + `Welcome survey completed` + `Week1 survey completed` + `Toolkit skills`) %>%
  #    arrange(desc(Total))
  #  
  # flow level data --------------------------------
  # sum of response to content, calm, check in, supportive, praise messages
  supportive_flow_names <- c("PLH - Supportive - Family", "PLH - Supportive - Help reminder", "PLH - Supportive - Share", "PLH - Supportive - Share - Enrollment",
                             paste0(prefix, " - PLH - Supportive - Share - Enrollment"), "PLH - Supportive - Budget", "PLH - Supportive - Activities for babies", "PLH - Supportive - Activities",
                             "PLH - Supportive - Behave reminder", "PLH - Supportive - Children reminder", "PLH - Supportive - Covid", "PLH - Supportive - Development",
                             "PLH - Supportive - Disabilities")
  supportive_calm <- "PLH - Supportive - Calm"
  supportive_praise <- "PLH - Supportive - Praise"
  supportive_activities <- "PLH - Supportive - Activities"
  check_in_flow_names <- c("PLH - Content - Extra - CheckIn - COVID", "PLH - Content - Positive - CheckIn - Book sharing", "PLH - Content - Positive - CheckIn - Budget adults", "PLH - Content - Positive - CheckIn - Budget with children", "PLH - Content - Positive - CheckIn - Community safety", "PLH - Content - Positive - CheckIn - Consequences", "PLH - Content - Positive - CheckIn - Crisis", "PLH - Content - Positive - CheckIn - Crying", "PLH - Content - Positive - CheckIn - Education", "PLH - Content - Positive - CheckIn - Emotion", "PLH - Content - Positive - CheckIn - Family", "PLH - Content - Positive - CheckIn - Ignore",
                           #"PLH - Content - Positive - CheckIn - Instructions",
                           "PLH - Content - Positive - CheckIn - IPV 1", "PLH - Content - Positive - CheckIn - IPV 2", "PLH - Content - Positive - CheckIn - IPV 3", "PLH - Content - Positive - CheckIn - IPV 4", "PLH - Content - Positive - CheckIn - IPV 5", "PLH - Content - Positive - CheckIn - Online adults", "PLH - Content - Positive - CheckIn - Online children", "PLH - Content - Positive - CheckIn - Praise", "PLH - Content - Positive - CheckIn - ProblemSolving", "PLH - Content - Positive - CheckIn - Redirect", "PLH - Content - Positive - CheckIn - Routines", "PLH - Content - Positive - CheckIn - Rules", "PLH - Content - Positive - CheckIn - Safe or unsafe touch", "PLH - Content - Relax - CheckIn - Anger management", "PLH - Content - Relax - CheckIn - List of things",
                           "PLH - Content - Relax - CheckIn - Loving Kindness", "PLH - Content - Relax - CheckIn - Notice how you feel", "PLH - Content - Relax - CheckIn - Three is a magical number", "PLH - Content - Time - CheckIn - One on one time")
  content_tip_flow_names <- c("PLH - Content - Positive - Behave - Consequences - Timed intro", "PLH - Content - Positive - Behave - Crisis - Timed intro", "PLH - Content - Positive - Behave - Crying - Timed intro", "PLH - Content - Positive - Behave - Emotion - Timed intro", "PLH - Content - Positive - Behave - Ignore - Timed intro", "PLH - Content - Positive - Behave - Praise - Timed intro", "PLH - Content - Positive - Behave - ProblemSolving - Timed intro", "PLH - Content - Positive - Behave - Redirect - Timed intro", "PLH - Content - Positive - Behave - Routines - Timed intro",
                              "PLH - Content - Positive - Book sharing - Timed intro", "PLH - Content - Positive - Budget adults - Timed intro", "PLH - Content - Positive - Budget with children - Timed intro","PLH - Content - Positive - Education - Timed intro",
                              "PLH - Content - Positive - Family - Timed intro", "PLH - Content - Positive - Online adults - Timed intro", "PLH - Content - Positive - Online children - Timed intro", "PLH - Content - Positive - Rules - Timed intro",
                              "PLH - Content - Positive - Safe or unsafe touch - Timed intro", "PLH - Content - Relax - Take a pause - Timed intro", "PLH - Content - Relax - Exercise", "PLH - Content - Time - One on one time baby - Timed intro", 
                              "PLH - Content - Extra - COVID", "PLH - Content - Extra - Disability", "PLH - Content - Positive - Family", 
                              "PLH - Content - Time - One on one time child - Timed intro", "PLH - Content - Time - One on one time teen - Timed intro", "PLH - Content - Positive - introduction", "PLH - Content - Positive - Positive instructions", "PLH - Content - Relax - Quick Pause", "PLH - Content - Relax - Anger management", "PLH - Content - Relax - Anger management 2", "PLH - Content - Positive - IPV", "PLH - Content - Positive - Community safety")
  df_created_on$row <- NULL
  if (country %in% c("Malaysia", "Philippines") && include_archived_data){
    archived_data <- readRDS(file = paste0(country, "_archived.RDS"))
    archived_data <- update_archived_data(curr_data = archived_data,
                                          date_to = date_to)}

  if (!country %in% c("Malaysia", "Philippines", "Jamaica", "South Africa", "South_Africa")){
    supportive_praise_flow <- get_flow_data(flow_name = supportive_praise, flow_type = "praise", include_archived_data = include_archived_data, date_to = "1970-01-01")
    supportive_calm_flow <- get_flow_data(flow_name = supportive_calm, flow_type = "calm", include_archived_data = include_archived_data, date_to = "1970-01-01")
    supportive_activities_flow <- get_flow_data(flow_name = supportive_activities, include_archived_data = include_archived_data, date_to = "1970-01-01")
    supportive_flow_names_flow <- get_flow_data(flow_name = supportive_flow_names, include_archived_data = include_archived_data, date_to = "1970-01-01")
    check_in_flow_names_flow <- get_flow_data(flow_name = check_in_flow_names, flow_type = "check_in", include_archived_data = include_archived_data, date_to = "1970-01-01")
    content_tip_flow_names_flow <- get_flow_data(flow_name = content_tip_flow_names, flow_type = "tips", include_archived_data = include_archived_data, date_to = "1970-01-01")
  } else {
    supportive_praise_flow <- get_flow_data(flow_name = supportive_praise, flow_type = "praise", include_archived_data = include_archived_data, date_from = date_from, date_to = date_to)
    supportive_calm_flow <- get_flow_data(flow_name = supportive_calm, flow_type = "calm", include_archived_data = include_archived_data, date_from = date_from, date_to = date_to)
    supportive_activities_flow <- get_flow_data(flow_name = supportive_activities, include_archived_data = include_archived_data, date_from = date_from, date_to = date_to)
    supportive_flow_names_flow <- get_flow_data(flow_name = supportive_flow_names, include_archived_data = include_archived_data, date_from = date_from, date_to = date_to)
    check_in_flow_names_flow <- get_flow_data(flow_name = check_in_flow_names, flow_type = "check_in", include_archived_data = include_archived_data, date_from = date_from, date_to = date_to)
    content_tip_flow_names_flow <- get_flow_data(flow_name = content_tip_flow_names, flow_type = "tips", include_archived_data = include_archived_data, date_from = date_from, date_to = date_to)
  }
  supportive_praise_flow$ID <- supportive_praise_flow$uuid
  supportive_praise_flow$uuid <- NULL
  supportive_praise_flow <- supportive_praise_flow %>% mutate(Flow = "Supportive Praise")
  supportive_praise_flow$response <- replace_na(supportive_praise_flow$response, "No Response")
  
  supportive_calm_flow$ID <- supportive_calm_flow$uuid
  supportive_calm_flow$uuid <- NULL
  supportive_calm_flow <- supportive_calm_flow %>% mutate(Flow = "Supportive Calm")
  
  supportive_activities_flow$ID <- supportive_activities_flow$uuid
  supportive_activities_flow$uuid <- NULL
  supportive_activities_flow <- supportive_activities_flow %>% mutate(Flow = "Supportive Activities")
  
  supportive_flow_names_flow$ID <- supportive_flow_names_flow$uuid
  supportive_flow_names_flow$uuid <- NULL
  supportive_flow_names_flow <- supportive_flow_names_flow %>% mutate(Flow = "Supportive Other")
  
  check_in_flow_names_flow$ID <- check_in_flow_names_flow$uuid
  check_in_flow_names_flow$uuid <- NULL
  if (nrow(check_in_flow_names_flow) > 0){
    check_in_flow_names_flow <- check_in_flow_names_flow %>% mutate(response = ifelse(response == "1", "Surprised",
                                                                                      ifelse(response == "2", "Happy",
                                                                                             ifelse(response == "3", "My child did not like it",
                                                                                                    ifelse(response == 4, "I don't know", 
                                                                                                           response)))))
    check_in_flow_names_flow <- check_in_flow_names_flow %>% mutate(response = ifelse(response == "neutral", "Neutral", response))
  }
  check_in_flow_names_flow <- check_in_flow_names_flow %>% mutate(Flow = "Check in")
  check_in_flow_names_flow$response <- replace_na(check_in_flow_names_flow$response, "No Response")
  check_in_flow_names_flow$managed_to_do_something <- replace_na(check_in_flow_names_flow$managed_to_do_something, "No Response")
  
  content_tip_flow_names_flow$ID <- content_tip_flow_names_flow$uuid
  content_tip_flow_names_flow$uuid <- NULL
  content_tip_flow_names_flow <- content_tip_flow_names_flow %>% mutate(Flow = "Content Tip")
  content_tip_flow_names_flow$category <- replace_na(content_tip_flow_names_flow$category, "No Response")
  
  all_flows <- dplyr::bind_rows(supportive_praise_flow, supportive_calm_flow, supportive_flow_names_flow,
                                check_in_flow_names_flow, content_tip_flow_names_flow)
  
  if (length(all_flows) == 1){
    all_flows <- dplyr::full_join(all_flows, df_created_on, by = character())
  } else {
    all_flows <- dplyr::full_join(all_flows, df_created_on)
  }
  
  # Survey Level Data ---------------------------------------------------------------------------------------------------------------------------
  play <- get_survey_data(contacts_unflat$fields$surveytime_datetime) %>% mutate(Group = "Play")
  praise <- get_survey_data(contacts_unflat$fields$surveypraise_datetime) %>% mutate(Group = "Praise")
  stress <- get_survey_data(contacts_unflat$fields$surveystress_datetime) %>% mutate(Group = "Stress")
  physical_abuse <- get_survey_data(contacts_unflat$fields$surveydiscipline_datetime) %>% mutate(Group = "Physical abuse")
  food_insecurity <- get_survey_data(contacts_unflat$fields$surveymoneymonth_datetime) %>% mutate(Group = "Food insecurity")
  psychological_abuse <- get_survey_data(contacts_unflat$fields$surveyshout_datetime) %>% mutate(Group = "Psychological abuse")
  financial_stress <- get_survey_data(contacts_unflat$fields$surveymoneyweek_datetime) %>% mutate(Group = "Financial stress")
  parenting_efficacy <- get_survey_data(contacts_unflat$fields$surveypositive_datetime) %>% mutate(Group = "Parenting efficacy")
  sex_prevention <- get_survey_data(contacts_unflat$fields$surveysexualabuse_datetime) %>% mutate(Group = "Sexual abuse prevention")
  child_behave <- get_survey_data(contacts_unflat$fields$surveybehave_rate_datetime) %>% mutate(Group = "Child Behaviour")
  # using datetime not just _rate because in _rate it doesn't state which survey the score is corresponding to
  # e.g. see contacts_unflat$fields$surveybehave_rate_datetime[[1]]
  
  pos_par <- merge(play, praise) #, by = c("row", "week"))
  child_mal <- merge(physical_abuse, psychological_abuse)#, by = c("row", "week"))
  positive_parenting <- pos_par
  child_maltreatment <- child_mal
  if (nrow(pos_par) > 0){
    positive_parenting <- pos_par %>%
      mutate(Group = "Positive parenting") %>%
      mutate(dt = NA) %>%
      mutate(vals = ifelse(is.na(vals.x), vals.y,
                           ifelse(is.na(vals.y), vals.x,
                                  vals.x + vals.y))) %>%
      dplyr::select(c(vals, row, week, dt, Group))
    
  }
  if (nrow(child_mal) > 0){
    child_maltreatment <- child_mal %>%
      mutate(Group = "Child maltreatment") %>%
      mutate(dt = NA) %>%
      mutate(vals = ifelse(is.na(vals.x), vals.y,
                           ifelse(is.na(vals.y), vals.x,
                                  vals.x + vals.y))) %>%
      dplyr::select(c(vals, row, week, dt, Group))
  }
  
  parenting_survey <- rbind(positive_parenting, child_maltreatment, play, praise, stress, physical_abuse, psychological_abuse, financial_stress, food_insecurity, parenting_efficacy, sex_prevention, child_behave)
  parenting_survey <- parenting_survey %>% mutate(week = fct_relevel(as.character(week), c("Baseline", "2", "3", "4", "5", "6", "7", "8", "9")))
  parenting_survey <- parenting_survey %>% mutate(Group = fct_expand(Group, c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse", "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")))
  parenting_survey <- parenting_survey %>% mutate(Group = fct_relevel(Group, c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse", "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")))
  
  contacts_unflat_ID_merge <- df_created_on %>% mutate(row = row)
  parenting_survey <- merge(parenting_survey, contacts_unflat_ID_merge) %>% arrange(row)
  parenting_survey <- parenting_survey %>% filter(consent == "Yes") #%>% filter(program == "Yes")
  if (!is.null(date_from)){
    parenting_survey <- parenting_survey %>%
      dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tz = "UTC") < as.POSIXct(parenting_survey$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!is.null(date_to)){
    parenting_survey <- parenting_survey %>%
      dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tz = "UTC") > as.POSIXct(parenting_survey$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  objects_to_return <- NULL
  objects_to_return[[1]] <- df
  objects_to_return[[2]] <- df_consent
  objects_to_return[[3]] <- all_flows
  objects_to_return[[4]] <- parenting_survey
  objects_to_return[[5]] <- womens_centre_data # for Jamaica only (otherwise NULL?)
  return(objects_to_return)
}
