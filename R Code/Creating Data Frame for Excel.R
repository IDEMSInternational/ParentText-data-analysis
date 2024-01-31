library(rapidpror)
library(tidyverse)
just_ipv <- TRUE
#country <- "South_Africa"
country <- "Jamaica"

set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])
set_rapidpro_uuid_names()

date_from = default_date_from
date_to = default_date_to

contacts_unflat <- get_user_data(flatten = FALSE, date_from = date_from, date_to = date_to)

# base data -----------------------------------------------------
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
    filter(created_on <= as.Date(date_to))
}
list_of_ids <- df_created_on %>%
  filter(consent == "Yes")
list_of_ids <- list_of_ids$ID

# TODO: this was in their version:
#consent <- df_consent$consent
#df_created_on <- data.frame(ID, created_on, consent, row = 1:length(ID))

# demographics -----------------------------------------------------------------------------
state_of_origin <- as.character(contacts_unflat$fields$state_of_origin)
state_of_origin <- dplyr::recode(state_of_origin, "1" = state_1, "2" = state_2, "3" = state_3, "4" = state_4, "5" = state_5,
                                 "6" = state_6, "7" = state_7, "8" = state_8, "9" = state_9, "10" = state_10, "11" = state_11,
                                 "12" = state_12, "13" = state_13, "14" = state_14, "15" = state_15, "16" = state_16, "17" = state_17)
next_tip_main <- as.numeric(as.character(contacts_unflat$fields$next_tip_main))
next_tip_morning <- as.numeric(as.character(contacts_unflat$fields$next_tip_morning))
next_tip_evening <- as.numeric(as.character(contacts_unflat$fields$next_tip_evening))
parent_gender <- contacts_unflat$fields$gender
parent_gender <- factor(ifelse(parent_gender %in% c("female", "f", "woman", "Woman"), "Woman",
                               ifelse(parent_gender %in% c("male", "m", "man", "Man"), "Man",
                                      ifelse(parent_gender %in% "no", NA, parent_gender))))
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
                                                 `Prefer not to say` = "no")
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
                                         "Yes" = "yes")
#consent_survey_w1[is.na(consent_survey_w1)] <- "Did not interact"

#consent_survey_w1[is.na(consent_survey_w1)] <- "Did not interact"
consent_survey_w1 <- forcats::fct_relevel(consent_survey_w1, c("Yes", "No", "Did not interact"))

survey_consented_wk2_plus <- str_split(contacts_unflat$fields$surveyparentingbehave_datestamps, fixed("|"))
all_split_data <- NULL
if (length(survey_consented_wk2_plus) > 0){
  for (j in 1:length(survey_consented_wk2_plus)){
    if (length(survey_consented_wk2_plus[[j]]) == 1 && is.na(survey_consented_wk2_plus[[j]])){
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
IPV_tips_accessed <- contacts_unflat$fields$ipv_list_of_tips

df <- data.frame(ID, created_on, last_online, enrolled, consent, program,
                 gamification, personalisation, n_messages, language, parent_gender, child_gender, child_age_group, parent_child_relationship,
                 state_of_origin, 
                 parent_relationship, child_disabilities, recruitment_channel, parenting_goal,
                 active_users, active_users_7_days, comp_prog_overall, next_tip_main, next_tip_morning, next_tip_evening, parent_age, completed_welcome, comp_survey_w1, comp_survey_w2, consent_survey_w1,
                 challenge_behav, IPV_tips_accessed)

if (country %in% c("South Africa", "South_Africa", "Jamaica")){
  get_ipv_content <- contacts_unflat$fields$get_ipv_content
  df <- data.frame(df, get_ipv_content)
}

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

#df <- df %>%
#  mutate(length_in_programme = as.numeric(as.Date(last_online) - as.Date(created_on)) + 1)
#df <- merge(df, survey_consented_wk2_plus_data_wider, by = "ID") %>%
#  mutate(across(starts_with("consent_survey_"), ~replace_na(., 0)))

if (nrow(df) > 0){
  df <- df %>% mutate(order = 1:nrow(df))
}

if (length(survey_consented_wk2_plus_data_wider) > 0){
  df <- merge(df, survey_consented_wk2_plus_data_wider, by = "ID") %>% arrange(order)
}

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
    hook_data_all[[i]] <- data.frame(recent_hook_message_time = hook_messages_all, ID = ID_hook_all)
  }
  hook_message_all <- data.frame(plyr::ldply(hook_data_all))
  hook_message_all <- hook_message_all %>%
    mutate(hook_message_count = 1 + stringr::str_count(hook_message_all$recent_hook_message_time, pattern = fixed("|"))) %>%
    mutate(recent_hook_message_time = ifelse(stringi::stri_sub(recent_hook_message_time,-1) == "|",
                                          NA,
                                          recent_hook_message_time)) %>%
    mutate(hook_message_count = ifelse(is.na(recent_hook_message_time), 0, hook_message_count))
  hook_message_all$recent_hook_message_time <- as.POSIXct(gsub(".*,","",hook_message_all$recent_hook_message_time), format="%Y-%m-%dT%H:%M:%OS", tz = "EST") - lubridate::hm("6, 0")
  df <- dplyr::left_join(df, hook_message_all)
  df$created_on <- as.POSIXct(gsub(".*,","",df$created_on), format="%Y-%m-%dT%H:%M:%OS", tz = "EST") - lubridate::hm("6, 0")
  df$time_in_study <- df$recent_hook_message_time - df$created_on
}


################################
##### Hook message data frame ####
# Creating sheet with just hook messages
hook_messages <- contacts_unflat$fields$hook_message
hook_data_all <- NULL
  for (i in 1:length(hook_messages)){
    hook_messages_all <- hook_messages[[i]]
    ID_hook_all <- ID[i]
    hook_data_all[[i]] <- data.frame(recent_hook_message_time = hook_messages_all, ID = ID_hook_all)
  }
  hook_message_all <- data.frame(plyr::ldply(hook_data_all))
  
  # get all hook messages, in a long df. Create a new row at every "|"
  hook_message_all2 <- hook_message_all %>%
    separate_longer_delim(recent_hook_message_time, c(recent_hook_message_time = "|")) %>%
    filter(recent_hook_message_time != "") %>%
    filter(!is.na(recent_hook_message_time))
  
  hook_message <- data.frame(stringr::str_split(hook_message_all2$recent_hook_message_time, pattern = ",", simplify = TRUE))
  #hook_message <- data.frame(stringr::str_split(hook_message_all2$recent_hook_message_time, pattern = "-05:00", simplify = TRUE))
  names(hook_message) <- c("Message", "Time sent", "Time opened")
  
  hook_message_all2 <- bind_cols(hook_message, hook_message_all2)
  hook_message_all2$`Time sent` <- as.POSIXct(gsub(".*,","",hook_message_all2$`Time sent`), format="%Y-%m-%dT%H:%M:%OS", tz = "EST") - lubridate::hm("6, 0")
  hook_message_all2$`Time opened` <- as.POSIXct(gsub(".*,","",hook_message_all2$`Time opened`), format="%Y-%m-%dT%H:%M:%OS", tz = "EST") - lubridate::hm("6, 0")
  
  hook_message_all2 <- full_join(hook_message_all2, df_created_on)
################################

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
                           paste0(prefix, " - PLH - Supportive - Share - Enrollment"), "PLH - Supportive - Budget",
                           "PLH - Supportive - Behave reminder", "PLH - Supportive - Children reminder", "PLH - Supportive - Covid", "PLH - Supportive - Development",
                           "PLH - Supportive - Disabilities")
supportive_calm <- "PLH - Supportive - Calm"
supportive_praise <- "PLH - Supportive - Praise"
supportive_activities <-  c("PLH - Supportive - Activities for babies", "PLH - Supportive - Activities")
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
                                        date_to = date_to)
  }
#saveRDS(archived_data, file = paste0(country, "_archived.RDS"))

if ((country %in% c("Malaysia", "Philippines")) && include_archived_data){
  supportive_praise_flow <- get_flow_data(flow_name = supportive_praise, flow_type = "praise", include_archived_data = include_archived_data, get_by = "gotit", data_from_archived = archived_data)
  supportive_calm_flow <- get_flow_data(flow_name = supportive_calm, flow_type = "calm", include_archived_data = include_archived_data, get_by = "gotit", data_from_archived = archived_data)
  supportive_activities_flow <- get_flow_data(flow_name = supportive_activities, include_archived_data = include_archived_data, get_by = "gotit", data_from_archived = archived_data)
  supportive_flow_names_flow <- get_flow_data(flow_name = supportive_flow_names, include_archived_data = include_archived_data, get_by = "gotit", data_from_archived = archived_data)
  check_in_flow_names_flow <- get_flow_data(flow_name = check_in_flow_names, flow_type = "check_in", include_archived_data = include_archived_data, get_by = "gotit", data_from_archived = archived_data)
  content_tip_flow_names_flow <- get_flow_data(flow_name = content_tip_flow_names, flow_type = "tips", include_archived_data = include_archived_data, get_by = "gotit", data_from_archived = archived_data)
  } else {
  supportive_praise_flow <- get_flow_data(flow_name = supportive_praise, flow_type = "praise", include_archived_data = include_archived_data)
  supportive_calm_flow <- get_flow_data(flow_name = supportive_calm, flow_type = "calm", include_archived_data = include_archived_data)
  supportive_activities_flow <- get_flow_data(flow_name = supportive_activities, include_archived_data = include_archived_data)
  supportive_flow_names_flow <- get_flow_data(flow_name = supportive_flow_names, include_archived_data = include_archived_data)
  check_in_flow_names_flow <- get_flow_data(flow_name = check_in_flow_names, flow_type = "check_in", include_archived_data = include_archived_data)
  content_tip_flow_names_flow <- get_flow_data(flow_name = content_tip_flow_names, flow_type = "tips", include_archived_data = include_archived_data)
}
supportive_praise_flow$ID <- supportive_praise_flow$uuid
supportive_praise_flow$uuid <- NULL
supportive_praise_flow <- supportive_praise_flow %>% mutate(Flow = "Supportive Praise")
supportive_praise_flow$response <- replace_na(supportive_praise_flow$response, "No response")

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
check_in_flow_names_flow$response <- replace_na(check_in_flow_names_flow$response, "No response")
check_in_flow_names_flow$managed_to_do_something <- replace_na(check_in_flow_names_flow$managed_to_do_something, "No response")

content_tip_flow_names_flow$ID <- content_tip_flow_names_flow$uuid
content_tip_flow_names_flow$uuid <- NULL
content_tip_flow_names_flow <- content_tip_flow_names_flow %>% mutate(Flow = "Content Tip")
content_tip_flow_names_flow$category <- replace_na(content_tip_flow_names_flow$category, "No response")

all_flows <- dplyr::bind_rows(supportive_praise_flow, supportive_calm_flow, supportive_activities_flow, supportive_flow_names_flow,
                              check_in_flow_names_flow, content_tip_flow_names_flow)

supportive_praise_flow <- dplyr::left_join(supportive_praise_flow, df_created_on)
supportive_calm_flow <- dplyr::left_join(supportive_calm_flow, df_created_on)
supportive_flow_names_flow <- dplyr::left_join(supportive_flow_names_flow, df_created_on)
check_in_flow_names_flow <- dplyr::left_join(check_in_flow_names_flow, df_created_on)
content_tip_flow_names_flow <- dplyr::left_join(content_tip_flow_names_flow, df_created_on)
supportive_activities_flow <- dplyr::left_join(supportive_activities_flow, df_created_on)
if (length(all_flows) == 1){
  all_flows <- dplyr::left_join(all_flows, df_created_on, by = character())
} else {
  all_flows <- dplyr::left_join(all_flows, df_created_on)
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
contacts_unflat$fields$surveysexualabusetalk_datetime <- str_replace_all(contacts_unflat$fields$surveysexualabusetalk_datetime, "no", "0")
contacts_unflat$fields$surveysexualabusetalk_datetime <- str_replace_all(contacts_unflat$fields$surveysexualabusetalk_datetime, "yes", "1")
sex_abuse_talk <- get_survey_data(contacts_unflat$fields$surveysexualabusetalk_datetime) %>% mutate(Group = "Sexual abuse talk", row = 1:nrow(.)) %>%
  mutate(vals = ifelse(vals == "yes,", 1, 0))
sex_prevention <- get_survey_data(contacts_unflat$fields$surveysexualabuse_datetime) %>% mutate(Group = "Sexual abuse prevention")
child_behave <- get_survey_data(contacts_unflat$fields$surveybehave_rate_datetime) %>% mutate(Group = "Child Behaviour")
# using datetime not just _rate because in _rate it doesn't state which survey the score is corresponding to
# e.g. see contacts_unflat$fields$surveybehave_rate_datetime[[1]]

# TODO: change here - I made it from merge to bind_rows
pos_par <- full_join(play, praise, by = c("row", "week"))
child_mal <- full_join(physical_abuse, psychological_abuse, by = c("row", "week"))
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
parenting_survey <- rbind(positive_parenting, child_maltreatment, play, praise, stress, physical_abuse, psychological_abuse, financial_stress, food_insecurity, parenting_efficacy, sex_abuse_talk, sex_prevention, child_behave)
parenting_survey <- parenting_survey %>% mutate(week = fct_relevel(as.character(week), c("Baseline", "2", "3", "4", "5", "6", "7", "8", "9")))
parenting_survey <- parenting_survey %>% mutate(Group = fct_expand(Group, c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse", "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")))
parenting_survey <- parenting_survey %>% mutate(Group = fct_relevel(Group, c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse", "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")))

contacts_unflat_ID_merge <- df_created_on %>% mutate(row = 1:nrow(df_created_on))
parenting_survey <- full_join(parenting_survey, contacts_unflat_ID_merge) %>% arrange(row)
#parenting_survey <- parenting_survey %>% filter(consent == "Yes") %>% filter(program == "Yes")
#if (!is.null(date_from)){
#  parenting_survey <- parenting_survey %>%
#    dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tz = "UTC") < as.POSIXct(parenting_survey$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
#}
#if (!is.null(date_to)){
#  parenting_survey <- parenting_survey %>%
#    dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tz = "UTC") > as.POSIXct(parenting_survey$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
#}

parenting_survey_wider <- pivot_wider(parenting_survey, id_cols = c(ID, week), names_from = Group, values_from = c(vals, dt),
                                      values_fn = last) # iof mutliple responses, take the most recent.
parenting_survey_wider$`parenting_child_behaviour (severity)` <- parenting_survey_wider$parenting_child_behaviour
parenting_survey_wider$parenting_child_behaviour <- NULL
names(parenting_survey_wider)<-gsub("vals_","", names(parenting_survey_wider))
names(parenting_survey_wider)<-gsub("dt_","date_", names(parenting_survey_wider))
names(parenting_survey_wider)
df_uuid_challenge <- df %>%
  mutate(challenge_behav, created_on, consent) %>%
  dplyr::select(c(ID, challenge_behav, created_on, consent))
parenting_survey_wider <- merge(parenting_survey_wider, df_uuid_challenge)
parenting_survey_wider <- parenting_survey_wider %>% filter(!is.na(week))
parenting_survey_wider$`date_Positive parenting` <- NULL
parenting_survey_wider$`date_Child maltreatment` <- NULL
nrow(parenting_survey_wider)
#survey_consent_IDs <- unique(parenting_survey_wider$ID)

parenting_survey1 <- parenting_survey %>% filter(!is.na(week))
parenting_survey1 <- parenting_survey1 %>% mutate(week = ifelse(week == "Baseline", 1, week))
parenting_survey1 <- parenting_survey1 %>% mutate(week = paste0("w", week))
parenting_survey_even_wider <- pivot_wider(parenting_survey1, id_cols = c(ID), names_from = c(Group, week), values_from = c(vals, dt),
                                           values_fn = last) # iof mutliple responses, take the most recent.
names(parenting_survey_even_wider)<-gsub("vals_","", names(parenting_survey_even_wider))
names(parenting_survey_even_wider)<-gsub("dt_","date_", names(parenting_survey_even_wider))
parenting_survey_even_wider <- merge(parenting_survey_even_wider, df_uuid_challenge)

parenting_survey_even_wider <- parenting_survey_even_wider %>%
  dplyr::select(-c(starts_with("date_Child Behaviour"), starts_with("date_Positive parenting")))
# list of tips
#df_created_on <- df_created_on %>% mutate(row = 1:nrow(.))
#list_of_tips <- contacts_unflat$fields %>% dplyr::select(contains("list_of_tips"))
#tip_names <- names(list_of_tips)
#list_of_tips <- contacts_unflat$fields %>% dplyr::select(contains("list_of_tips")) %>% mutate(row = 1:nrow(.))
#list_of_tips <- merge(df_created_on, list_of_tips)

#list_of_tips <- list_of_tips %>% pivot_longer(cols = tip_names, names_to = "Tip Name", values_to = "Tip Number")
#list_of_tips1 <- list_of_tips %>% filter(!is.na(`Tip Number`))#
#
#list_of_tips1 <- list_of_tips1 %>%
#  mutate(`Tip Number` = strsplit(`Tip Number`, ",")) %>%
#  unnest(`Tip Number`)

#list_of_tips1 <- list_of_tips1 %>%
#  dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(list_of_tips1$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
#list_of_tips1 <- list_of_tips1 %>%
#  dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tzone = "UTC") > as.POSIXct(list_of_tips1$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
#list_of_tips1 <- list_of_tips1 %>% filter(consent == "Yes")
#list_of_tips1$row <- NULL
#tip_name <- read.xlsx(xlsxFile = "list of tips.xlsx", colNames = FALSE)
#tip_name$`.id` <- tip_name$X1
#tip_name$`Tip Name` <- tip_name$X2
#tip_name$X1 <- NULL
#tip_name$X2 <- NULL
#list_of_tips1 <- merge(list_of_tips1, tip_name)
#id.name <- data.frame(str_split(content_tip_flow_names_flow$`.id`, " - Timed intro", n = 2, simplify = TRUE))[1]
#content_tip_flow_names_flow$`.id` <- id.name$X1
#content_tip_flow_names_flow <- full_join(content_tip_flow_names_flow, list_of_tips1)
#

# Toolkit of skills data ------------------------------------------------------------------------
df_created_on1 <- data.frame(ID, created_on, consent, child_age_group)
toolkit <- (contacts_unflat$fields$toolkit)
incomplete_toolkit <- contacts_unflat$fields$incomplete_toolkit
skills <- c("One-on-one time", "Taking a Pause", "Managing anger", "Giving positive instructions",
            "Praising your children", "Establishing daily routines", "Setting household rules",
            "Redirecting behaviour", "Using appropriate consequences", "Shared reading",
            "Ignoring demanding behaviours", "Solving problems with your teen", "Keeping your child safe online",
            "Coping with crying", "Giving emotional support", "Responding to crises", 
            "Understanding safe/unsafe touch", "Making a happy and peaceful home", "Family budgeting",
            "Helping your children learn", "Keeping healthy", "Keeping it positive", "Keeping Calm",
            "Community safety", "Talking about COVID19", "Supporting children with disabilities", "Child development",
            "IPV")
detect_skill <- NULL
for (i in skills){
  complete_skill <- str_detect(toolkit, pattern = i)
  inprogress_skill <- str_detect(incomplete_toolkit, pattern = i)
  skill_data <- data.frame(complete_skill, inprogress_skill) %>%
    mutate(detect2 = ifelse(inprogress_skill==TRUE, "In Progress",
                            ifelse(complete_skill == TRUE, "Yes", "No"))) %>%
    mutate(detect2 = replace_na(detect2, "No"))
  if (nrow(skill_data %>% filter(complete_skill == TRUE) %>% filter(inprogress_skill == TRUE)) > 0){
    print(i)
  }
  detect_skill[[i]] <- skill_data$detect2
}

length(detect_skill[!is.na(detect_skill)])

content_tip_flow_names_flowb <- get_flow_data(flow_name = "PLH - Content - Time - One on one time baby - Timed intro", flow_type = "tips")
content_tip_flow_names_flowc <- get_flow_data(flow_name = "PLH - Content - Time - One on one time child - Timed intro", flow_type = "tips")
content_tip_flow_names_flowt <- get_flow_data(flow_name = "PLH - Content - Time - One on one time teen - Timed intro", flow_type = "tips")

# just check any that do not return any TRUEs
#for (i in skills){
#  if (length(which(detect_skill[[i]] == TRUE)) == 0) {
#    print(i)
#  }
#}

names(detect_skill) <- c("comp_prog_one_time", "comp_prog_adult", "comp_prog_anger_management", "comp_prog_instructions",
                         "comp_prog_praise","comp_prog_routines", "comp_prog_rules", "comp_prog_redirect", "comp_prog_consequences",
                         "comp_prog_book_sharing", "comp_prog_ignore", "comp_prog_problem_solving", "comp_prog_online", 
                         "comp_prog_crying", "comp_prog_emotion", "comp_prog_crisis", "comp_prog_touch", "comp_prog_family",
                         "comp_prog_budget", "comp_prog_education","comp_prog_exercise", "comp_prog_pos_intro", 
                         "comp_prog_keeping_calm", "comp_prog_community_safety", "comp_prog_covid", "comp_prog_disability",
                         "comp_prog_development")


toolkit_skills <- data.frame(df_created_on1, detect_skill)

# set dates/consent -----------------------------------------------------------------------------------------
df$consent_survey_wNA <- NULL

library(writexl)
list_of_datasets <- list("Calm flow" = supportive_calm_flow, "Praise flow" = supportive_praise_flow, "Supportive flow" = supportive_flow_names_flow,
                         "Check in flow" = check_in_flow_names_flow, "Content tip flow" = content_tip_flow_names_flow, "Activities flow" = supportive_activities_flow)
for (i in 1:length(list_of_datasets)){
  list_of_datasets[[i]] <- list_of_datasets[[i]] %>% mutate(interacted = ifelse(interacted == TRUE, "Yes", "No"))
  list_of_datasets[[i]] <- list_of_datasets[[i]] %>% filter(consent == "Yes")
  list_of_datasets[[i]] <- list_of_datasets[[i]] %>% dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(list_of_datasets[[i]]$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  if (!is.null(date_to)){ list_of_datasets[[i]] <- list_of_datasets[[i]] %>% dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tzone = "UTC") > as.POSIXct(list_of_datasets[[i]]$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))}
  #list_of_datasets[[i]] <- list_of_datasets[[i]] %>% select(-c("row"))
}

full_list_of_flows <- plyr::ldply(list_of_datasets)
toolkit_skills$created_on <- as.POSIXct(toolkit_skills$created_on, format="%Y-%m-%d", tzone = "UTC")
full_list_of_flows$created_on <- as.POSIXct(full_list_of_flows$created_on, format="%Y-%m-%d", tzone = "UTC")
full_data <- dplyr::full_join(dplyr::full_join(dplyr::full_join(df, toolkit_skills), parenting_survey_even_wider), full_list_of_flows)

super_list_of_datasets <- list("Demographics" = df, "Hook_Messages_draft" = hook_message_all2, "Parenting Survey" = parenting_survey_wider, "Parenting Survey (Wide)" = parenting_survey_even_wider,
                               "Toolkit Skills" = toolkit_skills, "Calm flow" = supportive_calm_flow, "Praise flow" = supportive_praise_flow,
                               "Activities flow" = supportive_activities_flow,
                               "Supportive flow" = supportive_flow_names_flow, "Check in flow" = check_in_flow_names_flow,
                               "Content tip flow" = content_tip_flow_names_flow,
                               "Full list of flows" = full_list_of_flows, "Full data" = full_data)
for (i in 1:length(super_list_of_datasets)){
  super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% filter(consent == "Yes")
  super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(super_list_of_datasets[[i]]$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  if (!is.null(date_to)){ super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tzone = "UTC") > as.POSIXct(super_list_of_datasets[[i]]$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")) }
}


# ---------------------------------------------------- #
engagement_data <- super_list_of_datasets$Demographics %>%
  dplyr::select(-c(enrolled, program, language, state_of_origin, recruitment_channel, active_users,
                   active_users_7_days, next_tip_main, next_tip_morning, next_tip_evening, comp_survey_w1, comp_survey_w2))
total_TRUE <- full_list_of_flows %>%
  group_by(ID) %>%
  filter(interacted == "Yes") %>%
  summarise(total_engagement_TRUES = n())

total_FALSE <- full_list_of_flows %>%
  group_by(ID) %>%
  summarise(total_engagement_response = n())
fraction_TRUE <- full_join(total_TRUE, total_FALSE)
fraction_TRUE <- fraction_TRUE %>%
  mutate(fraction_TRUE = total_engagement_TRUES/total_engagement_response)

engagement_data <- full_join(full_join(engagement_data, total_TRUE), fraction_TRUE)
engagement_data <- engagement_data %>%
  mutate(across(c("total_engagement_TRUES", "total_engagement_response", "fraction_TRUE"), ~replace_na(., 0)))
engagement_data$mean_TRUE <- engagement_data$total_engagement_TRUES/engagement_data$length_in_programme
engagement_data <- full_join(engagement_data, total_TRUE) %>%
  mutate(mean_TRUE = replace_na(mean_TRUE, 0))

by_flow_TRUE <- full_list_of_flows %>% group_by(ID, Flow) %>% filter(interacted == "Yes") %>% summarise(total_engagement_TRUES = n()) %>%
  pivot_wider(id_cols = ID, names_from = Flow, values_from = total_engagement_TRUES, names_prefix = "TRUES_")
engagement_data <- full_join(engagement_data, by_flow_TRUE) %>%
  mutate(across(starts_with("TRUES_"), ~replace_na(., 0)))

# content - nujmber of +ves, -ves, NAs
content_response <- list_of_datasets$`Content tip flow` %>%
  #mutate(category = ifelse(interacted == "No", "No Response", category)) %>%
  group_by(ID, category) %>%
  summarise(n()) %>%
  pivot_wider(id_cols = ID, names_from = "category", values_from = `n()`, names_prefix = "content_response_")
content_interactions <- list_of_datasets$`Content tip flow` %>%
  filter(interacted == "Yes") %>%
  group_by(ID) %>%
  summarise(content_total_interactions = n())

# NA is time outs?
engagement_data <- full_join(full_join(engagement_data, content_response), content_interactions) %>%
  mutate(across(starts_with("content_"), ~replace_na(., 0)))


# # number of days active for each individual
#active_days_data <- days_active_data(include_archived_data = TRUE, read_runs = TRUE, runs_data = "C:/lzc1n17/ParentText/result_flow_runs.RDS")

#engagement_data <- full_join(engagement_data, active_days_data)
#engagement_data <- engagement_data %>% mutate(frac_days_active = number_days_active/length_in_programme)
## check-in
check_in_response <- list_of_datasets$`Check in flow` %>%
  #mutate(response = ifelse(interacted == "No", "No Response", response)) %>%
  group_by(ID, response) %>%
  summarise(n()) %>%
  pivot_wider(id_cols = ID, names_from = "response", values_from = `n()`, names_prefix = "check_in_response_")
check_in_interactions <- list_of_datasets$`Check in flow` %>%
  filter(interacted == "Yes") %>%
  group_by(ID) %>%
  summarise(check_in_total_interactions = n())
engagement_data <- full_join(full_join(engagement_data, check_in_response), check_in_interactions) %>%
  mutate(across(starts_with("check_in_"), ~replace_na(., 0)))

# engagement with survey



# consented to survey (y/n)
engagement_data <- engagement_data %>%
  mutate(across(starts_with("consent_survey_"), ~as.numeric(.))) %>%
  mutate(across(starts_with("consent_survey_"), ~replace_na(., 0))) %>%
  mutate(consent_survey_w1 = ifelse(consent_survey_w1 == 1, 1, 0),
         consent_survey_w2 = ifelse(consent_survey_w2 == 1, 1, 0),
         consent_survey_w3 = ifelse(consent_survey_w3 == 1, 1, 0),
         consent_survey_w4 = ifelse(consent_survey_w4 == 1, 1, 0)) %>%
  mutate(total_survey_consent = consent_survey_w1 + consent_survey_w2 + consent_survey_w3 + consent_survey_w4) %>%
  dplyr::select(-c(consent_survey_w1, consent_survey_w2, consent_survey_w3, consent_survey_w4)) %>%
  mutate(mean_survey_consent = total_survey_consent/4)

# no. of responses to survey q's for +ve parenting etc.
all_vars <- c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse",
              "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")
survey_sum <- super_list_of_datasets$`Parenting Survey` %>%
  mutate(across(all_vars, ~replace_na(., 0))) %>%
  group_by(ID) %>%
  summarise(across(all_vars, ~ sum(., na.rm = TRUE)))

engagement_data <- dplyr::full_join(engagement_data, survey_sum) %>%
  mutate(across(all_vars, ~replace_na(., 0)))


# AB group data ----------------------------------------
contacts_group <- contacts_unflat$groups
names(contacts_group) <- contacts_unflat$uuid
contacts_group <- plyr::ldply(contacts_group)
contacts_group$ID <- contacts_group$.id
df_created_on_2 <- data.frame(ID, created_on, consent)
contacts_group <- dplyr::full_join(contacts_group, df_created_on_2)
contacts_group <- contacts_group %>%
  select(c("ID", "created_on", "consent", "name")) %>%
  filter(!name %in% c("consent", "in program", "joined", "passive drop out", "completed programme", "below_age",
                      "ipv baseline", "below age", "NA"))
group_values <- stringr::str_split(contacts_group$name, "_(?=[^_]+$)", simplify = TRUE)
contacts_group$group <- group_values[,1]
contacts_group$values <- group_values[,2]
contacts_group <- contacts_group %>% pivot_wider(id_cols = c(ID, created_on, consent), names_from = group, values_from = values)
contacts_group$"NA" <- NULL



# Just IPV for SA:
if (just_ipv){
  if (country %in% c("Jamaica")){
    ipv_jam <- "PLH - Content - Positive - IPV"
    ipv_jam_inds <- get_flow_data(flow_name = ipv_jam, flow_type = "tips", include_archived_data = include_archived_data)
    ipv_jam <- unique(ipv_jam_inds$uuid)
    df_consent_ipv <- (df_consent %>% filter(is.na(ipv_version)))
    df_consent_ipv <- (df_consent_ipv %>% filter(ID %in% ipv_jam))$ID
  }
  if (country %in% c("South Africa", "South_Africa")){
    df_consent_ipv <- (df_consent %>% filter(ipv_version == "yes"))$ID
  }
}

#################################

if ("consent_survey_wNA" %in% names(df)) df <- df %>% dplyr::select(-c(order, consent_survey_wNA))
super_list_of_datasets <- list("Demographics" = df, "Hook_Messages_draft" = hook_message_all2, "Contacts Groups" = contacts_group, "Parenting Survey" = parenting_survey_wider, "Parenting Survey (Wide)" = parenting_survey_even_wider,
                               "Toolkit Skills" = toolkit_skills, "Calm flow" = supportive_calm_flow, "Praise flow" = supportive_praise_flow,
                               "Supportive flow" = supportive_flow_names_flow, "Check in flow" = check_in_flow_names_flow,
                               "Activities flow" = supportive_activities_flow,
                               "Content tip flow" = content_tip_flow_names_flow, "Full list of flows" = full_list_of_flows,
                               "Full data" = full_data, "Engagement Data" = engagement_data)
for (i in 1:length(super_list_of_datasets)){
  
  if (country %in% c("South Africa", "South_Africa", "Jamaica")){
    if (just_ipv){
      super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% filter(ID %in% df_consent_ipv)
    }
  }
  
  if (i %in% c(7:12)){
    super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% mutate(interacted = ifelse(interacted == TRUE, "Yes", "No"))
  }
  super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% filter(consent == "Yes")
  super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(super_list_of_datasets[[i]]$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  if (!is.null(date_to)) {
    super_list_of_datasets[[i]] <- super_list_of_datasets[[i]] %>% dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tzone = "UTC") > as.POSIXct(super_list_of_datasets[[i]]$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  #list_of_datasets[[i]] <- list_of_datasets[[i]] %>% select(-c("row"))
}

write_xlsx(super_list_of_datasets, path = "SA_Data_20230912.xlsx")

# 
# IPV_tips_accessed <- contacts_unflat$fields$ipv_list_of_tips
# ID <- contacts_unflat$uuid
# df_IPV_tips <- data.frame(ID, IPV_tips_accessed)
# 
# # Additional IPV info for Moa:
# rapidpro_site = get_rapidpro_site()
# token = get_rapidpro_key()
# uuid_data = get_rapidpro_uuid_names()
# call_type = "runs.json?flow="
# uuid_flow <- uuid_data[which(uuid_data$name == "PLH - Content - Positive - IPV"),]
# get_command <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
# result_flow <- httr_get_call(get_command = get_command, token = token)
# ID <- result_flow$contact$uuid
# tip_no_ipv <- result_flow$values$list_of_tips$value
# 
# df_ipv <- data.frame(flow_name = "PLH - Content - Positive - IPV", ID, tip_no_ipv)
# df_ipv <- full_join(df_ipv, df_IPV_tips)
# 
# ipv_flow <- dplyr::left_join(df_ipv, df_created_on, by = "ID")
# ipv_flow <- ipv_flow
# ipv_flow$row <- NULL
# 
# ipv_flow <- ipv_flow %>% filter(ID %in% df_consent_ipv)
# ipv_flow <- ipv_flow %>% filter(consent == "Yes")
# ipv_flow <- ipv_flow %>% dplyr::filter(as.POSIXct(date_from, format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(ipv_flow$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
# if (!is.null(date_to)) {
#   ipv_flow <- ipv_flow %>% dplyr::filter(as.POSIXct(date_to, format="%Y-%m-%d", tzone = "UTC") > as.POSIXct(ipv_flow$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
# }
# 
# #list_of_datasets[[i]] <- list_of_datasets[[i]] %>% select(-c("row"))
# 
# 
# write_xlsx(ipv_flow, path = "SA_IPV_bonus_data_20221231.xlsx")
# 
