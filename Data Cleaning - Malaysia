library(httr)
library(jsonlite)
library(tidyverse)
source("Functions.R")
# source("Code Book.R")
#install_github("lilyclements/rapidpror")
#library(rapidpror)

# RapidPro set up --------------------------------------------------------------
#key <- read.table("C:/Users/lzc1n17/OneDrive - University of Southampton/PhD/IDEMS/ParentText/PT_malaysia_key.txt", quote="\"", comment.char="")
#key <- read.table("./tokens/PT_malaysia_key.txt", quote="\"", comment.char="")
set_rapidpro_key(key = key[[1]])
set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
set_rapidpro_uuid_names()

update_data <- function(date_from = "2021-12-07", date_to = NULL) {
  contacts_unflat <- get_user_data(flatten = FALSE, date_from = date_from, date_to = date_to)
  
  ID <- contacts_unflat$uuid
  
  # Variables Manipulation -------------------------------------------------------
  # get enrolled and consented data
  enrolled <- NULL
  true_consent <- NULL
  program <- NULL
  for (i in 1:length(contacts_unflat$groups)){
    contact_name <- contacts_unflat$groups[[i]]
    if (length(contact_name)==0) {
      enrolled[i] <- NA
      true_consent[i] <- NA
      program[i] <- NA
    } else{
      enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
      true_consent[i] <- ifelse(any(contact_name$name %in% "consent"), "Yes", "No")
      program[i] <- ifelse(any(contact_name$name %in% "in program"), "Yes", "No")
    }
  }
  
  enrolled <- factor(enrolled)
  true_consent <- factor(true_consent)
  program <- factor(program)
  enrolled <- forcats::fct_expand(enrolled, c("Yes", "No"))
  true_consent <- forcats::fct_expand(true_consent, c("Yes", "No"))
  program <- forcats::fct_expand(program, c("Yes", "No"))
  enrolled <- forcats::fct_relevel(enrolled, c("Yes", "No"))
  true_consent <- forcats::fct_relevel(true_consent, c("Yes", "No"))
  program <- forcats::fct_relevel(program, c("Yes", "No"))
  
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
                                                   `Prefer not to say`  = "no")
  parent_child_relationship <- forcats::fct_relevel(parent_child_relationship,
                                                    c("Parent", "Grandparent", "Aunt/Uncle", "Foster Parent", "Other", "Prefer not to say"))
  
  parent_relationship_status <- factor(contacts_unflat$fields$marital_status)
  parent_relationship_status <- forcats::fct_recode(parent_relationship_status,
                                                    `Prefer not to say`  = "no")
  parent_relationship_status <- fct_expand(parent_relationship_status, "Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say")
  parent_relationship_status <- forcats::fct_relevel(parent_relationship_status, c("Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say"))
  
  child_living_with_disabilities <- factor(contacts_unflat$fields$has_disability)
  child_living_with_disabilities <- fct_expand(child_living_with_disabilities, "Yes", "No")
  child_living_with_disabilities <- forcats::fct_recode(child_living_with_disabilities,
                                                        Yes = "yes",
                                                        No = "no",
                                                        `supp_disab`= "supp_disab")
  child_living_with_disabilities <- forcats::fct_relevel(child_living_with_disabilities,
                                                         c("Yes", "No", "supp_disab"))
  
  language <- factor(contacts_unflat$language)
  language <- fct_expand(language, "Did not respond", "MSA", "ENG")
  language <- forcats::fct_recode(language,
                                  MSA = "msa",
                                  ENG = "eng")
  language[is.na(language)] <- "Did not respond"
  language <- forcats::fct_relevel(language, c("ENG", "MSA", "Did not respond"))
  
  
  recruitment_channel <- factor(contacts_unflat$fields$enrollment)
  recruitment_channel <- forcats::fct_expand(recruitment_channel, c("LPPKN", "NGO", "Friends/Family", "U-Report", "Social media", "Other"))
  recruitment_channel <- forcats::fct_recode(recruitment_channel,
                                             `LPPKN` = "LPPKN",
                                             `NGO` = "Ngo",
                                             `Friends/Family` = "Someone",
                                             `U-Report` = "U-report",
                                             `Social media` = "Social",
                                             `Other` = "Other channel")
  recruitment_channel <- forcats::fct_relevel(recruitment_channel, c("LPPKN", "NGO", "Friends/Family", "U-Report", "Social media", "Other"))
  
  parenting_goals <- factor(as.numeric(contacts_unflat$fields$parenting_goal))
  parenting_goals <- forcats::fct_expand(parenting_goals, c("Relationship","Behaviour", "School", "COVID-19", "Stress", "Finances", "Family conflict", "Safety", "Disabilities", "Other"))
  parenting_goals <- forcats::fct_recode(parenting_goals,
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
  parenting_goals <- forcats::fct_relevel(parenting_goals,
                                          c("Relationship","Behaviour",
                                            "School", "COVID-19",
                                            "Stress", "Finances",
                                            "Family conflict", "Safety",
                                            "Disabilities", "Other"))
  parenting_goals_wrap <- str_wrap_factor(parenting_goals, width = 15)
  
  # Calculations -----------------------------------------------------------------
  # active users # N = contacts for which the time difference between the current time and the datetime variable "last_seen_on" is less than 24 h 
  active_users_24hr <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 24
  active_users_24hr <- factor(active_users_24hr)
  if (length(levels(active_users_24hr)) == 1){
    if (levels(active_users_24hr) == "FALSE"){
      levels(active_users_24hr) <- c(levels(active_users_24hr),"TRUE")
    } else if (levels(active_users_24hr) == "TRUE"){
      levels(active_users_24hr) <- c(levels(active_users_24hr),"FALSE")
    }
  }
  active_users_24hr <- forcats::fct_expand(active_users_24hr, c("Yes", "No"))
  active_users_24hr <- forcats::fct_recode(active_users_24hr,
                                           "No" = "FALSE",
                                           "Yes" = "TRUE")
  active_users_24hr <- forcats::fct_relevel(active_users_24hr, c("Yes", "No"))
  
  active_users_7d <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 7*24
  active_users_7d <- factor(active_users_7d)
  if (length(levels(active_users_7d)) == 1){
    if (levels(active_users_7d) == "FALSE"){
      levels(active_users_7d) <- c(levels(active_users_7d),"TRUE")
    } else if (levels(active_users_7d) == "TRUE"){
      levels(active_users_7d) <- c(levels(active_users_7d),"FALSE")
    }
  }
  active_users_7d <- forcats::fct_expand(active_users_7d, c("Yes", "No"))
  active_users_7d <- forcats::fct_recode(active_users_7d,
                                         "No" = "FALSE",
                                         "Yes" = "TRUE")
  active_users_7d <- forcats::fct_relevel(active_users_7d, c("Yes", "No"))
  
  # comp_prog_overall
  # TODO: only should be lookign at this for those who consented
  n_skills <- as.numeric(as.character(contacts_unflat$fields$n_skills))
  
  # Participant age etc
  # TODO: only should be lookign at this for those who consented
  parent_age <- as.numeric(as.character(contacts_unflat$fields$age))
  
  # completed surveys
  survey_completed_welcome <- factor(contacts_unflat$fields$completed_welcome)
  survey_completed_welcome <- forcats::fct_expand(survey_completed_welcome, c("Yes", "No"))
  survey_completed_welcome <- forcats::fct_recode(survey_completed_welcome,
                                                  "No" = "no",
                                                  "Yes" = "yes")
  survey_completed_welcome[is.na(survey_completed_welcome)] <- "No"
  survey_completed_welcome <- forcats::fct_relevel(survey_completed_welcome, c("Yes", "No"))
  
  survey_completed_wk1 <- str_count(contacts_unflat$fields$surveyparenting_completion, fixed("|"))
  if (length(survey_completed_wk1) == 0){survey_completed_wk1 <- rep(NA, length(enrolled))}
  
  survey_completed_wk2_plus <- str_count(contacts_unflat$fields$surveyparentingbehave_completion, fixed("|"))
  if (length(survey_completed_wk2_plus) == 0){survey_completed_wk2_plus <- rep(NA, length(enrolled))}
  
  
  
  challenging_type <- contacts_unflat$fields$survey_behave_most_challenging
  challenging_type <- dplyr::case_when(
    child_age_group == "Baby" & challenging_type == "1" ~ "Crying",
    child_age_group == "Baby" & challenging_type == "2" ~ "Problems sleeping",
    child_age_group == "Baby" & challenging_type == "3" ~ "Acting clingy",
    child_age_group == "Baby" & challenging_type == "4" ~ "Whining",
    child_age_group == "Baby" & challenging_type == "5" ~ "Bad tempered",
    child_age_group == "Baby" & challenging_type == "6" ~ "Problems eating",
    child_age_group == "Baby" & challenging_type == "7" ~ "Stubborn/fussy",
    child_age_group == "Baby" & challenging_type == "8" ~ "Naughty behaviour",
    child_age_group == "Baby" & challenging_type == "9" ~ "Temper Tantrums",
    child_age_group %in% c("Child", "Default", "Teen") & challenging_type == "1" ~ "Refuses to obey",
    child_age_group %in% c("Child", "Default") & challenging_type == "2" ~ "Gets angry",
    child_age_group %in% c("Child", "Default", "Teen") & challenging_type == "3" ~ "Rude behaviour",
    child_age_group %in% c("Child", "Default") & challenging_type == "4" ~ "Mood swings",
    child_age_group %in% c("Child", "Default") & challenging_type == "5" ~ "Does not follow rules",
    child_age_group %in% c("Child", "Default") & challenging_type == "6" ~ "Stubbornness",
    child_age_group %in% c("Child", "Default" ~ "Teen") & challenging_type == "7" ~ "Breaks things",
    child_age_group %in% c("Child", "Default" ~ "Teen") & challenging_type == "8" ~ "Gets into fights",
    child_age_group %in% c("Child", "Default" ~ "Teen") & challenging_type == "9" ~ "Teases others",
    child_age_group %in% c("Teen") & challenging_type == "2" ~ "Temper Tantrums",
    child_age_group %in% c("Teen") & challenging_type == "4" ~ "Whining",
    child_age_group %in% c("Teen") & challenging_type == "5" ~ "Hyperactivity",
    child_age_group %in% c("Teen") & challenging_type == "6" ~ "Hits others")
  
  challenging_type <- forcats::fct_expand(challenging_type, c("Crying", "Problems sleeping", "Acting clingy", "Whining", "Bad tempered", "Problems eating", "Stubborn/fussy", "Naughty behaviour", "Temper Tantrums", "Refuses to obey", "Gets angry", "Rude behaviour", "Mood swings", "Does not follow rules", "Stubbornness", "Breaks things", "Gets into fights", "Teases others", "Hyperactivity", "Hits others"))
  challenging_type <- forcats::fct_relevel(challenging_type, c("Crying", "Problems sleeping", "Acting clingy", "Whining", "Bad tempered", "Problems eating", "Stubborn/fussy", "Naughty behaviour", "Temper Tantrums", "Refuses to obey", "Gets angry", "Rude behaviour", "Mood swings", "Does not follow rules", "Stubbornness", "Breaks things", "Gets into fights", "Teases others", "Hyperactivity", "Hits others"))
  challenging_type_wrap <- str_wrap_factor(challenging_type, width = 10)
  
  df <- data.frame(ID, enrolled, true_consent, program, language, parent_gender, child_gender, child_age_group, parent_child_relationship, 
                   parent_relationship_status, child_living_with_disabilities, recruitment_channel, parenting_goals, parenting_goals_wrap,
                   active_users_24hr, active_users_7d, n_skills, parent_age, survey_completed_welcome, survey_completed_wk1, survey_completed_wk2_plus,
                   challenging_type, challenging_type_wrap)
  
  df <- df %>%
    mutate(consent = ifelse(language == "Did not respond", "Did not respond",
                            ifelse(true_consent == "Yes", "Yes", "No")))
  df <- df %>%
    mutate(consent = forcats::fct_relevel(consent, c("Yes", "No", "Did not respond")))
  
  # for Jamaica Only: Parent Pals data cleaning --------------------
  pp_n_recruited <- df %>% group_by(child_age_group) %>% # group_by will be "recruited by" in time
    mutate(survey_completed_welcome = ifelse(survey_completed_welcome == "Yes", 1, 0)) %>% # reorder welcome survey
    summarise(`Number recruited` = n(),
              `Toolkit skills` = sum(n_skills, na.rm = TRUE),
              `Welcome survey completed` = sum(survey_completed_welcome, na.rm = TRUE),
              `Week1 survey completed` = sum(survey_completed_wk1, na.rm = TRUE),
              `Week 2 survey completed` = sum(survey_completed_wk2_plus, na.rm = TRUE))
  pp_n_consent <- df %>% group_by(child_age_group, .drop = FALSE) %>% # group_by will be "recruited by" in time
    filter(consent == "Yes") %>%
    summarise(`Number consented` = n())
  pp_data_frame <- merge(pp_n_recruited, pp_n_consent)
  pp_data_frame <- pp_data_frame %>%
    mutate(`Recruited by` = 1:nrow(.)) %>%
    dplyr::select(`Recruited by`, `Number recruited`, `Number consented`, `Welcome survey completed`, `Week1 survey completed`,
                  `Week 2 survey completed`, `Toolkit skills`) %>%
    mutate(Total = `Number recruited` + `Number consented` + `Welcome survey completed` + `Week1 survey completed` + `Week 2 survey completed` + `Toolkit skills`) %>%
    arrange(desc(Total))
  
  # flow level data --------------------------------
  # sum of response to content, calm, check in, supportive, praise messages
  # supportive
  supportive_flow_names <- c("PLH - Content - Extra - CheckIn - COVID",
                             "PLH - Supportive - Family", "PLH - Supportive - Help reminder", "PLH - Supportive - Share", "PLH - Supportive - Share - Enrollment", "GG - PLH - Supportive - Share - Enrollment", "PLH - Supportive - Budget", "PLH - Supportive - Activities for babies", "PLH - Supportive - Activities",
                             "PLH - Supportive - Behave reminder", "PLH - Supportive - Children reminder", "PLH - Supportive - Covid", "PLH - Supportive - Development", "PLH - Supportive - Disabilities")
  
  supportive_calm <- "PLH - Supportive - Calm"
  
  supportive_praise <- "PLH - Supportive - Praise"
  
  check_in_flow_names <- c("PLH - Content - Extra - CheckIn - COVID", "PLH - Content - Positive - CheckIn - Book sharing", "PLH - Content - Positive - CheckIn - Budget adults", "PLH - Content - Positive - CheckIn - Budget with children", "PLH - Content - Positive - CheckIn - Community safety", "PLH - Content - Positive - CheckIn - Consequences", "PLH - Content - Positive - CheckIn - Crisis", "PLH - Content - Positive - CheckIn - Crying", "PLH - Content - Positive - CheckIn - Education", "PLH - Content - Positive - CheckIn - Emotion", "PLH - Content - Positive - CheckIn - Family", "PLH - Content - Positive - CheckIn - Ignore",
                           #"PLH - Content - Positive - CheckIn - Instructions",
                           "PLH - Content - Positive - CheckIn - IPV 1", "PLH - Content - Positive - CheckIn - IPV 2", "PLH - Content - Positive - CheckIn - IPV 3", "PLH - Content - Positive - CheckIn - IPV 4", "PLH - Content - Positive - CheckIn - IPV 5", "PLH - Content - Positive - CheckIn - Online adults", "PLH - Content - Positive - CheckIn - Online children", "PLH - Content - Positive - CheckIn - Praise", "PLH - Content - Positive - CheckIn - ProblemSolving", "PLH - Content - Positive - CheckIn - Redirect", "PLH - Content - Positive - CheckIn - Routines", "PLH - Content - Positive - CheckIn - Rules", "PLH - Content - Positive - CheckIn - Safe or unsafe touch", "PLH - Content - Relax - CheckIn - Anger management", "PLH - Content - Relax - CheckIn - Connect", "PLH - Content - Relax - CheckIn - List of things",
                           "PLH - Content - Relax - CheckIn - Loving Kindness", "PLH - Content - Relax - CheckIn - Notice how you feel", "PLH - Content - Relax - CheckIn - Three is a magical number", "PLH - Content - Time - CheckIn - One on one time")
  
  content_tip_flow_names <- c("PLH - Content - Positive - Behave - Consequences - Timed intro", "PLH - Content - Positive - Behave - Crisis - Timed intro", "PLH - Content - Positive - Behave - Crying - Timed intro", "PLH - Content - Positive - Behave - Emotion - Timed intro", "PLH - Content - Positive - Behave - Ignore - Timed intro", "PLH - Content - Positive - Behave - Praise - Timed intro", "PLH - Content - Positive - Behave - ProblemSolving - Timed intro", "PLH - Content - Positive - Behave - Redirect - Timed intro", "PLH - Content - Positive - Behave - Routines - Timed intro",
                              "PLH - Content - Positive - Book sharing - Timed intro", "PLH - Content - Positive - Budget adults - Timed intro", "PLH - Content - Positive - Budget with children - Timed intro","PLH - Content - Positive - Education - Timed intro",
                              "PLH - Content - Positive - Family - Timed intro", "PLH - Content - Positive - Online adults - Timed intro", "PLH - Content - Positive - Online children - Timed intro", "PLH - Content - Positive - Rules - Timed intro",
                              "PLH - Content - Positive - Safe or unsafe touch - Timed intro", "PLH - Content - Relax - Take a pause - Timed intro", "PLH - Content - Relax - Exercise", "PLH - Content - Time - One on one time baby - Timed intro",
                              "PLH - Content - Time - One on one time child - Timed intro", "PLH - Content - Time - One on one time teen - Timed intro", "PLH - Content - Positive - introduction", "PLH - Content - Positive - Positive instructions", "PLH - Content - Relax - Quick Pause", "PLH - Content - Relax - Anger management", "PLH - Content - Relax - Anger management 2", "PLH - Content - Positive - IPV", "PLH - Content - Positive - Community safety")
  
  supportive_calm_flow <- get_flow_data(flow_name = supportive_calm, date_from = date_from, date_to = date_to)
  supportive_praise_flow <- get_flow_data(flow_name = supportive_praise, date_from = date_from, date_to = date_to)
  supportive_flow_names_flow <- get_flow_data(flow_name = supportive_flow_names, date_from = date_from, date_to = date_to)
  check_in_flow_names_flow <- get_flow_data(flow_name = check_in_flow_names, date_from = date_from, date_to = date_to)
  content_tip_flow_names_flow <- get_flow_data(flow_name = content_tip_flow_names, date_from = date_from, date_to = date_to)
  
  # Survey Level Data ---------------------------------------------------------------------------------------------------------------------------
  # get all survey values
  week <- c(rep("Base", nrow(contacts_unflat)),
            rep("2", nrow(contacts_unflat)),
            rep("3", nrow(contacts_unflat)),
            rep("4", nrow(contacts_unflat)),
            rep("5", nrow(contacts_unflat)),
            rep("6", nrow(contacts_unflat)),
            rep("7", nrow(contacts_unflat)),
            rep("8", nrow(contacts_unflat)),
            rep("9", nrow(contacts_unflat)))
  
  
  play <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveytime_datetime))) %>% mutate(Group = "Play")
  praise <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveypraise_datetime))) %>% mutate(Group = "Praise")
  stress <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveystress_datetime))) %>% mutate(Group = "Stress")
  physical_abuse <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveydiscipline_datetime))) %>% mutate(Group = "Physical abuse")
  food_insecurity <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveymoneymonth_datetime))) %>% mutate(Group = "Food insecurity")
  psychological_abuse <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveyshout_datetime))) %>% mutate(Group = "Psychological abuse")
  financial_stress <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveymoneyweek_datetime))) %>% mutate(Group = "Financial stress")
  parenting_efficacy <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveypositive_datetime))) %>% mutate(Group = "Parenting efficacy")
  sex_prevention <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveysexualabuse_datetime))) %>% mutate(Group = "Sexual abuse prevention")
  # using datetime not just _rate because in _rate it doesn't state which survey the score is corresponding to
  # e.g. see contacts_unflat$fields$surveybehave_rate_datetime[[1]]
  child_behave <- data.frame(week, vals = unlist(get_survey_data(contacts_unflat$fields$surveybehave_rate_datetime))) %>% mutate(Group = "Child Behaviour")
  
  positive_parenting <- data.frame(week, vals = pmax(play$vals, praise$vals, na.rm = TRUE)) %>% mutate(Group = "Positive parenting")
  child_maltreatment <- data.frame(week, vals = pmax(physical_abuse$vals, psychological_abuse$vals, na.rm = TRUE)) %>% mutate(Group = "Child maltreatment")
  parenting_survey <- rbind(positive_parenting, child_maltreatment, play, praise, stress, physical_abuse, psychological_abuse, financial_stress, food_insecurity, parenting_efficacy, sex_prevention, child_behave)
  parenting_survey <- parenting_survey %>% mutate(week = fct_relevel(week, c("Base", "2", "3", "4", "5", "6", "7", "8", "9")))
  parenting_survey <- parenting_survey %>% mutate(Group = fct_expand(Group, c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse", "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")))
  parenting_survey <- parenting_survey %>% mutate(Group = fct_relevel(Group, c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress", "Physical abuse", "Psychological abuse", "Financial stress", "Food insecurity", "Parenting efficacy", "Sexual abuse prevention", "Child Behaviour")))
  parenting_survey$ID <- contacts_unflat$uuid
  
  # last online plot -------------------------------------------------------------------------------------------------------------
  last_online <- as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT", tz = "UTC")
  df$last_online <- last_online
  
  objects_to_return <- NULL
  objects_to_return[[1]] <- df
  objects_to_return[[2]] <- supportive_calm_flow
  objects_to_return[[3]] <- supportive_praise_flow
  objects_to_return[[4]] <- check_in_flow_names_flow
  objects_to_return[[5]] <- content_tip_flow_names_flow
  objects_to_return[[6]] <- supportive_flow_names_flow
  objects_to_return[[7]] <- enrolled
  objects_to_return[[8]] <- true_consent
  objects_to_return[[9]] <- program
  objects_to_return[[10]] <- parenting_survey
  objects_to_return[[11]] <- pp_data_frame # for Jamaica only (otherwise NULL?)
  return(objects_to_return)
}

#updated_data <- update_data()
#df <- updated_data[[1]]
#supportive_calm_flow <- updated_data[[2]]
#supportive_praise_flow <- updated_data[[3]]
#check_in_flow_names_flow <- updated_data[[4]]
#content_tip_flow_names_flow <- updated_data[[5]]
#supportive_flow_names_flow <- updated_data[[6]]
#enrolled <- updated_data[[7]]
#true_consent <- updated_data[[8]]
#program <- updated_data[[9]]
#parenting_survey <- updated_data[[10]]

# retention_exit ---------------------------------------------------------------
# number of contacts for which the contact field "exit_message" is not empty &
# they are NOT in the group "in program"
#df %>% filter(program == "No") %>% nrow(.)
#contacts_unflat$fields$exit_message




#parenttext_shiny(data = "Hi")
parenttext_shiny(data = "Jamaica")
