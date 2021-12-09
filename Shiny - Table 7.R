library(shiny)
library(shinythemes)
library(shinyjs)
#library(rpivotTable)
library(plotly)
library(shinydashboard)
library(httr)
library(jsonlite)
library(tidyverse)
#source("Functions.R")
# source("Code Book.R")
#install_github("lilyclements/rapidpror")
#library(rapidpror)

# RapidPro set up --------------------------------------------------------------

key <- read.table("./tokens/PT_malaysia_key.txt", quote="\"", comment.char="")
set_rapidpro_key(key = key)
set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
set_rapidpro_uuid_names()

update_data <- function() {
  contacts_unflat <- get_user_data(flatten = FALSE, date_from = "2021-12-07")
  
  ID <- contacts_unflat$uuid
  
  # Variables Manipulation -------------------------------------------------------
  # get enrolled and consented data
  enrolled <- NULL
  consent <- NULL
  program <- NULL
  for (i in 1:length(contacts_unflat$groups)){
    contact_name <- contacts_unflat$groups[[i]]
    if (length(contact_name)==0) {
      enrolled[i] <- NA
      consent[i] <- NA
      program[i] <- NA
    } else{
      enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
      consent[i] <- ifelse(any(contact_name$name %in% "consent"), "Yes", "No")
      program[i] <- ifelse(any(contact_name$name %in% "in program"), "Yes", "No")
    }
  }
  
  enrolled <- factor(enrolled)
  consent <- factor(consent)
  program <- factor(program)
  enrolled <- forcats::fct_expand(enrolled, c("Yes", "No"))
  consent <- forcats::fct_expand(consent, c("Yes", "No"))
  program <- forcats::fct_expand(program, c("Yes", "No"))
  enrolled <- forcats::fct_relevel(enrolled, c("Yes", "No"))
  consent <- forcats::fct_relevel(consent, c("Yes", "No"))
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
  
  df <- data.frame(ID, enrolled, consent, program, parent_gender, child_gender, child_age_group, parent_child_relationship, 
                   parent_relationship_status, child_living_with_disabilities, parenting_goals, parenting_goals_wrap,
                   active_users_24hr, active_users_7d, n_skills, parent_age, survey_completed_wk1, survey_completed_wk2_plus,
                   challenging_type, challenging_type_wrap)
  
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
  
  supportive_calm_flow <- get_flow_data(flow_name = supportive_calm, date_from = "2021-12-07")
  supportive_praise_flow <- get_flow_data(flow_name = supportive_praise, date_from = "2021-12-07")
  supportive_flow_names_flow <- get_flow_data(flow_name = supportive_flow_names, date_from = "2021-12-07")
  check_in_flow_names_flow <- get_flow_data(flow_name = check_in_flow_names, date_from = "2021-12-07")
  content_tip_flow_names_flow <- get_flow_data(flow_name = content_tip_flow_names, date_from = "2021-12-07")
  
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
  objects_to_return[[8]] <- consent
  objects_to_return[[9]] <- program
  objects_to_return[[10]] <- parenting_survey
  return(objects_to_return)
}

updated_data <- update_data()
df <- updated_data[[1]]
supportive_calm_flow <- updated_data[[2]]
supportive_praise_flow <- updated_data[[3]]
check_in_flow_names_flow <- updated_data[[4]]
content_tip_flow_names_flow <- updated_data[[5]]
supportive_flow_names_flow <- updated_data[[6]]
enrolled <- updated_data[[7]]
consent <- updated_data[[8]]
program <- updated_data[[9]]
parenting_survey <- updated_data[[10]]

# retention_exit ---------------------------------------------------------------
# number of contacts for which the contact field "exit_message" is not empty &
# they are NOT in the group "in program"
#df %>% filter(program == "No") %>% nrow(.)
#contacts_unflat$fields$exit_message

# Define UI
ui <- dashboardPage(
  header = dashboardHeader(title = "ParentText Dashboard"),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Engagement", tabName = "engagement", icon = icon("clipboard")),
      menuItem("Behaviours", tabName = "behaviours", icon = icon("brain"))
    )),
  dashboardBody(
    fluidRow(
      shinydashboard::valueBoxOutput("myvaluebox1", width=2),
      shinydashboard::valueBoxOutput("myvaluebox2", width=2),
      shinydashboard::valueBoxOutput("myvaluebox3", width=2),
      shinydashboard::valueBoxOutput("myvaluebox4", width=2),
      shinydashboard::valueBoxOutput("myvaluebox5", width=2)),
    tabItems(
      # First tab content
      
      tabItem(tabName = "demographics",
              fluidRow(
                column(10, align = "centre",
                       box(splitLayout(h2("Demographics"), icon("users", "fa-6x"),
                                       cellArgs = list(style = "vertical-align: top"),
                                       cellWidths = c("80%", "20%")),
                           width = 10,
                           title = NULL,
                           collapsible = FALSE,
                           solidHeader = TRUE,
                           background = "light-blue",
                           height = "95px"))),
              tabsetPanel(type = "tabs",
                          tabPanel("Overall",
                                   fluidRow(column(12,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  box( width=NULL,
                                                                       collapsible = FALSE,
                                                                       title = "Consent Frequency",
                                                                       status = "primary", # primary, success, info, warning, danger
                                                                       solidHeader = TRUE,
                                                                       plotlyOutput(outputId = "plot_consent", height = "240"),
                                                                       shiny::tableOutput("consent_summary")),
                                                                  box( width=NULL,
                                                                       title = "Enrollment Frequency",
                                                                       status = "primary",
                                                                       solidHeader = TRUE,
                                                                       collapsible = FALSE,
                                                                       plotlyOutput(outputId = "plot_category", height = "240", width = "100%"),
                                                                       shiny::tableOutput("enrolled_summary")
                                                                  ),
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top"))),
                                                         width = 10),
                                                       
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Parent Demographics",
                                                                      status = "primary", # primary, success, info, warning, danger
                                                                      solidHeader = TRUE,
                                                                      shiny::tableOutput("parent_gender_summary"),
                                                                      shiny::tableOutput("parent_age_summary"),
                                                                      shiny::tableOutput("parent_child_relationship_summary"),
                                                                      shiny::tableOutput("parent_relationship_status_summary"),
                                                                  ),
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Child Demographics",
                                                                      status = "primary", # primary, success, info, warning, danger
                                                                      solidHeader = TRUE,
                                                                      shiny::tableOutput("child_gender_summary"),
                                                                      shiny::tableOutput("child_age_summary"),
                                                                      shiny::tableOutput("child_living_with_disabilities_summary"),
                                                                      
                                                                  ), # close child box
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top"))),
                                                         width = 10), # fluid row close
                                                       fluidRow(
                                                         column(12,
                                                                box( height="300px",  width=12,
                                                                     collapsible = FALSE,
                                                                     title = "Parenting Goals",
                                                                     status = "primary", # primary, success, info, warning, danger
                                                                     solidHeader = TRUE,
                                                                     plotlyOutput(outputId = "parenting_goals_plot", height = "240", width = "100%")
                                                                )
                                                         )
                                                       ) # close fluid row
                                                   )))), # close box, col, fluid row, tab panel
                          tabPanel("By group",
                                   fluidRow(column(12,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       fluidRow(
                                                         column(
                                                           width = 12,
                                                           #align = "center",
                                                           fluidRow(
                                                             column(6, uiOutput("groups")),
                                                           ) #fluid row closure
                                                         ) #Outer column closure
                                                       ),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Consent Frequency",
                                                                      status = "primary", # primary, success, info, warning, danger
                                                                      solidHeader = TRUE,
                                                                      plotlyOutput(outputId = "plot_consent_group", height = "240"),
                                                                      shiny::tableOutput("consent_summary_group")),
                                                                  box(width=NULL,
                                                                      title = "Enrollment Frequency",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      collapsible = FALSE,
                                                                      plotlyOutput(outputId = "plot_category_group", height = "240", width = "100%"),
                                                                      shiny::tableOutput("enrolled_summary_group")
                                                                  ),
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top"))),
                                                         width = 10),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Parent Demographics",
                                                                      status = "primary", # primary, success, info, warning, danger
                                                                      solidHeader = TRUE,
                                                                      shiny::tableOutput("parent_gender_group_summary"),
                                                                      shiny::tableOutput("parent_age_group_summary"),
                                                                      shiny::tableOutput("parent_child_relationship_group_summary"),
                                                                      shiny::tableOutput("parent_relationship_status_group_summary"),
                                                                  ),
                                                                  
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Child Demographics",
                                                                      status = "primary", # primary, success, info, warning, danger
                                                                      solidHeader = TRUE,
                                                                                  shiny::tableOutput("child_gender_group_summary"),
                                                                                  shiny::tableOutput("child_age_group_summary"),
                                                                                  shiny::tableOutput("child_living_with_disabilities_group_summary"),

                                                                  ), # close child box
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top")), # split layout for parent to child demographics close
                                                                fluidRow(
                                                                  column(12,
                                                                         box( height="300px",  width=12,
                                                                              collapsible = FALSE,
                                                                              title = "Parenting Goals",
                                                                              status = "primary", # primary, success, info, warning, danger
                                                                              solidHeader = TRUE,
                                                                              plotlyOutput(outputId = "parenting_goals_group_plot", height = "240", width = "100%")
                                                                         )
                                                                  )
                                                                ) # close fluid row
                                                         )
                                                       )
                                                   )))) # close box, col, fluid row, tab panel
              ) # close tabset panel
      ), # close tab
      
      # Second tab content
      tabItem(tabName = "engagement",
              fluidRow(
                column(10, align = "centre",
                       box(splitLayout(h2("Engagement"), icon("clipboard", "fa-6x"),
                                       cellArgs = list(style = "vertical-align: top"),
                                       cellWidths = c("80%", "20%")),
                           width = 10,
                           title = NULL,
                           collapsible = FALSE,
                           solidHeader = TRUE,
                           background = "light-blue",
                           height = "95px"))),
              tabsetPanel(type = "tabs",
                          tabPanel("Overall",
                                   fluidRow(column(10,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  shiny::tableOutput("active_users_24hr_summary"),
                                                                  shiny::tableOutput("active_users_7d_summary"),
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top"))),
                                                         width = 10),
                                                       br(),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                plotlyOutput(outputId = "last_online_plot"),
                                                         )),
                                                       br(),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                shiny::tableOutput("comp_prog_summary"),
                                                                shiny::tableOutput("completed_survey_summary"),
                                                                shiny::tableOutput("all_flows_response"))
                                                         ),
                                                       fluidRow(
                                                         column(12,
                                                                box( height="300px",  width=12,
                                                                     collapsible = FALSE,
                                                                     title = "Plot of Flows",
                                                                     status = "primary", # primary, success, info, warning, danger
                                                                     solidHeader = TRUE,
                                                                     plotlyOutput(outputId = "plot_flow", height = "240", width = "100%")
                                                                )
                                                         )
                                                       ) # close fluid row
                                                   ))) # close box, col, fluid row
                          ), # close Overall tab
                          tabPanel("By group",
                                   fluidRow(column(10,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       
                                                       fluidRow(
                                                         column(6,
                                                                uiOutput("groups_engagement")),
                                                       ), #fluid row closure
                                                       
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  shiny::tableOutput("active_users_24hr_group_summary"),
                                                                  shiny::tableOutput("active_users_7d_group_summary"),
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top"))),
                                                         width = 10),
                                                       br(),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                plotlyOutput(outputId = "last_online_group_plot"),
                                                         )),
                                                       br(),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                shinydashboard::valueBoxOutput("comp_prog_group_summary", width = 12),
                                                                cellArgs = list(style = "vertical-align: top")))
                                                   )))
                          ) # close by group tab
              ) # close tab type
      ), # close engagement tab
      
      tabItem(tabName = "behaviours",
              fluidRow(
                column(10, align = "centre",
                       box(splitLayout(h2("Behaviours"), icon("brain", "fa-6x"),
                                       cellArgs = list(style = "vertical-align: top"),
                                       cellWidths = c("80%", "20%")),
                           width = 10,
                           title = NULL,
                           collapsible = FALSE,
                           solidHeader = TRUE,
                           background = "light-blue",
                           height = "95px"))),
              tabsetPanel(type = "tabs",
                          tabPanel("Overall",
                                   fluidRow(column(10,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       fluidRow(column(6, uiOutput("groups_survey"))),
                                                       fluidRow(
                                                         column(12, align = "center",
                                                                #splitLayout(
                                                                  shiny::tableOutput("parenting_survey_summary"),
                                                                  plotlyOutput(outputId = "parenting_survey_plot"),#, height = "580px")
                                                                 # cellWidths = c("50%", "50%"),
                                                                #  cellArgs = list(style = "vertical-align: top"))
                                                                )
                                                                ),
                                                       br(),
                                                       
                                                       
                                                       fluidRow(
                                                         column(12,
                                                                box(width=12,
                                                                    collapsible = FALSE,
                                                                    title = "Behaviour Problems",
                                                                    status = "primary", # primary, success, info, warning, danger
                                                                    solidHeader = TRUE,
                                                                    splitLayout(
                                                                      plotlyOutput("behaviour_baby_plot"),
                                                                      plotlyOutput("behaviour_child_plot"),
                                                                      cellWidths = c("50%", "50%"),
                                                                      cellArgs = list(style = "vertical-align: top")),
                                                                    splitLayout(
                                                                      plotlyOutput("behaviour_teen_plot"),
                                                                      plotlyOutput("behaviour_default_plot"),
                                                                      cellWidths = c("50%", "50%"),
                                                                      cellArgs = list(style = "vertical-align: top"))
                                                                )
                                                         )
                                                       ) # close fluid row
                                                   )))), # close box, column, fluid row, tab
                          tabPanel("By group",
                                   fluidRow(column(10,
                                                   box(
                                                     fluidRow(
                                                       column(6,
                                                              uiOutput("groups_behaviour")),
                                                     ), #fluid row closure
                                                     splitLayout(
                                                       plotlyOutput("behaviour_baby_group_plot"),
                                                       plotlyOutput("behaviour_child_group_plot"),
                                                       cellWidths = c("50%", "50%"),
                                                       cellArgs = list(style = "vertical-align: top")),
                                                     footer = splitLayout(
                                                       plotlyOutput(outputId = "behaviour_teen_group_plot"),
                                                       plotlyOutput(outputId = "behaviour_default_group_plot"),
                                                       cellWidths = c("50%", "50%"),
                                                       cellArgs = list(style = "vertical-align: top")
                                                     ),
                                                     width = 12,
                                                     solidHeader = TRUE,
                                                     status = "primary", # primary, success, info, warning, danger
                                                     title = "Behaviour Problems"
                                                   )))) # close box, column, fluid row, tab
              ) # close tabsetPanel
      ) # close behaviour tab
    ) # close items
  ) # close body
) # close function

# Define server function
server <- function(input, output) {
  
  autoRefresh <- reactiveTimer(6 * 60 * 60 * 1000)
  
  observe({
    autoRefresh()
    
    updated_data <- update_data()
    df <- updated_data[[1]]
    supportive_calm_flow <- updated_data[[2]]
    supportive_praise_flow <- updated_data[[3]]
    check_in_flow_names_flow <- updated_data[[4]]
    content_tip_flow_names_flow <- updated_data[[5]]
    supportive_flow_names_flow <- updated_data[[6]]
    enrolled <- updated_data[[7]]
    parenting_survey <- updated_data[[10]]
  })
  
  updated_data <- update_data()
  df <- updated_data[[1]]
  supportive_calm_flow <- updated_data[[2]]
  supportive_praise_flow <- updated_data[[3]]
  check_in_flow_names_flow <- updated_data[[4]]
  content_tip_flow_names_flow <- updated_data[[5]]
  supportive_flow_names_flow <- updated_data[[6]]
  enrolled <- updated_data[[7]]
  consent <- updated_data[[8]]
  program <- updated_data[[9]]
  parenting_survey <- updated_data[[10]]
  
  # Subset data
  selected_data <- reactive({
    df
  })
  
  output$groups <- renderUI({
    df <- df
    selectInput(
      inputId = "grouper",
      label = "Group variable",
      choices = c("Parent Gender" = "parent_gender",
                  "Child Gender" = "child_gender",
                  "Child Age Group" = "child_age_group"),
      selected = "parent_gender"
    )
  })
  
  output$groups_engagement <- renderUI({
    df <- df
    selectInput(
      inputId = "grouper_engagement",
      label = "Group variable",
      choices = c("Parent Gender" = "parent_gender",
                  "Child Gender" = "child_gender",
                  "Child Age Group" = "child_age_group"),
      selected = "parent_gender"
    )
  })
  
  output$groups_behaviour <- renderUI({
    df <- df
    selectInput(
      inputId = "grouper_behaviour",
      label = "Group variable",
      choices = c("Parent Gender" = "parent_gender",
                  "Child Gender" = "child_gender",
                  "Child Age Group" = "child_age_group"),
      selected = "parent_gender"
    )
  })
  
  output$groups_survey <- renderUI({
    df <- df
    selectInput(
      inputId = "grouper_survey",
      label = "Survey to View",
      choices = c("Baseline" = "Base",
                  "Survey 2" = "2",
                  "Survey 3" = "3",
                  "Survey 4" = "4",
                  "Survey 5" = "5",
                  "Survey 6" = "6",
                  "Survey 7" = "7",
                  "Survey 8" = "8",
                  "Survey 9" = "9"),
      selected = "Base"
    )
  })
  
  observeEvent(input$groupby, {
    if(input$groupby){
      shinyjs::enable("grouper")
    }else{
      shinyjs::disable("grouper")
    }
  })
  
  enrolled_summary_group <- reactive({
    req(input$grouper)
    enrolled_table <- summary_PT(df, c(enrolled, (!!!rlang::syms(input$grouper))), naming_convention = TRUE)
    names(enrolled_table)[length(enrolled_table)] <- "Count"
    enrolled_table
  })
  
  enrolled_summary <- reactive({
    enrolled_table <- summary_PT(df, enrolled, naming_convention = TRUE)
    names(enrolled_table)[2] <- "Count"
    enrolled_table
  })
  
  output$plot_category <- renderPlotly({
    ggplot(df, aes(x = enrolled)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Enrolled", y = "Count") +
      theme_classic()
  })
  
  output$plot_category_group <- renderPlotly({
    req(input$grouper)
    ggplot(df, aes(x = enrolled, fill = (!!!rlang::syms(input$grouper)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Enrolled", y = "Count") +
      theme_classic()
  })
  
  output$last_online_plot <- renderPlotly({
    ggplot(df, aes(x = last_online)) +
      geom_point(stat="count") +
      geom_line(stat = "count") +
      labs(x = "Date Last Online", y = "Frequency") +
      viridis::scale_colour_viridis(discrete = TRUE, na.value = "navy") +
      theme_classic()
  })
  
  output$last_online_group_plot <- renderPlotly({
    req(input$grouper_engagement)
    ggplot(df, aes(x = last_online, colour = (!!!rlang::syms(input$grouper_engagement)))) +
      geom_point(stat="count") +
      geom_line(stat = "count") +
      labs(x = "Date Last Online", y = "Frequency") +
      viridis::scale_colour_viridis(discrete = TRUE, na.value = "navy") +
      theme_classic()
  })
  
  output$plot_consent <- renderPlotly({
    ggplot(df, aes(x = consent)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Consent", y = "Count") +
      theme_classic()
  })
  
  output$plot_consent_group <- renderPlotly({
    req(input$grouper)
    ggplot(df, aes(x = consent, fill = (!!!rlang::syms(input$grouper)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Consented", y = "Count") +
      theme_classic()
  })
  
  output$parenting_goals_plot <- renderPlotly({
    df_goals <- df %>% group_by(parenting_goals_wrap) %>% summarise(n = n())
    
    fig <- plot_ly(df_goals, labels = ~parenting_goals_wrap, values = ~n, type = 'pie')
    fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$parenting_goals_group_plot <- renderPlotly({
    req(input$grouper)
    ggplot(df, aes(x = parenting_goals_wrap, fill = (!!!rlang::syms(input$grouper)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Parenting goals", y = "Count") +
      theme_classic()
  })
  
  
  consent_summary <- reactive({
    summary_PT(df, "consent", enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  
  consent_summary_group <- reactive({
    req(input$grouper)
    summary_PT(df, c(consent, !!!rlang::syms(input$grouper)), enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_gender_summary <- reactive({
    summary_PT(df, parent_gender, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_gender_group_summary <- reactive({
    summary_PT(df, c(parent_gender, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  child_gender_summary <- reactive({
    summary_PT(df, child_gender, enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  
  child_gender_group_summary <- reactive({
    req(input$grouper)
    summary_PT(df, c(child_gender, (!!!rlang::syms(input$grouper))), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  child_age_summary <- reactive({
    summary_PT(df, child_age_group, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  child_age_group_summary <- reactive({
    req(input$grouper)
    summary_PT(df, c(child_age_group, (!!!rlang::syms(input$grouper))), enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  parent_child_relationship_summary <- reactive({
    summary_PT(df, parent_child_relationship, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  parent_child_relationship_group_summary <- reactive({
    req(input$grouper)
    summary_PT(df, c(parent_child_relationship, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  child_living_with_disabilities_summary <- reactive({
    summary_PT(df, child_living_with_disabilities, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  child_living_with_disabilities_group_summary <- reactive({
    req(input$grouper)
    summary_PT(df, c(child_living_with_disabilities, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  parent_relationship_status_summary <-  reactive({
    summary_PT(df, parent_relationship_status, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  parent_relationship_status_group_summary <-  reactive({
    summary_PT(df, c(parent_relationship_status, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  active_users_24hr_group_summary <- reactive({
    req(input$grouper_engagement)
    summary_PT(df, c(active_users_24hr, !!!rlang::syms(input$grouper_engagement)), program, together = TRUE, naming_convention = TRUE)
  })
  active_users_24hr_summary <- reactive({
    summary_PT(df, active_users_24hr, program, together = TRUE, naming_convention = TRUE)
  })
  active_users_7d_summary <- reactive({
    summary_PT(df, active_users_7d, program, together = TRUE, naming_convention = TRUE)
  })
  active_users_7d_group_summary <- reactive({
    req(input$grouper_engagement)
    summary_PT(df, c(active_users_7d, !!!rlang::syms(input$grouper_engagement)), program, together = TRUE, naming_convention = TRUE)
  })

  comp_prog_summary <- reactive({
    comp_prog_df <- df %>% 
      filter(consent == "Yes") %>%
      summarise(program_completion_mean = round(mean(n_skills, na.rm = TRUE), 2),
                program_completion_sd = round(sd(n_skills, na.rm = TRUE), 2))
    colnames(comp_prog_df) <- naming_conventions(colnames(comp_prog_df))
    comp_prog_df
  })
  
  comp_prog_group_summary <- reactive({
    req(input$grouper_engagement)
    comp_prog_df <- df %>% group_by(!!!rlang::syms(input$grouper_engagement)) %>%
      filter(consent == "Yes") %>%
      summarise(program_completion_mean = round(mean(n_skills, na.rm = TRUE), 2),
                program_completion_sd = round(sd(n_skills, na.rm = TRUE), 2))
    colnames(comp_prog_df) <- naming_conventions(colnames(comp_prog_df))
    comp_prog_df
  })
  
  parent_age_summary <- reactive({
    req(input$grouper)
    parent_age_df <- df %>% 
      filter(consent == "Yes") %>%
      summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    parent_age_df
  })
  parent_age_group_summary <- reactive({
    req(input$grouper)
    parent_age_df <- df %>% group_by(!!!rlang::syms(input$grouper)) %>%
      filter(consent == "Yes") %>%
      summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    parent_age_df
  })
  # Note: These are the *number* of people that have completed the survey
  completed_survey_summary <- reactive({
    df_consent <- df %>%
      filter(consent == "Yes")
    survey_completed <- NULL
    survey_completed[[1]] <- df_consent %>% summarise(n = sum(survey_completed_wk1 == 1, na.rm = TRUE))
    survey_completed[[1]]$perc <- survey_completed[[1]]$n/nrow(df_consent) * 100
    survey_completed[[1]]$Week <- "Week 1"
    for (i in 2:9){
      survey_completed[[i]] <- df_consent %>% summarise(n = sum(survey_completed_wk2_plus == i, na.rm = TRUE))
      survey_completed[[i]]$perc <- survey_completed[[i]]$n/nrow(df_consent) * 100
      survey_completed[[i]]$Week <- paste("Week ", i, sep = "")
    }
    survey_completed <- plyr::ldply(survey_completed)
    
    survey_completed <- survey_completed %>%
      mutate("Completed survey (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
      dplyr::select(-c(n, perc))
    
    pivot_wider(survey_completed, names_from = Week, values_from = `Completed survey (%)`)
    
  })
  
  completed_survey_group_summary <- reactive({
    req(input$grouper)
    df_consent <- df %>%
      filter(consent == "Yes")
    survey_completed <- NULL
    survey_completed[[1]] <- df_consent %>% group_by(!!!rlang::syms(input$grouper)) %>% summarise(n = sum(survey_completed_wk1 == 1, na.rm = TRUE))
    survey_completed[[1]]$perc <- survey_completed[[1]]$n/nrow(df_consent) * 100
    survey_completed[[1]]$Week <- "Week 1"
    for (i in 2:9){
      survey_completed[[i]] <- df_consent %>% group_by(!!!rlang::syms(input$grouper)) %>% summarise(n = sum(survey_completed_wk2_plus == i, na.rm = TRUE))
      survey_completed[[i]]$perc <- survey_completed[[i]]$n/nrow(df_consent) * 100
      survey_completed[[i]]$Week <- paste("Week ", i, sep = "")
    }
    survey_completed <- plyr::ldply(survey_completed)

    survey_completed <- survey_completed %>%
      mutate("Completed survey (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
      dplyr::select(-c(n, perc))
    
    pivot_wider(survey_completed, names_from = Week, values_from = `Completed survey (%)`)
  })
  
  supportive_calm_flow_df <- flow_data_summary_function(supportive_calm_flow)
  #if (is.null(supportive_calm_flow_df)) { supportive_calm_flow_df <- data.frame(response = c("No", "Yes"), count = c(NA, NA)); colnames(supportive_calm_flow_df)[2] <- "Count (%)"}
  supportive_praise_flow_df <- flow_data_summary_function(supportive_praise_flow)
  #if (is.null(supportive_praise_flow_df)) { supportive_praise_flow_df <- data.frame(response = c("No", "Yes"), count = c(NA, NA)); colnames(supportive_praise_flow_df)[2] <- "Count (%)"}
  supportive_flow_df <- flow_data_summary_function(supportive_flow_names_flow)
  check_in_flow_df <- flow_data_summary_function(check_in_flow_names_flow)
  content_flow_df <- flow_data_summary_function(content_tip_flow_names_flow)
  
  all_flows_response <- reactive({
    all_flows_df <- rbind(content_flow_df, check_in_flow_df, supportive_calm_flow_df, supportive_praise_flow_df, supportive_flow_df)
    all_flows_df <- separate(all_flows_df, `Count (%)`, into = "Value") %>% mutate(Value = as.numeric(as.character(Value)))
    all_flows_df_total <- sum(all_flows_df$Value, na.rm = TRUE) 
    all_flows_df_summary <- all_flows_df %>% group_by(response) %>% summarise(n = sum(Value, na.rm = TRUE),
                                                                              perc = n/all_flows_df_total*100)
    all_flows_df_summary <- all_flows_df_summary %>%
      mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
      dplyr::select(-c(n, perc))
    colnames(all_flows_df_summary)[2] <- c("Overall response")
    all_flows_df_summary <- all_flows_df_summary %>% map_df(rev)
    
    colnames(content_flow_df)[2] <- "Content flows"
    colnames(check_in_flow_df)[2] <- "Check-in flows"
    colnames(supportive_calm_flow_df)[2] <- "Calm flow"
    colnames(supportive_praise_flow_df)[2] <- "Praise flow"
    colnames(supportive_flow_df)[2] <- "Other supportive flows"
    
    table_flows_df <- left_join(left_join(left_join(left_join(left_join(content_flow_df, check_in_flow_df), supportive_calm_flow_df), supportive_praise_flow_df), supportive_flow_df), all_flows_df_summary)
    colnames(table_flows_df)[1] <- "Response"
    table_flows_df
  })
  
  # Survey stuff -----------------------------------------------------------------------
  
  parenting_survey_summary <- reactive({
    req(input$grouper_survey)
    
    parenting_survey %>%
      dplyr::filter(week == input$grouper_survey) %>%
      group_by(week, Group) %>%
      summarise(Mean = mean(vals, na.rm = TRUE), SD = sd(vals, na.rm = TRUE), Sum = sum(vals, na.rm = TRUE)) %>%
      mutate(Scale = ifelse(Group %in% c("Food insecurity", "Sexual abuse prevention"), "0-30", "0-7")) %>%
      mutate(Mean = round(Mean, 2),
             SD = round(SD, 2),
             Sum = round(Sum, 0))
  })
  
  output$parenting_survey_plot <- renderPlotly({
    parenting_survey1 <- parenting_survey %>% mutate(week = as.numeric(week)) %>% filter(Group %in% c("Positive parenting", "Child maltreatment", "Stress", "Child Behaviour"))
    parenting_survey_plot <- summarySE(parenting_survey1, groups = c(week, Group), var = vals, na.rm = TRUE)
    ggplot(parenting_survey_plot, aes(x=week, y=mean, colour=Group, group = Group), width = 2) + 
      geom_line() +
      geom_point(data = parenting_survey_plot, aes(size = N)) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
      viridis::scale_color_viridis(discrete = TRUE) +
      labs(x = "Survey", y = "Frequency", title = "Survey Responses with (SE) Error Bars") +
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),
                         labels=c("Baseline", "2", "3", "4", "5", "6", "7", "8", "9")) +
      theme_classic()
  })
  
  output$behaviour_baby_plot <- renderPlotly({
    df_baby <- df %>% filter(child_age_group == "Baby")
    
    ggplot(df_baby, aes(x = challenging_type_wrap)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
  })
  
  output$behaviour_child_plot <- renderPlotly({
    df_child <- df %>% filter(child_age_group == "Child")
    
    ggplot(df_child, aes(x = challenging_type_wrap)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
  })
  
  output$behaviour_teen_plot <- renderPlotly({
    df_teen <- df %>% filter(child_age_group == "Teen")
    
    ggplot(df_teen, aes(x = challenging_type_wrap)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
  })
  
  output$behaviour_default_plot <- renderPlotly({
    df_default <- df %>% filter(child_age_group == "Default")
    
    ggplot(df_default, aes(x = challenging_type_wrap)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
  })
  
  output$behaviour_baby_group_plot <- renderPlotly({
    req(input$grouper_behaviour)
    df_baby <- df %>% filter(child_age_group == "Baby")
    
    ggplot(df_baby, aes(x = challenging_type_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
    
  })
  
  output$behaviour_child_group_plot <- renderPlotly({
    req(input$grouper_behaviour)
    df_child <- df %>% filter(child_age_group == "Child")
    
    ggplot(df_child, aes(x = challenging_type_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
    
  })
  
  output$behaviour_teen_group_plot <- renderPlotly({
    req(input$grouper_behaviour)
    df_teen <- df %>% filter(child_age_group == "Teen")
    
    ggplot(df_teen, aes(x = challenging_type_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
    
  })
  
  output$behaviour_default_group_plot <- renderPlotly({
    req(input$grouper_behaviour)
    df_default <- df %>% filter(child_age_group == "Default")
    
    ggplot(df_default, aes(x = challenging_type_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = NULL, y = NULL) +
      facet_grid(cols = vars(child_age_group)) +
      theme_classic()
  })
  
  # Flows stuff ------------------------------------------------------------
  
  output$plot_flow <- renderPlotly({
    colnames(supportive_calm_flow_df)[2] <- "Supportive - Calm flow"
    colnames(supportive_praise_flow_df)[2] <- "Supportive - Praise flow"
    colnames(supportive_flow_df)[2] <- "Supportive flow"
    colnames(check_in_flow_df)[2] <- "Check-in flows"
    colnames(content_flow_df)[2] <- "Content flows"
    table_flows_df <- left_join(left_join(left_join(left_join(supportive_calm_flow_df, supportive_praise_flow_df), supportive_flow_df), check_in_flow_df), content_flow_df)
    colnames(table_flows_df)[1] <- "Response"
    flow_pivot <- pivot_longer(table_flows_df,
                               cols = c(`Supportive - Calm flow`, `Supportive - Praise flow`, `Supportive flow`, `Check-in flows`, `Content flows`),
                               names_to = "Flow name",
                               values_to = "Value") %>%
      separate(Value, into = "Value") %>%
      mutate(Value = as.numeric(as.character(Value)))
    
    ggplot(flow_pivot, aes(x = Response, y = Value, fill = `Flow name`)) +
      geom_bar(stat = "identity") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      theme_classic()
  })
  
  # Output render ------------------------------------------------------------
  
  df_enrolled <- summary_PT(df,  enrolled,  enrolled, "Yes")
  df_enrolled <- df_enrolled %>% mutate(group =  enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
  
  df_consented <- summary_PT(df,  consent,  consent, "Yes")
  df_consented <- df_consented %>% mutate(group =  consent, count = consent_n) %>% dplyr::select(c(group, count))
  
  df_program <- summary_PT(df,  program,  program, "Yes")
  df_program <- df_program %>% mutate(group =  program, count = program_n) %>% dplyr::select(c(group, count))
  
  df_active_24 <- (summary_PT(df, active_users_24hr, program) %>% filter(active_users_24hr == "Yes"))$active_users_24hr_n
  df_active_7d <- (summary_PT(df, active_users_7d, program) %>% filter(active_users_7d == "Yes"))$active_users_7d_n
  
  output$myvaluebox1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_enrolled$count[1], subtitle = "Enrolled", icon = icon("user"),
                             color = "aqua"
    )
  })
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_consented$count[1],subtitle = "Consented",icon = icon("check"),
                             color = "green"
    )
  })
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_program$count[1],subtitle = "In Program",icon = icon("clipboard"),
                             color = "yellow"
    )
  })
  output$myvaluebox4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_active_24,subtitle = "Active in last 24 hours",icon = icon("clock"),
                             color = "purple"
    )
  })
  output$myvaluebox5 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_active_7d, subtitle = "Active in last 7 days", icon = icon("signal"),
                             color = "fuchsia"
    )
  })
  output$enrolled_summary <- shiny::renderTable({(enrolled_summary())}, striped = TRUE)
  output$enrolled_summary_group <- shiny::renderTable({(enrolled_summary_group())}, striped = TRUE)
  output$consent_summary <- shiny::renderTable({(consent_summary())}, striped = TRUE)
  output$consent_summary_group <- shiny::renderTable({(consent_summary_group())}, striped = TRUE)
  output$parent_gender_summary <- shiny::renderTable({(parent_gender_summary())}, striped = TRUE)
  output$parent_gender_group_summary <- shiny::renderTable({(parent_gender_group_summary())}, striped = TRUE)
  output$parent_age_summary <- shiny::renderTable({(parent_age_summary())}, striped = TRUE)
  output$parent_age_group_summary <- shiny::renderTable({(parent_age_group_summary())}, striped = TRUE)
  output$child_gender_summary <- shiny::renderTable({(child_gender_summary())}, striped = TRUE)
  output$child_gender_group_summary <- shiny::renderTable({(child_gender_group_summary())}, striped = TRUE)
  output$child_age_summary <- shiny::renderTable({(child_age_summary())}, striped = TRUE)
  output$child_age_group_summary <- shiny::renderTable({(child_age_group_summary())}, striped = TRUE)
  output$parent_child_relationship_summary <- shiny::renderTable({(parent_child_relationship_summary())}, caption = "Relationship between the parent and child", striped = TRUE)
  output$parent_child_relationship_group_summary <- shiny::renderTable({(parent_child_relationship_group_summary())}, caption = "Relationship between the parent and child", striped = TRUE)
  output$parent_relationship_status_group_summary <- shiny::renderTable({(parent_relationship_status_group_summary())}, caption = "Relationship status of the parent", striped = TRUE)
  output$parent_relationship_status_summary <- shiny::renderTable({(parent_relationship_status_summary())}, caption = "Relationship status of the parent", striped = TRUE)
  output$child_living_with_disabilities_summary <- shiny::renderTable({(child_living_with_disabilities_summary())}, caption = "Does the child have a disability?", striped = TRUE)
  output$child_living_with_disabilities_group_summary <- shiny::renderTable({(child_living_with_disabilities_group_summary())}, caption = "Does the child have a disability?", striped = TRUE)
  output$active_users_24hr_summary <- shiny::renderTable({(active_users_24hr_summary())}, striped = TRUE)
  output$active_users_24hr_group_summary <- shiny::renderTable({(active_users_24hr_group_summary())}, striped = TRUE)
  output$active_users_7d_summary <- shiny::renderTable({(active_users_7d_summary())}, striped = TRUE)
  output$active_users_7d_group_summary <- shiny::renderTable({(active_users_7d_group_summary())}, striped = TRUE)
  output$comp_prog_summary <- shiny::renderTable({(comp_prog_summary())}, caption = "Number of skills in toolkit", striped = TRUE)
  output$comp_prog_group_summary <- shiny::renderTable({(comp_prog_group_summary())}, caption = "Number of skills in toolkit", striped = TRUE)
  output$completed_survey_summary <- shiny::renderTable({{completed_survey_summary()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have completed different surveys")
  output$completed_survey_group_summary <- shiny::renderTable({{completed_survey_group_summary()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have completed different surveys")
  output$all_flows_response <- shiny::renderTable({(all_flows_response())}, caption = "Count (%) for each flow", striped = TRUE)
  output$parenting_survey_summary <- shiny::renderTable({(parenting_survey_summary())}, caption = "How many times in the past week ... \n For Sexual abuse prevention, the timeframe is how many days in the past month.", striped = TRUE)
}

# Create Shiny object
shinyApp(ui = ui, server = server)
