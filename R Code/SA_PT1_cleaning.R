library(httr)
library(jsonlite)
library(tidyverse)
#source("Functions.R")
#source("R Shiny Template.R")
# source("Code Book.R")
#install_github("lilyclements/rapidpror")
#library(rapidpror)
consent_only <- TRUE
# RapidPro set up --------------------------------------------------------------
update_data <- function(country = "Malaysia", date_from = "2021-10-14", date_to = NULL, include_archived_data = FALSE, consent_only = TRUE) {
    contacts_unflat <- import_list("South_Africa_Data_20230712.xlsx")

    df <- contacts_unflat$Demographics
    df$parenting_goal_wrap <- str_wrap_factor(df$parenting_goal, width = 15)
    df$challenge_behav_wrap <- str_wrap_factor(df$challenge_behav, width = 10)
    
    df$cens <- 1
    
    # true_consent, and ipv_version missing
    df_consent <- df %>% dplyr::select(c(ID, created_on, program, enrolled, language, consent,
                                         parent_gender, child_gender, child_age_group))
    
    all_flows <- contacts_unflat$`Full list of flows`
    #dplyr::bind_rows(contacts_unflat$`Praise flow`, contacts_unflat$`Calm flow`, contacts_unflat$`Supportive flow`,
    #                              contacts_unflat$`Check in flow`, contacts_unflat$`Content tip flow`)
    
    parenting_survey <- contacts_unflat$`Parenting Survey`
    parenting_survey <- parenting_survey %>% dplyr::select(-c(starts_with("date_")))
    parenting_survey <- parenting_survey %>%
      pivot_longer(cols = c("Positive parenting", "Child maltreatment", "Play", "Praise", "Stress",
                            "Physical abuse", "Psychological abuse", "Financial stress",
                            "Food insecurity", "Parenting efficacy", "Sexual abuse talk",
                            "Sexual abuse prevention", "Child Behaviour"),
                   values_to = "vals", names_to = "Group")
    
  objects_to_return <- NULL
  objects_to_return[[1]] <- df
  objects_to_return[[2]] <- df_consent
  objects_to_return[[3]] <- all_flows
  objects_to_return[[4]] <- parenting_survey
  return(objects_to_return)
}




x <- contacts_unflat$`Full list of flows` %>%
  filter(interacted == "Yes") %>%
  group_by(ID) %>% 
  summarise(max_time = as.Date(max(created_run_on)),
            created_on = max(created_on)) %>%
  mutate(exit_time = as.Date(max_time) - as.Date(created_on))

ggplot(x, aes(y = exit_time)) + geom_boxplot() +
  labs(y = "Length in Study (days)") +
  theme_bw()


View(contacts_unflat$Hook_Messages_draft)
