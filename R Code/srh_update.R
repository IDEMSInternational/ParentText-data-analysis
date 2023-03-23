# RapidPro set up --------------------------------------------------------------
update_data <- function(type = "SRH", date_from = "2021-10-14", date_to = NULL, include_archived_data = FALSE, consent_only = TRUE) {
  set_rapidpro_site(site = site)
  set_rapidpro_key(key = key[[1]])
  set_rapidpro_uuid_names()
  
  contacts_unflat <- get_user_data(call_type="contacts.json?joined=srh_user")
  
  created_on <- contacts_unflat$created_on
  did_not_consent <- contacts_unflat$fields$did_not_consent
  ID <- contacts_unflat$uuid
  last_online <- as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%d", tz = "UTC")
  
  enrolled <- NULL
  true_consent <- NULL
  program <- NULL
  if (length(contacts_unflat$groups) > 0){
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
  }
  
  group <- NULL
  for (i in 1:length(contacts_unflat$groups)){
    contact_name <- contacts_unflat$groups[[i]]
    if (length(contact_name)==0) {
      group[i] <- NA
    } else{
      group[i] <- ifelse(any(contact_name$name %in% "srh user"), "srh_user", "none")
    }
  }
  
  enrolled <- factor(enrolled)
  true_consent <- factor(true_consent)
  program <- factor(program)
  group <- factor(group)
  enrolled <- forcats::fct_expand(enrolled, c("Yes", "No"))
  true_consent <- forcats::fct_expand(true_consent, c("Yes", "No"))
  program <- forcats::fct_expand(program, c("Yes", "No"))
  enrolled <- forcats::fct_relevel(enrolled, c("Yes", "No"))
  true_consent <- forcats::fct_relevel(true_consent, c("Yes", "No"))
  program <- forcats::fct_relevel(program, c("Yes", "No"))
  
  #Show number of active users in the last 24 hours and 7 days (based on the last_seen_on variable)
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
  active_users_24_hrs <- forcats::fct_relevel(active_users, c("Yes", "No"))
  
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
  
  
  #Demographics (# of users for each level)
  #Fields.gender (levels: Male/Female/Non-binary/None)
  gender <- contacts_unflat$fields$gender
  gender <- factor(ifelse(gender %in% c("female", "f", "Female", "woman", "Woman"), "Woman",
                          ifelse(gender %in% c("male", "m", "Male", "man", "Man"), "Man",
                                 ifelse(gender %in% c("no", "B"), NA, gender))))
  gender <- fct_expand(gender, "Woman", "Man")
  gender <- forcats::fct_relevel(gender, c("Woman", "Man"))
  
  #Fields.age (define age groups? E.g. <14, 14-16, 17-18, 19-24, >24? Values to be checked with NFPB)
  age <- contacts_unflat$fields$age_range
  age <- fct_relevel(age, "Under 13", after = 0)
  
  #Contact level info  (# of users for each level):
  #  Urn type (whatsapp, facebook, instagram, sms, telegram)
  avatar <- factor(contacts_unflat$fields$avatar)
  urn <- unlist(contacts_unflat$urns)
  urn <- factor(gsub("\\:.*", "", urn))
  
  df <- data.frame(ID, group, created_on, true_consent, program, active_users_24_hrs, active_users_7_days, avatar, urn,
                   gender, age)
  
  # SRH Data Frame - Flows ----------------------------------------------------
  SRH_flow_names <- paste0("SRH - Answer - ", c("Menstruation", "Pregnancy", "Puberty", "STIs",
                                                "Gender", "Sexuality", "Abstinence", "Mental Health",
                                                "Violence & Abuse", "Healthy Relationships", "Parenting"))
  
  srh_data <- srh_table_output(flow_names = SRH_flow_names)
  #srh_data <- srh_data1
  #srh_data <- srh_table_output(all_flow_names = all_flow_names1)
  names(srh_data) <- SRH_flow_names
  
  # Consented ---------------------------------------------
  if (consent_only){
    if (length(contacts_unflat$groups) > 0){
      row <- 1:length(ID)
    } else {
      row <- NULL
      row <- factor(row)
    }
    df_created_on <- data.frame(ID, created_on, true_consent, program, row = row)
    consented_ind <- (df_created_on %>% filter(true_consent == TRUE))$ID
    
    df <- df %>% filter(true_consent %in% consented_ind)
    for (i in 1:length(srh_data)){
      if (!is.null(srh_data[[i]])){
        srh_data[[i]] <- srh_data[[i]] %>% filter(uuid %in% consented_ind)
      }
    }
  }
  
  # SRH Flow ---------------------------------------------
  srh_flow_freq <- flow_cat_frequency(table = srh_data)
  
  objects_to_return <- NULL
  objects_to_return[[1]] <- df
  objects_to_return[[2]] <- srh_data
  objects_to_return[[3]] <- srh_flow_freq
  objects_to_return[[4]] <- "0"
  return(objects_to_return)
}
