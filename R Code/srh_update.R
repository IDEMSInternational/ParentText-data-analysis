# RapidPro set up --------------------------------------------------------------
update_data <- function(type = "SRH", date_from = "2021-10-14", date_to = NULL, include_archived_data = FALSE, consent_only = TRUE) {
  set_rapidpro_site(site = site)
  set_rapidpro_key(key = key[[1]])
  set_rapidpro_uuid_names()
  
  # what is the UUID for the SRH user group?
  #x <- get_user_data(call_type="contacts.json")
  #plyr::ldply(x) %>% filter(name == "srh user")
  # can see the group ID.
  
  contacts_unflat <- get_user_data(call_type="contacts.json?group=4db95dd7-fb47-4394-9ff5-0e9f5bce2db5")
  # srh user: 4db95dd7-fb47-4394-9ff5-0e9f5bce2db5
  # srh completed registration: 19713029-0c5f-4e9c-a611-7329304068b6
  
  created_on <- contacts_unflat$created_on
  did_not_consent <- contacts_unflat$fields$did_not_consent
  ID <- contacts_unflat$uuid
  last_online <- as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%d", tz = "UTC")
  
  enrolled <- NULL
  program <- NULL
  if (length(contacts_unflat$groups) > 0){
    for (i in 1:length(contacts_unflat$groups)){
      contact_name <- contacts_unflat$groups[[i]]
      if (length(contact_name)==0) {
        enrolled[i] <- NA
        program[i] <- NA
      } else{
        enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
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
  program <- factor(program)
  group <- factor(group)
  enrolled <- forcats::fct_expand(enrolled, c("Yes", "No"))
  program <- forcats::fct_expand(program, c("Yes", "No"))
  enrolled <- forcats::fct_relevel(enrolled, c("Yes", "No"))
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
  urn <- unlist(contacts_unflat$urns) # error here
  urn <- factor(gsub("\\:.*", "", urn))
  
  # region
  region <- contacts_unflat$fields$region
  region <- gsub(".*> ","",region)
  
  # feedback text
  feedback_text <- contacts_unflat$fields$srh_feedback_text
  feedback_rate <- contacts_unflat$fields$srh_feedback_rate
  feedback_rate <- forcats::fct_relevel(feedback_rate, c("Absolutely love it!", "It was good",
                                                         "Could be better", "Did not help at all"))
  
  df <- data.frame(ID, group, created_on, program, active_users_24_hrs, active_users_7_days, avatar, urn,
                   gender, age, region, feedback_text, feedback_rate)
  
  # SRH Data Frame - Flows ----------------------------------------------------
  SRH_flow_names <- paste0("SRH - Answer - ", c("Menstruation", "Pregnancy", "Puberty", "STIs",
                                                "Gender", "Sexuality", "Abstinence", "Mental health",
                                                "Violence abuse", "Healthy relationships", "Contraceptives"))
  all_flow_names <- get_flow_names() %>% dplyr::select(c(name, uuid))
  
  # if the number of individuals is GREATER than the number of flows, run this instead:
  #fun2 <- function(all_flow_names, rapidpro_site, call_type, token){
    srh_data <- NULL
    for (i in SRH_flow_names){
      i_flow_names <- (all_flow_names %>% filter(grepl(i, name)))$uuid
      flow_uuid <- i_flow_names
      runs_unflat_i <- purrr::map(.x = i_flow_names,
                                  .f = ~get_user_data(call_type=paste0("runs.json?flow=", .x)))
      srh_data[[i]] <- bind_rows(runs_unflat_i, `.id` = "ID")
    }
  names(srh_data) <- SRH_flow_names
  #   return(srh_data)
  # }
  # system.time(fun2(all_flow_names, rapidpro_site, call_type, token))
  all_flow_data <- bind_rows(srh_data, `.id` = "ID")
  
  # if the number of flows is GREATER than the number of individuals, run this:
  
  # system.time for the other methodis:
  # system.time(get_user_data(call_type="runs.json?group=4db95dd7-fb47-4394-9ff5-0e9f5bce2db5",
  #                     flatten = TRUE))
  # # system.time for the purrr method is: 
  # #user  system elapsed 
  # #10.76    0.44  259.03 
  # runs_unflat <- purrr::map(.x = df$ID,
  #                           .f = ~get_user_data(call_type=paste0("runs.json?contact=", .x), flatten = TRUE))
  # all_flow_data <- bind_rows(runs_unflat, `.id` = "ID")
  # srh_data <- NULL
  # for (i in SRH_flow_names){   # SRH - Answer - Violence abuse, 
  #   i_flow_names <- (all_flow_names %>% filter(grepl(i, name)))$name
  #   srh_data[[i]] <- all_flow_data %>% filter(`flow.name` %in% i_flow_names) %>%
  #     dplyr::mutate(flow.name = naming_conventions(flow.name, replace = paste0(i, " - "))) %>%
  #     dplyr::select(c(flow = flow.name, uuid = contact.uuid, interacted = responded, created_on))
  # }
  
  #get_user_data(call_type=paste0("runs.json?contact=27cc8393-e8be-44bf-b964-cb1a4cddc007&ad9f75a6-e222-405e-9ff6-c8a44b3aba94"), flatten = TRUE)
  
  # SRH Flow ---------------------------------------------
  srh_flow_freq <- flow_cat_frequency(table = srh_data)
  
  # by UUID ----------------------------------------------
  table <- NULL
  SRH_names <- naming_conventions(SRH_flow_names, replace = "SRH - Answer - ")
  for (i in 1:length(srh_data)){
    if (is.null(srh_data[[i]])){
      table[[i]] <- data.frame("0", 0)
    } else {
      table[[i]] <- srh_data[[i]] %>% group_by(uuid) %>% summarise(n())
    }
    names(table[[i]]) <- c("uuid", SRH_names[i])
  }
  srh_by_uuid <- table[[1]]
  for (i in 2:length(table)){
    srh_by_uuid <- full_join(srh_by_uuid, table[[i]])
  }
  srh_by_uuid <- srh_by_uuid %>% filter(uuid != 0) %>% mutate(across(SRH_names, ~replace_na(.x, 0)))
  srh_by_uuid <- janitor::adorn_totals(srh_by_uuid, where = "both")
  
  objects_to_return <- NULL
  objects_to_return[[1]] <- df
  objects_to_return[[2]] <- srh_data
  objects_to_return[[3]] <- srh_flow_freq
  objects_to_return[[4]] <- srh_by_uuid
  return(objects_to_return)
}
