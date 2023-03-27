update_data <- function(){
  # Create the data sets ------------------------------------
  set_rapidpro_key(key[[1]])
  set_rapidpro_site(site)
  ticket_data <- get_data_from_rapidpro_api(call_type = "tickets.json",
                               date_from = NULL,
                               date_to = NULL,
                               flatten = TRUE)
  user_data <- get_user_data(date_from = NULL,
                             date_to = NULL)
  
  ticket_data$opened_on <- as.POSIXct(ticket_data$opened_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  ticket_data$closed_on <- as.POSIXct(ticket_data$closed_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  ticket_data$time_to_close <- difftime(ticket_data$closed_on, ticket_data$opened_on, units = "hours")
  
  ### Ticket Level ###
  # Gender of the contact (@fields.gender)
  # Parish of the contact (@fields.region)
  ticket_data <- ticket_data %>% dplyr::select(uuid, status, opened_on, closed_on, time_to_close,
                                               counsellor = assignee.name)
  ticket_data <- ticket_data %>%
    mutate(opened_on = as.Date(opened_on),
           closed_on = as.Date(closed_on))
  
  parent_gender <- user_data$fields$gender
  parent_gender <- factor(ifelse(parent_gender %in% c("female", "f", "woman", "Woman"), "Woman",
                                 ifelse(parent_gender %in% c("male", "m", "man", "Man", "Male"), "Man",
                                        ifelse(parent_gender %in% "no", NA, parent_gender))))
  parent_gender <- fct_expand(parent_gender, "Woman", "Man")
  parent_gender <- forcats::fct_relevel(parent_gender, c("Woman", "Man"))
  
  state_of_origin <- as.character(user_data$fields$state_of_origin)
  state_of_origin <- dplyr::recode(state_of_origin, "1" = state_1, "2" = state_2, "3" = state_3, "4" = state_4, "5" = state_5,
                                   "6" = state_6, "7" = state_7, "8" = state_8, "9" = state_9, "10" = state_10, "11" = state_11,
                                   "12" = state_12, "13" = state_13, "14" = state_14, "15" = state_15, "16" = state_16, "17" = state_17)
  user_data <- user_data %>%
    dplyr::select(uuid = uuid) %>%
    mutate(gender = parent_gender,
           parish = state_of_origin) %>%
    filter(uuid %in% ticket_data$uuid)
  ticket_data1 <- full_join(ticket_data, user_data)
  ticket_data$link <- paste0("https://rapidpro.ilhasoft.mobi/ticket/all/open/", ticket_data$uuid) 
  ticket_data$` ` <- NA
  # Counsellor Level ------------------------------

  # Counsellor/Ticket Level
  
  objects_to_return <- NULL
  objects_to_return[[1]] <- ticket_data
  objects_to_return[[2]] <- user_data
  return(objects_to_return)
}
