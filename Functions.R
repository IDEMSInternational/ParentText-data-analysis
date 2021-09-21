#' Interaction with api
#' Description: Make API call, specify query string, save data in dataframe

#' 1. Defining package environment -------------------------------------------
#' Sorting set and get calls for: key, site, uuid flow names
#' 
#' Define package environment
pkg.env <- new.env(parent = emptyenv())

# Set defaults values in the environment: key and site
assign("rapidpro_key", NULL, envir = pkg.env)
assign("rapidpro_site", NULL, envir = pkg.env)

# Functions to reset the values in the environment: key and site
set_rapidpro_key = function(key) {
  assign("rapidpro_key", key, envir = pkg.env)
}

set_rapidpro_site = function(site) {
  assign("rapidpro_site", site, envir = pkg.env)
}

# Functions to call the values in the environment: key and site
get_rapidpro_key = function() {
  get("rapidpro_key", envir = pkg.env)
}

get_rapidpro_site = function() {
  get("rapidpro_site", envir = pkg.env)
}

# Function to get a list of the UUID of flows
get_flow_names <- function(call_type="flows.json", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = TRUE){
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  flow_names <- results$results
  if (flatten){
    flow_names <- jsonlite::flatten(flow_names)
  }
  return(flow_names)
}

set_rapidpro_uuid_names = function(uuid_names = get_flow_names()){#[c("uuid", "name")]) {
  assign("rapidpro_uuid_names", uuid_names, envir = pkg.env)
}

get_rapidpro_uuid_names = function(){
  get("rapidpro_uuid_names", envir = pkg.env)
}

#' 2. Retreiving Information -------------------------------------------
#' Get general data
get_data_from_rapidpro_api <- function(call_type, rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = TRUE){
  # TODO put in checks - check site is correct, then token, then call_type
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  user_result <- results$results
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  return(user_result)
}

# Get user (contacts) data
get_user_data <- function(call_type="contacts.json", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = TRUE){
  # todo: checks/error handling messages.
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  user_result <- results$results
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  return(user_result)
}

# Get run (flow) data
get_flow_data <- function(uuid_data = get_rapidpro_uuid_names(), flow_name, result, call_type="runs.json?flow=", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE, checks = FALSE){
  # todo: checks/error handling messages.
  
  if (checks){
    i = 1
    if (nrow(uuid_data[which(uuid_data$name == flow_name),]) == 0 & i == 1){
      message("flow_name not recognised. Updating uuid_data sheet")
      set_rapidpro_uuid_names()
      uuid_data = get_rapidpro_uuid_names()
      i = i + 1
      if (nrow(uuid_data[which(uuid_data$name == flow_name),]) == 0){
        stop("flow_name not recognised.")
      } else {
        message("flow_name recognised. Updated uuid_data sheet")
      }
    }
  }

  uuid_flow <- uuid_data[which(uuid_data$name == flow_name),]
  source_1 <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if (length(results$results) == 0){
   #stop("no data in flow")
   flow_interaction <- NULL
  } else {
    result_flow <- results$results
    uuid <- result_flow$contact$uuid
    response <- result_flow$responded
    category <- result_flow$values$result$category            
    flow_interaction <- tibble::tibble(uuid, response) #, category)
    flow_interaction <- flow_interaction %>% mutate(flow_type = uuid_flow[1]) 
  }
 # if (flatten){
 #   flow_interaction <- jsonlite::flatten(flow_interaction)
 # }
  return(flow_interaction)
}
# TODO: result1 is the name. Need to get the result names.

get_flow_data2 <- function(flow_name = NULL, uuid_name, result, call_type="runs.json?flow=", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key()){
  uuid_flow <- uuid_name
  source_1 <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if (length(results$results) == 0){
    #stop("no data in flow")
    flow_interaction <- NULL
  } else {
    result_flow <- results$results
    uuid <- result_flow$contact$uuid
    response <- result_flow$responded
    #category <- result_flow$values$result$category            
    flow_interaction <- tibble::tibble(uuid, response, category)
    flow_interaction <- flow_interaction %>% mutate(flow_type = uuid_flow[1]) 
  }
  return(flow_interaction)
}

#' 3. Information at user level -----------------------------------------------------
#' * Contact field values with possibility to choose which variables to display?
#' Not interested in internal variables for system functionalities, filter based on name (REGEX) or provide a list
#' * Group membership
#' 
#' Contact field values - see Code Book.R - should we make this into a function?
#' 
#' Getting group data for an individual
#' TODO: Is this what we want? Or do we want all the group data, together.

get_user_group_data <- function(user_data = get_user_data(), name = NULL, uuid = NULL){
  
  # check name/uuid for typo
  if (length(which(get_user_data()$name == name)) == 0){
    if (length(which(get_user_data()$uuid == uuid)) == 0){
      stop("Neither name nor uuid supplied recognised")
    } else{
      message("name supplied is not recognised. Using the uuid instead.")
    }
  } else {
    if (length(which(get_user_data()$uuid == uuid)) == 0){
      message("uuid supplied is not recognised. Using the name instead.")
    } else{
      if(which(get_user_data()$name == name) != which(get_user_data()$uuid == uuid)){
        warning("name and uuid supplied do not match. Using the name supplied.")
    }
    }
  }
  
  if (is.null(name)){
    if (is.null(uuid)){
      stop("Either `name` or `uuid` must be supplied.")
    } else {
      get_user_data()$groups[[which(uuid == uuid)]]
    }
  } else {
    get_user_data()$groups[[which(name == name)]]
  }
}

#' 4. Information at flow level --------------------------------------------------------------

# Table 7 Functions -----------------
summary_PT <- function(data = df, summary_var, denominator = NULL, denominator_level = "Yes"){

  if (!is.null(denominator)) {
    summary_perc <- data %>%
      filter({{ denominator }} == denominator_level) %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n(),
                "{{summary_var}}_perc" := n()/nrow(.) * 100)
    
    return(summary_perc)
  } else {
    summary_n <- data %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n())
    return(summary_n)
  }
}

flow_data_function <- function(flow_name){
  response_message <- NULL
  for (i in 1:length(flow_name)){
    response_message[[i]] <- get_flow_data(flow_name = flow_name[i])
  }
  names(response_message) <- flow_name[1:length(response_message)]
  for (i in 1:length(response_message)){
    if (!is.null(response_message[[i]])){
      response_message[[i]] <- jsonlite::flatten(response_message[[i]])
    }
  }
  response_message <- plyr::ldply(response_message)
  return(response_message %>% group_by(response) %>% summarise(count = n(), perc = round(n()/nrow(.)*100,2)))
}

#' * General level
#' *** Number of runs (proportions)
#' *** Number of interactions (responded==TRUE) (proportions)
#' * Result level
#' *** Rates of responses (corresponding to categories in wfr nodes) for a given result (specify name of result and categories)
#' *** The possibility to filter and summarise the information at flow level based on users/groups/contact field information

# General level - number of responses, category answers
response_rate_graphs<-function(flow_interaction, flow_name){
  print(flow_interaction %>% group_by(response) %>% summarise(n())) 
  print(flow_interaction %>% group_by(category) %>% summarise(n())) 
  
#  ggplot(flow_interaction, aes(x = response)) +
#    geom_bar() +
#    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
#  
#  flow_interaction_response <- flow_interaction %>% filter(response == TRUE)
#  ggplot(flow_interaction_response, aes(x = category)) +
#    geom_bar() +
#    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
}

# TODO: Result level - what is WFR



create_user_dataframe <- function(flow_interaction){
  temp<-flow_interaction %>% group_by(uuid,response) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
  temp2<-flow_interaction %>% filter(response == TRUE) %>% group_by(uuid,category) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
}
#' Interaction with api
#' Description: Make API call, specify query string, save data in dataframe

#' 1. Defining package environment -------------------------------------------
#' Sorting set and get calls for: key, site, uuid flow names
#' 
#' Define package environment
pkg.env <- new.env(parent = emptyenv())

# Set defaults values in the environment: key and site
assign("rapidpro_key", NULL, envir = pkg.env)
assign("rapidpro_site", NULL, envir = pkg.env)

# Functions to reset the values in the environment: key and site
set_rapidpro_key = function(key) {
  assign("rapidpro_key", key, envir = pkg.env)
}

set_rapidpro_site = function(site) {
  assign("rapidpro_site", site, envir = pkg.env)
}

# Functions to call the values in the environment: key and site
get_rapidpro_key = function() {
  get("rapidpro_key", envir = pkg.env)
}

get_rapidpro_site = function() {
  get("rapidpro_site", envir = pkg.env)
}

# Function to get a list of the UUID of flows
get_flow_names <- function(call_type="flows.json", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = TRUE){
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  flow_names <- results$results
  if (flatten){
    flow_names <- jsonlite::flatten(flow_names)
  }
  return(flow_names)
}

set_rapidpro_uuid_names = function(uuid_names = get_flow_names()){#[c("uuid", "name")]) {
  assign("rapidpro_uuid_names", uuid_names, envir = pkg.env)
}

get_rapidpro_uuid_names = function(){
  get("rapidpro_uuid_names", envir = pkg.env)
}

#' 2. Retreiving Information -------------------------------------------
#' Get general data
get_data_from_rapidpro_api <- function(call_type, rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = TRUE){
  # TODO put in checks - check site is correct, then token, then call_type
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  user_result <- results$results
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  return(user_result)
}

# Get user (contacts) data
get_user_data <- function(call_type="contacts.json", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = TRUE){
  # todo: checks/error handling messages.
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  user_result <- results$results
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  return(user_result)
}

# Get run (flow) data
get_flow_data <- function(uuid_data = get_rapidpro_uuid_names(), flow_name, result, call_type="runs.json?flow=", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE, checks = FALSE){
  # todo: checks/error handling messages.
  
  if (checks){
    i = 1
    if (nrow(uuid_data[which(uuid_data$name == flow_name),]) == 0 & i == 1){
      message("flow_name not recognised. Updating uuid_data sheet")
      set_rapidpro_uuid_names()
      uuid_data = get_rapidpro_uuid_names()
      i = i + 1
      if (nrow(uuid_data[which(uuid_data$name == flow_name),]) == 0){
        stop("flow_name not recognised.")
      } else {
        message("flow_name recognised. Updated uuid_data sheet")
      }
    }
  }

  uuid_flow <- uuid_data[which(uuid_data$name == flow_name),]
  source_1 <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if (length(results$results) == 0){
   #stop("no data in flow")
   flow_interaction <- NULL
  } else {
    result_flow <- results$results
    uuid <- result_flow$contact$uuid
    response <- result_flow$responded
    category <- result_flow$values$result$category            
    flow_interaction <- tibble::tibble(uuid, response) #, category)
    flow_interaction <- flow_interaction %>% mutate(flow_type = uuid_flow[1]) 
  }
 # if (flatten){
 #   flow_interaction <- jsonlite::flatten(flow_interaction)
 # }
  return(flow_interaction)
}
# TODO: result1 is the name. Need to get the result names.

get_flow_data2 <- function(flow_name = NULL, uuid_name, result, call_type="runs.json?flow=", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key()){
  uuid_flow <- uuid_name
  source_1 <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if (length(results$results) == 0){
    #stop("no data in flow")
    flow_interaction <- NULL
  } else {
    result_flow <- results$results
    uuid <- result_flow$contact$uuid
    response <- result_flow$responded
    #category <- result_flow$values$result$category            
    flow_interaction <- tibble::tibble(uuid, response, category)
    flow_interaction <- flow_interaction %>% mutate(flow_type = uuid_flow[1]) 
  }
  return(flow_interaction)
}

#' 3. Information at user level -----------------------------------------------------
#' * Contact field values with possibility to choose which variables to display?
#' Not interested in internal variables for system functionalities, filter based on name (REGEX) or provide a list
#' * Group membership
#' 
#' Contact field values - see Code Book.R - should we make this into a function?
#' 
#' Getting group data for an individual
#' TODO: Is this what we want? Or do we want all the group data, together.

get_user_group_data <- function(user_data = get_user_data(), name = NULL, uuid = NULL){
  
  # check name/uuid for typo
  if (length(which(get_user_data()$name == name)) == 0){
    if (length(which(get_user_data()$uuid == uuid)) == 0){
      stop("Neither name nor uuid supplied recognised")
    } else{
      message("name supplied is not recognised. Using the uuid instead.")
    }
  } else {
    if (length(which(get_user_data()$uuid == uuid)) == 0){
      message("uuid supplied is not recognised. Using the name instead.")
    } else{
      if(which(get_user_data()$name == name) != which(get_user_data()$uuid == uuid)){
        warning("name and uuid supplied do not match. Using the name supplied.")
    }
    }
  }
  
  if (is.null(name)){
    if (is.null(uuid)){
      stop("Either `name` or `uuid` must be supplied.")
    } else {
      get_user_data()$groups[[which(uuid == uuid)]]
    }
  } else {
    get_user_data()$groups[[which(name == name)]]
  }
}

#' 4. Information at flow level --------------------------------------------------------------

# Table 7 Functions -----------------
summary_PT <- function(data = df, summary_var, denominator = NULL, denominator_level = "Yes"){

  if (!is.null(denominator)) {
    summary_perc <- data %>%
      filter({{ denominator }} == denominator_level) %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n(),
                "{{summary_var}}_perc" := n()/nrow(.) * 100)
    
    return(summary_perc)
  } else {
    summary_n <- data %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n())
    return(summary_n)
  }
}

flow_data_function <- function(flow_name){
  response_message <- NULL
  for (i in 1:length(flow_name)){
    response_message[[i]] <- get_flow_data(flow_name = flow_name[i])
  }
  names(response_message) <- flow_name[1:length(response_message)]
  for (i in 1:length(response_message)){
    if (!is.null(response_message[[i]])){
      response_message[[i]] <- jsonlite::flatten(response_message[[i]])
    }
  }
  response_message <- plyr::ldply(response_message)
  return(response_message %>% group_by(response) %>% summarise(count = n(), perc = round(n()/nrow(.)*100,2)))
}

#' * General level
#' *** Number of runs (proportions)
#' *** Number of interactions (responded==TRUE) (proportions)
#' * Result level
#' *** Rates of responses (corresponding to categories in wfr nodes) for a given result (specify name of result and categories)
#' *** The possibility to filter and summarise the information at flow level based on users/groups/contact field information

# General level - number of responses, category answers
response_rate_graphs<-function(flow_interaction, flow_name){
  print(flow_interaction %>% group_by(response) %>% summarise(n())) 
  print(flow_interaction %>% group_by(category) %>% summarise(n())) 
  
#  ggplot(flow_interaction, aes(x = response)) +
#    geom_bar() +
#    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
#  
#  flow_interaction_response <- flow_interaction %>% filter(response == TRUE)
#  ggplot(flow_interaction_response, aes(x = category)) +
#    geom_bar() +
#    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
}

# TODO: Result level - what is WFR



create_user_dataframe <- function(flow_interaction){
  temp<-flow_interaction %>% group_by(uuid,response) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
  temp2<-flow_interaction %>% filter(response == TRUE) %>% group_by(uuid,category) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
}
