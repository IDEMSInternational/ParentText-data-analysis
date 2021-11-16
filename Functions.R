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

# Calling data
httr_get_call <- function(get_command, token = get_rapidpro_key()){
  response <- httr::GET(get_command, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if(!is.null(results$'next')){
    bind_rows(results$results, httr_get_call(results$'next',token))
  } else {
    return(results$results)
  }
}

# Function to get a list of the UUID of flows
get_flow_names <- function(call_type = "flows.json", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE){
  # TODO put in checks - check site is correct, then token, then call_type
  get_command <- paste(rapidpro_site, call_type, sep = "")
  flow_names <- httr_get_call(get_command = get_command, token = token)
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
get_data_from_rapidpro_api <- function(call_type, rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE){
  get_command <- paste(rapidpro_site, call_type, sep = "")
  user_result <- httr_get_call(get_command = get_command, token = token)
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  return(user_result)
}

# Get user (contacts) data
get_user_data <- function(call_type="contacts.json", rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE){
  # todo: checks/error handling messages.
  get_command <- paste(rapidpro_site, call_type, sep = "")
  user_result <- httr_get_call(get_command = get_command, token = token)
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
  get_command <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
  flow_result <- httr_get_call(get_command = get_command, token = token)
  
  if (length(flow_result) == 0){
    #stop("no data in flow")
    flow_interaction <- NULL
  } else {
    result_flow <- flow_result
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

# aesthetcs - remving _ and making first letter capital

naming_conventions <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}

# wrapping text but retaining the levels
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

flow_data_function1 <- function(flow_name){
  response_message <- NULL
  for (i in 1:length(flow_name)){
    response_message[[i]] <- get_flow_data(flow_name = flow_name[i])
  }
  if (!is.null(response_message)){
    names(response_message) <- flow_name[1:length(response_message)]
    for (i in 1:length(response_message)){
      if (!is.null(response_message[[i]])){
        response_message[[i]] <- jsonlite::flatten(response_message[[i]])
      }
    }
    return(response_message)
  }
}



#' 4. Information at flow level --------------------------------------------------------------

# Table 7 Functions -----------------
summary_PT <- function(data = df, summary_var, denominator = NULL, denominator_level = "Yes", together = FALSE, naming_convention = FALSE){
  
  if (!is.null(denominator)) {
    summary_perc <- data %>%
      filter({{ denominator }} == denominator_level) %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n(),
                "{{summary_var}}_perc" := n()/nrow(.) * 100)
    
    if (together == TRUE){
      colnames(summary_perc)[length(summary_perc)-1] <- "n"
      colnames(summary_perc)[length(summary_perc)] <- "perc"
      summary_perc <- summary_perc %>%
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 1), ")")) %>%
        dplyr::select(-c(n, perc))
    }
    
    if (naming_convention == TRUE){
      colnames(summary_perc) <- naming_conventions(colnames(summary_perc))
    }
    
    return(summary_perc)
  } else {
    summary_n <- data %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n())
    if (naming_convention == TRUE){
      colnames(summary_n) <- naming_conventions(colnames(summary_n))
    }
    return(summary_n)
  }
}



flow_data_summary_function <- function(response_message){
  if (!is.data.frame(response_message)){
    response_message <- plyr::ldply(response_message) 
  }
    return(response_message %>%
             group_by(response, .drop = FALSE) %>%
             summarise(count = n(), perc = round(n()/nrow(.)*100,2)) %>%
             mutate("Count (%)" := str_c(`count`, ' (', round(`perc`, 1), ")")) %>%
             dplyr::select(-c(count, perc)) %>%
             mutate(response = factor(ifelse(response == TRUE, "Yes", "No"))) %>% map_df(rev))
    #mutate(response = forcats::fct_relevel(response, c("Yes", "No"))))
  }

# Information at survey level ----------------------------------------------------------------

survey_datetime_split <- function(parenting_variable, survey_no = 1){
  # split the string by different surveys taken
  split_parenting <- str_split(parenting_variable, pattern = fixed("|"))
  
  # for each individual, get each survey value (split by ,)
  parenting_response <- NULL
  for (i in 1:length(split_parenting)){
    split_parenting_2 <- str_split(split_parenting[[i]], ",") 
    
    # if it is NA, keep as NA
    if (is.na(split_parenting_2[[1]][3])){
      parenting_response[i] <- NA
    } else{
      # which survey to consider? Baseline = 1, week1 = 2, etc.
      # so get that correct week lot of surveys
      for (j in 2:length(split_parenting_2) - 1){
        if (as.numeric(as.character(split_parenting_2[[j]][2])) != survey_no) {
          split_parenting_2[[j]][3] ="1970-01-01T00:00:00.873007+08:00"
        }
      }
      
      # take the response value corresponding to the most recent date
      response <- NA
      date <- as.POSIXct("1970-01-01T00:00:00.873007+08:00", format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
      
      for (j in 2:length(split_parenting_2) - 1){
        if (date < as.POSIXct(split_parenting_2[[j]][3], format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")) {
          response <- split_parenting_2[[j]][1]
        }
      }
      parenting_response[i] <- as.numeric(as.character(response))
    }
  }
  return(parenting_response)
}

survey_datetime_split_multiple <- function(parenting_variable, survey_max = 9){
  survey_entry <- NULL
  for (i in 1:survey_max){
    survey_entry[[i]] <- survey_datetime_split(parenting_variable, survey_no = i)
  }
  week <- c(rep("Base", length(parenting_variable)),
            rep("2", length(parenting_variable)),
            rep("3", length(parenting_variable)),
            rep("4", length(parenting_variable)),
            rep("5", length(parenting_variable)),
            rep("6", length(parenting_variable)),
            rep("7", length(parenting_variable)),
            rep("8", length(parenting_variable)),
            rep("9", length(parenting_variable)))
  vals <- unlist(survey_entry)
  return(data.frame(week, vals))
}

# Function based on http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# for plotting means and error bars
summarySE <- function(data=NULL, var, groups=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {

  # Summary - vector with N, mean, and sd for each group var
  datac <- data %>%
      group_by(across({{ groups }})) %>%
      summarise(N = sum(!is.na({{ var }})),
                mean = mean({{ var }}, na.rm = na.rm),
                sd = sd({{ var }}, na.rm = na.rm))  

  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
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
