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
  flow_interaction <- NULL
  for (i in 1:length(flow_name)){
    uuid_flow <- uuid_data[which(uuid_data$name == flow_name[i]),]
    get_command <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
    result_flow <- httr_get_call(get_command = get_command, token = token)
    if (length(result_flow) == 0){
      flow_interaction[[i]] <- NULL
    } else {
      uuid <- result_flow$contact$uuid
      response <- result_flow$responded
      category <- result_flow$values$result$category            
      flow_interaction[[i]] <- tibble::tibble(uuid, response) #, category)
      flow_interaction[[i]] <- flow_interaction[[i]] %>% mutate(flow_type = uuid_flow[1])
      #if (flatten){
      flow_interaction[[i]] <- jsonlite::flatten(flow_interaction[[i]])
      #}
    }
  }
  names(flow_interaction) <- flow_name[1:length(flow_interaction)]
  return(flow_interaction)
}


survey_datetime_split_multiple <- function(parenting_variable, survey_max = 9){
  survey_entry <- NULL
  for (i in 1:survey_max){
    # split the string by different surveys taken
    split_parenting <- str_split(parenting_variable, pattern = fixed("|"))
    
    # for each individual, get each survey value (split by ,)
    parenting_response <- NULL
    for (j in 1:length(split_parenting)){
      split_parenting_2 <- str_split(split_parenting[[j]], ",") 
      
      # if it is NA, keep as NA
      if (is.na(split_parenting_2[[1]][3])){
        parenting_response[j] <- NA
      } else{
        # which survey to consider? Baseline = 1, week1 = 2, etc.
        # so get that correct week lot of surveys
        for (k in 2:length(split_parenting_2) - 1){
          if (as.numeric(as.character(split_parenting_2[[k]][2])) != i) {
            split_parenting_2[[k]][3] ="1970-01-01T00:00:00.873007+08:00"
          }
        }
        
        # take the response value corresponding to the most recent date
        response <- NA
        date <- as.POSIXct("1970-01-01T00:00:00.873007+08:00", format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        
        for (k in 2:length(split_parenting_2) - 1){
          if (date < as.POSIXct(split_parenting_2[[k]][3], format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")) {
            response <- split_parenting_2[[k]][1]
          }
        }
        parenting_response[j] <- as.numeric(as.character(response))
      }
    }
    survey_entry[[i]] <- parenting_response
  }
  return(survey_entry)
}


#' 3. Summarising data - ParentText analysis -------------------------------------------------------------------------------------------------------------

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
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
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

flow_data_summary_function <- function(flow_interaction){
  if (!is.data.frame(flow_interaction)){
    flow_interaction <- plyr::ldply(flow_interaction) 
  }
  return(flow_interaction %>%
           group_by(response, .drop = FALSE) %>%
           summarise(count = n(), perc = round(n()/nrow(.)*100,2)) %>%
           mutate("Count (%)" := str_c(`count`, ' (', round(`perc`, 1), ")")) %>%
           dplyr::select(-c(count, perc)) %>%
           mutate(response = factor(ifelse(response == TRUE, "Yes", "No"))) %>% map_df(rev))
  #mutate(response = forcats::fct_relevel(response, c("Yes", "No"))))
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
