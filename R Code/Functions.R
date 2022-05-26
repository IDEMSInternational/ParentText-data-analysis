#' Interaction with api
#' Description: Make API call, specify query string, save data in dataframe

#' 1. Defining package environment -------------------------------------------
#' Sorting set and get calls for: key, site, uuid flow names
#' 
#' Define package environment
utils::globalVariables(c("pkg_env"))
pkg_env <- new.env(parent = emptyenv())
pkg_env$rapidpro_key <- NULL
pkg_env$rapidpro_site <- NULL
pkg_env$rapidpro_uuid_names <- NULL 

set_rapidpro_key = function(key) {
  if (!is.character(key)){
    stop("`key` provided should be a character variable")
  }
  pkg_env$rapidpro_key <- key 
}

set_rapidpro_site = function(site) {
  if (!is.character(site)){
    stop("`site` provided should be a character variable")
  }
  pkg_env$rapidpro_site <- site 
}

set_rapidpro_uuid_names = function(uuid_names = get_flow_names()){
  pkg_env$rapidpro_uuid_names <- uuid_names 
}

get_rapidpro_key = function() {
  get("rapidpro_key", envir = pkg_env)
}

get_rapidpro_site = function() {
  get("rapidpro_site", envir = pkg_env)
}

get_rapidpro_uuid_names = function(){
  get("rapidpro_uuid_names", envir = pkg_env)
}

get_user_data <- function(rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE, date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC"){
  get_data_from_rapidpro_api(call_type = "contacts.json", rapidpro_site = rapidpro_site, token = token, flatten = flatten, date_from = date_from, date_to = date_to, format_date = format_date, tzone_date = tzone_date)
}

get_archived_data <- function(rapidpro_site = get_rapidpro_site(), call_type = "archives.json", token = get_rapidpro_key(), period = "monthly", flatten = FALSE, date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC"){
  get_command <- paste(rapidpro_site, call_type, sep = "")
  result_flow <- httr_get_call(get_command = get_command, token = token)
  if (period == "daily"){
    result_flow <- result_flow %>% dplyr::filter(period == "daily")
  } else if (period == "monthly"){
    result_flow <- result_flow %>% dplyr::filter(period == "monthly")
  } else {
    result_flow <- result_flow
  }
  result_flow <- result_flow %>% filter(archive_type == "run")
  archived_data_bank <- NULL
  for (i in 1:nrow(result_flow)){
    archived_data_bank[[i]] <- jsonlite::stream_in(
      gzcon(url(result_flow$download_url[i])), flatten = FALSE
    )
  }
  names(archived_data_bank) <- (result_flow$start_date)
  return(archived_data_bank)
}
#archived_data <- get_archived_data()
#getwd()
#saveRDS(archived_data, file = "archived_data_monthly.RDS")
#saveRDS(archived_data, file = "archived_data.RDS")

get_flow_names <- function(rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE, date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC"){
  get_data_from_rapidpro_api(call_type = "flows.json", rapidpro_site = rapidpro_site, token = token, flatten = flatten, date_from = date_from, date_to = date_to, format_date = format_date, tzone_date = tzone_date)
}

httr_get_call <- function(get_command, token = get_rapidpro_key()){
  if (is.null(token)){
    stop("token is NULL. Set token with `set_rapidpro_key`.")
    # could there be a case where the key isn't needed?
  }
  if (is.null(get_command)){
    stop("get_command is NULL. Expecting a website.")
  }
  response <- httr::GET(get_command, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  if(!is.null(results$'next')){
    dplyr::bind_rows(results$results, httr_get_call(results$'next', token))
  } else {
    return(results$results)
  }
}

days_active_data <- function(uuid_data = get_rapidpro_uuid_names(), flow_name, call_type = "runs.json", rapidpro_site = get_rapidpro_site(),
                             token = get_rapidpro_key(), flatten = FALSE, include_archived_data = FALSE, runs_data = "result_flow_runs.RDS", read_runs = FALSE,
                             get_by = "gotit", data_from_archived = archived_data,
                             download_archived_data = FALSE, read_archived_data_from = "archived_data_monthly.RDS", archive_call_type = "archives.json",
                             archive_period = "monthly"){
  
  
  if(read_runs){
    result_flow_runs <- readRDS(runs_data)
  } else {
    get_command <- paste(rapidpro_site, call_type, sep = "")
    result_flow <- httr_get_call(get_command = get_command, token = token)
    result_flow_runs <- result_flow
  }
  
  #saveRDS(result_flow_runs, "result_flow_runs.RDS")
  
  data_flow <- result_flow_runs %>% 
    filter(responded == TRUE) %>%
    mutate(day_created = as.Date(created_on))
  day_active <- data_flow$day_created
  ID <- data_flow$contact$uuid
  day_created <- data.frame(day_active, ID)
  day_created <- unique(day_created)
  active_days_nonarch <- day_created
  
  # # for archives data
  if (!include_archived_data){
    active_days_data <- active_days_nonarch
  } else {
    flow_data_bank[[1]] <- flow_data
    if (get_by == "download"){
      archived_data <- get_archived_data(rapidpro_site = rapidpro_site, call_type = archive_call_type, token = token,
                                         period = archive_period, flatten = flatten, date_from = date_from, date_to = date_to,
                                         format_date = format_date, tzone_date = tzone_date)
    } else if (get_by == "read"){
      archived_data <- readRDS(read_archived_data_from)
    } else {
      archived_data <- data_from_archived
    }
    active_days <- NULL
    result_flow <- archived_data
    for (k in 1:length(archived_data)){
      data_flow <- result_flow[[k]]
      if (nrow(data_flow) > 0){
        data_flow <- data_flow %>% 
          filter(responded == TRUE) %>%
          mutate(day_created = as.Date(created_on))
        day_active <- data_flow$day_created
        ID <- data_flow$contact$uuid
        day_created <- data.frame(day_active, ID)
        day_created <- unique(day_created)
        active_days[[k]] <- day_created
      }
    }
    active_days_archived <- plyr::ldply(active_days)
    active_days_archived <- unique(active_days_archived)
    active_days_data <- bind_rows(active_days_nonarch, active_days_archived)
    
  }
  active_days_data <- unique(active_days_data) 
  active_days_data <- active_days_data %>%
    group_by(ID) %>%
    summarise(number_days_active = n())
  return(active_days_data)
}

get_flow_data <- function(uuid_data = get_rapidpro_uuid_names(), flow_name, call_type = "runs.json?flow=", rapidpro_site = get_rapidpro_site(),
                          token = get_rapidpro_key(), flatten = FALSE, checks = FALSE, flow_type = "none", include_archived_data = FALSE,
                          get_by = "gotit", data_from_archived = archived_data,
                          download_archived_data = FALSE, read_archived_data_from = "archived_data_monthly.RDS", archive_call_type = "archives.json",
                          archive_period = "monthly", date_from = NULL, date_to = NULL,
                          format_date = "%Y-%m-%d", tzone_date = "UTC"){
  
  if (is.null(rapidpro_site)){
    stop("rapidpro_site is NULL. Set a website with `set_rapidpro_site`.")
  }
  if (is.null(token)){
    stop("token is NULL. Set a token with `set_rapidpro_key`.")
  }
  if (is.null(call_type)){
    stop("call_type is NULL. Expecting a valid call_type.")
  }
  if (!is.character(call_type)){
    stop("call_type should be a character variable.")
  }
  if (is.null(flow_name)){
    stop("flow_name is NULL. Expecting a valid flow_name")
  }
  if (!is.character(flow_name)){
    stop("flow_name should be a character variable.")
  }
  if (!is.logical(flatten)){
    stop("flatten should be TRUE or FALSE")
  }
  if (!is.logical(checks)){
    stop("checks should be TRUE or FALSE")
  }
  
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
  flow_data_bank <- NULL
  flow_data <- NULL
  for (i in flow_name){
    j <- which(flow_name == i)
    uuid_flow <- uuid_data[which(uuid_data$name == i),]
    get_command <- paste(rapidpro_site, call_type, uuid_flow[1], sep = "")
    result_flow <- httr_get_call(get_command = get_command, token = token)
    
    if (length(result_flow) == 0){
      flow_data[[j]] <- NULL
    } else {
      flow_data[[j]] <- flow_data_calculation(result_flow = result_flow, flatten = flatten, flow_type = flow_type, date_from = date_from, date_to = date_to,
                                              format_date = format_date, tzone_date = tzone_date) %>%
        dplyr::mutate(flow_type = uuid_flow[1,1]) 
    }
  }
  if (!is.null(flow_data)){
    names(flow_data) <- flow_name[1:length(flow_data)]
  }
  flow_data <- plyr::ldply(flow_data)
  
  if (!include_archived_data){
    flow_data_bank <- flow_data
  } else {
    flow_data_bank[[1]] <- flow_data
    if (get_by == "download"){
      archived_data <- get_archived_data(rapidpro_site = rapidpro_site, call_type = archive_call_type, token = token,
                                         period = archive_period, flatten = flatten, date_from = date_from, date_to = date_to,
                                         format_date = format_date, tzone_date = tzone_date)
    } else if (get_by == "read"){
      archived_data <- readRDS(read_archived_data_from)
    } else {
      archived_data <- data_from_archived
    }
    arch_data_bank <- NULL
    for (i in flow_name){
      j <- which(flow_name == i)
      uuid_flow <- uuid_data[which(uuid_data$name == i),]
      arch_data <- NULL
      for (k in 1:length(archived_data)){
        arch_flow_data_K <- archived_data[[k]] %>% filter(archived_data[[k]]$flow$name == i)
        arch_data[[k]] <- flow_data_calculation(result_flow = arch_flow_data_K, flow_type = flow_type)
      }
      names(arch_data) <- names(archived_data)[1:length(arch_data)]
      arch_data_bank[[j]] <- plyr::ldply(arch_data)
      arch_data_bank[[j]]$`.id` <- NULL
      arch_data_bank[[j]] <- arch_data_bank[[j]] %>% dplyr::mutate(flow_type = uuid_flow[1,1])
    }
    names(arch_data_bank) <- flow_name
    arch_data_bank <- plyr::ldply(arch_data_bank)
    
    flow_data_bank[[2]] <- arch_data_bank
    names(flow_data_bank) <- c("Current", "Archived")
    flow_data_bank <- plyr::ldply(flow_data_bank)
  }
  
  
  return(flow_data_bank)
  
}

# flow_type.uuid - todo.

flow_data_calculation <- function(result_flow, flatten = FALSE, flow_type = "none", date_from = NULL, date_to = NULL,
                                  format_date = "%Y-%m-%d", tzone_date = "UTC"){
  if (length(result_flow) == 0){
    flow_interaction <- NULL
  } else {
    if (!is.null(date_from)){
      result_flow <- result_flow %>% dplyr::filter(as.POSIXct(date_from, format=format_date, tzone = tzone_date) < as.POSIXct(result_flow$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    }
    if (!is.null(date_to)){
      result_flow <- result_flow %>% dplyr::filter(as.POSIXct(date_to, format=format_date, tzone = tzone_date) > as.POSIXct(result_flow$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    }
    uuid <- result_flow$contact$uuid
    interacted <- result_flow$responded
    created_run_on <- result_flow$created_on
    exit_type <- result_flow$exit_type
    modified_on <- result_flow$modified_on
    exited_on <- result_flow$exited_on
    
    # for check in:
    if (flow_type == "praise" && nrow(result_flow$values) > 0){
      response <- result_flow$values$praise_interaction$category 
      if (!is.null(response)){
        flow_interaction <- tibble::tibble(uuid, interacted, response)
        response <- replace_na(response, "No response")
      } else {
        flow_interaction <- tibble::tibble(uuid, interacted, response = "No response") 
      }
    } else if (flow_type == "calm" && !is.null(result_flow$values$calm_interaction)){
      response <- result_flow$values$calm_interaction$category
      response <- replace_na(response, "No response")
      flow_interaction <- tibble::tibble(uuid, interacted, response)
    } else if (flow_type == "check_in" && nrow(result_flow$values) > 0){
      if (is.null(result_flow$values$checkin_managed$category)){
        managed_to_do_something <- "No response"
      } else {
        managed_to_do_something <- result_flow$values$checkin_managed$category
      }
      if (is.null(result_flow$values$checkin_how$category)){
        response <- "No response"
      } else {
        response <- result_flow$values$checkin_how$category
      }
      flow_interaction <- tibble::tibble(uuid, interacted, managed_to_do_something, response)
    } else if (flow_type == "tips" && nrow(result_flow$values) > 0){
      if (is.null(result_flow$values$know_more$category)){
        category <- "No response"
      } else {
        category <- result_flow$values$know_more$category
      }
      flow_interaction <- tibble::tibble(uuid, interacted, category)
    } else {
      flow_interaction <- tibble::tibble(uuid, interacted)
      ##flow_interaction <- tibble::tibble(uuid, interacted, exit_type, created_on, modified_on, exited_on)
    }
    #result <- na.omit(unique(flatten(result_flow$values)$name))[1]
    #if (length(result) == 1){
    #  category <- flatten(result_flow$values %>% dplyr::select({{ result }}))$category
    #} else {
    #  warning("category result not found")
    #  category <- NA
    #}
    #if (flatten){
    flow_interaction <- jsonlite::flatten(flow_interaction)
    #}
  }
  return(flow_interaction)
}


get_data_from_rapidpro_api <- function(call_type, rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE,
                                       date_from, date_to, format_date = "%Y-%m-%d", tzone_date = "UTC"){
  if (is.null(rapidpro_site)){
    stop("rapidpro_site is NULL. Set a website with `set_rapidpro_site`.")
  }
  if (is.null(token)){
    stop("token is NULL. Set a token with `set_rapidpro_key`.")
  }
  if (is.null(call_type)){
    stop("call_type is NULL. Expecting a valid call_type.")
  }
  if (!is.logical(flatten)){
    stop("flatten should be TRUE or FALSE")
  }
  get_command <- paste(rapidpro_site, call_type, sep = "")
  user_result <- httr_get_call(get_command = get_command, token = token)
  if (flatten){
    user_result <- jsonlite::flatten(user_result)
  }
  if (!is.null(date_from)){
    user_result <- user_result %>% dplyr::filter(as.POSIXct(date_from, format=format_date, tzone = tzone_date) < as.POSIXct(user_result$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!is.null(date_to)){
    user_result <- user_result %>% dplyr::filter(as.POSIXct(date_to, format=format_date, tzone = tzone_date) > as.POSIXct(user_result$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  return(user_result)
}

get_survey_data <- function(parenting_variable){
  all_split_data <- NULL
  parenting_variable <- parenting_variable
  split_parenting <- stringr::str_split(parenting_variable, pattern = stringr::fixed("|"))
  if (length(split_parenting) > 0) {
    for (j in 1:length(split_parenting)){
      if (is.na(split_parenting[[j]])){
        split_data <- data.frame(V1 = NA, row = j, V2 = NA, V3 = NA)
      } else {
        split_parenting_2 <- stringr::str_split(split_parenting[[j]], ",")
        split_data <- plyr::ldply(split_parenting_2[1:(length(split_parenting_2)-1)])
        split_data <- split_data %>% mutate(row = j)
      }
      all_split_data[[j]] <- split_data
    }
  }
  all_split_data <- plyr::ldply(all_split_data)
  if (length(split_parenting) > 0) { names(all_split_data) <- c("vals", "row", "week", "dt") }
  all_split_data$week <- as.numeric(as.character(all_split_data$week))
  all_split_data$vals <- as.numeric(as.character(all_split_data$vals))
  all_split_data$week <- ifelse(all_split_data$week == "1", "Baseline", all_split_data$week)
  
  return(all_split_data)
}


# aesthetics - removing _, and making first letter capital

naming_conventions <- function(x, replace, replace_after) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}

# wrapping text but retaining the levels
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# same function used in parent text
summary_calculation <- function(data = plhdata_org_clean, factors, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                                together = FALSE, include_margins = FALSE){
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    summary_output <- data %>%
      mutate(across(c({{ columns_to_summarise }}), ~ as.character(.x))) %>%
      group_by(across(c({{ columns_to_summarise }}, {{ factors }})), .drop = FALSE) %>%
      summarise(n = n(),
                perc = n()/nrow(.) * 100)
    if (include_margins){
      cts_margin <- data %>%
        group_by(across(c({{ columns_to_summarise }})), .drop = FALSE) %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)      
      ftr_margin <- data %>%
        group_by(across(c({{ factors }})), .drop = FALSE) %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)      
      corner_margin <- data %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)
      summary_output <- bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id %in% c(2, 4), "Total", .x))) %>%
        mutate(across({{ columns_to_summarise }}, ~ifelse(id %in% c(3, 4), "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        mutate(across({{ columns_to_summarise }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
      if (together){
        summary_output <- summary_output %>%
          mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
          dplyr::select(-c(n, perc))
      }
    }
  } else {
    summary_output <- data %>%
      group_by(across({{ factors }}), .drop = FALSE) %>%
      #mutate(across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      summarise(across({{ columns_to_summarise }}, ~mean(.x, na.rm = TRUE)))
    
    if (include_margins){
      corner_margin <- data %>%
        summarise(across(c({{ columns_to_summarise }}), ~mean(.x, na.rm  = TRUE)))
      
      summary_output <- bind_rows(summary_output, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id == 2, "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
    
  }
  return(unique(summary_output))
}

summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                          replace = "rp.contact.field.", include_margins = FALSE, wider_table = TRUE,
                          display_table = FALSE, naming_convention = TRUE, include_percentages = FALSE,
                          together = TRUE){
  
  summaries <- match.arg(summaries)
  
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      summaries = summaries,
                                      together = together)
  return_table_names <- naming_conventions(colnames(return_table), replace = replace)
  if (summaries == "mean"){
    if (naming_convention){
      colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
    }
  }
  if (display_table){
    if (summaries == "frequencies"){
      return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n)
    }
    
    return_table <- gt(as_tibble(return_table)) %>%
      tab_header(
        title = paste(return_table_names[1], "by", return_table_names[2])  # fix up. 
      ) %>%
      tab_style(locations = list(cells_body(columns = 1)),
                style = list(cell_borders(
                  sides = "right",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold"))) %>%
      tab_style(locations = list(cells_column_labels(columns = gt::everything())),
                style = list(cell_borders( 
                  sides = "bottom",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold")))
    #if (summaries == "mean"){
    #  names(return_table$`_data`) <- naming_conventions(names(return_table$`_data`), replace = replace)
    #}
  } else {
    if (summaries == "frequencies"){
      if (wider_table && !missing(columns_to_summarise)){
        if (together){
          values_from <- "Count (%)"
        } else {
          values_from <- "n"
        }
        return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = values_from, names_prefix = "")
      }
      if (naming_convention){
        colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
      }
    }
  }
  return(return_table)
}

flow_data_summary_function <- function(flow_interaction, flow_name = NULL){
  if (!is.data.frame(flow_interaction)){
    flow_interaction <- plyr::ldply(flow_interaction) 
  }
  flow_interaction$interacted <- ifelse(flow_interaction$interacted == TRUE, "Yes", "No")
  flow_interaction$interacted <- forcats::fct_expand(flow_interaction$interacted, c("Yes", "No"))
  
  flow_interaction_output <- flow_interaction %>%
    group_by({{ flow_name }}, interacted, .drop = FALSE) %>%
    summarise(count = n(), perc = round(n()/nrow(.)*100,2)) %>%
    mutate("Count (%)" := str_c(`count`, ' (', round(`perc`, 1), ")")) %>%
    dplyr::select(-c(count, perc)) %>% map_df(rev)
  
  return(flow_interaction_output)
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

create_user_dataframe <- function(flow_interaction){
  temp<-flow_interaction %>% group_by(uuid,response) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
  temp2<-flow_interaction %>% filter(response == TRUE) %>% group_by(uuid,category) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
}
