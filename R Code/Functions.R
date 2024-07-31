


flow_data_table_function2 <- function(flow_interaction, flow_name = NULL){
  if (!is.data.frame(flow_interaction)){
    flow_interaction <- plyr::ldply(flow_interaction) 
  }
  
  if (!is.null(flow_interaction$interacted)){
    #flow_interaction$interacted <- ifelse(flow_interaction$interacted == TRUE, "Yes", "No")
    flow_interaction$interacted <- forcats::fct_expand(flow_interaction$interacted, c("Yes", "No"))
  } else {
    flow_interaction$interacted <- NA
  }
  flow_interaction_output <- flow_interaction %>%
    group_by({{ flow_name }}, interacted, .drop = FALSE) %>%
    summarise(Count = n()) %>% #, perc = round(n()/nrow(.)*100,2)) %>%
    #mutate("Count (%)" := str_c(`Count`, ' (', round(`perc`, 1), ")")) %>%
    #dplyr::select(-c(count, perc)) %>%
    map_df(rev)
  return(flow_interaction_output)
}

goal_transitions <- function(data = checkin_data, goal_id = "learning") {

  data <- data %>%
    mutate(time_type = ifelse(time == "Pre", "Pre", "Post"))
  
  # Separate checkin_data into pre_goal_checkin_data and post_goal_checkin_data
  pre_goal_checkin_data <- data %>%
    filter(time_type == "Pre") %>%
    filter(ID == goal_id) %>%
    select(uuid, response_old = response, ID, time_old = time)
  
  post_goal_checkin_data <- data %>%
    filter(time_type == "Post") %>%
    filter(ID == goal_id) %>%
    select(uuid, response_new = response, ID, time_new = time, improvement)
  
  # Perform a full join
  count_df <- full_join(pre_goal_checkin_data, post_goal_checkin_data, by = "uuid") %>%
    filter(!is.na(time_new)) %>%
    count(uuid, response_old, response_new, time_old, time_new)
  
  return(count_df)
}

goal_transitions_table <- function(data = checkin_data, ID_goal = "learning"){
  return(data %>%
    dplyr::filter(ID == ID_goal) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(total = n()) %>%
    dplyr::group_by(time, response, total) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::mutate(`Count (%)` = paste0(Count, " (", round(Count/total * 100, 1), "%)")) %>%
    dplyr::select(-c(total, Count)) %>%
    pivot_wider(names_from = time, values_from = `Count (%)`, values_fill = "0 (0%)"))
}

get_data_from_rapidpro_api <- function(call_type, rapidpro_site = get_rapidpro_site(), token = get_rapidpro_key(), flatten = FALSE,
                                       date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC"){
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
  user_result <- rapidpror:::httr_get_call(get_command = get_command, token = token)
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

days_active_data <- function(uuid_data = get_rapidpro_uuid_names(), flow_name, call_type = "runs.json", rapidpro_site = get_rapidpro_site(),
                             token = get_rapidpro_key(), flatten = FALSE, include_archived_data = FALSE, runs_data = "result_flow_runs.RDS", read_runs = FALSE,
                             get_by = "gotit", data_from_archived = archived_data,
                             read_archived_data_from = "archived_data_monthly.RDS", archive_call_type = "archives.json",
                             archive_period = "none"){
  
  
  if(read_runs){
    result_flow_runs <- readRDS(runs_data)
  } else {
    get_command <- paste(rapidpro_site, call_type, sep = "")
    result_flow <- rapidpror:::httr_get_call(get_command = get_command, token = token)
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
  
  # for archived data
  if (!include_archived_data){
    active_days_data <- active_days_nonarch
  } else {
    flow_data_bank[[1]] <- flow_data
    if (get_by == "read"){
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

get_survey_data <- function(parenting_variable){
  all_split_data <- NULL
  parenting_variable <- parenting_variable
  split_parenting <- stringr::str_split(parenting_variable, pattern = stringr::fixed("|"))
  if (length(split_parenting) > 0) {
    for (j in 1:length(split_parenting)){
      if (any(is.na(split_parenting[[j]]))){
        split_data <- data.frame(V1 = NA, V2 = NA, V3 = NA, row = j)
      } else {
        split_parenting_2 <- stringr::str_split(split_parenting[[j]], ",")
        split_data <- plyr::ldply(split_parenting_2[1:(length(split_parenting_2)-1)])
        split_data <- split_data %>% mutate(row = j)
      }
      all_split_data[[j]] <- split_data
    }
  }
  all_split_data <- plyr::ldply(all_split_data)
  if (length(split_parenting) > 0) { names(all_split_data) <- c("vals", "week", "dt", "row") }
  all_split_data$week <- as.numeric(as.character(all_split_data$week))
  all_split_data$vals <- as.numeric(as.character(all_split_data$vals))
  all_split_data$week <- ifelse(all_split_data$week == "1", "Baseline", all_split_data$week)
  
  return(all_split_data)
}

#' Unlist variables in a variable
#' @description After importing data from RapidPro, unlist the variables that are stored in a list
#'
#' @param data The data frame containing the data
#' @param variable The variable that contains the groups
#'
#' @return returns a data frame containing the unlisted data.
#' @export
# just consent for now
unlist_rapidpro_variable <- function(data, variable = "groups"){
  if (variable %in% names(data) && class(data[[variable]]) == "list"){
    consent <- NULL
    program <- NULL
    enrolled <- NULL
    if (length(data[[variable]]) > 0){
      for (i in 1:length(data[[variable]])){
        contact_name <- data[[variable]][[i]]
        if (length(contact_name)==0) {
          consent[i] <- NA
          program[i] <- NA
          enrolled[i] <- NA
        } else {
          consent[i] <- ifelse(any(contact_name$name %in% "consent"), "Yes", "No")
          program[i] <- ifelse(any(contact_name$name %in% "in program"), "Yes", "No")
          enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
        }
      }
    }
  }
  data$consent <- consent
  data$program <- program
  data$enrolled <- enrolled
  return(data)
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
                                together = FALSE, include_margins = FALSE, drop = FALSE){
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    summary_output <- data %>%
      mutate(across(c({{ columns_to_summarise }}), ~ (as.character(.x)))) %>%
      group_by(across(c({{ columns_to_summarise }}, {{ factors }})), .drop = drop) %>%
      summarise(n = n(),
                perc = n()/nrow(.) * 100) %>%
      ungroup()
    if (include_margins){
      cts_margin <- data %>%
        group_by(across(c({{ columns_to_summarise }})), .drop = drop) %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)      
      ftr_margin <- data %>%
        group_by(across(c({{ factors }})), .drop = drop) %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)      
      corner_margin <- data %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)
      summary_output <- bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        #ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id %in% c(2, 4), "Total", .x))) %>%
        mutate(across({{ columns_to_summarise }}, ~ifelse(id %in% c(3, 4), "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        mutate(across({{ columns_to_summarise }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
    if (together){
      summary_output <- summary_output %>%
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
    }
  } else {
    summary_output <- data %>%
      group_by(across({{ factors }})) %>%
      #summarise(across({{ columns_to_summarise }}, ~mean(.x, na.rm = TRUE)))
      summarise(across({{ columns_to_summarise }},
                       list(mean = ~ mean(.x, na.rm = TRUE),
                            min = ~ min(.x, na.rm = TRUE),
                            max = ~ max(.x, na.rm = TRUE)),
                       .names = "{.col}_{.fn}"))
    
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
  if (length(data %>% dplyr::select({{ factors }})) == 1){
    cell_values_levels <- data %>% pull({{ factors }}) %>% levels()
    if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
    
    summary_output <- summary_output %>%
      dplyr::mutate(dplyr::across({{ factors }},
                                  ~ factor(.x))) %>%
      dplyr::mutate(dplyr::across({{ factors }},
                                  ~ forcats::fct_relevel(.x, cell_values_levels)))
    summary_output <- summary_output %>% dplyr::arrange({{ factors }})
  }
  if (length(data %>% dplyr::select({{ columns_to_summarise }})) == 1){
    cell_values_levels <- data %>% pull({{ columns_to_summarise }}) %>% levels()
    if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
    
    if (summaries != "mean"){ # doesn't work for mean because we have multiple columns and rename them
      summary_output <- summary_output %>%
        dplyr::mutate(dplyr::across({{ columns_to_summarise }},
                                    ~ factor(.x))) %>%
        dplyr::mutate(dplyr::across({{ columns_to_summarise }},
                                    ~ forcats::fct_relevel(.x, cell_values_levels)))
      summary_output <- summary_output %>% dplyr::arrange({{ columns_to_summarise }})
    }
  }
  return(unique(summary_output))
}

summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                          replace = "rp.contact.field.", include_margins = FALSE, wider_table = TRUE,
                          display_table = FALSE, naming_convention = TRUE, include_percentages = FALSE,
                          together = TRUE, drop = FALSE){
  
  summaries <- match.arg(summaries)
  
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      summaries = summaries,
                                      together = together,
                                      drop = drop)
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
      all_factors <- str_split(gsub("^c\\(|\\)$", "", deparse(substitute(factors))), pattern = ", ")
      all_columns_to_summarise <- str_split(gsub("^c\\(|\\)$", "", deparse(substitute(columns_to_summarise))), pattern = ", ")
      if (wider_table && !missing(columns_to_summarise) && (any(all_factors[[1]] %in% (all_columns_to_summarise)[[1]]) == FALSE)){
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
  if ("Total" %in% colnames(return_table)){
    return_table <- return_table %>%
      relocate(Total, .after = last_col())
  }
  return(return_table)
}

flow_data_table_function <- function(flow_interaction, flow_name = NULL){
  if (!is.data.frame(flow_interaction)){
    flow_interaction <- plyr::ldply(flow_interaction) 
  }
  
  if (!is.null(flow_interaction$interacted)){
    flow_interaction$interacted <- ifelse(flow_interaction$interacted == TRUE, "Yes", "No")
    flow_interaction$interacted <- forcats::fct_expand(flow_interaction$interacted, c("Yes", "No"))
  } else {
    flow_interaction$interacted <- NA
  }
  flow_interaction_output <- flow_interaction %>%
    group_by({{ flow_name }}, interacted, .drop = FALSE) %>%
    summarise(Count = n()) %>% #, perc = round(n()/nrow(.)*100,2)) %>%
    #mutate("Count (%)" := str_c(`Count`, ' (', round(`perc`, 1), ")")) %>%
    #dplyr::select(-c(count, perc)) %>%
    map_df(rev)
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

# SRH data
srh_table_output <- function(flow_names = SRH_flow_names){
  srh_table <- NULL
  all_flow_names <- get_flow_names() %>% dplyr::select((name))
  for (i in flow_names){
    i_flow_names <- (all_flow_names %>% filter(grepl(i, name)))$name
    if (length(i_flow_names) != 0){
      i_flow <- rapidpror::get_flow_data(flow_name = i_flow_names)
      i_flow$`.id` <- (gsub(".*- ", "", i_flow$`.id`))
      srh_table[[which(flow_names == i)]] <- i_flow
      names(srh_table)[[which(flow_names == i)]] <- i
    }
  }
  return(srh_table)
}

flow_cat_frequency <- function(table = srh_data){
  freq_table <- NULL
  for (i in 1:length(table)){
    if (!is.null(table[[i]]) && nrow(table[[i]]) > 0){
      #table[[i]] <- table[[i]] %>% filter(interacted == TRUE)
      table[[i]]$flow <- table[[i]]$flow$name
      i_flow_n <- summary_table(table[[i]], factors = `flow`)
      i_flow_n$Flow <- gsub(".*- ","", i_flow_n$Flow)
      freq_table[[i]] <- i_flow_n
      names(freq_table)[[i]] <- names(table)[[i]]
    }
  }
  return(freq_table)
}

summary_plot <- function(data = plhdata_org_clean, columns_to_summarise, naming_convention = TRUE, replace = "rp.contact.field.",
                         replace_after = NULL,
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data%>%select(.data[[columns_to_summarise]])), replace = replace, replace_after = replace_after)	
  
  return_plot <- ggplot(data) +	
    viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +	
    labs(x = x_axis_label, y = "Count") +	
    theme_classic()	
  
  if(plot_type == "histogram"){
    return_plot <- return_plot + geom_histogram(data = data, aes(x = .data[[columns_to_summarise]]), stat = "count")
  } else {
    return_plot <- return_plot + geom_boxplot(data = data, aes(y = .data[[columns_to_summarise]]))
  }
  
  return(return_plot)	
}

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

numeric_summary <- function(data, factors, columns_to_summarise, summaries){
  summary_output <- data %>%
    group_by(across({{ factors }}), .drop = drop) %>%
    #mutate(across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
    summarise(across({{ columns_to_summarise }},
                     list(mean = ~ mean(.x, na.rm = TRUE),
                          min = ~ min(.x, na.rm = TRUE),
                          max = ~ max(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}"))
  return(summary_output)
}

#
#
##Get the survey data
##'
##@param parenting_variable A character vector containing ...
##@param survey_max A numerical value denoting ...
##'
##@return A list of survey data
##@export
##'
#get_survey_data <- function(parenting_variable, survey_max = 9){
#  survey_entry <- NULL
#  for (i in 1:survey_max){
#    # split the string by different surveys taken
#    split_parenting <- stringr::str_split(parenting_variable, pattern = stringr::fixed("|"))
#    
#    # for each individual, get each survey value (split by ,)
#    parenting_response <- NULL
#    for (j in 1:length(split_parenting)){
#      split_parenting_2 <- stringr::str_split(split_parenting[[j]], ",") 
#      
#      # if it is NA, keep as NA
#      if (is.na(split_parenting_2[[1]][3])){
#        parenting_response[j] <- NA
#      } else{
#        # which survey to consider? Baseline = 1, week1 = 2, etc.
#        # so get that correct week lot of surveys
#        for (k in 2:length(split_parenting_2) - 1){
#          if (as.numeric(as.character(split_parenting_2[[k]][2])) != i) {
#            split_parenting_2[[k]][3] ="1970-01-01T00:00:00.873007+08:00"
#          }
#        }
#        
#        # take the response value corresponding to the most recent date
#        response <- NA
#        date <- as.POSIXct("1970-01-01T00:00:00.873007+08:00", format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
#        
#        for (k in 2:length(split_parenting_2) - 1){
#          if (date < as.POSIXct(split_parenting_2[[k]][3], format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")) {
#            response <- split_parenting_2[[k]][1]
#          }
#        }
#        parenting_response[j] <- as.numeric(as.character(response))
#      }
#    }
#    survey_entry[[i]] <- parenting_response
#  }
#  return(survey_entry)
#}