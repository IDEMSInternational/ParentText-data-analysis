# set-up
set_rapidpro_site(site = site)
set_rapidpro_key(key = key$V1)
set_rapidpro_uuid_names()

#### Get the data ##############################################################
contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL)
names(contacts_unflat$groups) <- contacts_unflat$uuid
groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
groups_data <- groups_data %>%
  dplyr::filter(name %in% c("mexicopilot")) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::select(-uuid)
groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)

valid_ids <- as.character(groups_data$id)
contacts_unflat <- contacts_unflat %>% dplyr::filter(uuid %in% valid_ids)
research_ids <- contacts_unflat$fields$research_id


# functions
read_corpora <- function(data, nested = TRUE){
  data_all <- NULL
  description <- NULL
  # check different data types that are in the rcorpora package
  # first check if it is a data frame outright. If it is, then we just need to return the data
  if (is.data.frame(data)){
    return(data)
  } 
  # If it isn't a data frame, we check each element of the `data` argument
  data_unlist <- NULL
  for (i in 1:length(data)){
    print(class(data[[i]]))
    # first, check for description and metadata
    if (!is.null(names(data[i])) && names(data[i]) == "description") {
      description <- data[i][[1]]
    } else if (!is.null(names(data[i])) && names(data[i]) == "meta"){
      data_unlist[[i]] <- NULL
      # then check if the element is a vector, matrix, data frame, or list.
    } else if (class(data[[i]]) %in% c("character", "factor", "logical", "numeric", "integer")){
      data_unlist[[i]] <- data.frame(list = data[[i]])
    } else if ("matrix" %in% class(data[[i]])){
      data_unlist[[i]] <- data.frame(variable1 = names(data)[i], list = do.call(paste, c(data.frame(data[[i]]), sep="-")))
    } else if (class(data[[i]]) == "data.frame"){
      data_unlist[[i]] <- data.frame(list = data[[i]])
    } else if (class(data[[i]]) == "list"){
      if (length(data[[i]]) == 0) {
        data_unlist[[i]] <- data.frame(NA)
      } else {
        
        # if (nested){
        # unlist the list, to create a data frame with two elements: list name ("rowname") and value
        # if there are nested lists, the "rowname" variable combines the different lists together.
        # We want to separate these into separate variables to make the data more usable and readable.
        # We do this by `str_split_fixed`, and `gsub`.
        
        # if there's nesting, but it's not named, then we get issues I think
        # for (k in 1:length(data)){
        #   if (length(data[[k]]) > 1 && is.null(names(data[[k]]))){
        #     names(data[[k]]) <- paste0(letters[1:length(data[[k]])])
        #   }
        # }
        # 
        new_data <- tidyr::as_tibble(unlist(data[[i]]), rownames = "rowname")
        split <- stringr::str_split_fixed(string=new_data$rowname, pattern=stringr::coll(pattern="."), n=Inf)
        #split <- gsub("[0-9]$|[0-9][0-9]$","",split)
        data_unlist[[i]] <- cbind(names(data)[i], data.frame(split), value = new_data$value)
        # add in the separated list to the value variable, and rename the variables
        # } else {
        #   new_data <- do.call(rbind, lapply(names(data[[i]]), function(key) {
        #     vals <- unlist(data[[key]])
        #     data.frame(ID = key, Value = vals, stringsAsFactors = FALSE)
        #   }))
        #   data_unlist[[i]] <- new_data
        # }
        
        names(data_unlist[[i]]) <- c(paste0("variable", 1:(length(data_unlist[[i]])-1)), "list")
        
        #        data_unlist[[i]] <- cbind(data.frame(split), value = new_data$value)
        #        names(data_unlist[[i]]) <- c(paste0("variable", 1:(length(data_unlist[[i]])-1)), "list")
      } # end of ifelse lists
    } # end of list
  } # end of for loop
  names(data_unlist) <- names(data[1:length(data_unlist)])
  data_all <- plyr::ldply(data_unlist, .id = "variable1")
  
  if (!is.null(description)){
    return (data.frame(description = description, data_all))
  } 
  return (data.frame(data_all))
}

#Connect to Database to get original data
source("config/personal_setup_mx.R")
faciNK_data <- postgresr::get_user_data(site = plh_con, filter = FALSE)
names(faciNK_data) <- gsub(x = names(faciNK_data), pattern = "\\-", replacement = ".")  

parent_data <- purrr::map(
  faciNK_data$`rp.contact.field.parent_data`, 
  ~ if(!is.na(.x)) jsonlite::fromJSON(.x)
)
family_data <- purrr::map(
  faciNK_data$`rp.contact.field.family_data`, 
  ~ if(!is.na(.x)) jsonlite::fromJSON(.x)
)
parent_data_table <- NULL
family_data_table <- NULL
valid_research_id <- research_ids
facilitators <- NULL
j <- 1
for (i in 1:length(parent_data)){
  parent_data_table_i <- bind_rows(parent_data[[i]])
  
  group <- faciNK_data$`rp.contact.field.current_package`[[i]]
  
  # get external if it is one of valid_research_id
  external_id <- parent_data_table_i$external_id 
  match_id <- match(external_id, valid_research_id)
  
  if (!is.na(match_id) && any(match_id)){
    print(i)
    facilitators[j] <- i
    my_list <- family_data[[i]]
    indices <- rep(seq_along(my_list), times = sapply(my_list, length))
    parent_data_table_i <- parent_data_table_i %>% mutate(group_name = group)
    parent_data_table_i$parent_group_no <- indices
    parent_data_table_i <- parent_data_table_i %>%
      mutate(duplicated_parent_group = as.integer(!duplicated(indices)))
    parent_data_table[[j]] <- parent_data_table_i
    j <- j+1
  }
}
names(parent_data_table) <- faciNK_data$app_user_id[facilitators]
parent_data_table <- bind_rows(parent_data_table, .id = "facilitator")

# ok so now we want to find people who are parent_group_no > 1 so that we can filter the children to the 
# unique children in the analysis. 
parent_data_table <- parent_data_table %>% dplyr::select(c(facilitator, parent_group_no, external_id, group_name, duplicated_parent_group))
parent_data_table <- unique(parent_data_table)

parent_data_table$group_name <- case_match(
  parent_data_table$group_name,
  "bc_dif" ~ 1,             # randomly generated
  "mich_dif" ~ 2,
  "cdmx_dif" ~ 3,
  "chih_dif" ~ 4,
  "cdmx_bs" ~ 5,
  "edomx_bs" ~ 6
)
# which IDs are repeated in parent_data_table?

# merge with df   #df_unjoin is a copy of df
df <- parent_data_table %>%
  
  group_by(facilitator) %>%
  
  mutate(duplicated_parent_group = ifelse(is.na(duplicated_parent_group) | duplicated_parent_group == 1, 1, 2)) %>%
  
  # remove the ungrouped individuals.
  #' if someone is Unknown and they have no facilitator then is it safe to assume these are not individuals who are on the Chatbot.
  #' # Since we're now reading in all individuals who are in mexicopilot or no group.
  mutate(indicator = ifelse(group == "None" & is.na(facilitator), 1, 0)) %>%
  filter(indicator != 1) %>%
  ungroup()

#### parent_data_table <- readxl::read_excel("MX_fac_data_20240719.xlsx")
facs <- unique(df$facilitator)

faciNK_data <- faciNK_data %>% filter(app_user_id %in% facs) #our_facilitators)

faciNK_data <- faciNK_data %>%
  rename_with(~str_replace(., "rp.contact.field.\\.", ""), starts_with("rp.contact.field."))

#faciNK_data <- faciNK_data1
faciNK_data$contact_fields <- NULL
x <- faciNK_data$rp.contact.field.current_package
faciNK_data$rp.contact.field.current_package <- NULL

faci_all <- NULL
for (j in 1:length(faciNK_data$rp.contact.field.attendance_data)){
  faci1 <- fromJSON(faciNK_data$rp.contact.field.attendance_data[j])
  faci1_o <- NULL
  for (i in names(faci1)){
    faci1_o[[i]] <- read_corpora(faci1[[i]]) %>% mutate(week = i)
  }
  faci_all[[j]] <- bind_rows(faci1_o) %>% mutate(facilitator = faciNK_data$app_user_id[j]) %>%
    mutate(package = x[j])
}
faci_all <- bind_rows(faci_all)
faci_all <- faci_all %>% dplyr::select(c(facilitator, week, variable = variable1, value = list))
faci_all <- faci_all %>% filter(!variable %in% c("report_id", "parent_group_no"))
faci_all$rp.contact.field.current_package <- NULL
#writexl::write_xlsx(faci_all, "MY_facilitator_data_20240416.xlsx")

# adding in IDs
faci1_o1 <- NULL
for (j in 1:length(faciNK_data$rp.contact.field.parent_data)){
  faci1 <- fromJSON(faciNK_data$rp.contact.field.parent_data[[j]])
  faci1_o <- read_corpora(faci1) %>% dplyr::filter(variable2 %in% c("parent_no", "external_id"))
  faci1_o <- pivot_wider(faci1_o, values_from = list, names_from = variable2)
  faci1_o1[[j]] <- faci1_o %>%
    mutate(facilitator = faciNK_data$app_user_id[j])
}
faci1_o1 <- bind_rows(faci1_o1) %>%
  dplyr::select(facilitator, value = variable1, research_id = external_id)

# # proof that the archived data is meaningless:
# faci1_o12 <- NULL
# for (j in 1:length(faciNK_data$rp.contact.field.archived_parent_data)){
#   faci1 <- fromJSON(faciNK_data$rp.contact.field.archived_parent_data[[j]])
#   if (length(faci1) > 0){
#     faci1_o <- read_corpora(faci1) %>% dplyr::filter(variable2 %in% c("parent_no", "external_id"))
#     faci1_o <- faci1_o %>% dplyr::select(-c("variable1"))
#     if (j == 7){ faci1_o <- faci1_o %>% filter(list != 1) }
#     faci1_o$ID <- rep(seq(1, nrow(faci1_o) / 2), each = 2)
#     faci1_o12[[j]] <- pivot_wider(faci1_o, names_from = "variable2", values_from = "list") %>%
#       mutate(facilitator = faciNK_data$app_user_id[j]) %>%
#       mutate(parent_no = paste0("parent_", parent_no))
#   }
# }
# 
# faci1_o12 <- bind_rows(faci1_o12) %>%
#   dplyr::select(facilitator, value = parent_no, research_id = external_id)
#

# faci_all - comes from attendance data. 
# faci1_o1 - from parent data we can see parents who are associated to facilitators but we don't know when
# they were present (onboarding / week 1) because they're not in attendance data

df_id <- data.frame(research_id = research_ids, id = valid_ids)

x <- full_join(faci_all, faci1_o1)


# faci_all
# faci_all %>% filter(variable == "present")
# all fine

# faci1_o1

x$facilitator_id <- x$facilitator
x <- x %>%
  mutate(facilitator_id = factor(facilitator),
         ID_indicator = LETTERS[as.integer(facilitator_id)])

x <- x %>% mutate(variable = if_else(variable == "present", "parent", variable))
x_parent <- x %>% filter(!is.na(research_id))
x_non_parent <- x %>% filter(is.na(research_id))
y <- left_join(x_parent, df_id)
x <- bind_rows(y, x_non_parent)
x <- x %>% arrange(ID_indicator)

# # number of individuals each week
x_parent1 <- x_parent %>%
  mutate(week = replace_na(week, "unknown")) %>%
  dplyr::select(facilitator_id, ID_indicator, value, week) %>%
  unique() %>%
  group_by(facilitator_id, ID_indicator, week) %>%
  summarise(`number of parents` = n()) #%>%
#pivot_wider(names_from = week, values_from = `number of parents`)
# 
# # time
df_time_spent <- x_non_parent %>%
  dplyr::select(-c(research_id)) %>%
  dplyr::filter(variable %in% c("prep_time", "travel_time", "session_time")) %>%
  dplyr::mutate(value = as.numeric(value))


summary_onboarding <- x_non_parent %>%
  filter(week == "onboarding") %>%
  dplyr::select(-c(research_id)) %>%
  dplyr::filter(variable %in% c("welcome_name_game", "welcome_overview", "start_ID_capture",
                                "start_phone_capture", "start_phone_functions", "activity_1",
                                "activity_2", "close", "close_reflection", "close_goodbye"))

summary_onboarding$variable <- fct_relevel(summary_onboarding$variable, unique(summary_onboarding$variable))

x_non_parent_msgs <- x_non_parent %>%
  filter(week != "onboarding") %>%
  dplyr::select(-c(research_id)) %>%
  dplyr::filter(variable %in% c("sent_messages"))



#' It might be helpful to see a basic line chart that shows reports
#' submitted over time.
#' You might have 1 line for each report type with weeks or months on the x-axis

View(x)

# value = time, week = onbording or week1, 

# TODO: check with esmee that variable == "edited" is the report time.
# x_report_time <- x %>%
#   filter(variable == "edited") %>%
#   mutate(time = as.Date(value))
# 
# ggplot(x_report_time, aes(x = time, y = "1")) + geom_line()

#' On top: The total number of parents currently enrolled via Facilitator App. 
x <- x %>%
  mutate(unique_facilitator = ifelse(is.na(facilitator) | duplicated(facilitator), 2, 1),
         unique_researcher = ifelse(is.na(research_id) | duplicated(research_id), 2, 1))

x <- x %>%
  mutate(week_parent = ifelse(is.na(research_id), "NA",
                               ifelse(is.na(week) & is.na(variable), "unknown",
                                      ifelse(week == "onboarding" & variable == "parent", "onboarding",
                                             ifelse(week == "week_1" & variable == "parent", "week_1", 
                                                    "check")))))

#' The total parents reported as participating in a given week
# participated in onboarding week
parent_by_week <- x_parent %>%
  mutate(week = replace_na(week, "Unknown Week")) %>%
  dplyr::select(research_id, week) %>%
  unique() %>%
  group_by(week) %>%
  summarise(`Number Present` = n()) %>%
  mutate(week = fct_recode(week, Onboarding = "onboarding", `Week 1` = "week_1")) %>%
  mutate(week = fct_relevel(week, "Unknown Week", after = Inf))

# in week 1





# 
# 
# 
# # Create the heatmap
# ggplot(summary_onboarding) +
#   geom_tile(aes(x = variable, y = facilitator, fill = value), color = "white") +
#   scale_fill_manual(values = c(`TRUE` = "green", `FALSE` = "red")) +
#   theme_minimal() +
#   labs(title = "Heatmap of Activities",
#        x = "Activity",
#        y = "Facilitator") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


# summary_onboarding %>%
#   pivot_wider(names_from = variable, values_from = value)
# # 
# # Convert TRUE/FALSE to 1/0 for calculation
# calc_data <- as.data.frame(lapply(summary_onboarding[-c(1, 2)], as.logical))
# calc_data <- as.data.frame(lapply(calc_data, as.integer))
# 
# # Calculate totals row and totals column
# totals_row <- colSums(calc_data)
# totals_col <- rowSums(calc_data)
# 
# # Add totals row to the original data frame
# totals_row_df <- data.frame(t(c("Total", "", totals_row)), stringsAsFactors = FALSE)
# colnames(totals_row_df) <- colnames(summary_onboarding)
# data_with_totals <- rbind(x, totals_row_df)
# 
# # Add totals column to the original data frame
# data_with_totals$Total <- c(totals_col, sum(totals_row))
# 
# data_with_totals <- data_with_totals %>%
#   mutate(across(everything(), ~ifelse(.x == TRUE, "Yes", ifelse(.x == FALSE, "No", .x))))
# 

