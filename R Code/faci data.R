#' I was wondering if the FaciNK data for the KEMAS group was ready to send to us?
#' 
#' We need to estimate costing for the implementation, and there should be some relevant data
#' (e.g. time spent by facilitators) collected in FaciNK. If it isn’t ready to send,
#' could you give us a summary of the results relating to that data?

our_facilitators <- unique(df$facilitator)

faciNK_data <- faciNK_data %>% filter(app_user_id %in% our_facilitators)

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
faci_all <- faci_all %>% mutate(group = ifelse(package == "masw",
                                               "CSOS",
                                               "KEMAS"))
faci_all <- faci_all %>% dplyr::select(facilitator, week, variable = variable1, value = list, group)
faci_all <- faci_all %>% filter(!variable %in% c("report_id", "parent_group_no"))
faci_all$rp.contact.field.current_package <- NULL
#writexl::write_xlsx(faci_all, "MY_facilitator_data_20240416.xlsx")

# adding in IDs
faci1_o1 <- NULL
for (j in 1:length(faciNK_data$rp.contact.field.parent_data)){
  faci1 <- fromJSON(faciNK_data$rp.contact.field.parent_data[[j]])
  faci1_o <- read_corpora(faci1) %>% dplyr::filter(variable2 %in% c("parent_no", "external_id"))
  faci1_o <- unique(faci1_o) %>% dplyr::select(-c("variable1"))
  faci1_o$ID <- rep(seq(1, nrow(faci1_o) / 2), each = 2)
  faci1_o1[[j]] <- pivot_wider(faci1_o, names_from = "variable2", values_from = "list") %>%
    mutate(facilitator = faciNK_data$app_user_id[j]) %>%
    mutate(parent_no = paste0("parent_", parent_no))
}
faci1_o1 <- bind_rows(faci1_o1) %>%
  dplyr::select(facilitator, value = parent_no, research_id = external_id)

if ("754971" %in% faci1_o1$research_id){
  faci1_o1 <- faci1_o1 %>%
    mutate(research_id = if_else(research_id == "754971", "754791", research_id))
}


# proof that the archived data is meaningless:
faci1_o12 <- NULL
for (j in 1:length(faciNK_data$rp.contact.field.archived_parent_data)){
  faci1 <- fromJSON(faciNK_data$rp.contact.field.archived_parent_data[[j]])
  if (length(faci1) > 0){
    faci1_o <- read_corpora(faci1) %>% dplyr::filter(variable2 %in% c("parent_no", "external_id"))
    faci1_o <- faci1_o %>% dplyr::select(-c("variable1"))
    if (j == 11){ faci1_o <- faci1_o %>% filter(list != 1) }
    faci1_o$ID <- rep(seq(1, nrow(faci1_o) / 2), each = 2)
    faci1_o12[[j]] <- pivot_wider(faci1_o, names_from = "variable2", values_from = "list") %>%
      mutate(facilitator = faciNK_data$app_user_id[j]) %>%
      mutate(parent_no = paste0("parent_", parent_no))
  }
}

faci1_o12 <- bind_rows(faci1_o12) %>%
  dplyr::select(facilitator, value = parent_no, research_id = external_id)


x <- left_join(faci_all, faci1_o1)
x <- left_join(x, faci1_o12, by = c("facilitator", "value"))
#x %>% filter(!is.na(research_id.x)) %>% filter(!is.na(research_id.y))
x <- x %>%
  mutate(research_id = ifelse(is.na(research_id.x), research_id.y,
                              ifelse(is.na(research_id.y), research_id.x,
                                     NA))) %>%
  dplyr::select(-c(research_id.x, research_id.y))

x <- x %>% mutate(variable = if_else(variable == "present", "parent", variable))
x <- full_join(x, df_id)
writexl::write_xlsx(x, "MY_facilitator_data_20240416.xlsx") # overall together


# df_id <- df %>% dplyr::select(c(research_id, id))
# x_parent_data <- x %>% filter(variable == "parent") %>%
#   dplyr::select(-c(variable))
# x_parent_data <- full_join(x_parent_data, df_id)
# writexl::write_xlsx(x_parent_data, "MY_facilitator_parent_data_20240416.xlsx") # data at the facilitator, week, and parent level for the present parents each week
# 
# x_fac_data <- x %>% filter(variable != "parent")
# x_parent_data <- full_join(x_parent_data, df_id)
# writexl::write_xlsx(x_parent_data, "MY_facilitator_parent_data_20240416.xlsx") # data at the facilitator, week, and parent level for the present parents each week




read_corpora <- function(data){
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
    # first, check for description and metadata
    if (!is.null(names(data[i])) && names(data[i]) == "description") {
      description <- data[i][[1]]
    } else if (!is.null(names(data[i])) && names(data[i]) == "meta"){
      data_unlist[[i]] <- NULL
      # then check if the element is a vector, matrix, data frame, or list.
    } else if (class(data[[i]]) %in% c("character", "factor", "logical", "numeric", "integer")){
      data_unlist[[i]] <- data.frame(list = data[[i]])
    } else if ("matrix" %in% class(data[[i]])){
      data_unlist[[i]] <- data.frame(list = do.call(paste, c(data.frame(data[[i]]), sep="-")))
    } else if (class(data[[i]]) == "data.frame"){
      data_unlist[[i]] <- data.frame(list = data[[i]])
    } else if (class(data[[i]]) == "list"){
      if (length(data[[i]]) == 0) {
        data_unlist[[i]] <- data.frame(NA)
      } else {
        # unlist the list, to create a data frame with two elements: list name ("rowname") and value
        # if there are nested lists, the "rowname" variable combines the different lists together.
        # We want to separate these into separate variables to make the data more usable and readable.
        # We do this by `str_split_fixed`, and `gsub`.
        new_data <- tidyr::as_tibble(unlist(data), rownames = "rowname")
        split <- stringr::str_split_fixed(string=new_data$rowname, pattern=stringr::coll(pattern="."), n=Inf)
        split <- gsub("[0-9]$|[0-9][0-9]$","",split)
        # add in the separated list to the value variable, and rename the variables
        data_unlist[[i]] <- cbind(data.frame(split), value = new_data$value)
        names(data_unlist[[i]]) <- c(paste0("variable", 1:(length(data_unlist[[i]])-1)), "list")
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
