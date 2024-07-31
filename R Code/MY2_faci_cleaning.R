# run to line 438 in shiny_cleaning_PT2.R (df <- df %>% ungroup())

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


#### parent_data_table <- readxl::read_excel("MX_fac_data_20240719.xlsx")
facs <- unique(df$facilitator)

faciNK_data <- faciNK_data %>% filter(app_user_id %in% facs) #our_facilitators)

faciNK_data <- faciNK_data %>%
  rename_with(~str_replace(., "rp.contact.field.\\.", ""), starts_with("rp.contact.field."))

#faciNK_data <- faciNK_data1
faciNK_data$contact_fields <- NULL
# x <- faciNK_data$rp.contact.field.current_package
# faciNK_data$rp.contact.field.current_package <- NULL

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
  faci1_o <- unique(faci1_o) %>% dplyr::select(-c("variable1"))
  faci1_o$ID <- rep(seq(1, nrow(faci1_o) / 2), each = 2)
  faci1_o1[[j]] <- pivot_wider(faci1_o, names_from = "variable2", values_from = "list") %>%
    mutate(facilitator = faciNK_data$app_user_id[j]) %>%
    mutate(parent_no = paste0("parent_", parent_no))
}
faci1_o1 <- bind_rows(faci1_o1) %>%
  dplyr::select(facilitator, value = parent_no, research_id = external_id)

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

research_ids <- df$research_id
valid_ids <- df$id
df_id <- data.frame(research_id = research_ids, id = valid_ids)

x <- full_join(faci_all, faci1_o1)

package_info <- faciNK_data %>% dplyr::select(c("rp.contact.field.current_package", "app_user_id", "createdAt", "updatedAt"))

y <- full_join(x, package_info, by = c("facilitator" = "app_user_id"))

# MY_data_20240610 <- read_excel("MY_data.20240610.xlsx")
# unique(MY_data_20240610)

writexl::write_xlsx(y, "MY_facilitator_data_20240729.xlsx")

#faciNK_data$rp.contact.field.current_package



x <- package_info %>% filter(rp.contact.field.current_package == "masw") %>% pull(app_user_id)

y1 <- y %>% filter(facilitator %in% x) %>% pull(research_id) %>% unique()

df1 <- df %>% filter(research_id %in% y1)

View(df1)

# 

y1 <- y %>% filter(research_id %in% c(347363, 354767, 351065, 353533, 336257, 349831)) 
View(y1)


# 3507adcfcb069036
# bdea28b5-5436-4c8d-b6a0-44d01c266818
# 0b0f69d149fe2322
# e94dae2527feead4
# b4cd34e40bd7a0dd
# 01a192d9-411b-47fc-a18d-7bb7b620692b

# 354767, 336257, 353533            3507adcfcb069036
# 349831, 353533, 347363, 354767    bdea28b5-5436-4c8d-b6a0-44d01c266818   and then also 
# 353533,                   0b0f69d149fe2322 351065, 321321
# 347363                    e94dae2527feead4   351065
# 349831, 336257,  353533   b4cd34e40bd7a0dd
# 336257, 347363

#y %>% filter(facilitator == "01a192d9-411b-47fc-a18d-7bb7b620692b") %>% pull(research_id) %>% unique()
