# R code: ParentApp South Africa
pak::pak("IDEMSInternational/rapidpror")

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(rjson)
library(readxl)
library(httr)
library(jsonlite)
library(rapidpror)
library(DT)
library(rio)
library(openappr)
library(httr)
library(jsonlite)

source("Functions.R")

# Replace key <- with the key you have for South Africa
key <- read.table("tokens/PT2_south_africa_key.txt", quote="\"", comment.char="")
site <- "https://app.rapidpro.io/api/v2/"

# set-up
set_rapidpro_site(site = site)
set_rapidpro_key(key = key$V1)
set_rapidpro_uuid_names()

# from rapidpror
handle_type_flow <- function(data, type = "completed", type_2 = "category") {
  response <- ifelse(is.na(data$values[[type]][[type_2]]), "No response", data$values[[type]][[type_2]])
  return(tibble::tibble(uuid = data$contact$uuid, interacted = data$responded, response, created_run_on = data$created_on))
}

################################################################################
#### Get the data ##############################################################
################################################################################
contacts_unflat <- rapidpror::get_user_data(flatten = FALSE, date_from = NULL, filter_variable  = NULL )

names(contacts_unflat$groups) <- contacts_unflat$uuid
groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
groups_data <- groups_data %>%
  dplyr::filter(name %in% c("pilot chihuahua")) %>% #, "pilot", "mexicopilot")) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::select(-uuid)
groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)

df <- data.frame(groups_data)

#### CODEBOOK BITS ------------------------------------------------------------
# Replace this with your local directory to the codebook
rapidpro_variables <- read_excel("C:/Users/lclem/Downloads/PLH ParentText CODE BOOK.xlsx")

rapidpro_variables$`Data Element Name` <- gsub("_t_", "", rapidpro_variables$`Data Element Name`)

sa_data <- jsonlite::flatten(contacts_unflat) %>%
  rename_with(~str_replace(., "fields\\.", ""), starts_with("fields."))

# Filter to the relevant variables -------------------------------------------------------------------
unique(rapidpro_variables$Country)

# e.g. for Mexico
# Replace this with the variables of interest for South Africa. - I think "all", "All", "SA", and "SA only", but do check.
rapidpro_variables <- rapidpro_variables %>% filter(Country %in% c("all", "All", "mexico"))

sa_data <- add_na_variable(sa_data, rapidpro_variables$`Data Element Name`) %>%
  dplyr::select(all_of(rapidpro_variables$`Data Element Name`))

sa_data <- sa_data[, colSums(!is.na(sa_data)) > 0]

sa_data$urns <- NULL
sa_data$family_name <- NULL
sa_data$first_user_name <- NULL
sa_data$child_nickname <- NULL

contacts_unflat$fields$goals_accessed

contacts_unflat1 <- contacts_unflat %>% filter(uuid %in% df$uuid_2)
nrow(contacts_unflat1)

contacts_unflat1$fields$n_goals_completed
contacts_unflat1$fields$s_modules_compl


# ENGAGEMENT DATA ===============================================================
# this is for Mexico. Not sure what you want for South AFrica here -- whichever groups_data relates to SA
df_groups <- groups_data %>% dplyr::select(c(id, pilot, mexicopilot, `pilot chihuahua`)) 
df <- NULL
df$research_id <- contacts_unflat$fields$research_id
df <- data.frame(df)
df$uuid_2 <- contacts_unflat$uuid
df$n_goals_completed_f <- factor(contacts_unflat$fields$n_goals_completed)
df$n_goals_completed <- as.numeric(contacts_unflat$fields$n_goals_completed)
df$n_goals_prog <- as.numeric(contacts_unflat$fields$n_goals_prog)
df$perc_goals_completed <- round(df$n_goals_completed / df$n_goals_prog * 100, 1)
df$perc_goals_completed_f <- df$perc_goals_completed # _f <- factor(df$perc_goals_completed)

# goal ids
# put in the relevant goals here:
goals <- c("relation", "stress")
#              "develop_t", "learning_t", "structure_t",
#              "behave_t", "wellbeing_t", "safety_t",
#              "develop_c", "learning_c", "structure_c",
#              "behave_c", "wellbeing_c", "safety_c", "ipv", "budget")
n_mod_completed <- paste0("goal_", goals, "_n_mod_compl")
n_mod_total <- paste0("goal_", goals, "_n_mod")

contacts_unflat$fields <- add_na_variable(contacts_unflat$fields, n_mod_completed)
contacts_unflat$fields <- add_na_variable(contacts_unflat$fields, n_mod_total)

contacts_unflat$fields <- contacts_unflat$fields %>%
  # modules they've completed
  dplyr::mutate(across(all_of(n_mod_completed), ~as.numeric(.))) %>%
  dplyr::mutate(across(all_of(n_mod_completed), ~replace_na(., 0))) %>%
  dplyr::mutate(n_modules_completed = rowSums(across(n_mod_completed))) %>%
  
  # total number in modules
  dplyr::mutate(across(all_of(n_mod_total), ~as.numeric(.))) %>%
  dplyr::mutate(across(all_of(n_mod_total), ~replace_na(., 0))) %>%
  dplyr::mutate(n_modules_total = rowSums(across(n_mod_total))) %>%
  
  # percentage completed
  dplyr::mutate(perc_modules_completed = round(n_modules_completed/n_modules_total * 100, 1))

df$n_modules_completed_numeric <- contacts_unflat$fields$n_modules_completed
df$n_modules_completed <- as_factor(contacts_unflat$fields$n_modules_completed)
df$perc_modules_completed <- (contacts_unflat$fields$perc_modules_completed)

df$n_modules_total <- contacts_unflat$fields$n_modules_total

# checks with phone number
# I think this is just Mexico stuff:
plh_data_mx <- readxl::read_excel("C:/Users/lclem/Downloads/Copia for PLH_ Parenting_at_Scale_Baseline_FullDataset.xlsx")
plh_data_mx <- plh_data_mx %>% dplyr::select(c("Demo-ep_uid", "Demo-ep_phone_entered"))
incorrect_ids <- c("783485", "893881", "831097", "715805")
additional_ids <- c("349782", "473468", "692834", "113613", "696326")
valid_ids <- c(plh_data_mx$`Demo-ep_uid`, incorrect_ids, additional_ids)
df_1 <- df %>% filter(research_id %in% valid_ids)
nrow(plh_data_mx)
nrow(df_1)



# # there's four in there with no id
# no_id <- plh_data_mx %>%
#   mutate(x = ifelse(`Demo-ep_uid` %in% df$research_id,
#                     1,
#                     0)) %>%
#   filter(x == 0)
## Phone Numbers
# df$phone <- contacts_unflat$urns
# df$phone <- unlist(df$phone)
# df$phone <- gsub("whatsapp:", "", df$phone)
# df$phone <- substr(df$phone, nchar(df$phone) - 9, nchar(df$phone))
# plh_data_mx <- plh_data_mx %>% dplyr::select(c("Demo-ep_uid", "Demo-ep_phone_entered"))
# plh_data_mx$`Demo-ep_phone_entered` <- gsub("\\s+", "", plh_data_mx$`Demo-ep_phone_entered`)
# df_1 <- df %>% filter(phone %in% plh_data_mx$`Demo-ep_phone_entered`)
# plh_data_mx <- plh_data_mx %>%
#   mutate(is_match = ifelse(`Demo-ep_phone_entered` %in% df$phone,
#                            1,
#                            0))
# no_phone <- plh_data_mx %>%
#   filter(is_match == 0)
# plh_data_mx$`Demo-ep_uid` <- as.character(plh_data_mx$`Demo-ep_uid`)
# df_phone_nos <- full_join(df %>% dplyr::select(c(research_id, phone)), plh_data_mx,
#                   by = c("research_id" = "Demo-ep_uid")) %>%
#   filter(!is.na(is_match))
# writexl::write_xlsx(df_phone_nos, "MX_different_phones.xlsx")


df <- left_join(df_1, df_groups , by = c("uuid_2" = "id"))
# 
# df_na <- df %>% filter(is.na(pilot)) %>% pull("uuid_2")
# View(contacts_unflat %>% filter(uuid %in% df_na))

# for saving MX
df_send <- df %>%
  dplyr::select(c(id = uuid_2, formando = pilot, iniciar = mexicopilot, afectiva = `pilot chihuahua`, research_id,
                  n_goals_completed, total_goals_to_do = n_goals_prog, perc_goals_completed,
                  n_modules_completed, total_modules_to_do = n_modules_total, perc_modules_completed))

writexl::write_xlsx(df_send, paste0("MX_engagement_", format(Sys.Date(), "%Y%m%d"),".xlsx"))





# Do we have faciNK data for SA? - for MX, it is this: ==============================================================

# individuals in df_send$research_id
valid_ids <- df_send$research_id

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
    #print(class(data[[i]]))
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
valid_research_id <- valid_ids
facilitators <- NULL
j <- 1
for (i in 1:length(parent_data)){
  parent_data_table_i <- bind_rows(parent_data[[i]])
  
  group <- faciNK_data$`rp.contact.field.current_package`[[i]]
  
  # get external if it is one of valid_research_id
  external_id <- parent_data_table_i$external_id 
  match_id <- match(external_id, valid_research_id)
  
  id <- faciNK_data$app_user_id[i]
  
  if (group %in% c("study_dif_bc", "study_dif_mich", "study_dif_chih", "study_dif_cdmx")){
    #if ((!is.na(match_id) && any(match_id)) || id == "954c174f-4e0a-4837-8506-c9798bba5a42"){
    print(i)
    #print(i)
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
parent_data_table <- parent_data_table %>% dplyr::select(c(facilitator, parent_group_no, external_id, group_name, duplicated_parent_group, first_name, last_name))
#parent_data_table$parent_group_no <- NULL
parent_data_table <- unique(parent_data_table)

parent_data_table <- parent_data_table %>%
  dplyr::select(facilitator_id = facilitator,
                research_id = external_id,
                first_name,
                last_name)
parent_data_table <- parent_data_table %>%
  dplyr::mutate(in_rapidpro = ifelse(research_id %in% valid_ids, 1, 0))

writexl::write_xlsx(parent_data_table, "facilitator_app_parent_data.xlsx")


# 
# faci1_o1 <- bind_rows(faci1_o1) %>%
#   dplyr::select(facilitator, value = variable1, research_id = external_id)
# 
# research_ids <- valid_ids
# df_id <- data.frame(research_id = research_ids, id = valid_ids)
# 
# # left join -- only have facilitators who have used the app.
# facs_in_app <- full_join(faci_all, faci1_o1)
# 
# facs_in_app_parent_ids <- facs_in_app %>% filter(!is.na(research_id))
# 
# rapidpro_engagement_data <- df_send %>% dplyr::mutate(missing_data = ifelse(is.na(formando), 1, 0)) %>%
#   dplyr::select(c(research_id, missing_data))
# 
# facs_in_app_parent_ids <- facs_in_app_parent_ids %>% dplyr::select(c(facilitator, research_id))
# facs_in_app_parent_ids <- unique(facs_in_app_parent_ids)
# facs_in_app_parent_ids <- full_join(facs_in_app_parent_ids, rapidpro_engagement_data)
# facs_in_app_parent_ids <- facs_in_app_parent_ids %>%
#   filter(research_id %in% valid_ids)
# View(facs_in_app_parent_ids)
# 
# # they want parent names in there too
# 
# all_mx <- readxl::read_excel("all_mx_data_not_anon.xlsx")
# all_mx <- all_mx %>% dplyr::select(c(research_id, first_user_name, family_name))
# View(all_mx)
# 
# facs_in_app_parent_ids <- left_join(facs_in_app_parent_ids, all_mx)
# 
# writexl::write_xlsx(facs_in_app_parent_ids, "facilitator_parent_data.xlsx")

parent_data_table$facilitator <- parent_data_table$facilitator_id
parent_data_table$facilitator_id <- NULL
x <- full_join(parent_data_table, facs_in_app_parent_ids)
View(x)
