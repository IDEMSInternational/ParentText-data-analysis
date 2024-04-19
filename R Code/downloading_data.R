add_na_variable <- function(data = user_data, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}


####### GET DATA:
# for downloading data - end of Shiny_cleaning_PT2:
all_valid_ids <- df$id

set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])
#set_rapidpro_uuid_names()

contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL, call_type = "contacts.json")

# 
contacts_unflat <- contacts_unflat %>% dplyr::filter(as.Date(created_on) >= as.Date("2023-08-17"))
## Set up of variables

# TOP BOXES ----------------------------------------------------
if (country == "Malaysia_2"){
  names(contacts_unflat$groups) <- contacts_unflat$uuid
  val <- NULL
  j <- 1
  for (i in 1:length(contacts_unflat$groups)){
    if (length(contacts_unflat$groups[[i]]) == 0){
      val[[j]] <- c(id = contacts_unflat$uuid[i])
      j <- j + 1
    }
  }
  vals <- bind_rows(val) %>% mutate(csos = 0, kemas = 0)
  groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
  groups_data <- groups_data %>%
    dplyr::filter(name %in% c("kemas", "csos", "joined")) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::select(-uuid)
  groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
  groups_data <- bind_rows(groups_data, vals)
} else {
  names(contacts_unflat$groups) <- contacts_unflat$uuid
  groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
  groups_data <- groups_data %>%
    dplyr::filter(name %in% c("in program", "joined", "enrolled")) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::select(-uuid)
  groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
}

contacts_unflat <- contacts_unflat %>%
  dplyr::filter(uuid %in% all_valid_ids) %>%
  arrange(uuid)


#### CODEBOOK BITS ------------------------------------------------------------

rapidpro_variables <- read_excel("C:/Users/lclem/Downloads/PLH ParentText CODE BOOK.xlsx")

if (country == "Mexico"){
  rapidpro_variables$`Data Element Name` <- gsub("_t_", "", rapidpro_variables$`Data Element Name`)
}
sa_data <- jsonlite::flatten(contacts_unflat) %>%
  rename_with(~str_replace(., "fields\\.", ""), starts_with("fields."))


# Filter to the relevant variables -------------------------------------------------------------------
unique(rapidpro_variables$Country)

# e.g. for Malaysia
if (country %in% c("Malaysia", "Malaysia_2")){
  rapidpro_variables <- rapidpro_variables %>% filter(Country %in% c("all", "All", "no SA"))
}

sa_data <- add_na_variable(sa_data, rapidpro_variables$`Data Element Name`) %>%
  dplyr::select(all_of(rapidpro_variables$`Data Element Name`))

sa_data <- sa_data[, colSums(!is.na(sa_data)) > 0]

writexl::write_xlsx(sa_data, "MY_data_5.20240415.xlsx")

##################################################################################################

# data_given_hi <- malaysia_users_snapshot_29022024 %>%
#   rename_with(~str_replace(., "fields\\.", ""), starts_with("fields."))
# 
# data_given_hi <- add_na_variable(data_given_hi, rapidpro_variables$`Data Element Name`) %>%
#   dplyr::select(all_of(rapidpro_variables$`Data Element Name`))
# 
# data_given_hi <- data_given_hi[, colSums(!is.na(data_given_hi)) > 0]
# 
# got_uuid <- df$id
# 
# data_given_hi <- data_given_hi %>%
#   filter(uuid %in% got_uuid)
#   #mutate(indicator = ifelse(uuid %in% got_uuid, 2, 1))
# hi$groups <- NULL
# hi$urns <- NULL
# x <- bind_rows(data_given_hi, hi)
# View(x)
#writexl::write_xlsx(x, "MY_data.20240416.xlsx")

#got_uuid <- sa_data$uuid

data_given_hi <- malaysia_users_snapshot_29022024 %>% filter(!uuid %in% sa_data$uuid)

# 
# hi <- sa_data %>% filter(!uuid %in% got_uuid)
# hi <- hi %>% filter(as.Date(created_on) < as.Date("2024-01-01"))
# 
# #870091 - KEMAS    # filter to unique research IDs
# 
# hi <- hi %>% filter(uuid != "33a88885-586b-42ff-a553-10e431e104ee")

#

#writexl::write_xlsx(data_given_hi, "MY_data.20240416.xlsx")
#writexl::write_xlsx(x, "MY_data_extra_ones.20240416.xlsx")



######################################################################################################

source("config/personal_setup_mx.R")
faciNK_data <- postgresr::get_user_data(site = plh_con, filter = FALSE)
names(faciNK_data) <- gsub(x = names(faciNK_data), pattern = "\\-", replacement = ".")  

faciNK_data <- faciNK_data %>%
  filter(rp.contact.field.current_package %in% c("cdmx_dif", "cdmx_bs", "bc_dif", "edomx_bs", "mich_dif", "chih_dif"))

parent_data <- purrr::map(faciNK_data$`rp.contact.field.parent_data`, jsonlite::fromJSON)
family_data <- purrr::map(faciNK_data$`rp.contact.field.family_data`, jsonlite::fromJSON)
parent_data_table <- NULL
family_data_table <- NULL
valid_research_id <- mx_data1$research_id
facilitators <- NULL

j <- 1
for (i in 1:length(parent_data)){
  parent_data_table_i <- bind_rows(parent_data[[i]])
  
  group <- faciNK_data$`rp.contact.field.current_package`[[i]]
  
  # get external if it is one of valid_research_id
  external_id <- parent_data_table_i$external_id 
  match_id <- match(external_id, valid_research_id)
  
  
  # [1]   9  30  51  54  70  78  84  92 115 127
  
  # or if no masw 5, 28, 46, 49, 52, 64, 68
  if (!is.na(match_id) && any(match_id)){
    print(i)
    if ("754971" %in% parent_data_table_i$external_id){
      parent_data_table_i <- parent_data_table_i %>%
        mutate(external_id = ifelse(external_id == "754971", "754791", external_id))
    }
    facilitators[j] <- i
    my_list <- family_data[[i]]
    indices <- rep(seq_along(my_list), times = sapply(my_list, length))
    parent_data_table_i <- parent_data_table_i %>% mutate(kemas_group = group)
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
parent_data_table <- parent_data_table %>% dplyr::select(c(facilitator, parent_group_no, external_id, kemas_group, duplicated_parent_group))
parent_data_table <- unique(parent_data_table)


parent_data_table$group <-parent_data_table$kemas_group 
parent_data_table$kemas_group <- NULL

unique(parent_data_table$group)

parent_data_table$group <- ifelse(parent_data_table$group == "cdmx_dif", "A",
                                  ifelse(parent_data_table$group == "mich_dif", "B",
                                         ifelse(parent_data_table$group == "bc_dif", "C",
                                                "D")))

writexl::write_xlsx(parent_data_table, "Mexico_facilitator_data.20240315.xlsx")




