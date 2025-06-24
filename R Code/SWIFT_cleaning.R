#average goals and modules completed from a percentage of total to a M number and SD
country <- "SWIFT"# South_Africa_2" #Malaysia_2"

### ParentText 2.0 ###
set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])
set_rapidpro_uuid_names()

bind_rows <- dplyr::bind_rows
filter <- dplyr::filter
contacts_unflat <- rapidpror::get_user_data(flatten = TRUE, date_from = NULL, call_type = "contacts.json",
                                            filter_variable = NULL)
#contacts_unflat <- contacts_unflat2
names(contacts_unflat) <- gsub("^fields\\.", "", names(contacts_unflat))

# Add group data
names(contacts_unflat$groups) <- contacts_unflat$uuid
val <- NULL
j <- 1
for (i in 1:length(contacts_unflat$groups)){
  if (length(contacts_unflat$groups[[i]]) == 0){
    val[[j]] <- c(id = contacts_unflat$uuid[i])
    j <- j + 1
  }
}
vals <- bind_rows(val)


groups_data <- plyr::ldply(contacts_unflat$groups, function(x) {
  if (is.null(x)) {
    return(data.frame(group = NA))
  } else if (is.atomic(x)) {
    return(data.frame(group = x))
  } else if (is.data.frame(x)) {
    return(x)
  } else {
    return(data.frame(group = as.character(x)))
  }
}, .id = "id")

groups_data <- groups_data %>%
  dplyr::mutate(value = 1) %>%
  dplyr::select(-uuid)
groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)
groups_data <- bind_rows(groups_data, vals)

df <- data.frame(groups_data) %>%
  mutate(id = as.character(id)) %>%
  arrange(id)

valid_ids <- df %>% filter(notification.testers != 1) %>% filter(testers != 1) %>% pull(id)
df$testers <- NULL
df$notification.testers <- NULL
contacts_unflat <- contacts_unflat %>%
  dplyr::filter(uuid %in% valid_ids) %>%
  arrange(uuid)

contacts_unflat <- left_join(contacts_unflat, df, by = c("uuid" = "id"))
contacts_unflat$groups <- NULL
# Exclude variables for the full data
full_data <- contacts_unflat
full_data$urns <- NULL
full_data$name <- NULL
full_data$child_nickname <- NULL
full_data$first_user_name <- NULL
full_data$family_name <- NULL
full_data$sq_base_firstname <- NULL
full_data$sq_base_familyname <- NULL




# In the non anonymised data set, include users who have values "sometimes" or "often" in at least one of the following variables:
base_vars <- c("svexptouchedteen", "svexpshownteen", "svexpdoteen", "svexptouchedchild", "svshownchild", "svdochild")
prefixes <- c("sq_base_", "sq_1m_", "sq_3m_")
flag_vars <- paste0(rep(prefixes, each = length(base_vars)), base_vars)

flagged_uuids <- contacts_unflat %>% dplyr::select(c(uuid, all_of(flag_vars))) %>%
  dplyr::filter(if_any(all_of(flag_vars), ~ . %in% c("often", "sometimes"))) %>%
  dplyr::pull(uuid)

flagged_data <- contacts_unflat %>%
  dplyr::filter(uuid %in% flagged_uuids) %>%
  dplyr::select(c(uuid, urns, name, child_nickname, first_user_name, family_name, sq_base_firstname, sq_base_familyname, attachment_language,
                  all_of(flag_vars)))

full_data <- full_data %>%
  dplyr::mutate(flagged = ifelse(uuid %in% flagged_uuids, 1, 0))

# So we have two data sets: flagged_data and full_data

source("SWIFT_credentials_data.R")
