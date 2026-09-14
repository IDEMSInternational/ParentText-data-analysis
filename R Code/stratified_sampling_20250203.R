contacts_unflat <- readRDS("~/GitHub/ParentText-data-analysis/R Code/ALL_SA_DATA_contacts_unflat_20250520.rds")

# TOP BOXES ----------------------------------------------------
group_names <- c("non eligible", "below age", "completed baseline", "completed program", "consented later", "left program", "edge case", "passive drop out",
                 "DTOne Transaction Failed", "to be sent airtime", "joined", "enrolled", "in 1 month survey", "in 3 month survey", "in baseline",
                 "in booster offer", "in program", "still active 1 month", "still active 3 month", "from rct", "rct",
                 "core", "booster", "share", "social", "parenttext", "support", "washtext", "swift",
                 "woman female child", "woman female teen", "woman male child", "woman male teen", "man female child", "man female teen",
                 "man male child", "man male teen", "edge cases", "edge case bug",
                 "7month first batch", "7 month final push", "7 month last final push", "7month 2nd resend survey",
                 "7month first batch", "7month resend survey", "Make 7month month manual payment", "received 7 month")

#contacts_unflat1 <- contacts_unflat
#group_names <- "7month first batch"

#saveRDS(contacts_unflat, "SA_RCT_all_vars_20240507.rds")

names(contacts_unflat$groups) <- contacts_unflat$uuid
groups_data <- plyr::ldply(contacts_unflat$groups, .id = "id")
groups_data <- groups_data %>%
  dplyr::filter(name %in% group_names) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::select(-uuid)
groups_data <- groups_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0)

df <- data.frame(groups_data) %>%
  mutate(id = as.character(id)) %>%
  arrange(id)

valid_ids <- df$id

contacts_unflat <- contacts_unflat %>%
  dplyr::filter(uuid %in% valid_ids) %>%
  arrange(uuid)

contacts_unflat <- full_join(contacts_unflat, groups_data, by = c("uuid" = "id"))


#contacts_unflat <- flatten(contacts_unflat)
# writexl::write_xlsx(contacts_unflat, path = "sa_users_snapshot_20240306.xlsx")
# saveRDS(contacts_unflat, file = "malaysia_users_snapshot_29022024.rds")

x <- data.frame(df = df$id, cu = contacts_unflat$uuid) %>%
  mutate(hi = ifelse(df == cu, 1, 0)) %>%
  filter(hi == 0)
if (nrow(x) > 0) stop("Check ID order")

names(contacts_unflat) <- gsub("^fields\\.", "", names(contacts_unflat))


################################################################################

# ALL_SA_DATA_contacts_unflat_20250520

contacts_unflat1 <- contacts_unflat %>%
  dplyr::mutate(name = paste0(first_user_name, " ", family_name)) %>%
  dplyr::select(c(uuid, name, `home language` = languages_spoken, `app language` = language,
                  baseline_completion_time, seven_month_completion_time))

# we can only consider users with a phone number and who have agreed to be contacted
all_users <- contacts_unflat %>%
  filter(!is.na(transaction_id)) %>%
  filter(t_quals_participation == "yes")

booster_users <- all_users %>%
  filter(booster == 1) %>%
  mutate(group = "booster") %>%
  dplyr::select(c(uuid, group, gender,
                  child_age,
                  parent_age, 
                  languages_spoken, 
                  attachment_language,
                  location,
                  goal_relation_n_mod_compl,
                  goal_develop_n_mod_compl,
                  transaction_id))

core_users <- all_users %>%
  filter(core == 1) %>%
  mutate(group = "core") %>%
  dplyr::select(c(uuid,
                  group,
                  gender,
                  child_age,
                  parent_age, 
                  languages_spoken, 
                  attachment_language,
                  location,
                  goal_relation_n_mod_compl,
                  transaction_id))

# include variables:
# baseline completion time, start time
# and these ones we're using (goal_relation_n_mod_compl, goal_develop_n_mod_compl, goal_wash_n_mod_compl)

# booster
booster_qualitative <- booster_users %>%
  filter(!is.na(goal_relation_n_mod_compl)) %>%
  filter(!is.na(goal_develop_n_mod_compl)) %>%
  mutate(engagement_group_1 = ifelse(goal_relation_n_mod_compl == 0, "low", ifelse(goal_relation_n_mod_compl < 5, "mid", "high"))) %>%
  mutate(engagement_group_2 = ifelse(goal_develop_n_mod_compl == 0, "low", ifelse(goal_develop_n_mod_compl < 4, "mid", "high"))) %>%
  mutate(cluster = paste0(gender, "_", engagement_group_1, "_", engagement_group_2))

#' TODO: we want to have 300-500 people overall.
booster_sample <- booster_qualitative %>%
  group_by(cluster) %>%
  group_split() %>% # Split data into a list by group
  map_dfr(~ {
    if (unique(.x$cluster) %in% c("woman_low_low", "man_low_low")) {
      slice_sample(.x, n = 110) # Sample 8 rows for woman_*   (why is n = 11?)
    } else if (unique(.x$cluster) %in% c("woman_mid_high", "woman_high_high", "woman_high_mid", "woman_mid_mid",
                                         "man_mid_high", "man_high_high", "man_high_mid", "man_mid_mid")) {
      slice_sample(.x, n = 20) # Sample 2 rows for man_*, etc
    }
  })
booster_sample <- booster_sample %>%
  mutate(cluster = ifelse(cluster %in% c("woman_low_low", "man_low_low"),
                          "Non-engager",
                          "Engager"))

# core
core_qualitative <- core_users %>%
  filter(!is.na(goal_relation_n_mod_compl)) %>%
  filter(gender != "no answer given") %>%
  mutate(engagement_group = ifelse(goal_relation_n_mod_compl == 0, "none", ifelse(goal_relation_n_mod_compl < 3, "low", ifelse(goal_relation_n_mod_compl < 5, "mid", "high")))) %>%
  mutate(cluster = paste0(gender, "_", engagement_group))

core_sample <- core_qualitative %>%
  group_by(cluster) %>%
  group_split() %>% # Split data into a list by group
  map_dfr(~ {
    if (unique(.x$cluster) %in% c("woman_none", "man_none")) {
      slice_sample(.x, n = 110) # Sample 8 rows for woman_*
    } else {
      slice_sample(.x, n = 20) # Sample 2 rows for man_*
    }
  }) %>%
  mutate(cluster = ifelse(cluster %in% c("woman_none", "man_none"),
                          "Non-engager",
                          "Engager"))


# then include:

# INCLUDE first and last names

booster_sample <- left_join(booster_sample, contacts_unflat1)
core_sample <- left_join(core_sample, contacts_unflat1)

write.csv(booster_sample, "C:/Users/lclem/Downloads/booster_sample_2.csv")
write.csv(core_sample, "C:/Users/lclem/Downloads/core_sample_2.csv")
