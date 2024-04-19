
# read in the contacts_unflat from 29th Feb.
#contacts_unflat vs df_21

#View(contacts_unflat$fields)

remove_na_columns <- function(df) {
  # Identify rows with complete data
  complete_rows <- complete.cases(df)
  
  # Filter columns with any non-NA values
  df <- df[, colSums(!is.na(df)) > 0]
  
  return(df)
}



#' We had 538 variables in contacts_unflat$fields but actually most were all NAs
#' so we only have 174 variables.
contacts_unflat$fields <- remove_na_columns(contacts_unflat$fields)

contacts_unflat$last_seen_on <- as.Date(contacts_unflat$last_seen_on)
contacts_unflat <- contacts_unflat %>% filter(last_seen_on > as.Date("2024-02-21"))

nrow(contacts_unflat)

ids <- contacts_unflat$uuid

# (there's 133 from the joined group, but 6 are from no group)
# there's 139 people who have updated since the 21st. So now how many columns aren't NA?
contacts_unflat$fields <- remove_na_columns(contacts_unflat$fields)

# okay we now have 149 variables. That's fine. 

# what is "end_time"? For a couple, end_time < 21st Feb.

# some of these variables would not have changed


# we can sort out the last_goal_completed stuff
x <- data.frame(stringr::str_split(contacts_unflat$fields$time_last_goal_completed, fixed("|"), simplify = TRUE))
x$ID <- contacts_unflat$uuid

x <- x %>% pivot_longer(cols = !ID)
x$value <- as.Date(x$value)
x1 <- x %>% filter(value > as.Date("2024-02-21"))
nrow(x1)

# using `time_last_goal_completed` we can see that 67 additional goals were completed in the extra 8 days


contacts_unflat$fields$n_goals_completed
#


#df_21 <- readxl::read_xlsx(path = "C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/df_malaysia_21022024.xlsx")
#df_29 <- readxl::read_xlsx(path = "C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/df_malaysia_29022024.xlsx")

df_both <- full_join(df_21, df_29, by = c("id" = "id"))

df_21$n_goals_completed
df_29$n_goals_completed


df_both$n_goals_diff = ifelse(df_both$n_goals_completed.x == df_both$n_goals_completed.y, 0, 1)
df_both %>% filter(n_goals_diff == 1) %>% dplyr::select(c(id, n_goals_completed.x, n_goals_completed.y)) %>% View()


df_both$n_mods_diff = ifelse(df_both$n_modules_completed_numeric.x == df_both$n_modules_completed_numeric.y, 0, 1)
df_both %>% filter(n_mods_diff == 1) %>% dplyr::select(c(id, n_modules_completed_numeric.x, n_modules_completed_numeric.y)) %>% View()


# sanity check
df_both$child_age.diff = ifelse(df_both$child_age.x == df_both$child_age.y, 0, 1)
df_both %>% filter(child_age.diff == 1) %>% dplyr::select(c(id, child_age.x, child_age.y)) %>% View()

result_flow2 <- readRDS("C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/result_flows_21022024.rds")










############################

# look at the data snapshot I took on 13th Feb.
malaysia_users_snapshot <- read_excel("malaysia_users_snapshot.xlsx")

malaysia_users_snapshot <- malaysia_users_snapshot %>% filter(uuid %in% ids)

# We have 16 people here who didn't update until before 13th!
malaysia_users_snapshot$last_seen_on <- as.Date(malaysia_users_snapshot$last_seen_on)
malaysia_users_snapshot <- malaysia_users_snapshot %>%
  filter(last_seen_on <= as.Date("2024-02-13"))
nrow(malaysia_users_snapshot)

ID_for_16 <- malaysia_users_snapshot$uuid

ID_for_16

pt_malaysia_user_flat <- read_excel("pt_malaysia_user_flat.xlsx")

pt_malaysia_user_flat <- pt_malaysia_user_flat %>%
  filter(uuid %in% ID_for_16)

