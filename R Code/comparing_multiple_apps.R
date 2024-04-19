#' course completion:
#' "The average user completes X% of the course" for:
#'  PText 1.0 South Africa,
#'  2.0 South Africa,
#'  2.0 Malaysia,
#'  5 Day Mexico
#'  
#'  Bar Chart
#'  
#'  Also, can we see the average length of "staying in the chatbot" for
#'   PText 1.0 South Africa,
#'   2.0 South Africa, 
#'   2.0 Malaysia? 

#' average length at what point?
#' and completion of what - goals? 
#' what point? - snapshot taken for PT on 29th, have this data for 21st for MY
#' SA, unsure. Have data on 11th October, or 18/1?


# Nov 3rd - data for SA? - Zulu language anyway. 

df_malaysia_21022024 <- readxl::read_xlsx("~/GitHub/ParentText-data-analysis/snapshots/df_malaysia_21022024.xlsx")
my_goals <- df_malaysia_21022024$perc_goals_completed
my_length <- df_malaysia_21022024$time_in_study_n
my_modules <- df_malaysia_21022024$perc_modules_completed

# SA on 18th 1st
load("~/GitHub/ParentText-data-analysis/R Code/sa_20240118.rds")
df_sa_18012024 <- df

sa_goals <- df_sa_18012024$perc_goals_completed
sa_length <- df_sa_18012024$time_in_study_n
sa_modules <- df_sa_18012024$perc_modules_completed

# sa on 11th October
sa_data_20231011 <- read_excel("parenttext_2_data_20231011.xlsx")
sa_goals2 <- sa_data_20231011$perc_goals_completed
sa_length2 <- as.numeric(sa_data_20231011$time_in_study)
sa_modules2 <- as.numeric(sa_data_20231011$perc_modules_completed)


Malaysia_Feb <- mean(my_goals, na.rm = TRUE)
SA_Jan <- mean(sa_goals, na.rm = TRUE)
SA_Oct <- mean(sa_goals2, na.rm = TRUE)

df <- data.frame(country = forcats::as_factor(c("SA (11th Oct)", "SA (18th Jan)", "MY (21st Feb)")),
                 value = c(SA_Oct, SA_Jan, Malaysia_Feb))

ggplot(df, aes(x = country, y = value)) + geom_bar(stat = "identity") +
  labs(x = "Country", y = "Goals Completed (%)")

# df_malaysia_21022024 %>% group_by(created_on) %>% summarise(n())# 6th Dec - 20
# df_sa_18012024 %>% group_by(created_on) %>% summarise(n())# 18th Aug - 6



df <- data.frame(country = forcats::as_factor(c("SA (11th Oct)", "SA (11th Oct)",
                                                "SA (18th Jan)", "SA (18th Jan)",
                                                "MY (21st Feb)", "MY (21st Feb)")),
                 stat = c(mean(sa_goals2, na.rm = TRUE), mean(sa_modules2, na.rm = TRUE), 
                           mean(sa_goals, na.rm = TRUE), mean(sa_modules, na.rm = TRUE), 
                           mean(my_goals, na.rm = TRUE), mean(my_modules, na.rm = TRUE)),
                 Value = rep(c("Goals", "Modules"), 3))

ggplot(df, aes(x = country, y = stat, fill = Value)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Completion (%)") +
  ggthemes::scale_fill_colorblind() +
  theme_bw() +
  geom_label(aes(label = paste0(round(stat, 1), "%")), fill = "white")



South_Africa_Data_20230712 <- read_excel("South_Africa_Data_20230712.xlsx")
south_africa_data_20221110 <- read_excel("~/GitHub/ParentText-data-analysis/snapshots/south_africa_data_20221110.xlsx")

# here - length is difference between hook message and created on
# in 1.0:
# time in study = last hook message - created_on

# in 2.0:
# time in study = end_time - created_on
#sa1_length1 <- mean(south_africa_data_20221110$time_in_study/24, na.rm = TRUE)
sa1_length <- mean(South_Africa_Data_20230712$time_in_study/24, na.rm = TRUE)


df2 <- data.frame(country = forcats::as_factor(c("SA 1.0 (12th July)", 
                                                 "SA 2.0 (11th Oct)",
                                                 "SA 2.0 (18th Jan)",
                                                 "MY 2.0 (21st Feb)")),
                 stat = c(mean(sa1_length, na.rm = TRUE), mean(sa_length2, na.rm = TRUE), 
                          mean(sa_length, na.rm = TRUE), mean(my_length, na.rm = TRUE)))


ggplot(df2, aes(x = country, y = stat, fill = country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Time in Study (Days)") +
  ggthemes::scale_fill_colorblind() +
  theme_bw() +
  geom_label(aes(label = paste0(round(stat, 1))), fill = "white")

