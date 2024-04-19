# ABTest
# gamification, personalisation, n-messages-per-day
# alternative, Default
# g:    a - "gamified", d - "non-gamified"
# p:    a - "non-personalised", d - "personalised"
# msgs: a - "1 or 2 messages per day", d - "3 messages per day"
df <- update_data()[[1]]

# time of day joined --------------
df$created_on_1 <- as.POSIXct(df$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "EST")
#df <- df %>% mutate(time_of_day = ifelse(created_on_1 < lubridate::hms("14:00:00"), "Morning", "Afternoon"))
df$created_on_1 <- df$created_on_1 - lubridate::hm("6, 0")
df <- tidyr::separate(df, created_on_1, c("created_date", "created_time"), sep = " ")
df$created_time <- lubridate::hms(df$created_time)
df <- df %>%
  mutate(t_o_d = ifelse(df$created_time < "8H 0M 0S",
                        "morning",
                        "afternoon"))

df$next_tip_main_c <- as.character(df$next_tip_main)
df <- df %>%
  mutate(next_tip_main_c = fct_relevel(next_tip_main_c, "1", "2", "3", "4", "5", "6", "7", "8", "10", "11"))

summary_table(df, gamification, next_tip_main_c, include_margins = TRUE, together = FALSE)
summary_table(df, personalisation, next_tip_main_c, include_margins = TRUE, together = FALSE)
summary_table(df, n_messages, next_tip_main_c, include_margins = TRUE, together = FALSE)

# drop down to group by whatever: gamification, personalisation, n_messages, child_age_group, parent_gender
ab_graph <- function(data, factor, factor2 = n_messages, column_to_summarise){
  objects_to_return <- NULL
  ab_df <- data %>%
    group_by({{ factor }}, {{factor2}}, {{ column_to_summarise }}) %>%
    summarise(count = n())
  plot_return <- ggplot(ab_df, aes(x = {{ column_to_summarise }}, y = count, colour = {{ factor }})) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(y = "Number of participants")
  
  if (!missing(factor2)){
    #if (!missing(factor3)){
    #  plot_return <- plot_return + facet_grid(cols = vars({{factor2}}),
    #                                          rows = vars({{factor3}}))
    #} else {
      plot_return <- plot_return + facet_grid(cols = vars({{factor2}}))
    #}
  }

  objects_to_return[[1]] <- ab_df
  objects_to_return[[2]] <- plot_return
  return(objects_to_return)
}
ab_graph(df, gamification, n_messages, next_tip_main)
ab_graph(df, personalisation, n_messages, next_tip_main)
ab_graph(df, n_messages, next_tip_main)

df_test <- df %>%
  group_by(n_messages, child_age_group, next_tip_main) %>%
  summarise(n())

df_test <- df_test %>%
  group_by(n_messages, child_age_group) %>%
  mutate(rev_cumsum = rev(cumsum(rev(`n()`)))) %>%
  mutate(next_tip_main = dplyr::lag(next_tip_main, default =0))

ggplot(df_test, aes(x = next_tip_main,
                     y = rev_cumsum)) +
  facet_grid(cols = vars(n_messages),
             rows = vars(child_age_group)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "Number of participants")

head(df_test)

df_test <- df_test %>%
  group_by(n_messages) %>%
  mutate(pct = (rev_cumsum - lead(rev_cumsum))/rev_cumsum * 100)
ggplot(df_test, aes(x = next_tip_main,
                    y = pct)) +
  facet_grid(cols = vars(n_messages)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "PCT lost from previous question")


# finished_tip = rev_cumsum
df_test <- df_test %>%
  group_by(parent_gender, n_messages) %>%
  mutate(first = first(rev_cumsum)) %>%
  mutate(pct = (rev_cumsum - lead(rev_cumsum))/first(rev_cumsum) * 100)
ggplot(df_test, aes(x = next_tip_main,
                    y = pct,
                    colour = factor(parent_gender))) +
  facet_grid(cols = vars(n_messages)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "PCT lost from previous question")





#---

# number of skills in toolkit
df$created_on_day <- as.Date(df$created_on, format="%Y-%m-%d", tz = "UTC")
df$date <- as.Date(Sys.Date(), format="%Y-%m-%d", tz = "UTC")
df <- df %>% mutate(date_diff = date - created_on_day)

df_date_diff <- df %>%
  filter(date_diff < 50) %>%
  group_by(date_diff, n_messages) %>%
  summarise(mean = mean(comp_prog_overall))
ggplot(df_date_diff, aes(x = date_diff, y = mean)) +
  geom_point() + 
  facet_grid(cols = vars(n_messages)) +
  labs(x = "Days since sign-up", y = "mean(comp_prog_overall)")

# number of skills in toolkit
df_date_diff <- df %>%
  filter(date_diff < 50) %>%
  group_by(date_diff, n_messages, t_o_d) %>%
  summarise(mean = mean(next_tip_main),
            min = min(next_tip_main),
            max = max(next_tip_main))
ggplot(df_date_diff, aes(x = date_diff, y = mean, colour = t_o_d)) +
  geom_point() + 
  facet_grid(cols = vars(n_messages)) +
  labs(x = "Days since sign-up", y = "mean(next_tip_main)")

ggplot(df_date_diff, aes(x = date_diff, y = mean)) +
  geom_point() + 
  geom_point(data = df_date_diff, aes(x = date_diff, y = min), colour = "blue") + 
  geom_point(data = df_date_diff, aes(x = date_diff, y = max), colour = "red") + 
  facet_grid(cols = vars(n_messages)) +
  labs(x = "Days since sign-up", y = "mean(next_tip_main)")


