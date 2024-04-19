goals_accessed <- data.frame(stringr::str_split(contacts_unflat$fields$goals_accessed, " ", simplify = TRUE))
goals_accessed <- bind_cols(ID = contacts_unflat$uuid, goals_accessed) %>%
  pivot_longer(cols = !ID) %>%
  mutate(name = as.numeric(as.factor(name)),
         value = as.factor(value))

goals_accessed_size <- goals_accessed %>%
  group_by(name, value) %>%
  summarise(n())

# Create a new data frame for transitions
transitions <- goals_accessed %>%
  arrange(ID, name) %>%
  group_by(ID) %>%
  mutate(value_end = lead(value), name_end = lead(name)) %>%
  filter(!is.na(value_end)) %>%
  ungroup()

# Count the number of occurrences for each transition
transition_count <- transitions %>%
  group_by(name) %>%
  count(value, value_end, name = "n_transitions")

transition_count <- transition_count %>% mutate(n_transitions = ifelse(value == "" & value_end == "", NA, n_transitions))

# Merge this information back into the transitions dataset
transitions <- transitions %>%
  left_join(transition_count, by = c("name", "value", "value_end"))

transitions$id <- transitions$ID
transitions$ID <- NULL

# this (above) would be calculated before in our shiny_cleaning_PT2 file
# but make sure we rename IDs to be the correct name.

# then this would be manipulation code to select our valid IDs
#transitions1 <- transitions %>% dplyr::select(!id) %>% unique()

# Then if I could do any plot, I would do this!!!!

# So this is a plot of the number transitioning between goals, coloured by the number of individuals
# the size of the point is also about the number who have arrived there. 
ggplot() +
  geom_segment(data = transitions, aes(x = name, xend = name_end, y = value, yend = value_end, color = n_transitions), size = 1) +
  geom_point(data = transitions, aes(x = name_end, y = value_end, size = n_transitions, colour = n_transitions)) +
  ggthemes::scale_colour_gradient_tableau(palette = "Green-Gold") +
  #scale_colour_gradientn(colours = c("black", "red", "orange", "yellow", "green", "blue", "purple")) +
  theme_minimal() +
  labs(x = "Goal Number", y = "Goal",
       size = "Number of Transitions",
       colour = "Number of Transitions")


# So this is a plot of the proportion transitioning between goals, coloured by the number of individuals
# the size of the point is also about the number who have arrived there. 
transitions_prop <- transitions %>%
  group_by(name_end) %>% 
  mutate(sum = sum(n_transitions, na.rm = TRUE),
         n_transitions = n_transitions/sum * 100)

ggplot() +
  geom_segment(data = transitions_prop, aes(x = name, xend = name_end, y = value, yend = value_end, color = n_transitions), size = 1) +
  geom_point(data = transitions_prop, aes(x = name_end, y = value_end, size = n_transitions, colour = n_transitions)) +
  ggthemes::scale_colour_gradient_tableau(palette = "Green-Gold") +
  theme_minimal() +
  labs(x = "Goal Number", y = "Goal",
       size = "Proportion of Transitions",
       colour = "Proportion of Transitions")

#
transition_count_2 <- transition_count %>% filter(name == 1) %>%
  mutate(total_transitions = sum(n_transitions, na.rm = TRUE),
         percent = n_transitions / total_transitions * 100)

# Create the plot
ggplot(transition_count_2, aes(x = value_end, y = n_transitions)) + 
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste(n_transitions, " (", sprintf("%.1f%%", percent), ")", sep = "")), 
            size = 3) +
  theme_minimal() +
  labs(x = "Goal", y = "Frequency")
  
####################################################################################################


checkin_data %>%
  dplyr::filter(ID == "learning") %>%
  dplyr::group_by(time) %>%
  dplyr::mutate(total = n()) %>%
  dplyr::group_by(time, response, total) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::mutate(`Count (%)` = paste0(Count, " (", round(Count/total * 100, 1), "%)")) %>%
  dplyr::select(-c(total, Count)) %>%
  pivot_wider(names_from = time, values_from = `Count (%)`)


# Then we do this in our excel sheet because it involves filtering to valid IDs
# Count the number of occurrences for each transition
transition_count <- list_goal_transition_data$learning %>%
    filter(uuid %in% valid_ids) %>%
    group_by(time_old, time_new, response_old, response_new) %>%
    summarise(n_transitions = sum(n), .groups = 'drop')
ggplot() +
  geom_segment(data = transition_count, aes(x = time_old,
                                            xend = time_new,
                                            y = response_old,
                                            yend = response_new,
                                            color = n_transitions), size = 1) +
  geom_point(data = transition_count, aes(x = time_new, y = response_new, size = n_transitions, colour = n_transitions)) +
  ggthemes::scale_colour_gradient_tableau(palette = "Green-Gold") +
  theme_minimal() +
  labs(x = "Survey Asked", y = "Response",
       size = "Number of Transitions",
       colour = "Number of Transitions") +
  scale_x_discrete(limits = c("Pre", "Post"))

  
  