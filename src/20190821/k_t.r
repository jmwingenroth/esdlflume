tidy_data %>%
  group_by(run) %>%
  summarise(k_t = lm())
