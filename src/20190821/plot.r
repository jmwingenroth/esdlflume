# plot

smooth_plot <- function(x) {
  x %>%
    ggplot(aes(x = t, y = mvc, color = run)) +
    geom_smooth()
}

p1 <- smooth_plot(tidy_data)

# remove runs not of interest

p2 <- tidy_data %>%
  filter(timepoint < 3) %>%
  ggplot(aes(x = run, y = mvc)) +
  geom_boxplot() +
  geom_hline(yintercept = 30) 

anomalous <- tidy_data %>%
  filter(timepoint < 3) %>%
  group_by(run) %>%
  summarise(id = median(mvc) < 30) %>%
  filter(id == TRUE) 

tidy_data <- tidy_data %>%
  filter(!run %in% anomalous$run)

# plot with other vars of interest

p3 <- tidy_data %>%
  filter(!run %in% c('0131pumpdata', '0308pumpdata')) %>%
  group_by(loc, height) %>%
  ggplot(aes(x = t, y = log(mvc), color = loc, lty = as.character(height))) +
  geom_line() +
  facet_wrap(~run) 

ggsave("20190821/rawsmoothed.png", p1)
ggsave("20190821/startingvals.png", p2)
ggsave("20190821/allruns.png", p3)
