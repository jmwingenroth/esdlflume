# plot

smooth_plot <- function(x) {
  x %>%
    ggplot(aes(x = timepoint, y = mvc, color = run)) +
    geom_smooth() +
    geom_point(alpha = .4)
}

p1 <- smooth_plot(d3)

p1

# remove runs with blanks

p2 <- d3 %>%
  filter(timepoint == 1) %>%
  ggplot(aes(x = run, y = mvc)) +
  geom_boxplot() +
  geom_hline(yintercept = 30) 

p2

tidy_data %>%
  filter(timepoint < 3) %>%
  group_by(run) %>%
  summarise(id = median(mvc) < 30) #%>%
  filter(id == TRUE) 

tidy_data <- tidy_data %>%
  filter(!run %in% anomalous$run)

# plot with other vars of interest

p3 <- tidy_data %>%
#  filter(!run %in% c('0131pumpdata', '0308pumpdata')) %>%
  group_by(loc, height) %>%
  ggplot(aes(x = t, y = log(mvc), color = loc, lty = as.character(height))) +
  geom_line() +
  facet_wrap(~run) 