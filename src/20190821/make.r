source("20190821/libs.r")
source("20190821/funs.r")

d1 <- load_pump_data(path = "../data/raw/")

d2 <- remove_blanks(remove_blanks(d1))

d3 <- filter(d2, timepoint < 20, mvc < 100, mvc > 0) %>%
  mutate(t = timepoint*300)

d3 %>%
  ggplot(aes(x = t, y = log(mvc), color = loc, lty = as.character(height))) +
  geom_line() +
  facet_wrap(~run) 

lmtable(d3) %>%
  left_join(r2table(d3), by = "date")

# TODO
# change plot names to dowel density
# GLMM of residuals to assess height and location effects
