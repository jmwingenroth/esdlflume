source("20190821/libs.r")
source("20190821/funs.r")

d1 <- load_pump_data(path = "../data/raw/peristaltic pumps/")

d2 <- remove_blanks(remove_blanks(d1))

d3 <- filter(d2, timepoint < 20, mvc < 100, mvc > 0) %>%
  mutate(t = timepoint*300) %>%
  mutate(run = str_sub(run,,6))

s1 <- lmtable(d3) %>%
  left_join(r2table(d3), by = "date") %>%
  mutate(r2 = as.numeric(r2))

d3 %>%
  left_join(s1, by = c("run" = "date")) %>%
  ggplot(aes(x = t, y = log(mvc), color = loc, lty = as.character(height))) +
  geom_point(alpha = .2, size = 1) +
  facet_wrap(~run) +
  geom_line(stat = "smooth", method = "lm", se = FALSE, alpha = .8) +
  geom_text(x = 500, y = 0, color = 'black', size = 3, hjust = "left", 
            aes(label = paste0("R2 = ",round(r2, digits = 4)))) +
  geom_text(x = 500, y = .5, color = 'black', size = 3, hjust = "left", 
            aes(label = paste0("K_t = ",round(k_t, digits = 6))))


# TODO
# change plot names to dowel density
# GLMM of residuals to assess height and location effects
