library(tidyverse)

ss1 <- read_csv("../data/raw/LISST portable size distn experiment 190422/WSD101.ASC", skip = 3, col_names = F)

ss2 <- read_csv("../data/raw/LISST portable size distn experiment 190422/WSD102.ASC", skip = 3, col_names = F)

ss12 <- t(ss1)[22:65,] %>%
  as.tibble() %>%
  mutate_at(c(1,3), as.numeric) %>%
  rename(grainsize = V1, conc_units = V2, conc = V3) %>%
  mutate(run = "#1")

ss22 <- t(ss2)[22:65,] %>%
  as.tibble() %>%
  mutate_at(c(1,3), as.numeric) %>%
  rename(grainsize = V1, conc_units = V2, conc = V3) %>%
  mutate(run = "#2")

data <- rbind(ss12, ss22)

theme_set(theme_bw())

data %>%  
  ggplot(aes(x = grainsize, y = conc, color = run)) +
  geom_line() +
  scale_x_log10(minor_breaks = c(1:10, seq(10,100,10))) +
  theme(panel.grid = element_line(color = "gray 90"))

            