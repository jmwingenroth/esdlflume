library(tidyverse)
library(plyr)
library(knitr)

highd2 <- read_csv("../data/raw/1204pumpdata.csv")
highd3 <- read_csv("../data/raw/0131pumpdata.csv")
midd2 <- read_csv("../data/raw/0701pumpdata.csv")

tidy1 <- highd2 %>%
  transmute(t = `Sample Time`, timepoint = `time series`, loc = Location,
         mvc = as.numeric(`mass volume concentration (ppm)`)) %>%
  filter(!is.na(t), !is.na(mvc)) %>%
  mutate(t = 300+(timepoint-3)*360, run = "High Density #2") %>%
  select(-timepoint, -loc) 

tidy2 <- highd3 %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=1) %>%
  mutate(t = 60*(5*(timepoint-2)+7), run = "High Density #3") %>%
  select(-timepoint, -loc) 

tidy3 <- midd2 %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  mutate(t = (timepoint)*300, run = "Mid Density #2") %>%
  select(-timepoint, -loc)


alldata <- rbind(tidy1, tidy2, tidy3)

alldata %>%
  ggplot(aes(x = t , y = log(mvc), color = run)) +
  geom_point() +
  geom_smooth(method = lm, formula = y~x, se = FALSE) +
  scale_color_manual(values = c("red", "blue", "orange"))

modelhd2 <- tidy1 %>%
  filter(run == "High Density #2") %>%
  lm(log(mvc) ~ t, data = .)

modelhd3 <- tidy2 %>%
  filter(run == "High Density #3") %>%
  lm(log(mvc) ~ t, data = .)

modelmd2 <- tidy3 %>%
  filter(run == "Mid Density #2") %>%
  lm(log(mvc) ~ t, data = .)

mods <- list(modelhd2,
             modelhd3,
             modelmd2)

biofk_t <- lapply(mods, FUN = function(x) x$coefficients[2]) %>%
  unlist()

names(biofk_t) <- c("high2", "high3", "mid2")

kable(biofk_t)
