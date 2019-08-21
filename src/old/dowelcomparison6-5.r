library(tidyverse)
library(plyr)
library(knitr)
########################## past runs

nodowels <- read_csv("../data/raw/1019pumpdata.csv")

nodowels_new <- read_csv("../data/raw/0321pumpdata.csv")

dowels <- read_csv("../data/raw/1115pumpdata.csv")

nodowels_2 <- nodowels %>%
  select(t = `Sample Time (s)`, mvc = `mass volume concentration (ppm)`) %>%
  mutate(run = "Zero Collectors #1") %>%
  filter(t < 7000)

nodowels_new_2 <- nodowels_new %>%
  select(t = `Time (s)`, mvc = `mass volume concentration (ppm)`) %>%
  mutate(run = "Zero Collectors #2") %>%
  filter(mvc < 90)

dowels2 <- dowels %>%
  mutate(`Sample Time` = if_else(`time series` > 2,
                                 (`time series` - 3)*300 + 330,
                                 as.numeric(`Sample Time`)),
         run = "High Density") %>%
  filter(`Sample Time` < 7000) %>%
  select(t = `Sample Time`, mvc = `mass volume concentration (ppm)`, run)

tidy <- rbind(nodowels_2, nodowels_new_2, dowels2)


###################################

## New data

data <- read_csv("../data/raw/0417pumpdata.csv")

data2 <- read_csv("../data/raw/0506pumpdata.csv")

data3 <- read_csv("../data/raw/0612pumpdata.csv")

tidydata <- data %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=1) %>%
  mutate(t = (timepoint-1)*300, run = "Low Density") %>%
  select(-timepoint, -loc) %>%
  filter(t>1200, t<4700|t>5500)

tidydata2 <- data2 %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=1) %>%
  mutate(t = (timepoint-1)*300, run = "Low Density #2") %>%
  select(-timepoint, -loc) 

tidydata3 <- data3 %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=1) %>%
  mutate(run = "Mid Density", t = as.integer(t)) %>%
  select(-timepoint, -loc) 

alldata <- rbind(tidy, tidydata, tidydata2, tidydata3)

alldata %>%
  ggplot(aes(x = t , y = log(mvc), color = run)) +
  geom_point() +
  geom_smooth(method = lm, formula = y~x, se = FALSE) +
  scale_color_manual(values = c("red", "blue", "light blue", "orange", "black", "gray"))

  ## Models

modelcontrol1 <- tidy %>%
  filter(run == "Zero Collectors #1") %>%
  lm(log(mvc) ~ t, data = .)



modelcontrol2 <- tidy %>%
  filter(run == "Zero Collectors #2") %>%
  lm(log(mvc) ~ t, data = .)



modelhigh1 <- tidy %>%
  filter(run == "High Density") %>%
  lm(log(mvc) ~ t, data = .)



modellow1 <- tidydata %>%
  filter(t>1200, t<4700|t>5500) %>%
  lm(log(mvc) ~ t, data = .)



modellow2 <- tidydata2 %>%
  lm(log(mvc) ~ t, data = .)



modelmid <- tidydata3 %>%
  lm(log(mvc) ~t, data = .)


###


mods <- list(modelcontrol1,
             modelcontrol2,
             modellow1,
             modellow2,
             modelmid,
             modelhigh1)

k_t <- lapply(mods, FUN = function(x) x$coefficients[2]) %>%
  unlist()

names(k_t) <- c("control1", "control2", "low1", "low2", "mid", "high")

table(k_t) %>% View()
