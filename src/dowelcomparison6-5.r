library(tidyverse)

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

alldata <- rbind(tidy, tidydata, tidydata2)

alldata %>%
  ggplot(aes(x = t , y = mvc, color = run)) +
  geom_smooth()+
  geom_point(alpha = .2)

## Models

ndm1 <- tidy %>%
  filter(run == "Zero Collectors #1") %>%
  lm(log(mvc) ~ t, data = .)

summary(ndm1)

ndm2 <- tidy %>%
  filter(run == "Zero Collectors #2") %>%
  lm(log(mvc) ~ t, data = .)

summary(ndm2)

ydm <- tidy %>%
  filter(run == "High Density") %>%
  lm(log(mvc) ~ t, data = .)

summary(ydm)

newm <- tidydata %>%
  filter(t>1200, t<4700|t>5500) %>%
  lm(log(mvc) ~ t, data = .)

summary(newm)

newm2 <- tidydata2 %>%
  lm(log(mvc) ~ t, data = .)

alldata %>%
  ggplot(aes(x = t, y = mvc, color = run)) +
  geom_jitter(alpha = .2, width = 100) +
  stat_function(fun = function(x) exp(coef(ndm1)[1]+coef(ndm1)[2]*x), color = "green", size = 0.05) +
  stat_function(fun = function(x) exp(coef(ndm2)[1]+coef(ndm2)[2]*x), color = "yellow", size = 0.05) +
  stat_function(fun = function(x) exp(coef(ydm)[1]+coef(ydm)[2]*x), color = "red", size = 0.05) +
  stat_function(fun = function(x) exp(coef(newm)[1]+coef(newm)[2]*x), color = "black", size = 0.05) +
  stat_function(fun = function(x) exp(coef(newm2)[1]+coef(newm2)[2]*x), color = "blue", size = 0.05) +
  xlim(c(0,NA)) +
  scale_color_manual(values = c("red", "black", "blue", "green", "yellow")) +
  labs(x = 'Time (s)', y = 'Concentration') +
  guides(color = guide_legend(title = "Run"))
  
       