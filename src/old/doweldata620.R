library(tidyverse)

dowels <- read_csv("../data/raw/0612pumpdata.csv")

tidy <- dowels %>%
  transmute(t = as.integer(`Time (s)`), mvc = `mass volume concentration (ppm)`) %>%
  filter(t<7000)

mod <- lm(log(mvc) ~ t, data=tidy)


tidy %>%
  ggplot(aes(x = t, y = mvc)) +
  geom_point() +
  stat_function(fun = function(x) exp(mod$coefficients[1]+mod$coefficients[2]*x), color = "green")

