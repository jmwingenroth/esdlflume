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

data4 <- read_csv("../data/raw/0701pumpdata.csv")

highd2 <- read_csv("../data/raw/1204pumpdata.csv")

highd3 <- read_csv("../data/raw/0131pumpdata.csv")

lowdtwenty <- read_csv("../data/raw/0815pumpdata.csv")

lowdten <- read_csv("../data/raw/0808pumpdata.csv")

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


tidydata4 <- data4 %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=1) %>%
  mutate(run = "Mid Density #2 (Biof)", t = as.integer(t)) %>%
  select(-timepoint, -loc) 

tidydata5 <- highd2 %>%
  transmute(t = `Sample Time`, timepoint = `time series`, loc = Location,
            mvc = as.numeric(`mass volume concentration (ppm)`)) %>%
  filter(!is.na(t), !is.na(mvc)) %>%
  mutate(t = 300+(timepoint-3)*360, run = "High Density #2 (Biof)") %>%
  select(-timepoint, -loc) 

tidydata6 <- highd3 %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=1) %>%
  mutate(t = 60*(5*(timepoint-2)+7), run = "High Density #3 (Biof)") %>%
  select(-timepoint, -loc) 

tidydata7 <- lowdtwenty %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  filter(timepoint!=20) %>%
  mutate(run = "Low Density (20 Hz)", t = as.integer(t)) %>%
  select(-timepoint, -loc) 

tidydata8 <- lowdten %>%
  select(t = `Time (s)`, timepoint = `time series`, loc = Location,
         mvc = `mass volume concentration (ppm)`) %>%
  mutate(run = "Low Density (10 Hz)", t = as.integer(t)) %>%
  select(-timepoint, -loc) 


alldata <- rbind(tidy, tidydata, tidydata2, tidydata3, tidydata4, tidydata5, tidydata6, tidydata7, tidydata8)

alldata %>%
  ggplot(aes(x = t , y = log(mvc), color = run)) +
  geom_jitter(alpha=0.3, width=100) +
  geom_smooth(method = lm, formula = y~x, se = FALSE) +
  scale_color_manual(values = c("#d95204", "#fac60c", "#fdff7a", "#397317", "#aff288", "#0f2f99", "#70bfff","#820a94","#e78fff", "#9842f5", "#ffffff"))

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



modelmid1 <- tidydata3 %>%
  lm(log(mvc) ~t, data = .)



modelmid2 <- tidydata4 %>%
  lm(log(mvc) ~t, data = .)



modelhigh2 <- tidydata5 %>%
  lm(log(mvc) ~t, data = .)



modelhigh3 <- tidydata6 %>%
  lm(log(mvc) ~t, data = .)


modellow4 <- tidydata7 %>%
  lm(log(mvc) ~t, data = .)

modellow3 <- tidydata8 %>%
  lm(log(mvc) ~t, data = .)

###


mods <- list(modelcontrol1,
             modelcontrol2,
             modellow1,
             modellow2,
             modelmid1,
             modelmid2,
             modelhigh1,
             modelhigh2,
             modelhigh3,
             modellow4,
             modellow3)

k_t <- lapply(mods, FUN = function(x) x$coefficients[2]) %>%
  unlist()

names(k_t) <- c("modelcontrol1", "modelcontrol2", "modellow1", "modellow2", "modelmid1", "modelmid2 (biof)","modelhigh1","modelhigh2 (biof)","modelhigh3 (biof)", "modellow4 (20hz)", "modellow3 (10Hz)")

kable(k_t)
