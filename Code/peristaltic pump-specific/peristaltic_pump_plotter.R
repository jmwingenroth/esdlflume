require(magrittr)
require(dplyr)
require(ggplot2)

file <- "C:\\Users\\Bearkey\\Documents\\Ecogeomorphic_Flume\\experiment_05_24_18\\05_24_18_PP_data.csv"
  
raw <- read.csv(file, header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(concentration=as.numeric(concentration), height=stringr::str_pad(height, width=2, side="left", pad="0")) %>%
  filter(!is.na(concentration)) %>%
  mutate(concentration=ifelse(concentration<0, 0, concentration), time=(time-min(time))*60, location=ifelse(location=="U", "upstream", "downstream")) %>%
  filter(time>=start_time, concentration<.5)

ggplot(raw, aes(time, concentration))+geom_path(aes(color=height))+facet_grid(location~.)+labs(title="Peristaltic Pump Sample Mass Concentrations", x="Time (s after 5 minutes from walnut shell addition)", y="Mass concentration (g/L)")