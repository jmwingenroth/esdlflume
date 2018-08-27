require(dplyr)
require(magrittr)

file <- "C:\\Users\\Bearkey\\Documents\\LISST\\LISST_data\\04_19_18_size_dist.asc"
correction <- TRUE #set this to TRUE if you want to remove all data points that have a transmission outside of the optimal range of 0.1 to 0.995
time_start <- 0 #set start time for the data analysis to exclude initialization
time_end <- Inf #set end time for data analysis

###Data Processing Section###
bins <- 1:32
raw <- read.table(file, header=FALSE) #reading the .asc file as a table

laser_reference <- any(raw[,36]==0) #laser reference values should all be much greater than 0

if (laser_reference==TRUE) {
  warning("Laser reference has a 0 value")
}

transmission <- any(c(raw[,41]<=0, raw[,41]>=1)) #it is physically impossible to have a transmission not in the range 0 to 1

if (transmission==TRUE) {
  warning("Transmission has value(s) outside of the range 0 to 1")
}

if (correction==TRUE) {
  raw <- raw %>%
    filter(V36>0, V41>=0.1, V41<=0.995)
}

time_converted_sec <- raw %>%
  mutate(min=as.numeric(ifelse(V40>=100, gsub("[[:digit:]]{2}$", "", V40), 0))) %>%
  mutate(sec=as.numeric(ifelse(V40>=1000, gsub("^[[:digit:]]{2}", "", V40), ifelse(V40<1000 & V40>=100, gsub("^[[:digit:]]", "", V40), V40)))) %>%
  mutate(sec=sec+(min*60)) %>%
  select(-min) #conversion of time units to seconds

diff <- c(0, diff(time_converted_sec[,"sec"]))
diff[diff<0] <- diff[diff<0]+3600
seconds <- cumsum(diff)+1

time_converted_final <- time_converted_sec %>%
  mutate(sec=seconds) #setting seconds to start at 0 and count increasing (as opposed to cyclically)

gathered_measurements <- time_converted_final %>%
  tidyr::gather(key=bin, value=measurement, 1:32) %>%
  mutate(bin=as.numeric(gsub("^V", "", bin))) %>%
  select(sec, bin, measurement) %>% #combining particle concentration data into a single field
  filter(sec>=time_start, sec<=time_end)
  
bin_plotter <- function(bin_k) {
  selected_bin <- filter(gathered_measurements, bin==bin_k)
  plot <- ggplot(selected_bin, aes(x=sec, y=measurement))+geom_point(size=0.2)+theme(axis.title=element_blank())
  return(plot)
}

plot_list <- lapply(bins, bin_plotter)
cowplot::plot_grid(plotlist=plot_list, nrow=4, ncol=8)