require(dplyr)
require(magrittr)
require(tidyr)
require(ggplot2)

#Data Quality Control:
#Laser reference >>0
#Transmission should be between 0 and 1 (low transmission indicates turbid water, high tranmission indicates clear water; should disregard these data)

file <- "C:\\Users\\Bearkey\\Documents\\LISST\\LISST_data\\04_19_18_size_dist.asc"
correction <- TRUE #set this to TRUE if you want to remove all data points that have a transmission outside of the optimal range of 0.1 to 0.995
time_interval <- 10 #time interval for data aggregation for plots in minutes

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
  mutate(group=as.character(ifelse(sec==0, 1, ceiling(sec/(60*time_interval))))) %>%
  gather(key=bin, value=measurement, 1:32) #combining particle concentration data into a single field
  
max_group_digit <- nchar(max(as.numeric(gathered_measurements[,"group"]))) #maximum number of digits of a time group (for data visualization)
number_of_groups <- max(as.numeric(gathered_measurements[,"group"])) #number of groups
  
bin_values <- data.frame(bin=paste("V", 1:32, sep=""),
              centers=c(7.95, 9.38, 11.07, 13.06, 15.42, 18.19, 21.47, 25.33, 29.90, 35.28,
              41.63, 49.13, 57.97, 68.41, 80.73, 95.26, 112.40, 132.70, 156.60, 184.70,
              218.00, 257.20, 303.60, 358.20, 422.80, 498.90, 588.80, 694.80, 819.90,
              967.50, 1142.00, 1347.00)) #log-spaced bin center values

particle_min <- 7.5 #smallest detectable particle size in LISST range
particle_max <- 1500 #largest detectable particle size in LISST range

bin_boundaries <- 10^seq(from=log10(particle_min), to=log10(particle_max), length.out=33) #computes boundaries of the binds
bin_sizes <- diff(bin_boundaries) #computes the size of each bin; each bin is roughly 1.18 (aka 200^(1/32)) times larger than the previous bin

bin_time_average <- gathered_measurements %>%
  group_by(group, bin) %>%
  summarize(avg=mean(measurement)) %>%
  left_join(bin_values, by="bin") %>%
  arrange(centers) %>%
  ungroup() %>%
  mutate(group=stringr::str_pad(group, width=max_group_digit, side="left", pad="0")) %>%
  as.data.frame() #average the particle concentrations by bin and time group
  
relative_bin_weight <- bin_time_average %>%
  group_by(group) %>%
  summarize(total=sum(avg)) %>%
  as.data.frame() #computes the total particle concentration by bin to determine the relative distribution of particles by bin (and thus particle size)
  
bin_time_proportional <- bin_time_average %>%
  left_join(relative_bin_weight, by="group") %>%
  mutate(proportion=avg/total) %>%
  mutate(proportion=ifelse(is.na(proportion), 0, proportion)) #computes the proportion of particles in each bin
  
median_particle_size_by_time <- bin_time_proportional %>%  
  group_by(group) %>%
  summarize(median_size=sum(proportion*centers)) %>%
  ungroup() %>%
  mutate(group=as.numeric(group)) %>%
  arrange(group) %>%
  as.data.frame() #computes median particle size by time group
  
empirical_CDF <- bin_time_average %>%
  mutate(g_no=as.numeric(group)) %>%
  arrange(g_no, centers) %>%
  left_join(relative_bin_weight, by="group") %>%
  group_by(group) %>%
  mutate(avg_cs=cumsum(avg)) %>%
  mutate(prop=avg_cs/total) %>%
  ungroup() %>%
  mutate(group=stringr::str_pad(group, width=max_group_digit, side="left", pad="0")) %>%
  as.data.frame() #computes an empirical cumulative density
  
#all plots use log scale
  
getPalette <- grDevices::colorRampPalette(c("blue", "red")) #makes a blue to red color palette for plots
  
plot_cdf <- ggplot(empirical_CDF, aes(centers, prop))+geom_path(aes(color=group))+labs(title="Cumulative Density Curve", x=expression(Median~particle~sizes~(mu~m)), y="Proportion")+scale_color_manual(values=getPalette(number_of_groups))+scale_x_log10()
plot_base <- ggplot(bin_time_average, aes(centers, avg))+geom_path(color=NA)+scale_color_manual(values=getPalette(number_of_groups))+coord_cartesian()+scale_x_log10()+labs(x=expression(Median~particle~sizes~(mu~m)), y=expression(Average~volumetric~concentration~(mu~L/L)))
plot_density <- plot_base+geom_path(aes(color=group))+ggtitle("Density Curve, unsmoothed")
plot_density_smoothed <- plot_base+geom_smooth(method=loess, se=FALSE, aes(color=group))+ggtitle("Density Curve, smoothed (LOESS local regression)")
plot_ndensity <- ggplot(bin_time_proportional, aes(centers, proportion))+geom_smooth(method=loess, se=FALSE, aes(color=group))+labs(title="Density Curve, normalized and smoothed (LOESS local regression)", x=expression(Median~particle~sizes~(mu~m)), y="Proportion")+scale_color_manual(values=getPalette(number_of_groups))
plot_list <- list(plot_cdf, plot_density, plot_density_smoothed, plot_ndensity)

dev.new()
cowplot::plot_grid(plotlist=plot_list)