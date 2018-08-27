require(dplyr)
require(magrittr)
require(tidyr)
require(ggplot2)

#Data Quality Control:
#Laser reference >>0
#Transmission should be between 0 and 1 (low transmission indicates turbid water, high tranmission indicates clear water; should disregard these data)

file <- "C:\\Users\\Bearkey\\Documents\\LISST\\LISST_data\\05_03_18_size_dist.asc"
correction <- TRUE #set this to TRUE if you want to remove all data points that have a transmission outside of the optimal range of 0.1 to 0.995

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

bin_values <- data.frame(bin=1:32,
                         centers=c(7.95, 9.38, 11.07, 13.06, 15.42, 18.19, 21.47, 25.33, 29.90, 35.28,
                                   41.63, 49.13, 57.97, 68.41, 80.73, 95.26, 112.40, 132.70, 156.60, 184.70,
                                   218.00, 257.20, 303.60, 358.20, 422.80, 498.90, 588.80, 694.80, 819.90,
                                   967.50, 1142.00, 1347.00)) #log-spaced bin center values

time_converted_sec <- raw %>%
  mutate(min=as.numeric(ifelse(V40>=100, gsub("[[:digit:]]{2}$", "", V40), 0))) %>%
  mutate(sec=as.numeric(ifelse(V40>=1000, gsub("^[[:digit:]]{2}", "", V40), ifelse(V40<1000 & V40>=100, gsub("^[[:digit:]]", "", V40), V40)))) %>%
  mutate(sec=sec+(min*60)) %>%
  select(-min) #conversion of time units to seconds

diff <- c(0, diff(time_converted_sec[,"sec"]))
diff[diff<0] <- diff[diff<0]+3600
seconds <- cumsum(diff)+1

volume_matrix <- raw %>%
  select(1:32) %>%
  as.matrix()

total_concentration <- data.frame(time=seconds, total_concentration=rowSums(volume_matrix))

colnames(volume_matrix) <- 1:32
normalized_volume_matrix <- t(scale(t(volume_matrix), center=FALSE, scale=colSums(t(volume_matrix))))
center_vector <- matrix(data=log10(bin_values[,2]), nrow=32, ncol=1)
convex_combination <- 10^(normalized_volume_matrix %*% center_vector)
convex_combination_time_series <- data.frame(time=seconds, value=as.vector(convex_combination))

CDF <- apply(normalized_volume_matrix, 1, cumsum) %>%
  matrix(ncol=1) %>%
  as.data.frame() %>%
  mutate(bin=rep(1:32, times=length(seconds)), time=rep(seconds, each=32)) %>%
  left_join(bin_values,by="bin") %>%
  mutate(group=as.character(time))

getPalette <- grDevices::colorRampPalette(c("blue", "red"))

#png("C:\\Users\\Bearkey\\Documents\\LISST\\images\\02_16_18_images\\weighted_average_02_16_18.png", width=1500, height=1000, pointsize=1, res=300)
#the png() function gives you the option to save a plot at a specified resolution

p1 <- ggplot(convex_combination_time_series, aes(x=time, y=value))+geom_point(size=0.5)+labs(title="Weighted Average Particle Size", x="Time (s)", y=expression(Particle~size~(mu~m)))
p2 <- ggplot(CDF, aes(x=centers, y=V1))+geom_path(aes(color=group))+labs(title="Empirical CDF", x=expression(Particle~size~(mu~m)), y="Proportion")+scale_color_manual(values=getPalette(length(seconds)))+theme(legend.position="none")
p3 <- ggplot(total_concentration, aes(x=time, y=total_concentration))+geom_point(size=0.5)+labs(title="Total Particle Concentration", x="Time (s)", y=expression(Particle~concentration~(mu~L/L)))