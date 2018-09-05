require(dplyr)
require(magrittr)

file <- "C:\\Users\\Bearkey\\Documents\\LISST\\LISST_data\\02_20_18_size_dist.asc"
correction <- TRUE #set this to TRUE if you want to remove all data points that have a transmission outside of the optimal range of 0.1 to 0.995
bin_k <- 2 #bin for which you want to calculate k
time_start <- 0 #set start time for the data analysis to exclude initialization
time_end <- Inf #set end time for data analysis
collector_density <- 183 #set this to the per area density of collectors in the test section (m^-2); for effective capture efficiency calculation
flow_velocity <- 0.05652 #set this to the flow velocity in the flume (m/s); for effective capture efficiency calculation

###Data Processing Section###
raw <- read.table(file, header=FALSE) #%>% mutate(V41=V41/100) #reading the .asc file as a table

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
  select(sec, bin, measurement) #combining particle concentration data into a single field

selected_bin <- filter(gathered_measurements, bin==bin_k) %>%
  filter(sec>=time_start, sec<=time_end)

bin_values <- data.frame(bin=1:32,
                         centers=c(7.95, 9.38, 11.07, 13.06, 15.42, 18.19, 21.47, 25.33, 29.90, 35.28,
                                   41.63, 49.13, 57.97, 68.41, 80.73, 95.26, 112.40, 132.70, 156.60, 184.70,
                                   218.00, 257.20, 303.60, 358.20, 422.80, 498.90, 588.80, 694.80, 819.90,
                                   967.50, 1142.00, 1347.00)) #log-spaced bin center values

bin_k_median <- bin_values[bin_k,2]

###Parameter Estimation Section###
##1: Linearize the exponential relationship by taking logs to obtain OLS approximations for parameters
concentration <- selected_bin[,"measurement"]
time<- selected_bin[,"sec"]

selected_bin_log <- selected_bin %>%
  mutate(log_concentration=log(concentration)) %>%
  filter(is.finite(log_concentration)==TRUE)

log_concentration <- selected_bin_log[,"log_concentration"]
time_log <- selected_bin_log[,"sec"]

ols_estimates <- coef(lm(log_concentration~time_log))
initial_concentration_ols <- exp(ols_estimates[1])
k_ols <- -ols_estimates[2]

##2: Perform nonlinear least squares to fit the exponential relationship using the OLS estimates as starting points
try_fit <- try(nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=initial_concentration_ols, k=k_ols), control=list(maxiter=200)))
if (class(try_fit)=="try-error") { 
  initial_concentration_ols <- initial_concentration_ols*100 #a cheap way to try to find a good starting point if the one from OLS turns out to be poor
  model_fit <- nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=initial_concentration_ols, k=k_ols), control=list(maxiter=200))
}
model_fit <- nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=initial_concentration_ols, k=k_ols), control=list(maxiter=200))
nls_estimates <- coef(model_fit)
initial_concentration_exp <- nls_estimates[1]
k_exp <- unname(nls_estimates[2]) #final estimate of k

#png("C:\\Users\\Bearkey\\Documents\\LISST\\images\\05_03_18_capture_rate.png", width=1500, height=1000, pointsize=5, res=300)
plot(concentration~time, main=paste("Time Series of Particle Concentration for bin", bin_k, "(median size:", bin_k_median, "microns)"), xlab="Time (s)", ylab=expression(Particle~concentration~(mu~L/L)))
curve(initial_concentration_exp*exp(-k_exp*x), col="red", lwd=2, add=TRUE) #check the model fit
legend(x="topright", legend=paste("k =", k_exp))
#dev.off()

###Effective Capture Efficiency###
#based on Fauria et al. (2015) equations 3 and 9; refer to paper for more information
ece <- function(k, u, collector_density,diameter=0.003175) (k*100)/(u*diameter*collector_density) #units: s^-1, m/s, m^-2, m
effective_capture_efficiency <- ece(k_exp, flow_velocity, collector_density) #u is the flow velocity; replace with actual flow velocity (e.g. from Vectrino)