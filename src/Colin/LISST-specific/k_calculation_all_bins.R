require(dplyr)
require(magrittr)

file = "/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/LISST data processing/processed_data/05_24_18_size_dist.asc"
correction = TRUE #set this to TRUE if you want to remove all data points that have a transmission outside of the optimal range of 0.1 to 0.995
time_start = 900 #set start time for the data analysis to exclude initialization
time_end = Inf #set end time for data analysis
collector_density = 0  # previous: 183 #set this to the per area density of collectors in the test section (m^-2); for effective capture efficiency calculation
flow_velocity = 0.05652 #set this to the flow velocity in the flume (m/s); for effective capture efficiency calculation

###Data Processing Section###
raw = read.table(file, header=FALSE) #%>% mutate(V41=V41/100) #reading the .asc file as a table

laser_reference = any(raw[,36]==0) #laser reference values should all be much greater than 0

if (laser_reference==TRUE) {
  warning("Laser reference has a 0 value")
}

transmission = any(c(raw[,41]<=0, raw[,41]>=1)) #it is physically impossible to have a transmission not in the range 0 to 1

if (transmission==TRUE) {
  warning("Transmission has value(s) outside of the range 0 to 1")
}

if (correction==TRUE) {
  raw = raw %>%
    filter(V36>0, V41>=0.1, V41<=0.995)
}

time_converted_sec = raw %>%
  mutate(min=as.numeric(ifelse(V40>=100, gsub("[[:digit:]]{2}$", "", V40), 0))) %>%
  mutate(sec=as.numeric(ifelse(V40>=1000, gsub("^[[:digit:]]{2}", "", V40), ifelse(V40<1000 & V40>=100, gsub("^[[:digit:]]", "", V40), V40)))) %>%
  mutate(sec=sec+(min*60)) %>%
  select(-min) #conversion of time units to seconds

diff = c(0, diff(time_converted_sec[,"sec"]))
diff[diff<0] = diff[diff<0]+3600
seconds = cumsum(diff)+1

time_converted_final = time_converted_sec %>%
  mutate(sec=seconds) #setting seconds to start at 0 and count increasing (as opposed to cyclically)

gathered_measurements = time_converted_final %>%
  tidyr::gather(key=bin, value=measurement, 1:32) %>%
  mutate(bin=as.numeric(gsub("^V", "", bin))) %>%
  select(sec, bin, measurement) #combining particle concentration data into a single field

# get total particle concentration at each time point
volume_matrix = time_converted_final %>%
  select(1:32) %>%
  as.matrix()
total_concentration_1 = data.frame(sec=seconds, measurement=rowSums(volume_matrix))
total_concentration = filter(total_concentration_1, sec>=time_start, sec<=time_end)

###Parameter Estimation Section###
##1: Linearize the exponential relationship by taking logs to obtain OLS approximations for parameters
concentration = total_concentration[,"measurement"]
time = total_concentration[,"sec"]

total_concentration_log = total_concentration %>%
  mutate(log_concentration=log(concentration)) %>%
  filter(is.finite(log_concentration)==TRUE)

log_concentration = total_concentration_log[,"log_concentration"]
time_log = total_concentration_log[,"sec"]

ols_estimates = coef(lm(log_concentration~time_log))
initial_concentration_ols = exp(ols_estimates[1])
k_ols = -ols_estimates[2]

##2: Perform nonlinear least squares to fit the exponential relationship using the OLS estimates as starting points
try_fit = try(nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=initial_concentration_ols, k=k_ols), control=list(maxiter=200)))
if (class(try_fit)=="try-error") { 
  initial_concentration_ols = initial_concentration_ols*100 #a cheap way to try to find a good starting point if the one from OLS turns out to be poor
  model_fit = nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=initial_concentration_ols, k=k_ols), control=list(maxiter=200))
}
model_fit = nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=initial_concentration_ols, k=k_ols), control=list(maxiter=200))
nls_estimates = coef(model_fit)
initial_concentration_exp = nls_estimates[1]
k_exp = unname(nls_estimates[2]) #final estimate of k

plot(concentration~time, main=paste("Time Series of Mean Particle Concentration Across All Bins"), xlab="Time (s)", ylab=expression(Particle~concentration~(mu~L/L)))
curve(initial_concentration_exp*exp(-k_exp*x), col="red", lwd=2, add=TRUE) #check the model fit
legend(x="topright", legend=paste("k =", k_exp))

###Effective Capture Efficiency###
#based on Fauria et al. (2015) equations 3 and 9; refer to paper for more information
ece = function(k, u, collector_density,diameter=0.003175) (k*100)/(u*diameter*collector_density) #units: s^-1, m/s, m^-2, m
effective_capture_efficiency = ece(k_exp, flow_velocity, collector_density) #u is the flow velocity; replace with actual flow velocity (e.g. from Vectrino)