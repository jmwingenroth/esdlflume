require(dplyr)
require(magrittr)
require(ggplot2)

file <- "C:\\Users\\Bearkey\\Documents\\Ecogeomorphic_Flume\\experiment_05_24_18\\05_24_18_PP_data.csv" #replace with absolute file path to peristaltic pump data
loc <- "downstream" #set to upstream or downstream
ht <- 14 #set to 5, 14, or 27
start_time <- 0 # set start time for data analysis
leak <- FALSE #removes all data at height 27 cm

ht <- stringr::str_pad(ht, width=2, side="left", pad="0")
raw <- read.csv(file, header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(concentration=as.numeric(concentration), height=stringr::str_pad(height, width=2, side="left", pad="0"), time=time-min(time)) %>%
  filter(!is.na(concentration)) %>%
  mutate(concentration=ifelse(concentration<0, 0, concentration), time=(time-min(time))*60, location=ifelse(location=="U", "upstream", "downstream")) %>%
  filter(time>=start_time)

if (leak) {
  raw <- filter(raw, height!="27")
}

base <- filter(raw, location==loc, height==ht)

concentration <- base[,"concentration"]
time<- base[,"time"]

log_values <- data.frame(log_time=log(time), log_concentration=log(concentration)) %>%
  filter(is.finite(log_time), is.finite(log_concentration))

k_ols <- -coef(lm(log_concentration~log_time, data=log_values))[2]/1000

model_fit <- nls(concentration~initial_concentration*exp(-k*time), start=list(initial_concentration=max(concentration), k=k_ols), control=list(maxiter=200))
nls_estimates <- coef(model_fit)
k_final <- unname(nls_estimates[2])

k_curve <- function(x) nls_estimates[1]*exp(-nls_estimates[2]*x)
mse <- ModelMetrics::mse(concentration, sapply(time, k_curve))

plot(concentration~time, data=base, main=paste("Time Series of Mass Concentration for", ht, "cm from bed,", loc), xlab="Time (s)", ylab="Mass concentration (g/L)")
curve(k_curve, from=min(time), to=max(time), col="red", lwd=2, add=TRUE)
legend(x="topright", legend=c(paste("k =", signif(k_final, 8)), paste("MSE =", signif(mse, 8))))