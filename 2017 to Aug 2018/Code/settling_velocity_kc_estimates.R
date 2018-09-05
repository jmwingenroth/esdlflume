require(dplyr)
require(magrittr)
require(ggplot2)

bin <- 2 #set this to the bin on which you want the Stokes' law calculations to be based
depth <- 0.4 #water depth in the flume (m)
particle_density <- 1275 #set this to the particle density (kg/m^3)
total_time <- 14400 #time elapsed during the experiment (s)
sediment_trap_data <- "C:\\Users\\Bearkey\\Documents\\Ecogeomorphic_Flume\\experiment_05_11_18\\05_11_18_sediment_trap.csv"
#file for sediment trap data should be a csv file with the first column named "position" with entries A to I
#and second column named "mass" with entries containing the mass (in grams) collected in each corresponding sediment trap
source("C:\\Users\\Bearkey\\Documents\\LISST\\LISST_code\\k_calculation.R") #replace path with path on your computer to the script k_calculation.R
#make sure the settings in k_calculation.R are correct (e.g. correct file path to data file)

trap_mass <- read.csv(sediment_trap_data, header=TRUE, stringsAsFactors=FALSE)
ST_method <- function(mass, time, density, radius=0.013) { #units: kg, s, kg/m^3, m
  cross_section <- pi*radius^2
  return((1/(density*cross_section))*(mass/time))
} #settling velocity estimate using sediment trap data

stokes <- function(particle_radius, particle_density, fluid_density=1000, dyn_viscosity=1.0002*10^(-3), g=9.8) { #units: kg/m^3, kg/m^3, m, Pa*s, m/s^2
  numerator <- 2*(particle_density-fluid_density)*g*particle_radius^2
  denominator <- 9*dyn_viscosity
  return(numerator/denominator)
} #settling velocity estimate using Stokes' law, assuming temperature is 20 degrees C (for dynamic viscosity)

kc_calculator <- function(settling_v, depth=0.4, volume=2.93, width=0.6, L=1.95) { #computes kc using equation 11 in Fauria et al. (2015)
  correction_coef <- volume/(L*width*depth)
  ks <- settling_v/depth
  return(correction_coef*(k_exp-ks))
}

bin_values <- data.frame(bin=1:32,
                         centers=c(7.95, 9.38, 11.07, 13.06, 15.42, 18.19, 21.47, 25.33, 29.90, 35.28,
                                   41.63, 49.13, 57.97, 68.41, 80.73, 95.26, 112.40, 132.70, 156.60, 184.70,
                                   218.00, 257.20, 303.60, 358.20, 422.80, 498.90, 588.80, 694.80, 819.90,
                                   967.50, 1142.00, 1347.00)) #data frame of bin median values

ST_estimates <- data.frame(trap_mass, settling_v=sapply(trap_mass[,"mass"]/1000, ST_method, total_time, particle_density))
stokes_estimates <- data.frame(bin_values, settling_v=sapply((bin_values[,"centers"]/2)*10^(-9), stokes, particle_density))

ST_estimates <- data.frame(ST_estimates, kc=sapply(ST_estimates$settling_v, kc_calculator, depth))
ks_stokes <- stokes_estimates[stokes_estimates$bin==bin,"settling_v"]/depth
kc_stokes <- kc_calculator(stokes_estimates[stokes_estimates$bin==bin,"settling_v"], depth)

ggplot(ST_estimates, aes(position, settling_v))+geom_col(fill="white", col="black")+labs(title="Sediment Trap Settling Velocity Estimates", x="Sediment trap", y="Estimated settling velocity (m/s)")
ggplot(stokes_estimates, aes(stringr::str_pad(bin, width=2, side="left", pad="0"), settling_v))+geom_col(fill="white", col="black")+labs(title="Stokes' Law Settling Velocity Estimates", x="Bin number", y="Estimated settling velocity (m/s)")