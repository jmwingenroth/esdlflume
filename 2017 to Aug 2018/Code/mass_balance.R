require(sp)
require(gstat)
require(dplyr)
require(raster)

pp <- "C:\\Users\\Bearkey\\Documents\\Ecogeomorphic_Flume\\experiment_05_03_18\\05_03_18_PP_data.csv"
st <- "C:\\Users\\Bearkey\\Documents\\Ecogeomorphic_Flume\\experiment_05_03_18\\05_03_18_sediment_trap.csv"
discharge <- (215*3.78541)/60 #liters per second
leak <- FALSE

x <- c(9, 30, 49, 14, 30, 44, 10, 30, 49) #sediment trap x-coordinates (cm; from flume inner wall)
y <- -c(15, 15, 15, 74.5, 93, 115, 175, 175, 175) #sediment trap y-coordinates (cm; from top of upstream test section)

coords <- data.frame(x, y)
mass <- read.csv(st, header=TRUE, stringsAsFactors=FALSE)[,"mass"]/(pi*1.3^2)

st_spdf <- SpatialPointsDataFrame(coords, data=as.data.frame(mass))

test_section <- raster(extent(0, 60, -200, 0))
res(test_section) <- 10
gs <- gstat(formula=mass~1, data=st_spdf, set=list(idp=2))
idw <- interpolate(test_section, gs)
plot(idw)

settled_mass <- cellStats(idw, stat="sum")*prod(res(test_section)) #estimated mass settled over test section area (g)

pp_data <- read.csv(pp, header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(concentration=as.numeric(concentration), height=stringr::str_pad(height, width=2, side="left", pad="0")) %>%
  filter(!is.na(concentration)) %>%
  mutate(concentration=ifelse(concentration<0, 0, concentration), time=(time-min(time))*60, location=ifelse(location=="U", "upstream", "downstream"))

if (leak) {
  pp_data <- filter(pp_data, height!="27")
}

spread_table <- pp_data %>%
  tidyr::spread(key=location, value=concentration) %>%
  filter(!is.na(downstream), !is.na(upstream)) %>%
  mutate(diff=upstream-downstream) %>%
  mutate(diff=ifelse(diff<0, 0, diff)) %>%
  group_by(time) %>%
  summarise(mean_loss=mean(diff)) %>%
  mutate(mean_loss=mean_loss*discharge) %>%
  as.data.frame()

#mod <- loess(mean_loss~time, data=spread_table)
#mod_function <- function(x) predict(mod, x)
#captured_mass <- integrate(mod_function, lower=0, upper=max(spread_table[,"time"]))$value #estimated mass captured by collectors and settling (g)
captured_mass <- mean(spread_table[,"mean_loss"])*(max(spread_table[,"time"])-min(spread_table[,"time"]))

collected_mass <- captured_mass-settled_mass #estimated mass collected by dowels (g)