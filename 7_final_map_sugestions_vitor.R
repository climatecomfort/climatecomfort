#install.packages("terra")
library(terra)
setwd("C:/Projeto/final/csvs")

final_data <- read.csv ("teste_ana2.csv")
View(final_data)

#temperature mean
temperature_mean <- mean (final_data$temperature_importance)
temperature_mean

#humidity mean
humidity_mean <- mean (final_data$humidity_importance)
humidity_mean

#ilumination mean
ilumination_mean <- mean (final_data$ilumination_importance)
ilumination_mean

#Rasters
temperature<-rast("ppd_mgcv_temperature_mean.tif")
plot(temperature)
humidity<-rast("ppd_mgcv_humidity_mean.tif")
plot(humidity)
radiation<-rast("ppd_mgcv_illumination_mean.tif")
plot(radiation)

# crs(temperature) / crs(humidity) / crs(radiation)

humidity_resampled<-resample(humidity,temperature)
radiation_resampled<-resample(radiation,temperature)

final_data_index <- ((temperature*temperature_mean)+(humidity_resampled*humidity_mean)+(radiation_resampled*ilumination_mean))/(temperature_mean+humidity_mean+ilumination_mean)
plot(final_data_index)

