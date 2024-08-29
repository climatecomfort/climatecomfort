#install.packages("readr")
library(readr)
#install.packages("tidyr")
library(tidyr)
#install.packages("sf")
library(sf)
#install.packages("terra")
library(terra) 
setwd("C:/Projeto/final/csvs")

results <- readRDS("final_adjusted_points.rds")
results_coords<-st_coordinates(results)
results_coords<-as.data.frame(results_coords)
results<-as.data.frame(results)
results$latitude<-results_coords$Y
results$longitude<-results_coords$X
results<-results[,-27]

results$id<-rep(seq_len(nrow(results))) # creates an ID for each row
results_long<-pivot_longer(data=results,cols=c(7,11),names_to="question",values_to="month")
results_long$temperature<-ifelse(results_long$question==results_long$question[1],
                                    results_long$temperature_more_comfortable,
                                    results_long$temperature_less_comfortable)
results_long$humidity<-ifelse(results_long$question==results_long$question[1],
                                results_long$humidity_more_comfortable,
                                results_long$humidity_less_comfortable)
results_long$illumination<-ifelse(results_long$question==results_long$question[1],
                                   results_long$illumination_more_comfortable,
                                   results_long$illumination_less_comfortable)
unique(results_long$temperature)
results_long$temperature<-factor(results_long$temperature, levels=c("Very cold","Cold","Slightly cold","Totally comfortable","Slightly hot","Hot","Very hot"))
unique(results_long$humidity)
results_long$humidity<-factor(results_long$humidity, levels=c("Very dry","Dry","Slightly dry","Totally comfortable","Slightly humid","Humid","Very humid"))
unique(results_long$illumination)
results_long$illumination<-factor(results_long$illumination, levels=c("A lot of lack of clarity","Lack of clarity","Slight lack of clarity","Totally comfortable","Slight excess of light","Moderate excess of light","Too much light"))
saveRDS(results_long, "results_long.rds")

#Slice into 12 dataframes, one for each month

sf_results_all <- st_as_sf(results_long, coords = c("longitude","latitude"), crs = 4326)

sf_results_january<-sf_results_all[results_long$month=="January",]
View(sf_results_january)
sf_results_february<-sf_results_all[results_long$month=="February",]
sf_results_march<-sf_results_all[results_long$month=="March",]
sf_results_april<-sf_results_all[results_long$month=="April",]
sf_results_may<-sf_results_all[results_long$month=="May",]
sf_results_june<-sf_results_all[results_long$month=="June",]
sf_results_july<-sf_results_all[results_long$month=="July",]
sf_results_august<-sf_results_all[results_long$month=="August",]
sf_results_september<-sf_results_all[results_long$month=="September",]
sf_results_october<-sf_results_all[results_long$month=="October",]
sf_results_november<-sf_results_all[results_long$month=="November",]
sf_results_december<-sf_results_all[results_long$month=="December",]

#January
spatvector_results_january<-vect(sf_results_january)
View(spatvector_results_january)
spatvector_results_january<-project(x=spatvector_results_january,y=temperature_january)
plot(temperature_january)
points(spatvector_results_january)

spatvector_results_january_temperature<-terra::extract(x=temperature_january,y=spatvector_results_january)
sf_results_january$temperature_month<-spatvector_results_january_temperature$mean
sf_results_january$temperature_month<-sf_results_january$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_january_humidity<-terra::extract(x=humidity_january,y=spatvector_results_january)
sf_results_january$humidity_month<-spatvector_results_january_humidity$mean

spatvector_results_january_radiation<-terra::extract(x=radiation_january,y=spatvector_results_january)
sf_results_january$radiation_month<-spatvector_results_january_radiation$mean

spatvector_results_january_total_cloud<-terra::extract(x=total_cloud_january,y=spatvector_results_january)
sf_results_january$total_cloud_month<-spatvector_results_january_total_cloud$mean

spatvector_results_january_wind<-terra::extract(x=wind_january,y=spatvector_results_january)
sf_results_january$wind_month<-spatvector_results_january_wind$mean

spatvector_results_january_mrt<-terra::extract(x=mrt_january,y=spatvector_results_january)
sf_results_january$mrt_month<-spatvector_results_january_mrt$mean

spatvector_results_january_utci<-terra::extract(x=utci_january,y=spatvector_results_january)
sf_results_january$utci_month<-spatvector_results_january_utci$mean

#February
spatvector_results_february<-vect(sf_results_february)
spatvector_results_february<-project(x=spatvector_results_february,y=temperature_february)
plot(temperature_february)
points(spatvector_results_february)

spatvector_results_february_temperature<-terra::extract(x=temperature_february,y=spatvector_results_february)
sf_results_february$temperature_month<-spatvector_results_february_temperature$mean
sf_results_february$temperature_month<-sf_results_february$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_february_humidity<-terra::extract(x=humidity_february,y=spatvector_results_february)
sf_results_february$humidity_month<-spatvector_results_february_humidity$mean

spatvector_results_february_radiation<-terra::extract(x=radiation_february,y=spatvector_results_february)
sf_results_february$radiation_month<-spatvector_results_february_radiation$mean

spatvector_results_february_total_cloud<-terra::extract(x=total_cloud_february,y=spatvector_results_february)
sf_results_february$total_cloud_month<-spatvector_results_february_total_cloud$mean

spatvector_results_february_wind<-terra::extract(x=wind_february,y=spatvector_results_february)
sf_results_february$wind_month<-spatvector_results_february_wind$mean

spatvector_results_february_mrt<-terra::extract(x=mrt_february,y=spatvector_results_february)
sf_results_february$mrt_month<-spatvector_results_february_mrt$mean

spatvector_results_february_utci<-terra::extract(x=utci_february,y=spatvector_results_february)
sf_results_february$utci_month<-spatvector_results_february_utci$mean

#March
spatvector_results_march<-vect(sf_results_march)
spatvector_results_march<-project(x=spatvector_results_march,y=temperature_march)
plot(temperature_march)
points(spatvector_results_march)

spatvector_results_march_temperature<-terra::extract(x=temperature_march,y=spatvector_results_march)
sf_results_march$temperature_month<-spatvector_results_march_temperature$mean
sf_results_march$temperature_month<-sf_results_march$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_march_humidity<-terra::extract(x=humidity_march,y=spatvector_results_march)
sf_results_march$humidity_month<-spatvector_results_march_humidity$mean

spatvector_results_march_radiation<-terra::extract(x=radiation_march,y=spatvector_results_march)
sf_results_march$radiation_month<-spatvector_results_march_radiation$mean

spatvector_results_march_total_cloud<-terra::extract(x=total_cloud_march,y=spatvector_results_march)
sf_results_march$total_cloud_month<-spatvector_results_march_total_cloud$mean

spatvector_results_march_wind<-terra::extract(x=wind_march,y=spatvector_results_march)
sf_results_march$wind_month<-spatvector_results_march_wind$mean

spatvector_results_march_mrt<-terra::extract(x=mrt_march,y=spatvector_results_march)
sf_results_march$mrt_month<-spatvector_results_march_mrt$mean

spatvector_results_march_utci<-terra::extract(x=utci_march,y=spatvector_results_march)
sf_results_march$utci_month<-spatvector_results_march_utci$mean

#April
spatvector_results_april<-vect(sf_results_april)
spatvector_results_april<-project(x=spatvector_results_april,y=temperature_april)
plot(temperature_april)
points(spatvector_results_april)

spatvector_results_april_temperature<-terra::extract(x=temperature_april,y=spatvector_results_april)
sf_results_april$temperature_month<-spatvector_results_april_temperature$mean
sf_results_april$temperature_month<-sf_results_april$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_april_humidity<-terra::extract(x=humidity_april,y=spatvector_results_april)
sf_results_april$humidity_month<-spatvector_results_april_humidity$mean

spatvector_results_april_radiation<-terra::extract(x=radiation_april,y=spatvector_results_april)
sf_results_april$radiation_month<-spatvector_results_april_radiation$mean

spatvector_results_april_total_cloud<-terra::extract(x=total_cloud_april,y=spatvector_results_april)
sf_results_april$total_cloud_month<-spatvector_results_april_total_cloud$mean

spatvector_results_april_wind<-terra::extract(x=wind_april,y=spatvector_results_april)
sf_results_april$wind_month<-spatvector_results_april_wind$mean

spatvector_results_april_mrt<-terra::extract(x=mrt_april,y=spatvector_results_april)
sf_results_april$mrt_month<-spatvector_results_april_mrt$mean

spatvector_results_april_utci<-terra::extract(x=utci_april,y=spatvector_results_april)
sf_results_april$utci_month<-spatvector_results_april_utci$mean

#May
spatvector_results_may<-vect(sf_results_may)
spatvector_results_may<-project(x=spatvector_results_may,y=temperature_may)
plot(temperature_may)
points(spatvector_results_may)

spatvector_results_may_temperature<-terra::extract(x=temperature_may,y=spatvector_results_may)
sf_results_may$temperature_month<-spatvector_results_may_temperature$mean
sf_results_may$temperature_month<-sf_results_may$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_may_humidity<-terra::extract(x=humidity_may,y=spatvector_results_may)
sf_results_may$humidity_month<-spatvector_results_may_humidity$mean

spatvector_results_may_radiation<-terra::extract(x=radiation_may,y=spatvector_results_may)
sf_results_may$radiation_month<-spatvector_results_may_radiation$mean

spatvector_results_may_total_cloud<-terra::extract(x=total_cloud_may,y=spatvector_results_may)
sf_results_may$total_cloud_month<-spatvector_results_may_total_cloud$mean

spatvector_results_may_wind<-terra::extract(x=wind_may,y=spatvector_results_may)
sf_results_may$wind_month<-spatvector_results_may_wind$mean

spatvector_results_may_mrt<-terra::extract(x=mrt_may,y=spatvector_results_may)
sf_results_may$mrt_month<-spatvector_results_may_mrt$mean

spatvector_results_may_utci<-terra::extract(x=utci_may,y=spatvector_results_may)
sf_results_may$utci_month<-spatvector_results_may_utci$mean

#June
spatvector_results_june<-vect(sf_results_june)
spatvector_results_june<-project(x=spatvector_results_june,y=temperature_june)
plot(temperature_june)
points(spatvector_results_june)

spatvector_results_june_temperature<-terra::extract(x=temperature_june,y=spatvector_results_june)
sf_results_june$temperature_month<-spatvector_results_june_temperature$mean
sf_results_june$temperature_month<-sf_results_june$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_june_humidity<-terra::extract(x=humidity_june,y=spatvector_results_june)
sf_results_june$humidity_month<-spatvector_results_june_humidity$mean

spatvector_results_june_radiation<-terra::extract(x=radiation_june,y=spatvector_results_june)
sf_results_june$radiation_month<-spatvector_results_june_radiation$mean

spatvector_results_june_total_cloud<-terra::extract(x=total_cloud_june,y=spatvector_results_june)
sf_results_june$total_cloud_month<-spatvector_results_june_total_cloud$mean

spatvector_results_june_wind<-terra::extract(x=wind_june,y=spatvector_results_june)
sf_results_june$wind_month<-spatvector_results_june_wind$mean

spatvector_results_june_mrt<-terra::extract(x=mrt_june,y=spatvector_results_june)
sf_results_june$mrt_month<-spatvector_results_june_mrt$mean

spatvector_results_june_utci<-terra::extract(x=utci_june,y=spatvector_results_june)
sf_results_june$utci_month<-spatvector_results_june_utci$mean

#July
spatvector_results_july<-vect(sf_results_july)
spatvector_results_july<-project(x=spatvector_results_july,y=temperature_july)
plot(temperature_july)
points(spatvector_results_july)

spatvector_results_july_temperature<-terra::extract(x=temperature_july,y=spatvector_results_july)
sf_results_july$temperature_month<-spatvector_results_july_temperature$mean
sf_results_july$temperature_month<-sf_results_july$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_july_humidity<-terra::extract(x=humidity_july,y=spatvector_results_july)
sf_results_july$humidity_month<-spatvector_results_july_humidity$mean

spatvector_results_july_radiation<-terra::extract(x=radiation_july,y=spatvector_results_july)
sf_results_july$radiation_month<-spatvector_results_july_radiation$mean

spatvector_results_july_total_cloud<-terra::extract(x=total_cloud_july,y=spatvector_results_july)
sf_results_july$total_cloud_month<-spatvector_results_july_total_cloud$mean

spatvector_results_july_wind<-terra::extract(x=wind_july,y=spatvector_results_july)
sf_results_july$wind_month<-spatvector_results_july_wind$mean

spatvector_results_july_mrt<-terra::extract(x=mrt_july,y=spatvector_results_july)
sf_results_july$mrt_month<-spatvector_results_july_mrt$mean

spatvector_results_july_utci<-terra::extract(x=utci_july,y=spatvector_results_july)
sf_results_july$utci_month<-spatvector_results_july_utci$mean

#August
spatvector_results_august<-vect(sf_results_august)
spatvector_results_august<-project(x=spatvector_results_august,y=temperature_august)
plot(temperature_august)
points(spatvector_results_august)

spatvector_results_august_temperature<-terra::extract(x=temperature_august,y=spatvector_results_august)
sf_results_august$temperature_month<-spatvector_results_august_temperature$mean
sf_results_august$temperature_month<-sf_results_august$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_august_humidity<-terra::extract(x=humidity_august,y=spatvector_results_august)
sf_results_august$humidity_month<-spatvector_results_august_humidity$mean

spatvector_results_august_radiation<-terra::extract(x=radiation_august,y=spatvector_results_august)
sf_results_august$radiation_month<-spatvector_results_august_radiation$mean

spatvector_results_august_total_cloud<-terra::extract(x=total_cloud_august,y=spatvector_results_august)
sf_results_august$total_cloud_month<-spatvector_results_august_total_cloud$mean

spatvector_results_august_wind<-terra::extract(x=wind_august,y=spatvector_results_august)
sf_results_august$wind_month<-spatvector_results_august_wind$mean

spatvector_results_august_mrt<-terra::extract(x=mrt_august,y=spatvector_results_august)
sf_results_august$mrt_month<-spatvector_results_august_mrt$mean

spatvector_results_august_utci<-terra::extract(x=utci_august,y=spatvector_results_august)
sf_results_august$utci_month<-spatvector_results_august_utci$mean

#September
spatvector_results_september<-vect(sf_results_september)
spatvector_results_september<-project(x=spatvector_results_september,y=temperature_september)
plot(temperature_september)
points(spatvector_results_september)

spatvector_results_september_temperature<-terra::extract(x=temperature_september,y=spatvector_results_september)
sf_results_september$temperature_month<-spatvector_results_september_temperature$mean
sf_results_september$temperature_month<-sf_results_september$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_september_humidity<-terra::extract(x=humidity_september,y=spatvector_results_september)
sf_results_september$humidity_month<-spatvector_results_september_humidity$mean

spatvector_results_september_radiation<-terra::extract(x=radiation_september,y=spatvector_results_september)
sf_results_september$radiation_month<-spatvector_results_september_radiation$mean

spatvector_results_september_total_cloud<-terra::extract(x=total_cloud_september,y=spatvector_results_september)
sf_results_september$total_cloud_month<-spatvector_results_september_total_cloud$mean

spatvector_results_september_wind<-terra::extract(x=wind_september,y=spatvector_results_september)
sf_results_september$wind_month<-spatvector_results_september_wind$mean

spatvector_results_september_mrt<-terra::extract(x=mrt_september,y=spatvector_results_september)
sf_results_september$mrt_month<-spatvector_results_september_mrt$mean

spatvector_results_september_utci<-terra::extract(x=utci_september,y=spatvector_results_september)
sf_results_september$utci_month<-spatvector_results_september_utci$mean

#October
spatvector_results_october<-vect(sf_results_october)
spatvector_results_october<-project(x=spatvector_results_october,y=temperature_october)
plot(temperature_october)
points(spatvector_results_october)

spatvector_results_october_temperature<-terra::extract(x=temperature_october,y=spatvector_results_october)
sf_results_october$temperature_month<-spatvector_results_october_temperature$mean
sf_results_october$temperature_month<-sf_results_october$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_october_humidity<-terra::extract(x=humidity_october,y=spatvector_results_october)
sf_results_october$humidity_month<-spatvector_results_october_humidity$mean

spatvector_results_october_radiation<-terra::extract(x=radiation_october,y=spatvector_results_october)
sf_results_october$radiation_month<-spatvector_results_october_radiation$mean

spatvector_results_october_total_cloud<-terra::extract(x=total_cloud_october,y=spatvector_results_october)
sf_results_october$total_cloud_month<-spatvector_results_october_total_cloud$mean

spatvector_results_october_wind<-terra::extract(x=wind_october,y=spatvector_results_october)
sf_results_october$wind_month<-spatvector_results_october_wind$mean

spatvector_results_october_mrt<-terra::extract(x=mrt_october,y=spatvector_results_october)
sf_results_october$mrt_month<-spatvector_results_october_mrt$mean

spatvector_results_october_utci<-terra::extract(x=utci_october,y=spatvector_results_october)
sf_results_october$utci_month<-spatvector_results_october_utci$mean

#November
spatvector_results_november<-vect(sf_results_november)
spatvector_results_november<-project(x=spatvector_results_november,y=temperature_november)
plot(temperature_november)
points(spatvector_results_november)

spatvector_results_november_temperature<-terra::extract(x=temperature_november,y=spatvector_results_november)
sf_results_november$temperature_month<-spatvector_results_november_temperature$mean
sf_results_november$temperature_month<-sf_results_november$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_november_humidity<-terra::extract(x=humidity_november,y=spatvector_results_november)
sf_results_november$humidity_month<-spatvector_results_november_humidity$mean

spatvector_results_november_radiation<-terra::extract(x=radiation_november,y=spatvector_results_november)
sf_results_november$radiation_month<-spatvector_results_november_radiation$mean

spatvector_results_november_total_cloud<-terra::extract(x=total_cloud_november,y=spatvector_results_november)
sf_results_november$total_cloud_month<-spatvector_results_november_total_cloud$mean

spatvector_results_november_wind<-terra::extract(x=wind_november,y=spatvector_results_november)
sf_results_november$wind_month<-spatvector_results_november_wind$mean

spatvector_results_november_mrt<-terra::extract(x=mrt_november,y=spatvector_results_november)
sf_results_november$mrt_month<-spatvector_results_november_mrt$mean

spatvector_results_november_utci<-terra::extract(x=utci_november,y=spatvector_results_november)
sf_results_november$utci_month<-spatvector_results_november_utci$mean

#December
spatvector_results_december<-vect(sf_results_december)
spatvector_results_december<-project(x=spatvector_results_december,y=temperature_december)
plot(temperature_december)
points(spatvector_results_december)

spatvector_results_december_temperature<-terra::extract(x=temperature_december,y=spatvector_results_december)
sf_results_december$temperature_month<-spatvector_results_december_temperature$mean
sf_results_december$temperature_month<-sf_results_december$temperature_month - 273.15  #converts from Kelvin to Celsius

spatvector_results_december_humidity<-terra::extract(x=humidity_december,y=spatvector_results_december)
sf_results_december$humidity_month<-spatvector_results_december_humidity$mean

spatvector_results_december_radiation<-terra::extract(x=radiation_december,y=spatvector_results_december)
sf_results_december$radiation_month<-spatvector_results_december_radiation$mean

spatvector_results_december_total_cloud<-terra::extract(x=total_cloud_december,y=spatvector_results_december)
sf_results_december$total_cloud_month<-spatvector_results_december_total_cloud$mean

spatvector_results_december_wind<-terra::extract(x=wind_december,y=spatvector_results_december)
sf_results_december$wind_month<-spatvector_results_december_wind$mean

spatvector_results_december_mrt<-terra::extract(x=mrt_december,y=spatvector_results_december)
sf_results_december$mrt_month<-spatvector_results_december_mrt$mean

spatvector_results_december_utci<-terra::extract(x=utci_december,y=spatvector_results_december)
sf_results_december$utci_month<-spatvector_results_december_utci$mean

#Unify all months
sf_results_all<-rbind(sf_results_january,sf_results_february,sf_results_march,sf_results_april,sf_results_may,sf_results_june,sf_results_july,sf_results_august,sf_results_september,sf_results_october,sf_results_november,sf_results_december)
plot(sf_results_all$temperature_month~sf_results_all$temperature)
sf_results_all$temperature_pmv<-as.integer(sf_results_all$temperature)
plot(sf_results_all$temperature_pmv~sf_results_all$temperature_month)
abline(lm(sf_results_all$temperature_pmv~sf_results_all$temperature_month))  #linha de tendência linear

##Includes mean, min and max annual values
spatvector_results_all<-vect(sf_results_all)
spatvector_results_all<-project(x=spatvector_results_all,y=temperature_january)

##Temperature
spatvector_results_all_temperature_mean<-terra::extract(x=temperature_mean,y=spatvector_results_all)
sf_results_all$temperature_mean<-spatvector_results_all_temperature_mean$mean
sf_results_all$temperature_mean<- sf_results_all$temperature_mean - 273.15

spatvector_results_all_temperature_min<-terra::extract(x=temperature_min,y=spatvector_results_all)
sf_results_all$temperature_min<-spatvector_results_all_temperature_min$min
sf_results_all$temperature_min<- sf_results_all$temperature_min - 273.15

spatvector_results_all_temperature_max<-terra::extract(x=temperature_max,y=spatvector_results_all)
sf_results_all$temperature_max<-spatvector_results_all_temperature_max$max
sf_results_all$temperature_max<- sf_results_all$temperature_max - 273.15

##Humidity
spatvector_results_all_humidity_mean<-terra::extract(x=humidity_mean,y=spatvector_results_all)
sf_results_all$humidity_mean<-spatvector_results_all_humidity_mean$mean

spatvector_results_all_humidity_min<-terra::extract(x=humidity_min,y=spatvector_results_all)
sf_results_all$humidity_min<-spatvector_results_all_humidity_min$min

spatvector_results_all_humidity_max<-terra::extract(x=humidity_max,y=spatvector_results_all)
sf_results_all$humidity_max<-spatvector_results_all_humidity_max$max

##Radiation
spatvector_results_all_radiation_mean<-terra::extract(x=radiation_mean,y=spatvector_results_all)
sf_results_all$radiation_mean<-spatvector_results_all_radiation_mean$mean

spatvector_results_all_radiation_min<-terra::extract(x=radiation_min,y=spatvector_results_all)
sf_results_all$radiation_min<-spatvector_results_all_radiation_min$min

spatvector_results_all_radiation_max<-terra::extract(x=radiation_max,y=spatvector_results_all)
sf_results_all$radiation_max<-spatvector_results_all_radiation_max$max

##Total_cloud
spatvector_results_all_total_cloud_mean<-terra::extract(x=total_cloud_mean,y=spatvector_results_all)
sf_results_all$total_cloud_mean<-spatvector_results_all_total_cloud_mean$mean

spatvector_results_all_total_cloud_min<-terra::extract(x=total_cloud_min,y=spatvector_results_all)
sf_results_all$total_cloud_min<-spatvector_results_all_total_cloud_min$min

spatvector_results_all_total_cloud_max<-terra::extract(x=total_cloud_max,y=spatvector_results_all)
sf_results_all$total_cloud_max<-spatvector_results_all_total_cloud_max$max

##Wind
spatvector_results_all_wind_mean<-terra::extract(x=wind_mean,y=spatvector_results_all)
sf_results_all$wind_mean<-spatvector_results_all_wind_mean$mean

spatvector_results_all_wind_min<-terra::extract(x=wind_min,y=spatvector_results_all)
sf_results_all$wind_min<-spatvector_results_all_wind_min$min

spatvector_results_all_wind_max<-terra::extract(x=wind_max,y=spatvector_results_all)
sf_results_all$wind_max<-spatvector_results_all_wind_max$max

##MRT
spatvector_results_all_mrt_mean<-terra::extract(x=mrt_mean,y=spatvector_results_all)
sf_results_all$mrt_mean<-spatvector_results_all_mrt_mean$mean
sf_results_all$mrt_mean<- sf_results_all$mrt_mean - 273.15

spatvector_results_all_mrt_min<-terra::extract(x=mrt_min,y=spatvector_results_all)
sf_results_all$mrt_min<-spatvector_results_all_mrt_min$min
sf_results_all$mrt_min<- sf_results_all$mrt_min - 273.15

spatvector_results_all_mrt_max<-terra::extract(x=mrt_max,y=spatvector_results_all)
sf_results_all$mrt_max<-spatvector_results_all_mrt_max$max
sf_results_all$mrt_max<- sf_results_all$mrt_max - 273.15

##UTCI
spatvector_results_all_utci_mean<-terra::extract(x=utci_mean,y=spatvector_results_all)
sf_results_all$utci_mean<-spatvector_results_all_utci_mean$mean
sf_results_all$utci_mean<- sf_results_all$utci_mean - 273.15

spatvector_results_all_utci_min<-terra::extract(x=utci_min,y=spatvector_results_all)
sf_results_all$utci_min<-spatvector_results_all_utci_min$min
sf_results_all$utci_min<- sf_results_all$utci_min - 273.15

spatvector_results_all_utci_max<-terra::extract(x=utci_max,y=spatvector_results_all)
sf_results_all$utci_max<-spatvector_results_all_utci_max$max
sf_results_all$utci_max<- sf_results_all$utci_max - 273.15

