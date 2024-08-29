#install.packages("terra")
library(terra)
setwd("C:/Projeto/final/scripts")

#Calculating wind speed based on north-south and east-west components, using Pitagoras' Theorem
#January
wind_january_u<-rast("Wind_U/january_u_component_wind_mean_rotated.tif")
wind_january_v<-rast("Wind_V/january_v_component_wind_mean_rotated.tif")
wind_january<-sqrt((wind_january_u*wind_january_u)+(wind_january_v*wind_january_v))
plot(wind_january)
writeRaster(wind_january,"wind_january.tif", overwrite=TRUE)
#February
wind_february_u<-rast("Wind_U/february_u_component_wind_mean_rotated.tif")
wind_february_v<-rast("Wind_V/february_v_component_wind_mean_rotated.tif")
wind_february<-sqrt((wind_february_u*wind_february_u)+(wind_february_v*wind_february_v))
plot(wind_february)
writeRaster(wind_february,"wind_february.tif", overwrite=TRUE)
#March
wind_march_u<-rast("Wind_U/march_u_component_wind_mean_rotated.tif")
wind_march_v<-rast("Wind_V/march_v_component_wind_mean_rotated.tif")
wind_march<-sqrt((wind_march_u*wind_march_u)+(wind_march_v*wind_march_v))
plot(wind_march)
writeRaster(wind_march,"wind_march.tif", overwrite=TRUE)
#April
wind_april_u<-rast("Wind_U/april_u_component_wind_mean_rotated.tif")
wind_april_v<-rast("Wind_V/april_v_component_wind_mean_rotated.tif")
wind_april<-sqrt((wind_april_u*wind_april_u)+(wind_april_v*wind_april_v))
plot(wind_april)
writeRaster(wind_april,"wind_april.tif", overwrite=TRUE)
#May
wind_may_u<-rast("Wind_U/may_u_component_wind_mean_rotated.tif")
wind_may_v<-rast("Wind_V/may_v_component_wind_mean_rotated.tif")
wind_may<-sqrt((wind_may_u*wind_may_u)+(wind_may_v*wind_may_v))
plot(wind_may)
writeRaster(wind_may,"wind_may.tif",overwrite=TRUE)
#June
wind_june_u<-rast("Wind_U/june_u_component_wind_mean_rotated.tif")
wind_june_v<-rast("Wind_V/june_v_component_wind_mean_rotated.tif")
wind_june<-sqrt((wind_june_u*wind_june_u)+(wind_june_v*wind_june_v))
plot(wind_june)
writeRaster(wind_june,"wind_june.tif", overwrite=TRUE)
#July
wind_july_u<-rast("Wind_U/july_u_component_wind_mean_rotated.tif")
wind_july_v<-rast("Wind_V/july_v_component_wind_mean_rotated.tif")
wind_july<-sqrt((wind_july_u*wind_july_u)+(wind_july_v*wind_july_v))
plot(wind_july)
writeRaster(wind_july,"wind_july.tif", overwrite=TRUE)
#August
wind_august_u<-rast("Wind_U/august_u_component_wind_mean_rotated.tif")
wind_august_v<-rast("Wind_V/august_v_component_wind_mean_rotated.tif")
wind_august<-sqrt((wind_august_u*wind_august_u)+(wind_august_v*wind_august_v))
plot(wind_august)
writeRaster(wind_august,"wind_august.tif", overwrite=TRUE)
#September
wind_september_u<-rast("Wind_U/september_u_component_wind_mean_rotated.tif")
wind_september_v<-rast("Wind_V/september_v_component_wind_mean_rotated.tif")
wind_september<-sqrt((wind_september_u*wind_september_u)+(wind_september_v*wind_september_v))
plot(wind_september)
writeRaster(wind_september,"wind_september.tif", overwrite=TRUE)
#October
wind_october_u<-rast("Wind_U/october_u_component_wind_mean_rotated.tif")
wind_october_v<-rast("Wind_V/october_v_component_wind_mean_rotated.tif")
wind_october<-sqrt((wind_october_u*wind_october_u)+(wind_october_v*wind_october_v))
plot(wind_october)
writeRaster(wind_october,"wind_october.tif", overwrite=TRUE)
#November
wind_november_u<-rast("Wind_U/november_u_component_wind_mean_rotated.tif")
wind_november_v<-rast("Wind_V/november_v_component_wind_mean_rotated.tif")
wind_november<-sqrt((wind_november_u*wind_november_u)+(wind_november_v*wind_november_v))
plot(wind_november)
writeRaster(wind_november,"wind_november.tif", overwrite=TRUE)
#December
wind_december_u<-rast("Wind_U/december_u_component_wind_mean_rotated.tif")
wind_december_v<-rast("Wind_V/december_v_component_wind_mean_rotated.tif")
wind_december<-sqrt((wind_december_u*wind_december_u)+(wind_december_v*wind_december_v))
plot(wind_december)
writeRaster(wind_december,"wind_december.tif", overwrite=TRUE)
##Average, max and min
##Wind
wind_mean<-app(c(wind_january,wind_february,wind_march,wind_april,wind_may,wind_june,wind_july,
                 wind_august,wind_september,wind_october,wind_november,wind_december),fun=mean)
plot(wind_mean)
writeRaster(wind_mean,"wind_mean.tif", overwrite=TRUE)
wind_min<-app(c(wind_january,wind_february,wind_march,wind_april,wind_may,wind_june,wind_july,
                 wind_august,wind_september,wind_october,wind_november,wind_december),fun=min)
plot(wind_min)
writeRaster(wind_min,"wind_min.tif", overwrite=TRUE)
wind_max<-app(c(wind_january,wind_february,wind_march,wind_april,wind_may,wind_june,wind_july,
                 wind_august,wind_september,wind_october,wind_november,wind_december),fun=max)
plot(wind_max)
writeRaster(wind_max,"wind_max.tif",overwrite=TRUE)
##Temperature
temperature_january<-rast("temperature/january_temperature_mean_rotated.tif")
temperature_february<-rast("temperature/february_temperature_mean_rotated.tif")
temperature_march<-rast("temperature/march_temperature_mean_rotated.tif")
temperature_april<-rast("temperature/april_temperature_mean_rotated.tif")
temperature_may<-rast("temperature/may_temperature_mean_rotated.tif")
temperature_june<-rast("temperature/june_temperature_mean_rotated.tif")
temperature_july<-rast("temperature/july_temperature_mean_rotated.tif")
temperature_august<-rast("temperature/august_temperature_mean_rotated.tif")
temperature_september<-rast("temperature/september_temperature_mean_rotated.tif")
temperature_october<-rast("temperature/october_temperature_mean_rotated.tif")
temperature_november<-rast("temperature/november_temperature_mean_rotated.tif")
temperature_december<-rast("temperature/december_temperature_mean_rotated.tif")

temperature_mean<-app(c(temperature_january,temperature_february,temperature_march,temperature_april,temperature_may,temperature_june,temperature_july,
                 temperature_august,temperature_september,temperature_october,temperature_november,temperature_december),fun=mean)
plot(temperature_mean)
writeRaster(temperature_mean,"temperature_mean.tif", overwrite=TRUE)
temperature_min<-app(c(temperature_january,temperature_february,temperature_march,temperature_april,temperature_may,temperature_june,temperature_july,
                temperature_august,temperature_september,temperature_october,temperature_november,temperature_december),fun=min)
plot(temperature_min)
writeRaster(temperature_min,"temperature_min.tif", overwrite=TRUE)
temperature_max<-app(c(temperature_january,temperature_february,temperature_march,temperature_april,temperature_may,temperature_june,temperature_july,
                temperature_august,temperature_september,temperature_october,temperature_november,temperature_december),fun=max)
plot(temperature_max)
writeRaster(temperature_max,"temperature_max.tif",overwrite=TRUE)

##Humidity
humidity_january<-rast("humidity/january_humidity_mean_rotated.tif")
humidity_february<-rast("humidity/february_humidity_mean_rotated.tif")
humidity_march<-rast("humidity/march_humidity_mean_rotated.tif")
humidity_april<-rast("humidity/april_humidity_mean_rotated.tif")
humidity_may<-rast("humidity/may_humidity_mean_rotated.tif")
humidity_june<-rast("humidity/june_humidity_mean_rotated.tif")
humidity_july<-rast("humidity/july_humidity_mean_rotated.tif")
humidity_august<-rast("humidity/august_humidity_mean_rotated.tif")
humidity_september<-rast("humidity/september_humidity_mean_rotated.tif")
humidity_october<-rast("humidity/october_humidity_mean_rotated.tif")
humidity_november<-rast("humidity/november_humidity_mean_rotated.tif")
humidity_december<-rast("humidity/december_humidity_mean_rotated.tif")

humidity_mean<-app(c(humidity_january,humidity_february,humidity_march,humidity_april,humidity_may,humidity_june,humidity_july,
                        humidity_august,humidity_september,humidity_october,humidity_november,humidity_december),fun=mean)
plot(humidity_mean)
writeRaster(humidity_mean,"humidity_mean.tif", overwrite=TRUE)
humidity_min<-app(c(humidity_january,humidity_february,humidity_march,humidity_april,humidity_may,humidity_june,humidity_july,
                       humidity_august,humidity_september,humidity_october,humidity_november,humidity_december),fun=min)
plot(humidity_min)
writeRaster(humidity_min,"humidity_min.tif", overwrite=TRUE)
humidity_max<-app(c(humidity_january,humidity_february,humidity_march,humidity_april,humidity_may,humidity_june,humidity_july,
                       humidity_august,humidity_september,humidity_october,humidity_november,humidity_december),fun=max)
plot(humidity_max)
writeRaster(humidity_max,"humidity_max.tif", overwrite=TRUE)

##Radiation
radiation_january<-rast("radiation/january_radiation_mean_rotated.tif")
radiation_february<-rast("radiation/february_radiation_mean_rotated.tif")
radiation_march<-rast("radiation/march_radiation_mean_rotated.tif")
radiation_april<-rast("radiation/april_radiation_mean_rotated.tif")
radiation_may<-rast("radiation/may_radiation_mean_rotated.tif")
radiation_june<-rast("radiation/june_radiation_mean_rotated.tif")
radiation_july<-rast("radiation/july_radiation_mean_rotated.tif")
radiation_august<-rast("radiation/august_radiation_mean_rotated.tif")
radiation_september<-rast("radiation/september_radiation_mean_rotated.tif")
radiation_october<-rast("radiation/october_radiation_mean_rotated.tif")
radiation_november<-rast("radiation/november_radiation_mean_rotated.tif")
radiation_december<-rast("radiation/december_radiation_mean_rotated.tif")

radiation_mean<-app(c(radiation_january,radiation_february,radiation_march,radiation_april,radiation_may,radiation_june,radiation_july,
                        radiation_august,radiation_september,radiation_october,radiation_november,radiation_december),fun=mean)
plot(radiation_mean)
writeRaster(radiation_mean,"radiation_mean.tif", overwrite=TRUE)
radiation_min<-app(c(radiation_january,radiation_february,radiation_march,radiation_april,radiation_may,radiation_june,radiation_july,
                       radiation_august,radiation_september,radiation_october,radiation_november,radiation_december),fun=min)
plot(radiation_min)
writeRaster(radiation_min,"radiation_min.tif", overwrite=TRUE)
radiation_max<-app(c(radiation_january,radiation_february,radiation_march,radiation_april,radiation_may,radiation_june,radiation_july,
                       radiation_august,radiation_september,radiation_october,radiation_november,radiation_december),fun=max)
plot(radiation_max)
writeRaster(radiation_max,"radiation_max.tif", overwrite=TRUE)

##Total_cloud
total_cloud_january<-rast("total_cloud/january_total_cloud_cover_mean_rotated.tif")
total_cloud_february<-rast("total_cloud/february_total_cloud_cover_mean_rotated.tif")
total_cloud_march<-rast("total_cloud/march_total_cloud_cover_mean_rotated.tif")
total_cloud_april<-rast("total_cloud/april_total_cloud_cover_mean_rotated.tif")
total_cloud_may<-rast("total_cloud/may_total_cloud_cover_mean_rotated.tif")
total_cloud_june<-rast("total_cloud/june_total_cloud_cover_mean_rotated.tif")
total_cloud_july<-rast("total_cloud/july_total_cloud_cover_mean_rotated.tif")
total_cloud_august<-rast("total_cloud/august_total_cloud_cover_mean_rotated.tif")
total_cloud_september<-rast("total_cloud/september_total_cloud_cover_mean_rotated.tif")
total_cloud_october<-rast("total_cloud/october_total_cloud_cover_mean_rotated.tif")
total_cloud_november<-rast("total_cloud/november_total_cloud_cover_mean_rotated.tif")
total_cloud_december<-rast("total_cloud/december_total_cloud_cover_mean_rotated.tif")

total_cloud_mean<-app(c(total_cloud_january,total_cloud_february,total_cloud_march,total_cloud_april,total_cloud_may,total_cloud_june,total_cloud_july,
                        total_cloud_august,total_cloud_september,total_cloud_october,total_cloud_november,total_cloud_december),fun=mean)
plot(total_cloud_mean)
writeRaster(total_cloud_mean,"total_cloud_mean.tif", overwrite=TRUE)
total_cloud_min<-app(c(total_cloud_january,total_cloud_february,total_cloud_march,total_cloud_april,total_cloud_may,total_cloud_june,total_cloud_july,
                       total_cloud_august,total_cloud_september,total_cloud_october,total_cloud_november,total_cloud_december),fun=min)
plot(total_cloud_min)
writeRaster(total_cloud_min,"total_cloud_min.tif", overwrite=TRUE)
total_cloud_max<-app(c(total_cloud_january,total_cloud_february,total_cloud_march,total_cloud_april,total_cloud_may,total_cloud_june,total_cloud_july,
                       total_cloud_august,total_cloud_september,total_cloud_october,total_cloud_november,total_cloud_december),fun=max)
plot(total_cloud_max)
writeRaster(total_cloud_max,"total_cloud_max.tif", overwrite=TRUE)

##MRT
mrt_january<-rast("mrt/january_mrt_mean_rotated.tiff")
mrt_february<-rast("mrt/february_mrt_mean_rotated.tiff")
mrt_march<-rast("mrt/march_mrt_mean_rotated.tiff")
mrt_april<-rast("mrt/april_mrt_mean_rotated.tiff")
mrt_may<-rast("mrt/may_mrt_mean_rotated.tiff")
mrt_june<-rast("mrt/june_mrt_mean_rotated.tiff")
mrt_july<-rast("mrt/july_mrt_mean_rotated.tiff")
mrt_august<-rast("mrt/august_mrt_mean_rotated.tiff")
mrt_september<-rast("mrt/september_mrt_mean_rotated.tiff")
mrt_october<-rast("mrt/october_mrt_mean_rotated.tiff")
mrt_november<-rast("mrt/november_mrt_mean_rotated.tiff")
mrt_december<-rast("mrt/december_mrt_mean_rotated.tiff")

mrt_mean<-app(c(mrt_january,mrt_february,mrt_march,mrt_april,mrt_may,mrt_june,mrt_july,
                mrt_august,mrt_september,mrt_october,mrt_november,mrt_december),fun=mean)
plot(mrt_mean)
writeRaster(mrt_mean,"mrt_mean.tif", overwrite=TRUE)
mrt_min<-app(c(mrt_january,mrt_february,mrt_march,mrt_april,mrt_may,mrt_june,mrt_july,
               mrt_august,mrt_september,mrt_october,mrt_november,mrt_december),fun=min)
plot(mrt_min)
writeRaster(mrt_min,"mrt_min.tif", overwrite=TRUE)
mrt_max<-app(c(mrt_january,mrt_february,mrt_march,mrt_april,mrt_may,mrt_june,mrt_july,
               mrt_august,mrt_september,mrt_october,mrt_november,mrt_december),fun=max)
plot(mrt_max)
writeRaster(mrt_max,"mrt_max.tif", overwrite=TRUE)

##UTCI
utci_january<-rast("utci/january_utci_mean_rotated.tif")
utci_february<-rast("utci/february_utci_mean_rotated.tif")
utci_march<-rast("utci/march_utci_mean_rotated.tif")
utci_april<-rast("utci/april_utci_mean_rotated.tif")
utci_may<-rast("utci/may_utci_mean_rotated.tif")
utci_june<-rast("utci/june_utci_mean_rotated.tif")
utci_july<-rast("utci/july_utci_mean_rotated.tif")
utci_august<-rast("utci/august_utci_mean_rotated.tif")
utci_september<-rast("utci/september_utci_mean_rotated.tif")
utci_october<-rast("utci/october_utci_mean_rotated.tif")
utci_november<-rast("utci/november_utci_mean_rotated.tif")
utci_december<-rast("utci/december_utci_mean_rotated.tif")

utci_mean<-app(c(utci_january,utci_february,utci_march,utci_april,utci_may,utci_june,utci_july,
                 utci_august,utci_september,utci_october,utci_november,utci_december),fun=mean)
plot(utci_mean)
writeRaster(utci_mean,"utci_mean.tif", overwrite=TRUE)
utci_min<-app(c(utci_january,utci_february,utci_march,utci_april,utci_may,utci_june,utci_july,
                utci_august,utci_september,utci_october,utci_november,utci_december),fun=min)
plot(utci_min)
writeRaster(utci_min,"utci_min.tif", overwrite=TRUE)
utci_max<-app(c(utci_january,utci_february,utci_march,utci_april,utci_may,utci_june,utci_july,
                utci_august,utci_september,utci_october,utci_november,utci_december),fun=max)
plot(utci_max)
writeRaster(utci_max,"utci_max.tif", overwrite=TRUE)

