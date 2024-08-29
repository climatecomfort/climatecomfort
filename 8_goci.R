#We calculate GOCI index based on :
#GOLASI, Iacopo et al. Complying with the demand of standardization in outdoor thermal comfort: a first approach to the Global Outdoor Comfort Index (GOCI). Building and Environment, v. 130, p. 104-119, 2018.
#We used the data from ERA5 Reanalysis from 1992 to 2021.
#Since GOCI is equivalent to PMV scale, PPD was calculated for each month, using Fanger (1970) formula, in:
#FANGER, P. O. Thermal Comfort. Danish Technical Press, Copenhagen, 1970. Republished by McGraw-Hill Book Co, New York,1973. 
#After that, PPD for each month was averaged as annual PPD
#Finally, the Pearson correlation of annual PPD of GOci and for the model calculated in this research was calculated.


# Raster of latitude
model_nrow<-nrow(temperature_january)
model_ncol<-ncol(temperature_january)
x <- rast(nrow=model_nrow, ncol=model_ncol)
lat <- init(x, 'y')
crs(lat)<-crs(temperature_january)
module<-function(i) abs(i)
lat<-app(lat, module)
plot(lat)

#Calculate mean goci for monthly data
mrt_january<-resample(mrt_january, temperature_january)
humidity_january<-resample(humidity_january, temperature_january)
wind_january<-resample(wind_january, temperature_january)
temperature_mean<-resample(temperature_mean, temperature_january)
temperature_max<-resample(temperature_max, temperature_january)
temperature_min<-resample(temperature_min, temperature_january)
lat<-resample(lat, temperature_january)

goci_january <- ((0.053*(mrt_january - 273.15))+(0.084 * (temperature_january- 273.15)) + (0.006 * humidity_january) + (-0.229 * wind_january) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_january)

mrt_february<-resample(mrt_february, temperature_february)
humidity_february<-resample(humidity_february, temperature_february)
wind_february<-resample(wind_february, temperature_february)
goci_february <- ((0.053*(mrt_february - 273.15))+(0.084 * (temperature_february- 273.15)) + (0.006 * humidity_february) + (-0.229 * wind_february) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_february)

mrt_march<-resample(mrt_march, temperature_march)
humidity_march<-resample(humidity_march, temperature_march)
wind_march<-resample(wind_march, temperature_march)
goci_march <- ((0.053*(mrt_march - 273.15))+(0.084 * (temperature_march- 273.15)) + (0.006 * humidity_march) + (-0.229 * wind_march) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_march)

mrt_april<-resample(mrt_april, temperature_april)
humidity_april<-resample(humidity_april, temperature_april)
wind_april<-resample(wind_april, temperature_april)
goci_april <- ((0.053*(mrt_april - 273.15))+(0.084 * (temperature_april- 273.15)) + (0.006 * humidity_april) + (-0.229 * wind_april) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_april)

mrt_may<-resample(mrt_may, temperature_may)
humidity_may<-resample(humidity_may, temperature_may)
wind_may<-resample(wind_may, temperature_may)
goci_may <- ((0.053*(mrt_may - 273.15))+(0.084 * (temperature_may- 273.15)) + (0.006 * humidity_may) + (-0.229 * wind_may) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_may)

mrt_june<-resample(mrt_june, temperature_june)
humidity_june<-resample(humidity_june, temperature_june)
wind_june<-resample(wind_june, temperature_june)
goci_june <- ((0.053*(mrt_june - 273.15))+(0.084 * (temperature_june- 273.15)) + (0.006 * humidity_june) + (-0.229 * wind_june) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_june)

mrt_july<-resample(mrt_july, temperature_july)
humidity_july<-resample(humidity_july, temperature_july)
wind_july<-resample(wind_july, temperature_july)
goci_july <- ((0.053*(mrt_july - 273.15))+(0.084 * (temperature_july- 273.15)) + (0.006 * humidity_july) + (-0.229 * wind_july) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_july)

mrt_august<-resample(mrt_august, temperature_august)
humidity_august<-resample(humidity_august, temperature_august)
wind_august<-resample(wind_august, temperature_august)
goci_august <- ((0.053*(mrt_august - 273.15))+(0.084 * (temperature_august- 273.15)) + (0.006 * humidity_august) + (-0.229 * wind_august) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_august)

mrt_september<-resample(mrt_september, temperature_september)
humidity_september<-resample(humidity_september, temperature_september)
wind_september<-resample(wind_september, temperature_september)
goci_september <- ((0.053*(mrt_september - 273.15))+(0.084 * (temperature_september- 273.15)) + (0.006 * humidity_september) + (-0.229 * wind_september) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_september)

mrt_october<-resample(mrt_october, temperature_october)
humidity_october<-resample(humidity_october, temperature_october)
wind_october<-resample(wind_october, temperature_october)
goci_october <- ((0.053*(mrt_october - 273.15))+(0.084 * (temperature_october- 273.15)) + (0.006 * humidity_october) + (-0.229 * wind_october) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_october)

mrt_november<-resample(mrt_november, temperature_november)
humidity_november<-resample(humidity_november, temperature_november)
wind_november<-resample(wind_november, temperature_november)
goci_november <- ((0.053*(mrt_november - 273.15))+(0.084 * (temperature_november- 273.15)) + (0.006 * humidity_november) + (-0.229 * wind_november) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_november)

mrt_december<-resample(mrt_december, temperature_december)
humidity_december<-resample(humidity_december, temperature_december)
wind_december<-resample(wind_december, temperature_december)
goci_december <- ((0.053*(mrt_december - 273.15))+(0.084 * (temperature_december- 273.15)) + (0.006 * humidity_december) + (-0.229 * wind_december) - (0.056 * (temperature_mean-273.15)) - (0.026 * (temperature_max - 273.15)) - (0.042 * (temperature_min - 273.15)) + (0.009 * lat)) - 0.908
plot(goci_december)


#convert from GOCI (PMV scale) to PPD (percentage predicted dissatisfied)
january_ppd_goci<-100-(95*exp(-((0.03353*(goci_january^4))+(0.2179*(goci_january^2)))))
plot(january_ppd_goci)

february_ppd_goci<-100-(95*exp(-((0.03353*(goci_february^4))+(0.2179*(goci_february^2)))))
plot(february_ppd_goci)

march_ppd_goci<-100-(95*exp(-((0.03353*(goci_march^4))+(0.2179*(goci_march^2)))))
plot(march_ppd_goci)

april_ppd_goci<-100-(95*exp(-((0.03353*(goci_april^4))+(0.2179*(goci_april^2)))))
plot(april_ppd_goci)

may_ppd_goci<-100-(95*exp(-((0.03353*(goci_may^4))+(0.2179*(goci_may^2)))))
plot(may_ppd_goci)

june_ppd_goci<-100-(95*exp(-((0.03353*(goci_june^4))+(0.2179*(goci_june^2)))))
plot(june_ppd_goci)

july_ppd_goci<-100-(95*exp(-((0.03353*(goci_july^4))+(0.2179*(goci_july^2)))))
plot(july_ppd_goci)

august_ppd_goci<-100-(95*exp(-((0.03353*(goci_august^4))+(0.2179*(goci_august^2)))))
plot(august_ppd_goci)

september_ppd_goci<-100-(95*exp(-((0.03353*(goci_september^4))+(0.2179*(goci_september^2)))))
plot(september_ppd_goci)

october_ppd_goci<-100-(95*exp(-((0.03353*(goci_october^4))+(0.2179*(goci_october^2)))))
plot(october_ppd_goci)

november_ppd_goci<-100-(95*exp(-((0.03353*(goci_november^4))+(0.2179*(goci_november^2)))))
plot(november_ppd_goci)

december_ppd_goci<-100-(95*exp(-((0.03353*(goci_december^4))+(0.2179*(goci_december^2)))))
plot(december_ppd_goci)

ppd_goci<-app(c(january_ppd_goci,february_ppd_goci,march_ppd_goci,april_ppd_goci,may_ppd_goci,june_ppd_goci,july_ppd_goci,
                august_ppd_goci,september_ppd_goci,october_ppd_goci,november_ppd_goci,december_ppd_goci),fun=mean)
plot(ppd_goci)

#install.packages("krhoma")
library(khroma)
YlOrBr <- color("YlOrBr")
plot(ppd_goci, col=YlOrBr(100),background="grey55")
sbar(d=2000,xy=c(-165,-65),divs=2,type="bar",below="km",ticks=T,cex=.6)
north(xy=c(-160,-45),type=2,cex=.7,label=NULL)

#Compare with annual average of PPD from temperature model of MGCV
ppd_temperature_mgcv<-(rast("ppd_mgcv_temperature_mean.tif"))
plot(ppd_temperature_mgcv)
ppd_temperature_mgcv_resampled<-resample(ppd_temperature_mgcv, ppd_goci)
plot(ppd_temperature_mgcv_resampled, col=YlOrBr(100),background="grey55")
sbar(d=2000,xy=c(-165,-65),divs=2,type="bar",below="km",ticks=T,cex=.6)
north(xy=c(-160,-45),type=2,cex=.7,label=NULL)

#Correlation of PPD from GOCI and MGCV model for temperature
ppd_goci_mgcv<-c(ppd_temperature_mgcv_resampled,ppd_goci)
names(ppd_goci_mgcv)<-c("mgcv","goci")
plot(ppd_goci_mgcv)
layerCor(ppd_goci_mgcv, "pearson",na.rm = TRUE) # Pearson correlation of 0.72
summary(ppd_temperature_mgcv, na.rm=TRUE) # mean is 64.32
summary(ppd_goci, na.rm=TRUE) # mean is 38.50
  