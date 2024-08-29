#modelo e predicao - regressao ordinal com efeitos aleatorios por pessoa (id)
#install.packages("mgcv")
library(mgcv)
#install.packages("DescTools")
library(DescTools)
#install.packages("visreg")
library(visreg)
setwd("C:/Projeto/final/csvs")

sf_results_all$country<-factor(sf_results_all$country)
sf_results_all$state_province<-factor(sf_results_all$state_province)
sf_results_all$city<-factor(sf_results_all$city)
sf_results_all$question_dummy <- ifelse(sf_results_all$question=="month_less_comfortable", -0.5, 0.5)

#Model for temperature
model_temperature_mgcv<-gam(temperature_pmv~s(temperature_month)+s(temperature_mean)+s(humidity_month, k=5)+s(humidity_mean)+s(radiation_month)+s(radiation_mean)+s(wind_month)+question_dummy+s(id,bs="re"),family=ocat(R=7),data=sf_results_all,method="REML")
summary(model_temperature_mgcv) 
AIC(model_temperature_mgcv)
gam.check(model_temperature_mgcv)
SomersDelta(x=model_temperature_mgcv$fitted.values,y=sf_results_all$temperature_pmv,conf.level = 0.95)
dev.off()
visreg(model_temperature_mgcv, xvar="temperature_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="temperature (celsius)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=-20,"Very cold")
text(y=-0.3,x=-20,"Cold")
text(y=1,x=-20,"Slightly cold")
text(y=2.5,x=-20,"Comfortable")
text(y=4,x=-20,"Slightly hot")
text(y=5,x=-20,"Hot")
text(y=6,x=-20,"Very hot")
visreg(model_temperature_mgcv, xvar="temperature_mean", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="mean annual temperature (celsius)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=-10,"Very cold")
text(y=-0.3,x=-10,"Cold")
text(y=1,x=-10,"Slightly cold")
text(y=2.5,x=-10,"Comfortable")
text(y=4,x=-10,"Slightly hot")
text(y=5,x=-10,"Hot")
text(y=6,x=-10,"Very hot")
visreg(model_temperature_mgcv, xvar="humidity_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="relative humidity (%)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=15,"Very cold")
text(y=-0.3,x=15,"Cold")
text(y=1,x=15, "  Slightly cold")
text(y=2.5,x=15,"  Comfortable")
text(y=4,x=15,"  Slightly hot")
text(y=5,x=15,"Hot")
text(y=6,x=15,"Very hot") 
visreg(model_temperature_mgcv, xvar="humidity_mean", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="mean annual relative humidity (%)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=25,"Very cold")
text(y=-0.3,x=25,"Cold")
text(y=1,x=25,"Slightly cold")
text(y=2.5,x=25,"Comfortable")
text(y=4,x=25,"Slightly hot")
text(y=5,x=25,"Hot")
text(y=6,x=25,"Very hot")
visreg(model_temperature_mgcv, xvar="radiation_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="surface solar radiation downwards (J/m2)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=2500000,"Very cold")
text(y=-0.3,x=2500000,"Cold")
text(y=1,x=2500000,"Slightly cold")
text(y=2.5,x=2500000,"Comfortable")
text(y=4,x=2500000,"Slightly hot")
text(y=5,x=2500000,"Hot")
text(y=6,x=2500000,"Very hot")
visreg(model_temperature_mgcv, xvar="radiation_mean", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="annual mean of surface solar radiation downwards (J/m2)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=8000000,"Very cold")
text(y=-0.3,x=8000000,"Cold")
text(y=1,x=8000000,"Slightly cold")
text(y=2.5,x=8000000,"Comfortable")
text(y=4,x=8000000,"Slightly hot")
text(y=5,x=8000000,"Hot")
text(y=6,x=8000000,"Very hot")
visreg(model_temperature_mgcv, xvar="wind_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="wind speed (m/s)")
abline(h=c(-1,0.45,1.63,3.51,4.51,5.49),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_temperature_mgcv)
text(y=-2,x=0.5,"                                                                                                                Very cold")
text(y=-0.3,x=0.5,"                                                                                                                Cold")
text(y=1,x=0.5,"                                                                                                                Slightly cold")
text(y=2.5,x=0.5,"                                                                                                                Comfortable")
text(y=4,x=0.5,"                                                                                                                Slightly hot")
text(y=5,x=0.5,"                                                                                                                Hot")
text(y=6,x=0.5,"                                                                                                                Very hot")

#Maps based on the models
#January
january_celsius_aggregated<-aggregate((temperature_january-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),january_celsius_aggregated)
january_humidity_aggregated<-resample(humidity_january,january_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,january_celsius_aggregated)
radiation_january_aggregated<-resample(radiation_january,january_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,january_celsius_aggregated)
wind_january_aggregated<-resample(wind_january,january_celsius_aggregated)
question_dummy_raster<-january_celsius_aggregated*0
id_raster<-january_celsius_aggregated*0
january_temperature_explanatory<-c(january_celsius_aggregated,temperature_mean_aggregated,january_humidity_aggregated,humidity_mean_aggregated,radiation_january_aggregated,radiation_mean_aggregated,wind_january_aggregated,question_dummy_raster,id_raster)
names(january_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
january_pmv_mgcv_temperature<-terra::predict(object=january_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(january_pmv_mgcv_temperature)

#February
february_celsius_aggregated<-aggregate((temperature_february-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),february_celsius_aggregated)
february_humidity_aggregated<-resample(humidity_february,february_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,february_celsius_aggregated)
radiation_february_aggregated<-resample(radiation_february,february_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,february_celsius_aggregated)
wind_february_aggregated<-resample(wind_february,february_celsius_aggregated)
question_dummy_raster<-february_celsius_aggregated*0
id_raster<-february_celsius_aggregated*0
february_temperature_explanatory<-c(february_celsius_aggregated,temperature_mean_aggregated,february_humidity_aggregated,humidity_mean_aggregated,radiation_february_aggregated,radiation_mean_aggregated,wind_february_aggregated,question_dummy_raster,id_raster)
names(february_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
february_pmv_mgcv_temperature<-terra::predict(object=february_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(february_pmv_mgcv_temperature)

#March
march_celsius_aggregated<-aggregate((temperature_march-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),march_celsius_aggregated)
march_humidity_aggregated<-resample(humidity_march,march_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,march_celsius_aggregated)
radiation_march_aggregated<-resample(radiation_march,march_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,march_celsius_aggregated)
wind_march_aggregated<-resample(wind_march,march_celsius_aggregated)
question_dummy_raster<-march_celsius_aggregated*0
id_raster<-march_celsius_aggregated*0
march_temperature_explanatory<-c(march_celsius_aggregated,temperature_mean_aggregated,march_humidity_aggregated,humidity_mean_aggregated,radiation_march_aggregated,radiation_mean_aggregated,wind_march_aggregated,question_dummy_raster,id_raster)
names(march_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
march_pmv_mgcv_temperature<-terra::predict(object=march_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(march_pmv_mgcv_temperature)

#April
april_celsius_aggregated<-aggregate((temperature_april-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),april_celsius_aggregated)
april_humidity_aggregated<-resample(humidity_april,april_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,april_celsius_aggregated)
radiation_april_aggregated<-resample(radiation_april,april_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,april_celsius_aggregated)
wind_april_aggregated<-resample(wind_april,april_celsius_aggregated)
question_dummy_raster<-april_celsius_aggregated*0
id_raster<-april_celsius_aggregated*0
april_temperature_explanatory<-c(april_celsius_aggregated,temperature_mean_aggregated,april_humidity_aggregated,humidity_mean_aggregated,radiation_april_aggregated,radiation_mean_aggregated,wind_april_aggregated,question_dummy_raster,id_raster)
names(april_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
april_pmv_mgcv_temperature<-terra::predict(object=april_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(april_pmv_mgcv_temperature)

#May
may_celsius_aggregated<-aggregate((temperature_may-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),may_celsius_aggregated)
may_humidity_aggregated<-resample(humidity_may,may_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,may_celsius_aggregated)
radiation_may_aggregated<-resample(radiation_may,may_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,may_celsius_aggregated)
wind_may_aggregated<-resample(wind_may,may_celsius_aggregated)
question_dummy_raster<-may_celsius_aggregated*0
id_raster<-may_celsius_aggregated*0
may_temperature_explanatory<-c(may_celsius_aggregated,temperature_mean_aggregated,may_humidity_aggregated,humidity_mean_aggregated,radiation_may_aggregated,radiation_mean_aggregated,wind_may_aggregated,question_dummy_raster,id_raster)
names(may_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
may_pmv_mgcv_temperature<-terra::predict(object=may_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(may_pmv_mgcv_temperature)

#June
june_celsius_aggregated<-aggregate((temperature_june-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),june_celsius_aggregated)
june_humidity_aggregated<-resample(humidity_june,june_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,june_celsius_aggregated)
radiation_june_aggregated<-resample(radiation_june,june_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,june_celsius_aggregated)
wind_june_aggregated<-resample(wind_june,june_celsius_aggregated)
question_dummy_raster<-june_celsius_aggregated*0
id_raster<-june_celsius_aggregated*0
june_temperature_explanatory<-c(june_celsius_aggregated,temperature_mean_aggregated,june_humidity_aggregated,humidity_mean_aggregated,radiation_june_aggregated,radiation_mean_aggregated,wind_june_aggregated,question_dummy_raster,id_raster)
names(june_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
june_pmv_mgcv_temperature<-terra::predict(object=june_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(june_pmv_mgcv_temperature)

#July
july_celsius_aggregated<-aggregate((temperature_july-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),july_celsius_aggregated)
july_humidity_aggregated<-resample(humidity_july,july_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,july_celsius_aggregated)
radiation_july_aggregated<-resample(radiation_july,july_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,july_celsius_aggregated)
wind_july_aggregated<-resample(wind_july,july_celsius_aggregated)
question_dummy_raster<-july_celsius_aggregated*0
id_raster<-july_celsius_aggregated*0
july_temperature_explanatory<-c(july_celsius_aggregated,temperature_mean_aggregated,july_humidity_aggregated,humidity_mean_aggregated,radiation_july_aggregated,radiation_mean_aggregated,wind_july_aggregated,question_dummy_raster,id_raster)
names(july_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
july_pmv_mgcv_temperature<-terra::predict(object=july_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(july_pmv_mgcv_temperature)

#August
august_celsius_aggregated<-aggregate((temperature_august-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),august_celsius_aggregated)
august_humidity_aggregated<-resample(humidity_august,august_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,august_celsius_aggregated)
radiation_august_aggregated<-resample(radiation_august,august_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,august_celsius_aggregated)
wind_august_aggregated<-resample(wind_august,august_celsius_aggregated)
question_dummy_raster<-august_celsius_aggregated*0
id_raster<-august_celsius_aggregated*0
august_temperature_explanatory<-c(august_celsius_aggregated,temperature_mean_aggregated,august_humidity_aggregated,humidity_mean_aggregated,radiation_august_aggregated,radiation_mean_aggregated,wind_august_aggregated,question_dummy_raster,id_raster)
names(august_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
august_pmv_mgcv_temperature<-terra::predict(object=august_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(august_pmv_mgcv_temperature)

#September
september_celsius_aggregated<-aggregate((temperature_september-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),september_celsius_aggregated)
september_humidity_aggregated<-resample(humidity_september,september_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,september_celsius_aggregated)
radiation_september_aggregated<-resample(radiation_september,september_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,september_celsius_aggregated)
wind_september_aggregated<-resample(wind_september,september_celsius_aggregated)
question_dummy_raster<-september_celsius_aggregated*0
id_raster<-september_celsius_aggregated*0
september_temperature_explanatory<-c(september_celsius_aggregated,temperature_mean_aggregated,september_humidity_aggregated,humidity_mean_aggregated,radiation_september_aggregated,radiation_mean_aggregated,wind_september_aggregated,question_dummy_raster,id_raster)
names(september_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
september_pmv_mgcv_temperature<-terra::predict(object=september_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(september_pmv_mgcv_temperature)

#October
october_celsius_aggregated<-aggregate((temperature_october-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),october_celsius_aggregated)
october_humidity_aggregated<-resample(humidity_october,october_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,october_celsius_aggregated)
radiation_october_aggregated<-resample(radiation_october,october_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,october_celsius_aggregated)
wind_october_aggregated<-resample(wind_october,october_celsius_aggregated)
question_dummy_raster<-october_celsius_aggregated*0
id_raster<-october_celsius_aggregated*0
october_temperature_explanatory<-c(october_celsius_aggregated,temperature_mean_aggregated,october_humidity_aggregated,humidity_mean_aggregated,radiation_october_aggregated,radiation_mean_aggregated,wind_october_aggregated,question_dummy_raster,id_raster)
names(october_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
october_pmv_mgcv_temperature<-terra::predict(object=october_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(october_pmv_mgcv_temperature)

#November
november_celsius_aggregated<-aggregate((temperature_november-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),november_celsius_aggregated)
november_humidity_aggregated<-resample(humidity_november,november_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,november_celsius_aggregated)
radiation_november_aggregated<-resample(radiation_november,november_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,november_celsius_aggregated)
wind_november_aggregated<-resample(wind_november,november_celsius_aggregated)
question_dummy_raster<-november_celsius_aggregated*0
id_raster<-november_celsius_aggregated*0
november_temperature_explanatory<-c(november_celsius_aggregated,temperature_mean_aggregated,november_humidity_aggregated,humidity_mean_aggregated,radiation_november_aggregated,radiation_mean_aggregated,wind_november_aggregated,question_dummy_raster,id_raster)
names(november_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
november_pmv_mgcv_temperature<-terra::predict(object=november_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(november_pmv_mgcv_temperature)

#December
december_celsius_aggregated<-aggregate((temperature_december-273.15),fact=3)  #increase pixel size, to allow processing
temperature_mean_aggregated<-resample((temperature_mean-273.15),december_celsius_aggregated)
december_humidity_aggregated<-resample(humidity_december,december_celsius_aggregated)
humidity_mean_aggregated<-resample(humidity_mean,december_celsius_aggregated)
radiation_december_aggregated<-resample(radiation_december,december_celsius_aggregated)
radiation_mean_aggregated<-resample(radiation_mean,december_celsius_aggregated)
wind_december_aggregated<-resample(wind_december,december_celsius_aggregated)
question_dummy_raster<-december_celsius_aggregated*0
id_raster<-december_celsius_aggregated*0
december_temperature_explanatory<-c(december_celsius_aggregated,temperature_mean_aggregated,december_humidity_aggregated,humidity_mean_aggregated,radiation_december_aggregated,radiation_mean_aggregated,wind_december_aggregated,question_dummy_raster,id_raster)
names(december_temperature_explanatory)<-c("temperature_month","temperature_mean","humidity_month","humidity_mean","radiation_month","radiation_mean","wind_month","question_dummy","id")
december_pmv_mgcv_temperature<-terra::predict(object=december_temperature_explanatory,model=model_temperature_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(december_pmv_mgcv_temperature)

#Joint all months
pmv_temperature_mgcv_all<-c(january_pmv_mgcv_temperature,february_pmv_mgcv_temperature,march_pmv_mgcv_temperature,april_pmv_mgcv_temperature,may_pmv_mgcv_temperature,june_pmv_mgcv_temperature,july_pmv_mgcv_temperature,august_pmv_mgcv_temperature,september_pmv_mgcv_temperature,october_pmv_mgcv_temperature,november_pmv_mgcv_temperature,december_pmv_mgcv_temperature)
pmv_mgcv_mean_temperature<-app(pmv_temperature_mgcv_all,fun=mean)
#install.packages("colorRamps")
library(colorRamps)
col5 <- colorRampPalette(c('blue', 'gray96', 'red'))  #create color ramp starting from blue to red
color_levels=6 #the number of colors to use
global(pmv_mgcv_mean_temperature, fun="range")
#install.packages("khroma")
library(khroma)
sunset <- color("sunset")
plot(pmv_mgcv_mean_temperature, col=sunset(100),background="grey55")
pmv_mgcv_mean_temperature
pmv_mgcv_mean_temperature_classes<-classify(x=pmv_mgcv_mean_temperature, rcl=c(-50,-1,0.45,1.63,3.51,4.51,5.49),include.lowest=TRUE)
plot(pmv_mgcv_mean_temperature_classes,type="classes",plg=list(legend=c("Very cold","Cold","Slightly cold","Comfortable","Slightly hot","Hot")),col=sunset(6),background="grey55",legend="left") #Classes: 0 = very cold, 1 = cold,  2= slightly cold, 3 = comfortable, 4 = slightly hot, 5= hot, 6 = very hot
sbar(d=2000,xy=c(-165,-65),divs=2,type="bar",below="km",ticks=T,cex=.6)
north(xy=c(-160,-45),type=2,cex=.7,label=NULL)

#PPD - Percentage Predicted Dissatisfied
plot(january_pmv_mgcv_temperature)
january_mgcv_temperature_clamp<-clamp(january_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
plot(january_mgcv_temperature_clamp)
january_mgcv_temperature_classes<-classify(x=january_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
plot(january_mgcv_temperature_classes)
m <- rbind(c(0,-3),c(1,-2), c(2, -1),c(3,0),c(4,1),c(5,2),c(6,3))
january_pmv_mgcv_temperature_scale<-classify(x=january_mgcv_temperature_classes,rcl=m)  
plot(january_pmv_mgcv_temperature_scale,type="classes",plg=list(legend=c("very cold","cold","slightly cold","comfortable","slightly hot","hot","very hot")))
january_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(january_pmv_mgcv_temperature_scale^4))+(0.2179*(january_pmv_mgcv_temperature_scale^2)))))
plot(january_ppd_mgcv_temperature)
february_mgcv_temperature_clamp<-clamp(february_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
february_mgcv_temperature_classes<-classify(x=february_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
february_pmv_mgcv_temperaturescale<-classify(x=february_mgcv_temperature_classes,rcl=m)
february_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(february_pmv_mgcv_temperaturescale^4))+(0.2179*(february_pmv_mgcv_temperaturescale^2)))))
march_mgcv_temperature_clamp<-clamp(march_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
march_mgcv_temperature_classes<-classify(x=march_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
march_pmv_mgcv_temperaturescale<-classify(x=march_mgcv_temperature_classes,rcl=m)
march_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(march_pmv_mgcv_temperaturescale^4))+(0.2179*(march_pmv_mgcv_temperaturescale^2)))))
april_mgcv_temperature_clamp<-clamp(april_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
april_mgcv_temperature_classes<-classify(x=april_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
april_pmv_mgcv_temperaturescale<-classify(x=april_mgcv_temperature_classes,rcl=m)
april_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(april_pmv_mgcv_temperaturescale^4))+(0.2179*(april_pmv_mgcv_temperaturescale^2)))))
may_mgcv_temperature_clamp<-clamp(may_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
may_mgcv_temperature_classes<-classify(x=may_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
may_pmv_mgcv_temperaturescale<-classify(x=may_mgcv_temperature_classes,rcl=m)
may_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(may_pmv_mgcv_temperaturescale^4))+(0.2179*(may_pmv_mgcv_temperaturescale^2)))))
june_mgcv_temperature_clamp<-clamp(june_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
june_mgcv_temperature_classes<-classify(x=june_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
june_pmv_mgcv_temperaturescale<-classify(x=june_mgcv_temperature_classes,rcl=m)
june_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(june_pmv_mgcv_temperaturescale^4))+(0.2179*(june_pmv_mgcv_temperaturescale^2)))))
july_mgcv_temperature_clamp<-clamp(july_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
july_mgcv_temperature_classes<-classify(x=july_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
july_pmv_mgcv_temperaturescale<-classify(x=july_mgcv_temperature_classes,rcl=m)
july_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(july_pmv_mgcv_temperaturescale^4))+(0.2179*(july_pmv_mgcv_temperaturescale^2)))))
august_mgcv_temperature_clamp<-clamp(august_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
august_mgcv_temperature_classes<-classify(x=august_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
august_pmv_mgcv_temperaturescale<-classify(x=august_mgcv_temperature_classes,rcl=m)
august_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(august_pmv_mgcv_temperaturescale^4))+(0.2179*(august_pmv_mgcv_temperaturescale^2)))))
september_mgcv_temperature_clamp<-clamp(september_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
september_mgcv_temperature_classes<-classify(x=september_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
september_pmv_mgcv_temperaturescale<-classify(x=september_mgcv_temperature_classes,rcl=m)
september_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(september_pmv_mgcv_temperaturescale^4))+(0.2179*(september_pmv_mgcv_temperaturescale^2)))))
october_mgcv_temperature_clamp<-clamp(october_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
october_mgcv_temperature_classes<-classify(x=october_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
october_pmv_mgcv_temperaturescale<-classify(x=october_mgcv_temperature_classes,rcl=m)
october_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(october_pmv_mgcv_temperaturescale^4))+(0.2179*(october_pmv_mgcv_temperaturescale^2)))))
november_mgcv_temperature_clamp<-clamp(november_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
november_mgcv_temperature_classes<-classify(x=november_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
november_pmv_mgcv_temperaturescale<-classify(x=november_mgcv_temperature_classes,rcl=m)
november_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(november_pmv_mgcv_temperaturescale^4))+(0.2179*(november_pmv_mgcv_temperaturescale^2)))))
december_mgcv_temperature_clamp<-clamp(december_pmv_mgcv_temperature,lower=-2, upper=6, values=TRUE)
december_mgcv_temperature_classes<-classify(x=december_mgcv_temperature_clamp, rcl=c(-2,-1,0.45,1.63,3.51,4.51,5.49,6),include.lowest=TRUE)
december_pmv_mgcv_temperaturescale<-classify(x=december_mgcv_temperature_classes,rcl=m)
december_ppd_mgcv_temperature<-100-(95*exp(-((0.03353*(december_pmv_mgcv_temperaturescale^4))+(0.2179*(december_pmv_mgcv_temperaturescale^2)))))


ppd_mgcv_temperature_todos<-c(january_ppd_mgcv_temperature,february_ppd_mgcv_temperature,march_ppd_mgcv_temperature,april_ppd_mgcv_temperature,may_ppd_mgcv_temperature,june_ppd_mgcv_temperature,july_ppd_mgcv_temperature,august_ppd_mgcv_temperature,september_ppd_mgcv_temperature,october_ppd_mgcv_temperature,november_ppd_mgcv_temperature,december_ppd_mgcv_temperature)
ppd_mgcv_temperature_mean<-app(ppd_mgcv_temperature_todos,fun=mean)
plot(ppd_mgcv_temperature_mean)
writeRaster(ppd_mgcv_temperature_mean,"ppd_mgcv_temperature_mean.tif", overwrite=TRUE)

##Model for temperature with MRT and without radiation
#model_temperature_mgcv<-gam(temperature_pmv~s(temperature_month)+s(temperature_mean)+s(humidity_month, k=5)+s(humidity_mean)+s(mrt_month)+s(mrt_mean)+s(wind_month)+question_dummy+s(id,bs="re"),family=ocat(R=7),data=sf_results_all,method="REML")
#summary(model_temperature_mgcv) 
#AIC(model_temperature_mgcv)
#gam.check(model_temperature_mgcv)
#SomersDelta(x=model_temperature_mgcv$fitted.values,y=sf_results_all$temperature_pmv,conf.level = 0.95)
#dev.off()

##Model for temperature with “wind_mean” 
#model_temperature_mgcv<-gam(temperature_pmv~s(temperature_month)+s(temperature_mean)+s(humidity_month, k=5)+s(humidity_mean)+s(radiation_month)+s(radiation_mean)+s(wind_month)+s(wind_mean)+question_dummy+s(id,bs="re"),family=ocat(R=7),data=sf_results_all,method="REML")
#summary(model_temperature_mgcv) 
#AIC(model_temperature_mgcv)
#gam.check(model_temperature_mgcv)
#SomersDelta(x=model_temperature_mgcv$fitted.values,y=sf_results_all$temperature_pmv,conf.level = 0.95)
