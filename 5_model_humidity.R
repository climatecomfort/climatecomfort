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
sf_results_all$humidity_pmv<-as.integer(sf_results_all$humidity)

#Model for humidity
model_humidity_mgcv<-gam(humidity_pmv~s(humidity_month)+s(humidity_mean,k=4)+s(temperature_month,k=4)+question_dummy+s(id,bs="re"),family=ocat(R=7),data=sf_results_all,method="REML")
summary(model_humidity_mgcv) 
AIC(model_humidity_mgcv)
gam.check(model_humidity_mgcv)
SomersDelta(x=model_humidity_mgcv$fitted.values,y=sf_results_all$humidity_pmv,conf.level = 0.95)
dev.off()
visreg(model_humidity_mgcv, xvar="humidity_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="relative humidity (%)")
abline(h=c(-1,0.1,0.88,1.92,2.87,4.03),col="red",lty=2) 
text(y=-3,x=20,"Very dry")
text(y=-0.3,x=20,"Dry")
text(y=0.5,x=20,"Slightly dry")
text(y=1.5,x=20,"Comfortable")
text(y=2.5,x=20,"Slightly humid")
text(y=3.5,x=20,"Humid")
text(y=4.5,x=20,"Very humid")
visreg(model_humidity_mgcv, xvar="humidity_mean", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="mean annual relative humidity (%)")
abline(h=c(-1,0.1,0.88,1.92,2.87,4.03),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_humidity_mgcv)
text(y=-3,x=25,"Very dry")
text(y=-0.3,x=25,"Dry")
text(y=0.5,x=25,"Slightly dry")
text(y=1.5,x=25,"Comfortable")
text(y=2.5,x=25,"Slightly humid")
text(y=3.5,x=25,"Humid")
text(y=4.5,x=25,"Very humid")
visreg(model_humidity_mgcv, xvar="temperature_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="temperature (Celsius)")
abline(h=c(-1,0.1,0.88,1.92,2.87,4.03),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_humidity_mgcv)
text(y=-3,x=-20,"Very dry")
text(y=-0.3,x=-20,"Dry")
text(y=0.5,x=-20,"Slightly dry")
text(y=1.5,x=-20,"Comfortable")
text(y=2.5,x=-20,"Slightly humid")
text(y=3.5,x=-20,"Humid")
text(y=4.5,x=-20,"Very humid")

#Maps based on the models
#January
january_humidity_aggregated<-aggregate(humidity_january,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,january_humidity_aggregated)
january_temperature_aggregated<-resample((temperature_january-273.15),january_humidity_aggregated)
id_raster<-january_humidity_aggregated*0
question_dummy_raster<-january_humidity_aggregated*0
january_humidity_explanatory<-c(january_humidity_aggregated,humidity_mean_aggregated,january_temperature_aggregated,question_dummy_raster,id_raster)
names(january_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
january_pmv_mgcv_humidity<-terra::predict(object=january_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(january_pmv_mgcv_humidity)

#February
february_humidity_aggregated<-aggregate(humidity_february,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,february_humidity_aggregated)
february_temperature_aggregated<-resample((temperature_february-273.15),february_humidity_aggregated)
id_raster<-february_humidity_aggregated*0
question_dummy_raster<-february_humidity_aggregated*0
february_humidity_explanatory<-c(february_humidity_aggregated,humidity_mean_aggregated,february_temperature_aggregated,question_dummy_raster,id_raster)
names(february_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
february_pmv_mgcv_humidity<-terra::predict(object=february_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(february_pmv_mgcv_humidity)

#March
march_humidity_aggregated<-aggregate(humidity_march,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,march_humidity_aggregated)
march_temperature_aggregated<-resample((temperature_march-273.15),march_humidity_aggregated)
id_raster<-march_humidity_aggregated*0
question_dummy_raster<-march_humidity_aggregated*0
march_humidity_explanatory<-c(march_humidity_aggregated,humidity_mean_aggregated,march_temperature_aggregated,question_dummy_raster,id_raster)
names(march_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
march_pmv_mgcv_humidity<-terra::predict(object=march_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(march_pmv_mgcv_humidity)

#April
april_humidity_aggregated<-aggregate(humidity_april,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,april_humidity_aggregated)
april_temperature_aggregated<-resample((temperature_april-273.15),april_humidity_aggregated)
id_raster<-april_humidity_aggregated*0
question_dummy_raster<-april_humidity_aggregated*0
april_humidity_explanatory<-c(april_humidity_aggregated,humidity_mean_aggregated,april_temperature_aggregated,question_dummy_raster,id_raster)
names(april_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
april_pmv_mgcv_humidity<-terra::predict(object=april_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(april_pmv_mgcv_humidity)

#May
may_humidity_aggregated<-aggregate(humidity_may,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,may_humidity_aggregated)
may_temperature_aggregated<-resample((temperature_may-273.15),may_humidity_aggregated)
id_raster<-may_humidity_aggregated*0
question_dummy_raster<-may_humidity_aggregated*0
may_humidity_explanatory<-c(may_humidity_aggregated,humidity_mean_aggregated,may_temperature_aggregated,question_dummy_raster,id_raster)
names(may_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
may_pmv_mgcv_humidity<-terra::predict(object=may_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(may_pmv_mgcv_humidity)

#June
june_humidity_aggregated<-aggregate(humidity_june,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,june_humidity_aggregated)
june_temperature_aggregated<-resample((temperature_june-273.15),june_humidity_aggregated)
id_raster<-june_humidity_aggregated*0
question_dummy_raster<-june_humidity_aggregated*0
june_humidity_explanatory<-c(june_humidity_aggregated,humidity_mean_aggregated,june_temperature_aggregated,question_dummy_raster,id_raster)
names(june_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
june_pmv_mgcv_humidity<-terra::predict(object=june_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(june_pmv_mgcv_humidity)

#July
july_humidity_aggregated<-aggregate(humidity_july,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,july_humidity_aggregated)
july_temperature_aggregated<-resample((temperature_july-273.15),july_humidity_aggregated)
id_raster<-july_humidity_aggregated*0
question_dummy_raster<-july_humidity_aggregated*0
july_humidity_explanatory<-c(july_humidity_aggregated,humidity_mean_aggregated,july_temperature_aggregated,question_dummy_raster,id_raster)
names(july_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
july_pmv_mgcv_humidity<-terra::predict(object=july_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(july_pmv_mgcv_humidity)

#August
august_humidity_aggregated<-aggregate(humidity_august,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,august_humidity_aggregated)
august_temperature_aggregated<-resample((temperature_august-273.15),august_humidity_aggregated)
id_raster<-august_humidity_aggregated*0
question_dummy_raster<-august_humidity_aggregated*0
august_humidity_explanatory<-c(august_humidity_aggregated,humidity_mean_aggregated,august_temperature_aggregated,question_dummy_raster,id_raster)
names(august_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
august_pmv_mgcv_humidity<-terra::predict(object=august_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(august_pmv_mgcv_humidity)

#September
september_humidity_aggregated<-aggregate(humidity_september,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,september_humidity_aggregated)
september_temperature_aggregated<-resample((temperature_september-273.15),september_humidity_aggregated)
id_raster<-september_humidity_aggregated*0
question_dummy_raster<-september_humidity_aggregated*0
september_humidity_explanatory<-c(september_humidity_aggregated,humidity_mean_aggregated,september_temperature_aggregated,question_dummy_raster,id_raster)
names(september_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
september_pmv_mgcv_humidity<-terra::predict(object=september_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(september_pmv_mgcv_humidity)

#October
october_humidity_aggregated<-aggregate(humidity_october,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,october_humidity_aggregated)
october_temperature_aggregated<-resample((temperature_october-273.15),october_humidity_aggregated)
id_raster<-october_humidity_aggregated*0
question_dummy_raster<-october_humidity_aggregated*0
october_humidity_explanatory<-c(october_humidity_aggregated,humidity_mean_aggregated,october_temperature_aggregated,question_dummy_raster,id_raster)
names(october_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
october_pmv_mgcv_humidity<-terra::predict(object=october_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(october_pmv_mgcv_humidity)

#November
november_humidity_aggregated<-aggregate(humidity_november,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,november_humidity_aggregated)
november_temperature_aggregated<-resample((temperature_november-273.15),november_humidity_aggregated)
id_raster<-november_humidity_aggregated*0
question_dummy_raster<-november_humidity_aggregated*0
november_humidity_explanatory<-c(november_humidity_aggregated,humidity_mean_aggregated,november_temperature_aggregated,question_dummy_raster,id_raster)
names(november_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
november_pmv_mgcv_humidity<-terra::predict(object=november_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(november_pmv_mgcv_humidity)

#December
december_humidity_aggregated<-aggregate(humidity_december,fact=3)  #increase pixel size, to allow processing
humidity_mean_aggregated<-resample(humidity_mean,december_humidity_aggregated)
december_temperature_aggregated<-resample((temperature_december-273.15),december_humidity_aggregated)
id_raster<-december_humidity_aggregated*0
question_dummy_raster<-december_humidity_aggregated*0
december_humidity_explanatory<-c(december_humidity_aggregated,humidity_mean_aggregated,december_temperature_aggregated,question_dummy_raster,id_raster)
names(december_humidity_explanatory)<-c("humidity_month","humidity_mean","temperature_month","question_dummy","id")
december_pmv_mgcv_humidity<-terra::predict(object=december_humidity_explanatory,model=model_humidity_mgcv,na.rm=TRUE,exclude="s(id)") #creates raster based on the model
plot(december_pmv_mgcv_humidity)

#Joint all months
pmv_humidity_mgcv_all<-c(january_pmv_mgcv_humidity,february_pmv_mgcv_humidity,march_pmv_mgcv_humidity,april_pmv_mgcv_humidity,may_pmv_mgcv_humidity,june_pmv_mgcv_humidity,july_pmv_mgcv_humidity,august_pmv_mgcv_humidity,september_pmv_mgcv_humidity,october_pmv_mgcv_humidity,november_pmv_mgcv_humidity,december_pmv_mgcv_humidity)
pmv_mgcv_mean_humidity<-app(pmv_humidity_mgcv_all,fun=mean)
plot(pmv_mgcv_mean_humidity)
pmv_mgcv_mean_humidity_classes<-classify(x=pmv_mgcv_mean_humidity, rcl=c(-50,-1,0.1,0.88,1.92,2.87,4.03),include.lowest=TRUE)
plot(pmv_mgcv_mean_humidity_classes,type="classes",plg=list(legend=c("Very dry","Dry","Slightly dry","Comfortable","Slightly humid"))) #Classes: 0=very dry, 1=dry,  2=slightly dry, 3=comfortable, 4=slightly humid


#PPD - Percentage Predicted Dissatisfied
plot(january_pmv_mgcv_humidity)
january_mgcv_humidity_clamp<-clamp(january_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
plot(january_mgcv_humidity_clamp)
january_mgcv_humidity_classes<-classify(x=january_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
plot(january_mgcv_humidity_classes)
m <- rbind(c(0,-3),c(1,-2), c(2, -1),c(3,0),c(4,1),c(5,2),c(6,3))
january_pmv_mgcv_humidity_scale<-classify(x=january_mgcv_humidity_classes,rcl=m)  
plot(january_pmv_mgcv_humidity_scale,type="classes",plg=list(legend=c("very dry","dry","slightly dry","comfortable","slightly humid","humid")))
january_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(january_pmv_mgcv_humidity_scale^4))+(0.2179*(january_pmv_mgcv_humidity_scale^2)))))
plot(january_ppd_mgcv_humidity)
february_mgcv_humidity_clamp<-clamp(february_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
february_mgcv_humidity_classes<-classify(x=february_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
february_pmv_mgcv_humidityscale<-classify(x=february_mgcv_humidity_classes,rcl=m)
february_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(february_pmv_mgcv_humidityscale^4))+(0.2179*(february_pmv_mgcv_humidityscale^2)))))
march_mgcv_humidity_clamp<-clamp(march_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
march_mgcv_humidity_classes<-classify(x=march_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
march_pmv_mgcv_humidityscale<-classify(x=march_mgcv_humidity_classes,rcl=m)
march_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(march_pmv_mgcv_humidityscale^4))+(0.2179*(march_pmv_mgcv_humidityscale^2)))))
april_mgcv_humidity_clamp<-clamp(april_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
april_mgcv_humidity_classes<-classify(x=april_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
april_pmv_mgcv_humidityscale<-classify(x=april_mgcv_humidity_classes,rcl=m)
april_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(april_pmv_mgcv_humidityscale^4))+(0.2179*(april_pmv_mgcv_humidityscale^2)))))
may_mgcv_humidity_clamp<-clamp(may_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
may_mgcv_humidity_classes<-classify(x=may_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
may_pmv_mgcv_humidityscale<-classify(x=may_mgcv_humidity_classes,rcl=m)
may_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(may_pmv_mgcv_humidityscale^4))+(0.2179*(may_pmv_mgcv_humidityscale^2)))))
june_mgcv_humidity_clamp<-clamp(june_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
june_mgcv_humidity_classes<-classify(x=june_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
june_pmv_mgcv_humidityscale<-classify(x=june_mgcv_humidity_classes,rcl=m)
june_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(june_pmv_mgcv_humidityscale^4))+(0.2179*(june_pmv_mgcv_humidityscale^2)))))
july_mgcv_humidity_clamp<-clamp(july_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
july_mgcv_humidity_classes<-classify(x=july_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
july_pmv_mgcv_humidityscale<-classify(x=july_mgcv_humidity_classes,rcl=m)
july_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(july_pmv_mgcv_humidityscale^4))+(0.2179*(july_pmv_mgcv_humidityscale^2)))))
august_mgcv_humidity_clamp<-clamp(august_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
august_mgcv_humidity_classes<-classify(x=august_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
august_pmv_mgcv_humidityscale<-classify(x=august_mgcv_humidity_classes,rcl=m)
august_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(august_pmv_mgcv_humidityscale^4))+(0.2179*(august_pmv_mgcv_humidityscale^2)))))
september_mgcv_humidity_clamp<-clamp(september_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
september_mgcv_humidity_classes<-classify(x=september_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
september_pmv_mgcv_humidityscale<-classify(x=september_mgcv_humidity_classes,rcl=m)
september_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(september_pmv_mgcv_humidityscale^4))+(0.2179*(september_pmv_mgcv_humidityscale^2)))))
october_mgcv_humidity_clamp<-clamp(october_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
october_mgcv_humidity_classes<-classify(x=october_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
october_pmv_mgcv_humidityscale<-classify(x=october_mgcv_humidity_classes,rcl=m)
october_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(october_pmv_mgcv_humidityscale^4))+(0.2179*(october_pmv_mgcv_humidityscale^2)))))
november_mgcv_humidity_clamp<-clamp(november_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
november_mgcv_humidity_classes<-classify(x=november_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
november_pmv_mgcv_humidityscale<-classify(x=november_mgcv_humidity_classes,rcl=m)
november_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(november_pmv_mgcv_humidityscale^4))+(0.2179*(november_pmv_mgcv_humidityscale^2)))))
december_mgcv_humidity_clamp<-clamp(december_pmv_mgcv_humidity,lower=-2, upper=6, values=TRUE)
december_mgcv_humidity_classes<-classify(x=december_mgcv_humidity_clamp, rcl=c(-2,-1,0.1,0.88,1.92,2.87,4.03,6),include.lowest=TRUE)
december_pmv_mgcv_humidityscale<-classify(x=december_mgcv_humidity_classes,rcl=m)
december_ppd_mgcv_humidity<-100-(95*exp(-((0.03353*(december_pmv_mgcv_humidityscale^4))+(0.2179*(december_pmv_mgcv_humidityscale^2)))))

ppd_mgcv_humidity_todos<-c(january_ppd_mgcv_humidity,february_ppd_mgcv_humidity,march_ppd_mgcv_humidity,april_ppd_mgcv_humidity,may_ppd_mgcv_humidity,june_ppd_mgcv_humidity,july_ppd_mgcv_humidity,august_ppd_mgcv_humidity,september_ppd_mgcv_humidity,october_ppd_mgcv_humidity,november_ppd_mgcv_humidity,december_ppd_mgcv_humidity)
ppd_mgcv_humidity_mean<-app(ppd_mgcv_humidity_todos,fun=mean)
plot(ppd_mgcv_humidity_mean)
writeRaster(ppd_mgcv_humidity_mean,"ppd_mgcv_humidity_mean.tif", overwrite=TRUE)
