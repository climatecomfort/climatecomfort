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
sf_results_all$illumination_pmv<-as.integer(sf_results_all$illumination)

#Model for illumination
model_illumination_mgcv<-gam(illumination_pmv~+s(radiation_month)+question_dummy+s(id,bs="re"),family=ocat(R=7),data=sf_results_all,method="REML")
summary(model_illumination_mgcv) 
AIC(model_illumination_mgcv)
gam.check(model_illumination_mgcv)
SomersDelta(x=model_illumination_mgcv$fitted.values,y=sf_results_all$illumination_pmv,conf.level = 0.95)
dev.off()
visreg(model_illumination_mgcv, xvar="radiation_month", gg=F, type="conditional", overlay=T, partial=T, rug=T, jitter=T, ylab="Response",top="points",ylab="Latent variable (breaks as PMV)",xlab="surface solar radiation downwards (J/m2)")
abline(h=c(-1,0.54,1.64,3.52,4.24,5.19),col="red",lty=2) # usa os mesmos pontos de quebra indicados por summary(modelo_illumination_mgcv)
text(y=-2,x=2500000,"     A lot of lack of clarity")
text(y=-0.3,x=2500000,"Lack of clarity")
text(y=1,x=2500000,"      Slight lack of clarity")
text(y=2.5,x=2500000,"Comfortable")
text(y=3.8,x=2500000, "         Slight excess of light")
text(y=4.5,x=2500000, "                  Moderate excess of light")
text(y=6,x=2500000,"  Too much light")

#Maps based on the models
#January
january_illumination_aggregated<-aggregate((radiation_january),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-january_illumination_aggregated*0
id_raster<-january_illumination_aggregated*0
january_illumination_explanatory<-c(january_illumination_aggregated,question_dummy_raster,id_raster)
names(january_illumination_explanatory)<-c("radiation_month","question_dummy","id")
january_pmv_mgcv_illumination<-terra::predict(object=january_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(january_pmv_mgcv_illumination)

#February
february_illumination_aggregated<-aggregate((radiation_february),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-february_illumination_aggregated*0
id_raster<-february_illumination_aggregated*0
february_illumination_explanatory<-c(february_illumination_aggregated,question_dummy_raster,id_raster)
names(february_illumination_explanatory)<-c("radiation_month","question_dummy","id")
february_pmv_mgcv_illumination<-terra::predict(object=february_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(february_pmv_mgcv_illumination)

#March
march_illumination_aggregated<-aggregate((radiation_march),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-march_illumination_aggregated*0
id_raster<-march_illumination_aggregated*0
march_illumination_explanatory<-c(march_illumination_aggregated,question_dummy_raster,id_raster)
names(march_illumination_explanatory)<-c("radiation_month","question_dummy","id")
march_pmv_mgcv_illumination<-terra::predict(object=march_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(march_pmv_mgcv_illumination)

#April
april_illumination_aggregated<-aggregate((radiation_april),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-april_illumination_aggregated*0
id_raster<-april_illumination_aggregated*0
april_illumination_explanatory<-c(april_illumination_aggregated,question_dummy_raster,id_raster)
names(april_illumination_explanatory)<-c("radiation_month","question_dummy","id")
april_pmv_mgcv_illumination<-terra::predict(object=april_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(april_pmv_mgcv_illumination)

#May
may_illumination_aggregated<-aggregate((radiation_may),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-may_illumination_aggregated*0
id_raster<-may_illumination_aggregated*0
may_illumination_explanatory<-c(may_illumination_aggregated,question_dummy_raster,id_raster)
names(may_illumination_explanatory)<-c("radiation_month","question_dummy","id")
may_pmv_mgcv_illumination<-terra::predict(object=may_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(may_pmv_mgcv_illumination)

#June
june_illumination_aggregated<-aggregate((radiation_june),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-june_illumination_aggregated*0
id_raster<-june_illumination_aggregated*0
june_illumination_explanatory<-c(june_illumination_aggregated,question_dummy_raster,id_raster)
names(june_illumination_explanatory)<-c("radiation_month","question_dummy","id")
june_pmv_mgcv_illumination<-terra::predict(object=june_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(june_pmv_mgcv_illumination)

#July
july_illumination_aggregated<-aggregate((radiation_july),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-july_illumination_aggregated*0
id_raster<-july_illumination_aggregated*0
july_illumination_explanatory<-c(july_illumination_aggregated,question_dummy_raster,id_raster)
names(july_illumination_explanatory)<-c("radiation_month","question_dummy","id")
july_pmv_mgcv_illumination<-terra::predict(object=july_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(july_pmv_mgcv_illumination)

#August
august_illumination_aggregated<-aggregate((radiation_august),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-august_illumination_aggregated*0
id_raster<-august_illumination_aggregated*0
august_illumination_explanatory<-c(august_illumination_aggregated,question_dummy_raster,id_raster)
names(august_illumination_explanatory)<-c("radiation_month","question_dummy","id")
august_pmv_mgcv_illumination<-terra::predict(object=august_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(august_pmv_mgcv_illumination)

#September
september_illumination_aggregated<-aggregate((radiation_september),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-september_illumination_aggregated*0
id_raster<-september_illumination_aggregated*0
september_illumination_explanatory<-c(september_illumination_aggregated,question_dummy_raster,id_raster)
names(september_illumination_explanatory)<-c("radiation_month","question_dummy","id")
september_pmv_mgcv_illumination<-terra::predict(object=september_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(september_pmv_mgcv_illumination)

#October
october_illumination_aggregated<-aggregate((radiation_october),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-october_illumination_aggregated*0
id_raster<-october_illumination_aggregated*0
october_illumination_explanatory<-c(october_illumination_aggregated,question_dummy_raster,id_raster)
names(october_illumination_explanatory)<-c("radiation_month","question_dummy","id")
october_pmv_mgcv_illumination<-terra::predict(object=october_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(october_pmv_mgcv_illumination)

#November
november_illumination_aggregated<-aggregate((radiation_november),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-november_illumination_aggregated*0
id_raster<-november_illumination_aggregated*0
november_illumination_explanatory<-c(november_illumination_aggregated,question_dummy_raster,id_raster)
names(november_illumination_explanatory)<-c("radiation_month","question_dummy","id")
november_pmv_mgcv_illumination<-terra::predict(object=november_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(november_pmv_mgcv_illumination)

#December
december_illumination_aggregated<-aggregate((radiation_december),fact=3)  #increase pixel size, to allow processing
question_dummy_raster<-december_illumination_aggregated*0
id_raster<-december_illumination_aggregated*0
december_illumination_explanatory<-c(december_illumination_aggregated,question_dummy_raster,id_raster)
names(december_illumination_explanatory)<-c("radiation_month","question_dummy","id")
december_pmv_mgcv_illumination<-terra::predict(object=december_illumination_explanatory,model=model_illumination_mgcv,na.rm=TRUE,exclude=c("question_dummy","s(id)")) #creates raster based on the model
plot(december_pmv_mgcv_illumination)

#Joint all months
pmv_illumination_mgcv_all<-c(january_pmv_mgcv_illumination,february_pmv_mgcv_illumination,march_pmv_mgcv_illumination,april_pmv_mgcv_illumination,may_pmv_mgcv_illumination,june_pmv_mgcv_illumination,july_pmv_mgcv_illumination,august_pmv_mgcv_illumination,september_pmv_mgcv_illumination,october_pmv_mgcv_illumination,november_pmv_mgcv_illumination,december_pmv_mgcv_illumination)
pmv_mgcv_mean_illumination<-app(pmv_illumination_mgcv_all,fun=mean)
plot(pmv_mgcv_mean_illumination)
pmv_mgcv_mean_illumination_classes<-classify(x=pmv_mgcv_mean_illumination, rcl=c(-50,-1,0.54,1.64,3.52,4.24,5.19),include.lowest=TRUE)
plot(pmv_mgcv_mean_illumination_classes,type="classes",plg=list(legend=c("Lack of clarity","Slight lack\n of clarity","Comfortable","Slight excess\n of light\n",  "Moderate excess\n of light"))) # Classes: 0 = very cold, 1 = cold,  2= slightly cold, 3 = comfortable, 4 = slightly hot, 5= hot, 6 = very hot

#PPD - Percentage Predicted Dissatisfied
plot(january_pmv_mgcv_illumination)
january_mgcv_illumination_clamp<-clamp(january_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
plot(january_mgcv_illumination_clamp)
january_mgcv_illumination_classes<-classify(x=january_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
plot(january_mgcv_illumination_classes)
m <- rbind(c(0,-3),c(1,-2), c(2, -1),c(3,0),c(4,1),c(5,2),c(6,3))
january_pmv_mgcv_illumination_scale<-classify(x=january_mgcv_illumination_classes,rcl=m)  
plot(january_pmv_mgcv_illumination_scale,type="classes",plg=list(legend=c("very cold","cold","slightly cold","comfortable","slightly hot","hot","very hot")))
january_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(january_pmv_mgcv_illumination_scale^4))+(0.2179*(january_pmv_mgcv_illumination_scale^2)))))
plot(january_ppd_mgcv_illumination)
february_mgcv_illumination_clamp<-clamp(february_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
february_mgcv_illumination_classes<-classify(x=february_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
february_pmv_mgcv_illuminationscale<-classify(x=february_mgcv_illumination_classes,rcl=m)
february_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(february_pmv_mgcv_illuminationscale^4))+(0.2179*(february_pmv_mgcv_illuminationscale^2)))))
march_mgcv_illumination_clamp<-clamp(march_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
march_mgcv_illumination_classes<-classify(x=march_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
march_pmv_mgcv_illuminationscale<-classify(x=march_mgcv_illumination_classes,rcl=m)
march_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(march_pmv_mgcv_illuminationscale^4))+(0.2179*(march_pmv_mgcv_illuminationscale^2)))))
april_mgcv_illumination_clamp<-clamp(april_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
april_mgcv_illumination_classes<-classify(x=april_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
april_pmv_mgcv_illuminationscale<-classify(x=april_mgcv_illumination_classes,rcl=m)
april_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(april_pmv_mgcv_illuminationscale^4))+(0.2179*(april_pmv_mgcv_illuminationscale^2)))))
may_mgcv_illumination_clamp<-clamp(may_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
may_mgcv_illumination_classes<-classify(x=may_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
may_pmv_mgcv_illuminationscale<-classify(x=may_mgcv_illumination_classes,rcl=m)
may_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(may_pmv_mgcv_illuminationscale^4))+(0.2179*(may_pmv_mgcv_illuminationscale^2)))))
june_mgcv_illumination_clamp<-clamp(june_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
june_mgcv_illumination_classes<-classify(x=june_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
june_pmv_mgcv_illuminationscale<-classify(x=june_mgcv_illumination_classes,rcl=m)
june_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(june_pmv_mgcv_illuminationscale^4))+(0.2179*(june_pmv_mgcv_illuminationscale^2)))))
july_mgcv_illumination_clamp<-clamp(july_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
july_mgcv_illumination_classes<-classify(x=july_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
july_pmv_mgcv_illuminationscale<-classify(x=july_mgcv_illumination_classes,rcl=m)
july_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(july_pmv_mgcv_illuminationscale^4))+(0.2179*(july_pmv_mgcv_illuminationscale^2)))))
august_mgcv_illumination_clamp<-clamp(august_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
august_mgcv_illumination_classes<-classify(x=august_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
august_pmv_mgcv_illuminationscale<-classify(x=august_mgcv_illumination_classes,rcl=m)
august_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(august_pmv_mgcv_illuminationscale^4))+(0.2179*(august_pmv_mgcv_illuminationscale^2)))))
september_mgcv_illumination_clamp<-clamp(september_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
september_mgcv_illumination_classes<-classify(x=september_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
september_pmv_mgcv_illuminationscale<-classify(x=september_mgcv_illumination_classes,rcl=m)
september_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(september_pmv_mgcv_illuminationscale^4))+(0.2179*(september_pmv_mgcv_illuminationscale^2)))))
october_mgcv_illumination_clamp<-clamp(october_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
october_mgcv_illumination_classes<-classify(x=october_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
october_pmv_mgcv_illuminationscale<-classify(x=october_mgcv_illumination_classes,rcl=m)
october_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(october_pmv_mgcv_illuminationscale^4))+(0.2179*(october_pmv_mgcv_illuminationscale^2)))))
november_mgcv_illumination_clamp<-clamp(november_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
november_mgcv_illumination_classes<-classify(x=november_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
november_pmv_mgcv_illuminationscale<-classify(x=november_mgcv_illumination_classes,rcl=m)
november_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(november_pmv_mgcv_illuminationscale^4))+(0.2179*(november_pmv_mgcv_illuminationscale^2)))))
december_mgcv_illumination_clamp<-clamp(december_pmv_mgcv_illumination,lower=-2, upper=6, values=TRUE)
december_mgcv_illumination_classes<-classify(x=december_mgcv_illumination_clamp, rcl=c(-2,-1,0.54,1.64,3.52,4.24,5.19,6),include.lowest=TRUE)
december_pmv_mgcv_illuminationscale<-classify(x=december_mgcv_illumination_classes,rcl=m)
december_ppd_mgcv_illumination<-100-(95*exp(-((0.03353*(december_pmv_mgcv_illuminationscale^4))+(0.2179*(december_pmv_mgcv_illuminationscale^2)))))


ppd_mgcv_illumination_todos<-c(january_ppd_mgcv_illumination,february_ppd_mgcv_illumination,march_ppd_mgcv_illumination,april_ppd_mgcv_illumination,may_ppd_mgcv_illumination,june_ppd_mgcv_illumination,july_ppd_mgcv_illumination,august_ppd_mgcv_illumination,september_ppd_mgcv_illumination,october_ppd_mgcv_illumination,november_ppd_mgcv_illumination,december_ppd_mgcv_illumination)
ppd_mgcv_illumination_mean<-app(ppd_mgcv_illumination_todos,fun=mean)
plot(ppd_mgcv_illumination_mean)
writeRaster(ppd_mgcv_illumination_mean,"ppd_mgcv_illumination_mean.tif", overwrite=TRUE)
