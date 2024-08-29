library(terra)

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

#correlation of each month between UTCI and MGCV moodles

utci_january<-resample(utci_january, january_pmv_mgcv_temperature)
ucti_monthly_correlation_january <-layerCor(c(utci_january,january_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_january  # 0.937 in January

utci_february<-resample(utci_february, february_pmv_mgcv_temperature)
ucti_monthly_correlation_february <-layerCor(c(utci_february,february_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_february  # 0.953 in february

utci_march<-resample(utci_march, march_pmv_mgcv_temperature)
ucti_monthly_correlation_march <-layerCor(c(utci_march,march_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_march  # 0.966 in march
plot(c(utci_march,march_pmv_mgcv_temperature))

utci_april<-resample(utci_april, april_pmv_mgcv_temperature)
ucti_monthly_correlation_april <-layerCor(c(utci_april,april_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_april  # 0.892 in april

utci_may<-resample(utci_may, may_pmv_mgcv_temperature)
ucti_monthly_correlation_may <-layerCor(c(utci_may,may_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_may  # 0.400 in may

utci_june<-resample(utci_june, june_pmv_mgcv_temperature)
ucti_monthly_correlation_june <-layerCor(c(utci_june,june_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_june  # 0.299 in june
plot(c(utci_june,june_pmv_mgcv_temperature))

utci_july<-resample(utci_july, july_pmv_mgcv_temperature)
ucti_monthly_correlation_july <-layerCor(c(utci_july,july_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_july  # 0.423 in july

utci_august<-resample(utci_august, august_pmv_mgcv_temperature)
ucti_monthly_correlation_august <-layerCor(c(utci_august,august_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_august  # 0.602 in august

utci_september<-resample(utci_september, september_pmv_mgcv_temperature)
ucti_monthly_correlation_september <-layerCor(c(utci_september,september_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_september  # 0.852 in september

utci_october<-resample(utci_october, october_pmv_mgcv_temperature)
ucti_monthly_correlation_october <-layerCor(c(utci_october,october_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_october  # 0.906 in october

utci_november<-resample(utci_november, november_pmv_mgcv_temperature)
ucti_monthly_correlation_november <-layerCor(c(utci_november,november_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_november  # 0.928 in november

utci_december<-resample(utci_december, december_pmv_mgcv_temperature)
ucti_monthly_correlation_december <-layerCor(c(utci_december,december_pmv_mgcv_temperature), "pearson",na.rm = TRUE) # Pearson correlation of 0.72
ucti_monthly_correlation_december  # 0.929 in december

utci_monthly<-c(ucti_monthly_correlation_january$pearson[1,2],ucti_monthly_correlation_february$pearson[1,2],ucti_monthly_correlation_march$pearson[1,2],ucti_monthly_correlation_april$pearson[1,2],ucti_monthly_correlation_may$pearson[1,2],ucti_monthly_correlation_june$pearson[1,2],ucti_monthly_correlation_july$pearson[1,2],ucti_monthly_correlation_august$pearson[1,2],ucti_monthly_correlation_september$pearson[1,2],ucti_monthly_correlation_october$pearson[1,2],ucti_monthly_correlation_november$pearson[1,2], ucti_monthly_correlation_december$pearson[1,2])
plot(utci_monthly,type="b",ylab="Pearson's Correlation",xlab="",ylim=0:1,xaxt='n')
axis(side=1,at=1:12,labels=month.name[1:12],las = 2)

mean(utci_monthly) #Average correlation is 0.757

