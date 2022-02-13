#Packages
library(tseries)
library(forecast)
library(IMTest)
library(FitAR)
library(stats)
library(EnvStats)
library(AER)

#Import Data
datacovid <- read.table("~/Semester 6/ADW/datacovid.txt", quote="\"", comment.char="", stringsAsFactors=TRUE)
View(datacovid)
covid=datacovid[,1]

#Identifikasi 
#Tahap 1
ts.plot(covid)
boxcox(covid)
#Hasil PCPP maksimal diperoleh dengan lambda 0.0 shg lakukan transformasi data dengan Zt=lnzt
tcovid=log(covid) #fungsi log() artinya ln transformasi
ts.plot(tcovid)


#Tahap 2 (Stasioneran mean)
acf(tcovid)
pacf(tcovid)
acf<-acf(tcovid,lag.max=10, plot=FALSE)
acf

fitpendapatan1=ar(tcovid,order=c(2),method = 'ols' )
fitpendapatan1
rec.ols=ar.ols(tcovid,order=2)
rec.ols

acf(diff(tcovid))
pacf(diff(tcobid))
acf(diff(diff(tcovid)))
pacf(diff(diff(tcovid)))
#ACF cut off di lag-1
#PACF cut off di lag -1,2,3

#Menentukan orde ARIMA(p,d,q)
#ARIMA(1,2,1)
#ARIMA(2,2,1)
#ARIMA(3,2,1)