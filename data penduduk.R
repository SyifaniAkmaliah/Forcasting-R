#Packages
library(tseries)
library(forecast)
library(IMTest)
library(FitAR)
library(stats)
library(EnvStats)
library(AER)

#Import Data
datapopulasi <- read.table("~/Semester 6/ADW/Datapopulasi.txt", quote="\"", comment.char="", stringsAsFactors=TRUE)
View(datapopulasi)
populasi=datapopulasi[,1]
adf.test(populasi)
plot(populasi)

#Identifikasi 
#Tahap 1
ts.plot(populasi)
boxcox(populasi)
#Tidak perlu ditransformasi

#Kestasioneran MEAN
acf(populasi)
pacf(populasi)

