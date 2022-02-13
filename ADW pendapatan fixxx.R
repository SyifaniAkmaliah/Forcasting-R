#Packages
library(tseries)
library(forecast)
library(IMTest)
library(FitAR)
library(stats)
library(EnvStats)
library(AER)

#Import data 
library(readxl)
datapendapatan <- read.table("~/Semester 6/ADW/datapendapatan.txt", quote="\"", comment.char="", stringsAsFactors=TRUE)
View(datapendapatan)
pendapatan=datapendapatan[,1]

#Tahap 1 (Stasioner dalam VARIANS)
ts.plot(pendapatan)
#Memastikan varians sudah stasioner ? transformasi boxcox
boxcox(pendapatan) #cari nilai PPC yang terbesar cocokan dengan tabel
summary(pendapatan) #gaperlu
is.numeric(pendapatan)
tpendapatan=sqrt(pendapatan) #transformasi dengan akar (0.5)
ts.plot(tpendapatan)


#Tahap 2 (stasioner dalam MEAN)
acf(tpendapatan) #kemungkinan berpola sinus menurun lemah
pacf(tpendapatan)
acf(diff(tpendapatan))
pacf(diff(tpendapatan))


#Tahap 3
#menentukan Orde ARIMA (p,d=1,q=1)
#ACF terpotong di lag 1 (lag garis yang dibawah)
#PACF terpotong di lag 1,2,3 (orde p)
#Arima(3,1,1)
#Arima(2,1,1)
#ARIMA(1,1,1)
#ARIMA(0,1,1) IMA
#ARIMA(3,1,1)


#Estimasi Parameter
fitpendapatan1=arima(tpendapatan,order=c(2,0,0),method = 'ols' )
coeftest(fitpendapatan1)
#Tidak ada paramater yang signifikan karena p value > 0.05
fitpendapatan2=arima(tpendapatan,order=c(2,1,1))
coeftest(fitpendapatan2)
#Hanya MA1 yang signifikan karena p value <0.05
fitpendapatan3=arima(tpendapatan,order=c(1,1,1))
coeftest(fitpendapatan3)
#Hanya MA(1) signifikan
fitpendapatan4=arima(tpendapatan,order=c(0,1,1))
coeftest(fitpendapatan4)
#Selueuh parameter signifikan (hanya MA(1))
#Jadi model terbaik sementara IMA(1,1)


#Uji Diagnosa model
#Plot Residual (model ARIMA(0,1,1))
ts.plot(fitpendapatan4$residuals)
#Setiap plot yang dihasilkan mean dan var konstan jika dilihat visual

#Uji Normalitas Visual Residual
qqnorm(fitpendapatan4$residuals)
qqline(fitpendapatan4$residuals)
#Residual yang terbentuk mendekati dist normal karena sebarannya mendekati garis


#Autokorelasi Residual
acf(fitpendapatan4$residuals, lag=10)
#Tidak ada yang cut off artinya tidak ada autokorelasi di residual


#Ljung-Boc test#
Box.test(fitpendapatan4$residuals, lag= 4,type="Ljung") #Mingguan (4 minggu) nilai pvalue>0.05 sudah memenuhi kecukupan model 
Box.test(fitpendapatan4$residuals, lag= 8,type="Ljung") #2 bulan nilai pvalue>0.05 sudah memenuhi kecukupan model 
Box.test(fitpendapatan4$residuals, lag= 12,type="Ljung") #3 bulan nilai pvalue>0.05 sudah memenuhi kecukupan model 
Box.test(fitpendapatan4$residuals, lag= 16, type="Ljung") #4 Bulan nilai pvalue>0.05 sudah memenuhi kecukupan model 
#LULUS UJI DIAGNOSA


#==========================#
#Forecasting ARIMA(0,1,1)
nilaiprediksi<-forecast(fitpendapatan4, h=1,level=c(99.5)) #h itu nilai satu minggu kedepan
plot(nilaiprediksi)
nilaiprediksi #Nilai yang dihasilkan masih akar karena tadi ditransformasikan

nilaiprediksifix=nilaiprediksi^2


#Estimasi Parameter
rec.ols=ar.ols(tpendapatan,order=2)
rec.ols
rec.ols$x.mean
rec.ols$ar
rec.ols$asy.se.coef
rec.ols$var.pred
