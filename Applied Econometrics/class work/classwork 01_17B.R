remove(list=ls())
###TIME SERIES ###

library(xts)
library(tseries)
library(forecast)
library(mFilter)

dataPathStr<-"D:/Sonali/000 La Vita Nuova/000 THE HNI/NISM/PGCDS/TERM 1/003 Applied Econometrics/class work/data"

gdpDatPth<-file.path(dataPathStr,"WDI_GDP_select countries_modified.csv")

gdpData<-read.csv(gdpDatPth)
head(gdpData, n=4)
str(gdpData)
attach(gdpData)

##us data
usData<-ts(US, freq=1, start=1970, end=2011)
plot(usData)
adf.test(usData)

usData1<-diff(log(usData))##growth rate
#usData1<-usData[2:42]-usData[1:41]
adf.test(usData1)
plot(usData1)
acf(usData1)
pacf(usData1)

usMdl<-arima(usData1,order=c(0,1,1))
str(usMdl)
usMdl$aic
summary(usMdl)

usMdl1<-auto.arima(usData1)
usMdl1$aic
summary(usMdl1)
decompose(usData)

usData1.hp<-hpfilter(usData1,freq=1)
plot(usData1.hp)
?decompose

stl(usData)


##brazil data
brazilData<-ts(Brazil, freq=1, start=1970, end=2011)
plot(brazilData)
adf.test(brazilData)
brazilData1<-diff(log(brazilData))
adf.test(brazilData1)
plot(brazilData1)
?par
acf(brazilData1)
pacf(brazilData1)

brzMdl1<-auto.arima(brazilData1)
summary(brzMdl1)

brazilData2<-diff(brazilData1)
adf.test(brazilData2)
par(mfrow=c(2,1))
plot(brazilData1)
plot(brazilData2)
acf(brazilData2)
pacf(brazilData2)

brzMdl2<-auto.arima(brazilData2)
summary(brzMdl2)

brz<-hpfilter(brazilData,freq=1)
plot(brz)
summary(brz)

brz<-hpfilter(brazilData,freq=1)
plot(brz)


##india data
indiaData<-ts(India, freq=1, start=1970, end=2011)
plot(indiaData)
adf.test(indiaData)
indiaData1<-diff(log(indiaData))
adf.test(indiaData1)
plot(indiaData1)
indiaData2<-diff(indiaData1)
adf.test(indiaData2)
plot(indiaData2)
acf(indiaData2)
pacf(indiaData2)

indMdl<-auto.arima(indiaData2)
summary(model)

indMdl1<-arima(indiaData2,order=c(0,2,1))
summary(model1)

accDatFile<-file.path("D:\\Sonali\\000 La Vita Nuova\\000 THE HNI\\NISM\\PGCDS\\TERM 1\\001 Mathematics for Analytics\\001 suneel classwork\\data_mu", "ACCEQN.csv")
accDat<-read.csv(accDatFile)
accTS<-ts(accDat, freq=248, start=c(2014,12,05), end=c(2015,12,04))


decMdl<-decompose(accTS)
summary(decMdl)
decMdl.hp<-hpfilter(accTS,freq=1)
plot(decMdl.hp)



plot(AirPassengers)
ap<-hpfilter(AirPassengers,freq=1)
plot(ap)







