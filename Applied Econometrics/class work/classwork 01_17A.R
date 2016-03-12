remove(list=ls())

###TIME SERIES ###

library(xts)
library(tseries)
library(forecast)
library(mFilter)

dataPathStr<-"D:/Sonali/000 La Vita Nuova/000 THE HNI/NISM/PGCDS/TERM 1/003 Applied Econometrics/class work/data"

gdpDatPth<-file.path(dataPathStr,"WDI_GDP_select countries_modified.csv")

gdpData<-read.csv(gdpDatPth)
colnames(gdpData)
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
par(mfrow=c(2,1))
acf(brazilData1)
pacf(brazilData1)
?par
brzMdl1<-auto.arima(brazilData1)
summary(brzMdl1)

brazilData2<-diff(brazilData1)
adf.test(brazilData2)
plot(brazilData1)
plot(brazilData2)
par(mfrow=c(2,1))
acf(brazilData2)
pacf(brazilData2)

brzMdl21<-arima(brazilData2,order=c(0,2,1))
summary(brzMdl21)

brzMdl2<-auto.arima(brazilData2)
summary(brzMdl2)

brz<-hpfilter(brazilData2,freq=1)
plot(brz)
summary(brz)

brz<-hpfilter(brazilData,freq=1)
plot(brz)


##india data
indiaData<-ts(gdpData$India, freq=1, start=1970, end=2011)
plot(indiaData)
adf.test(indiaData)
indiaData1<-diff(log(indiaData))
adf.test(indiaData1)
plot(indiaData1)
indiaData2<-diff(indiaData1)
adf.test(indiaData2)
plot(indiaData2)

indMdl<-auto.arima(indiaData1)
summary(indMdl)

indMdl1<-arima(indiaData1,order=c(0,1,1))
summary(indMdl1)

ind<-hpfilter(indiaData1,freq=1)
plot(ind)

str(indiaData)
decompose(indiaData) ##refuses to work!
stl(indiaData, s.window="periodic") ##refuses to work!



accDatFile<-file.path("D:\\Sonali\\000 La Vita Nuova\\000 THE HNI\\NISM\\PGCDS\\TERM 1\\001 Mathematics for Analytics\\001 suneel classwork\\data_mu", "ACCEQN.csv")
accDat<-read.csv(accDatFile)
accTS<-ts(accDat, freq=248, start=c(2014,12,05), end=c(2015,12,04))

decMdl<-plot(decompose(accTS))
summary(decMdl)
decMdl.hp<-hpfilter(accTS,freq=1)
plot(decMdl.hp)

plot(AirPassengers)
apDiff1<-diff(log(AirPassengers))
ap<-hpfilter(apDiff1,freq=1)
plot(ap)
plot(decompose(AirPassengers))
str(AirPassengers)

stl(AirPassengers)


################################################
require(graphics)

m <- decompose(co2)
m$figure
plot(m)

## example taken from Kendall/Stuart
x <- c(-50, 175, 149, 214, 247, 237, 225, 329, 729, 809,
       530, 489, 540, 457, 195, 176, 337, 239, 128, 102, 232, 429, 3,
       98, 43, -141, -77, -13, 125, 361, -45, 184)
x <- ts(x, start = c(1951, 1), end = c(1958, 4), frequency = 4)
m <- decompose(x)
## seasonal figure: 6.25, 8.62, -8.84, -6.03
round(decompose(x)$figure / 10, 2)
plot(m)








