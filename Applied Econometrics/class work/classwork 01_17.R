remove(list=ls())
###TIME SERIES ###

library(tseries)
library(forecast)


gdpData<-read.csv("WDI_GDP_select countries.csv")
head(gdpData, n=10)
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



