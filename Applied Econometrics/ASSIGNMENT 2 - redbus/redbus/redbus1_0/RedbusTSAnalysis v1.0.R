#remove(list=ls())

###Redbus website visit analysis
library(readxl)
library(tseries)
library(forecast)
library(Metrics)

dataPathStr<-"./data/Redbus modelling data"

fileNameStem<-"Redbus <yr> data.xlsx"
rbusData<-data.frame()

for (yr in 2013:2015)
{
  fileName<-gsub(x=fileNameStem, pattern="<yr>", replacement=yr)
  fileName<-file.path(dataPathStr,fileName)
  rbusData<-rbind(rbusData,read_excel(fileName, 
                                      sheet="Consolidated", skip=1))
}

# dim(rbusData)
# head(rbusData[,1], n=10)
# tail(rbusData[,1], n=10)
# colnames(rbusData)
#remove leading spaces...
colnames(rbusData)[1:2]<-c("Date","Day")

##constants
BIG_N<-nrow(rbusData)
##training dataset size
CUT_OFF<-730 ##2 yr (2013-2014)
N<-(BIG_N)-(CUT_OFF)
DIFF<-7
M<-N ##forecast periods for DIFF series... 

#write.csv(cbind(rbusData[1],rbusData[4]), 
#          "./data/Redbus modelling data/visits.csv")


############################## total visits analysis ##########################
##create time series - using the data for 2013 & 2014 only (730 rows)
visitsTS<-ts(rbusData[1:CUT_OFF,4], start=c(2013,1,1), freq=365)
par(mfrow=c(1,1))
plot(visitsTS, type='l')
adf.test(visitsTS) ##inaccurate!
plot(decompose(visitsTS))


##ARIMA modeling
par(mfrow=c(2,1))
acf(visitsTS)
pacf(visitsTS)
##need to difference - also, notice a weekly trend in pacf

#first difference
visitsTS1<-diff(log(visitsTS))
par(mfrow=c(1,1))
plot(visitsTS1, type='l')
par(mfrow=c(2,1))
acf(visitsTS1)
pacf(visitsTS1)
adf.test(visitsTS1) 

##AR(2)MA(2)   #aic = -1100.26
model2<-arima(visitsTS1, order=c(2,0,2)
              ,seasonal=list(order=c(1,0,1), period=7))
summary(model2)

##AR(3)MA(2)  #aic = -1118.98
model3<-arima(visitsTS1, order=c(3,0,2)
              ,seasonal=list(order=c(1,0,1), period=7))
summary(model3)

##AR(4)MA(2)  #aic = -1116.1
model4<-arima(visitsTS1, order=c(4,0,2)
              ,seasonal=list(order=c(1,0,1), period=7))
summary(model4)

##AR(5)MA(2)  #aic = -1107.59
model5<-arima(visitsTS1, order=c(5,0,2)
              ,seasonal=list(order=c(1,0,1), period=7))
summary(model5)

####### chosen model!!! #########
modelA<-model3
#################################
plot(modelA)
modelA_auto=auto.arima(visitsTS1)
summary(modelA_auto)
plot(modelA_auto)
fcstA<-forecast(modelA,h=N)
fcstA_auto<-forecast(modelA_auto,h=N)
#fcstA_auto<-forecast(modelA_auto,h=N)
str(fcstA)
par(mfrow=c(1,1))
plot(fcstA)

x<-rbusData[(CUT_OFF+1):(CUT_OFF+1+N-1),1]
y1<-diff(log(rbusData[(CUT_OFF):(CUT_OFF+N),4]))
y2<-fcstA$mean
y3<-fcstA_auto$mean

plot(x=x,y=y1,type="l",col="red", main='actual vs forecast', 
                       sub='red-actual blue-forecast',
                       xlab='day', ylab='actual vs forecast')
lines(x=x, y=y2, col="blue", type="l")
lines(x=x, y=y3, col="green", type="l")


z<-20
###zooming in into the first z periods 
plot(x=x[1:z], y=y1[1:z], type="l", col="red", main='actual vs forecast', 
     sub='red-actual blue-forecast',
     xlab='day', ylab='actual vs forecast')
lines(x=x[1:z], y=y2[1:z], col="blue", type="l")
lines(x=x[1:z], y=y3[1:z], col="green", type="l")




