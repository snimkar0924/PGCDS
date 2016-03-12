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





##root mean squared error - RMSE
RMSE1_man<-sqrt(sum(((y1-y2))^2)/N)
RMSE1_lib<-rmse(y1,y2)
RMSE1_lib_auto<-rmse(y1,y3)

##seasonality modeling - the weekly effect
##seventh difference
visitsTS7<-diff(log(visitsTS),DIFF)

par(mfrow=c(2,1))
acf(visitsTS7)
pacf(visitsTS7)

modelB<-arima(visitsTS7,order=c(5,0,4))
summary(modelB)
plot(modelB)
modelB_auto=auto.arima(visitsTS7)
summary(modelB_auto)

fcstB<-forecast(modelB,h=M)
fcstB_auto<-forecast(modelB_auto,h=M)
par(mfrow=c(1,1))
plot(fcstB)
str(fcstB)

x_7<-seq.Date(from=as.Date(rbusData[(CUT_OFF+1),1],format='%Y-%m-%d'),
              to=as.Date(rbusData[(CUT_OFF+M),1],format='%Y-%m-%d'), 
               by='day')
length(x_7)
y1_7<-diff(log(rbusData[(CUT_OFF+1-DIFF):(CUT_OFF+M),4]),7)
length(y1_7)
y2_7<-fcstB$mean
length(y2_7)
y3_7<-fcstB_auto$mean
length(y3_7)

plot(x=x_7[10], y=y1_7[10], type="l", col="red", main='actual vs forecast', 
                    sub='red-actual blue-forecast',
                    xlab='day', ylab='actual vs forecast')
lines(x=x_7[10], y=y2_7[10], col="blue", type="l")
lines(x=x_7[10], y=y3_7[10], col="green", type="l")
# lines(x=x_7, y=(y2_7-y1_7), col="darkgreen", type="l")

# plot(x=x_7, y=(y2_7-y1_7), col="darkgreen", type="l")
# xaxis=as.integer(rep(0,length(x_7)))
# lines(xaxis,col='red',type='l')

##root mean squared error - RMSE
RMSE7<-sqrt(sum((y1_7-y2_7)^2)/M)
RMSE7<-rmse(y1_7,y2_7)

# tmp<-cbind.data.frame(x,y1,y2)
# colnames(tmp)<-c("Date","Actual Value","Forecast Value")
# 
# write.csv(tmp,file.path(dataPathStr,"fcst.csv"))

############################# seats sold analysis ####################

seatsSold<-rbusData[,14]+rbusData[,15]+rbusData[,16]+rbusData[,17]
visits<-rbusData[,4]
head(visits)
plot(x=rbusData[,1], y=log(seatsSold), type='l',col='blue')
lines(x=rbusData[,1], y=log(visits),type="l",col="green")

boxplot(seatsSold)

colnames(rbusData)
m<-731
n<-839
x<-rbusData[m:n,1]
y1<-log(visits[m:n])
y2<-log(seatsSold[m:n])
length(y1)
length(y2)

f<-floor(min(y1,y2))
c<-ceiling(max(y1,y2))
inc<-(c-f)/(n-m)

y0<-seq(f,c,inc)
length(y0)

par(mfrow=c(1,1))
plot(x=rbusData[m:n,1],y=y0,type='n',col='white',
    xlab="visits - blue & seats - red ")
lines(x=x, y=y1,type='l',col='blue')
lines(x=x,y=y2,type='l',col='red')

?ccf
ccf(visits, seatsSold, ylab = "cross-correlation")
res<-ccf(diff(log(visits)), diff(log(seatsSold)), ylab = "cross-correlation")
plot(res)

seatsSoldTS<-ts(seatsSold[1:730],start=c(2013,01,01),freq=365)
plot(seatsSoldTS)

adf.test(seatsSoldTS)
par(mfrow=c(2,1))
acf(seatsSoldTS)
pacf(seatsSoldTS)

seatsSoldTS1<-diff(log(seatsSoldTS))
adf.test(seatsSoldTS1)
par(mfrow=c(2,1))
acf(seatsSoldTS1)
pacf(seatsSoldTS1)

length(seatsSoldTS1)
modelA<-arima(seatsSoldTS1,order=c(3,0,3),
              seasonal=list(order=c(1,0,0), period=7))
summary(modelA)
plot(modelA)
modelA_auto<-auto.arima(seatsSoldTS1)
summary(modelA_auto)
plot(modelA_auto)

fcstA<-forecast(modelA,h=109)
fcstA_auto<-forecast(modelA_auto,h=109)
par(mfrow=c(1,1))
plot(fcstA)

x<-rbusData[(CUT_OFF+1):(CUT_OFF+1+N-1),1]
y1<-diff(log(rbusData[(CUT_OFF):(CUT_OFF+N),4]))
y2<-fcstA$mean
y3<-fcstA_auto$mean

length(y1)
length(y2)

plot(x=x,y=y1,type="l",col="red", main='actual vs forecast', 
     sub='red-actual blue-forecast',
     xlab='day', ylab='actual vs forecast')
lines(x=x, y=y2, col="blue", type="l")
lines(x=x, y=y3, col="green", type="l")

z<-10
###zooming in into the first z periods 
plot(x=x[1:z], y=y1[1:z], type="l", col="red", main='actual vs forecast', 
     sub='red-actual blue-forecast',
     xlab='day', ylab='actual vs forecast')
lines(x=x[1:z], y=y2[1:z], col="blue", type="l")
lines(x=x[1:z], y=y3[1:z], col="green", type="l")


?arima
##root mean squared error - RMSE
#RMSE1<-sqrt(sum(((y1-y2)/y1)^2)/N)
RMSE1<-rmse(y1,y2)

##linear regression
model<-lm(seatsSold~visits-1)
summary(model)
plot(model)

