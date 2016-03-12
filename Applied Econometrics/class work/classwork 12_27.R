
#remove(list=ls())

##Time Series

#help("AirPassengers")
plot(AirPassengers) ##multiplicative seasonality

#help(decompose)

plot(decompose(AirPassengers))
plot(decompose(AirPassengers,type="multiplicative"))
str(decompose(AirPassengers,type="multiplicative"))
m<-decompose(AirPassengers,type="multiplicative")
t<-m$trend
plot(t)


#setwd("D:\\Sonali\\000 La Vita Nuova\\000 THE HNI\\NISM\\PGCDS\\TERM 1\\003 Applied Econometrics\\class work")

gdp<-read.csv("./data/WDI_GDP_select countries_modified.csv")
attach(gdp)

india<-ts(India,start=1970,end=2011,freq=1)
growth<-diff(log(india))

time<-seq(1:length(india))
model1<-lm(india~time)
summary(model1)
s<-coef(model1)
plot(india)
plot(growth)

abline(coef=s)
lines(india)

fitted<-fitted(model1)

par(mfrow=c(1,2))
plot(india)
plot(fitted)

model11<-lm(log(India)~time)
model22<-lm(log(China)~time)
model33<-lm(log(Brazil)~time)


##trend cycle decomposition
china<-ts(China,start=1970,end=2011,freq=1)
time<-seq(1:length(china))
model1<-lm(china~time)
summary(model1)
model1<-lm(log(china)~time)
summary(model1)

##y0=exp()
y0<-exp(4.62)
chinatrend<-y0+exp(0.07846*time)








