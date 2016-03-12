
##ARIMA
remove(y)
y1<-read.csv("./data/arima.csv")
attach(y1)
head(y1)
length(y) #=> 200 observations

model1<-lm(y[2:200]~y[1:199])
summary(model1)
alpha<-coef(model1)[1]
phi<-coef(model1)[2]

##long-term mean => forecast
frcst<-alpha/(1-phi)

##now use t distribution to get 95% confidence interval
#df = 200-2 since 2 params have been estimated already
t1<-qt(0.025,198)
t2<-qt(0.975,198)
##variance of error term 
e<-y-fitted(model1)
sigma2<-sum(e^2)/198 ##div by n-k (df)

lower<-frcst-(-t1*sqrt(sigma2/(1-(phi^2))))
upper<-frcst-(-t2*sqrt(sigma2/(1-(phi^2))))

### 95% of the times - forecast will lie between lower and upper

##99% confidence interval
t1<-qt(0.005,198)
t2<-qt(0.995,198)

lower<-frcst-(-t1*sqrt(sigma2/(1-(phi^2))))
upper<-frcst-(-t2*sqrt(sigma2/(1-(phi^2))))
##wider!!!


#####forcasting India's GDP growth rate

#remove(list=ls())
gdp<-read.csv("./data/WDI_GDP_select countries_modified.csv")
attach(gdp)
colnames(gdp)

india<-gdp$India
india<-ts(india, freq=1,start=1970,end=2011)
plot(india)
dindia<-diff(log(india))
plot(dindia)

##assuming an AR(1) process
model1<-lm(dindia[2:41]~dindia[1:40])
summary(model1)
alpha1<-coef(model1)[1]
phi1<-coef(model1)[2]

##long-term mean => forecast
frcst1<-alpha1/(1-phi1)

t1<-qt(0.005,38)
t2<-qt(0.995,38)
e1<-dindia-fitted(model1)
sigma21<-sum(e1^2)/38 ##div by n-k (df)
sqrt(sigma21)

lower<-frcst1-(-t1*sqrt(sigma21/(1-(phi1^2))))
upper<-frcst1-(-t2*sqrt(sigma21/(1-(phi1^2))))
###very wide range!!!!


#do this for australia
aussie<-gdp$Australia
aussie<-ts(aussie, freq=1,start=1970,end=2011)
plot(aussie)
daussie<-diff(log(aussie))
plot(daussie)

##assuming an AR(1) process
model2<-lm(daussie[2:41]~daussie[1:40])
summary(model2)
alpha2<-coef(model2)[1]
phi2<-coef(model2)[2]

##long-term mean => forecast
frcst2<-alpha2/(1-phi2)

t1<-qt(0.005,38)
t2<-qt(0.995,38)
e2<-daussie-fitted(model2)
sigma22<-sum(e2^2)/38 ##div by n-k (df)
sqrt(sigma22)

lower<-frcst2-(-t1*sqrt(sigma22/(1-(phi2^2))))
upper<-frcst2-(-t2*sqrt(sigma22/(1-(phi2^2))))


##test for stationarity - augmented dicky-fuller adf


##tightening the bands

#conditional forecast 
#predict the rate for the next year


###########################new data - AR(1) process manually generated
data1<-read.csv("./data/test1.csv")

x<-ts(data1$x,start=c(2000,01),end=c(2015,06),freq=12)
plot(x)
model<-lm(x[2:186]~x[1:185])
summary(model)











