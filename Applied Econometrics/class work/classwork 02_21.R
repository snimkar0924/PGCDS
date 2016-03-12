
##time series spectral representation
#rm(list=ls())
library(tseries)
library(moments)

data("EuStockMarkets")
plot(EuStockMarkets)
##perfect capital mobility - a simple arbitrage argument
colnames(EuStockMarkets)
str(EuStockMarkets)

dax<-EuStockMarkets[,1]
smi<-EuStockMarkets[,2]
cac<-EuStockMarkets[,3]
ftse<-EuStockMarkets[,4]
length(dax)

adf.test(dax)
adf.test(smi)
adf.test(cac)
adf.test(ftse)

adf.test(diff(dax))
adf.test(diff(smi))
adf.test(diff(cac))
adf.test(diff(fte))

par(mfrow=c(2,1))
acf(diff(dax))
pacf(diff(dax))
###diff(dax) - non-stationary...

###Eugene Fama - fundamental statement on market efficiency

par(mfrow=c(2,1))
acf(diff(smi))
pacf(diff(smi))


##co-integration tests

model1<-lm(dax~smi)
e1<-resid(model1)
par(mfrow=c(1,1))
plot(e1, type='l')
adf.test(e1)
# mean(e)
# sd(e)
# skewness(e)
# kurtosis(e)


model2<-lm(smi~dax)
e2<-resid(model2)
par(mfrow=c(1,1))
plot(e2, type='l')
adf.test(e2)
# mean(e)
# sd(e)
# skewness(e)
# kurtosis(e)


ddax<-diff(dax)
dsmi<-diff(smi)
length(ddax)
#error correction mechanism [ECM]
ecm1<-lm(ddax[3:1859]~ddax[2:1858]+ddax[1:1857]
                      +dsmi[2:1858]+dsmi[1:1857]
                      +e1[2:1858])
summary(ecm1)

ecm2<-lm(dsmi[3:1859]~ddax[2:1858]+ddax[1:1857]
         +dsmi[2:1858]+dsmi[1:1857]
         +e2[2:1858])
summary(ecm2)
##coeff of e - speed of adjustment


###dax is reacting to smi and not the other way around...



###VAR [Vector Auto Regression]- multiple TS' with no clear endogenous/ exogenous 
###  relationships
library(vars)
data("Canada")
class(Canada)

model<-VAR(Canada,p=2)
summary(model)
#impulse response function
plot(irf(model,n.ahead = 50))

###use the fmcg data.. 

##EuStockMarkets
str(EuStockMarkets)
modelEU<-VAR(EuStockMarkets,p=2)
summary(modelEU)
#impulse response function
plot(irf(modelEU,n.ahead = 50))

modeldiffEU<-VAR(diff(EuStockMarkets),p=2)
summary(modeldiffEU)
#impulse response function
plot(irf(modeldiffEU,n.ahead = 50))





