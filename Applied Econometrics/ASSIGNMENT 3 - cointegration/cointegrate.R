
library(tseries)
### cointegration
##read gold and nifty 50 historical price data
#rm(list=ls())
gold<-read.csv('./data/Gold_price.csv', stringsAsFactors=FALSE)
nifty<-read.csv('./data/Nifty50_price.csv', stringsAsFactors=FALSE)

gold[,'Date'] = as.Date(gold[,'Date'],format='%d-%b-%y')
nifty[,'Date'] = as.Date(nifty[,'Date'],format='%d-%b-%y')

gold<-gold[which(gold[,'Date'] %in% nifty[,'Date']),]

goldTS<-ts(gold[,3],start=c(2015,03,02),freq=247)
niftyTS<-ts(nifty$Close,start=c(2015,03,02),freq=247)

par(mfrow=c(2,1))
plot(goldTS, type='l')
plot(niftyTS, type='l')

model1<-lm(niftyTS~goldTS)
summary(model1)
e1<-residuals(model1)
adf.test(e1)
par(mfrow=c(1,1))
plot(e1, type='l')

model2<-lm(goldTS~niftyTS)
summary(model2)
e2<-residuals(model2)
adf.test(e2)
par(mfrow=c(1,1))
plot(e2, type='l')
par(mfrow=c(2,1))
acf(e2)
pacf(e2)

##create a co-integration model
goldTS1<-diff(log(goldTS))
niftyTS1<-diff(log(niftyTS))
l<-length(goldTS1)

ecm1<-lm(goldTS1[2:l] ~ goldTS1[1:(l-1)]
                    + niftyTS1[1:(l-1)]
                    + e2[1:(l-1)])
summary(ecm1)

# ecm2<-lm(goldTS1[3:l] ~ goldTS1[2:(l-1)]
#                         + goldTS1[1:(l-2)]
#                         + niftyTS1[2:(l-1)]
#                         + niftyTS1[1:(l-2)]
#                         + e2[2:(l-1)])
# summary(ecm2)

ecm1_1<-lm(niftyTS1[2:l] ~ goldTS1[1:(l-1)]
                         + niftyTS1[1:(l-1)]
                         + e2[1:(l-1)])
summary(ecm1_1)

# ecm2_1<-lm(niftyTS1[3:l] ~ goldTS1[2:(l-1)]
#                          + goldTS1[1:(l-2)]
#                          + niftyTS1[2:(l-1)]
#                          + niftyTS1[1:(l-2)]
#                          + e2[2:(l-1)])
# summary(ecm2_1)

###conclusion nifty leads gold not vice versa


library(vars)
gn<-VAR(cbind.data.frame(goldTS, niftyTS), p=1)
plot(irf((gn),n.ahead=10))

gn1<-VAR(cbind.data.frame(goldTS1, niftyTS1), p=1)
plot(irf((gn1),n.ahead=10))
