library(forecast)
library(PredictiveRegression)

library(MASS)
#bptest()

rm(list=ls())

x<-rnorm(300)
y<-2+0.3*x-0.02*I(x^2)+rnorm(300)

model1<-lm(y~x)
summary(model1)
#plot(resid(model1))

e2<-resid(model1)^2

#auxillary regression - regress e2 on x and x^2
model2<-lm(e2~x+I(x^2))
#bptest(model2)
mod2<-summary(model2)

#breusch pagan RN/ lagrange multiplier test
bp<-mod2$r.squared*300
1-pchisq(bp,2)



x<-seq(1,300,1)
v<-rnorm(0300,0,0.07*x) ##introduce heteroscedasticity
y<-2+0.3*x-0.02*I(x^2)+v
model1<-lm(y~x)
#plot(y~x)
e2<-resid(model1)^2

c<-coef(model1)
plot(y~x)
abline(c)
#auxillary regression - regress e2 on x and x^2
model2<-lm(e2~x+I(x^2))
#plot(model2)
#bptest(model2)
mod2<-summary(model2)

#breusch pagan RN/ lagrange multiplier test/ box pierre statistic
bp<-mod2$r.squared*297
1-pchisq(bp,2)



