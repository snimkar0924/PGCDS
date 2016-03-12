rm(list=ls())
library(AER)
library(glmulti)

data1<-read.csv(".\\box1.csv",header=T,sep=",")
attach(data1)

model1<-glm(Lifetime~movies+weekend+holiday+Drama+Romance+Action+sci+Comedy+Biography+Thriller-1,data=data1)
global.model1<-glmulti(model1,level=1,crit="aicc",method="g")
summary(global.model1)
weightable (global.model1)

model1<-glm(Lifetime~movies+holiday+Action+Thriller-1,data=data1)
summary(model1)

model1<-glm(Lifetime~movies+holiday+Action-1,data=data1)
summary(model1)

model2<-glm(Lifetime~sequel+larmA+larmB+larmC+larf+larfA+larfB+larfC+dirrA+dirrB+dirrC-1,data=data1)
global.model2<-glmulti(model2,level=1,crit="aicc",method="g")
summary(global.model2)
weightable(global.model2)
plot(model2)

model3 <- glmulti(Lifetime~larmA+dirrA+sequel-1,data=data1,crit="aicc",method="g",fitfunc=lm,level=1,maxit=50)
print(model3)

