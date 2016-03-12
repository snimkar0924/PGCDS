rm(list=ls())
library(AER)
library(glmulti)


data1<-read.csv(".\\Training Data.csv",header=T,sep=",")
attach(data1)
model1<-glm(Lifetime~movies+weekend+holiday+Drama+Romance+Action+sci+Comedy+Biography+Thriller-1,data=data1)
global.model1<-glmulti(model1,level=1,crit="aicc",method="g")
summary(global.model1)
weightable (global.model1)



data2<-read.csv(".\\box1.csv",header=T,sep=",")
attach(data2)
cols<-colnames(data2)

#exclude the Lifetime data from the data frame to pass this along to 
#data2<-data1[,-1]
#colnames(data2)

#model2<-lm(Lifetime~movies+holiday+Genre+sequel+larm1+larf1+dirr1+fg+imdb+cr+weeks+spots+channels+grps+tv+reach1+reach2+reach3+trailer+trailerl+trailerd+trailerc+songsv+songsl+songsd+songsc+moviepl+pagepposts+followers+tweets+favourites+searchv+searchc+twitter+comments+news+aggregator+blogs+Videos+sentiP+sentin-1)
model2<-glm(Lifetime~movies+holiday+Genre+sequel+larm1+larf1+dirr1+fg+imdb+cr+weeks+spots+channels+grps+tv-1)
summary(model2)
multi_model2<-glmulti(model2, crit="aicc",method="g")
rpt1<-summary(multi_model2)
rpt2<-weightable(multi_model2)

model3 <- glm(Lifetime~larmA+dirrA+sequel-1)
model3<-glmulti(model3,crit="aicc")
rpt1<-summary(model3)
rpt2<-weightable(model3)
print(model3)

