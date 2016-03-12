#rm(ls())
data<-read.csv("box.csv",header=T,sep=",")
attach(data)
colnames(data)

dat1<-cbind(od,holiday,sequel,Genre,larm1,larf1,dirr1)
model1<-lm(od~larm1+searchv+holiday+grps+I(grps^2)+news-1)
summary(model1)
f<-fitted(model1)
plot(density(od),col="red")
lines(density(f),col="blue")


model2<-lm(Lifetime~larm1+grps+news-1)
summary(model2)
