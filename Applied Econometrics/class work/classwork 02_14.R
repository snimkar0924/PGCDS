
library(graphics)
library(rgl)

data("iris")

head(iris)
unique(iris$Species)

pairs(iris[101:150,1:2])

panel.d<-function(x,...)
{
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(usr[1:2],0,0.5), col='black')
  lines(density(x), col=c("blue","green","yellow"))
}

x<-scale(iris[101:150,1:2])
r<-range(x)
pairs(x,diag.panel=panel.d,xlim=r,ylim=r)

library(lattice)
splom(iris[,1:4],groups = iris$Species, col=c('red','blue','green'))
splom(~iris[1:4],groups = iris$Species, col=c(10,20,30),
      cex=c(1,1,0.5),pch=c(1,2,3))

f<-function(x,y)
{
  z<-(1/(2*pi))*exp(-0.5*(x^2+y^2))
}

x<-y<-seq(-3,3,length=50)
z<-outer(x,y,f)

persp(x,y,z)


##books from Prof Hatekar 


data("volcano")
##contour map
image(volcano, col=terrain.colors(100),axes=F)
contour(volcano,asp=1,labcex=0.5)
par(mfrow=c(1,1), col='black')




##missing observations - using statistically sophisticated methods
## to interpolate/ extrapolate missing values.
library(Amelia)
###

###Granger causality
library(lmtest)
library(tseries)
data('ChickEgg')
colnames(ChickEgg)
dim(ChickEgg)
?ChickEgg

chicken<-ts(ChickEgg[,1],start=1930,freq=1)
egg<-ts(ChickEgg[,2],start=1930,freq=1)
adf.test(ChickEgg[,1])
adf.test(ChickEgg[,2])

dchick<-diff(chicken)
degg<-diff(egg)

grangertest(degg~dchick,order=4)
grangertest(dchick~degg,order=1)


######## 








library(tseries)
library(forecast)
?auto.arima







