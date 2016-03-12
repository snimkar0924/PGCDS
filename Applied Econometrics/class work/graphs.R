library(graphics)
data(iris)
library(rgl)
attach(iris)
pairs(iris[101:150,1:4])
panel.d<-function(x,...) { 
usr<-par("usr") 
on.exit(par(usr))
par(usr=c(usr[1:2],0,0.5))
lines(density(x))
}
x<-scale(iris[101:150,1:4])
r<-range(x)
pairs(x,diag.panel=panel.d,xlim=r,ylim=r)
library(lattice)
splom(iris[101:150,1:4])
splom(iris[,1:4],groups=iris$Species)
splom(~iris[1:4],groups=Species,dat=iris,col=c(10,20,30),pch=c(1,2,3),cex=c(1,1,0.5))
f<-function(x,y){
z<-(1/(2*pi))*exp(-.5*(x^2+y^2))
}
y<-x<-seq(-3,3,length=50)
z<-outer(x,y,f)
persp(x,y,z)
i=5
repeat {
persp(x,y,z,theta=i,phi=i,expand=0.6,ltheta=i,shade=0.75,ticktype="detailed", xlab="X", ylab="Y", zlab="f(x,y)")
i=i+0.5
if(i>360)
break
}
print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species))
print(cloud(Sepal.Length~Petal.Length*Petal.Width,data=iris,groups=Species,main="1",pch=1:3, scales=list(draw=FALSE),zlab="SL",screen=list(z=30,x=-75,y=0)),split=c(1,1,2,2),more=T)

print(cloud(Sepal.Width~Petal.Length*Petal.Width,data=iris,groups=Species,main="2",pch=1:3, scales=list(draw=FALSE),zlab="SW",screen=list(z=30,x=-75,y=0)),split=c(2,1,2,2),more=T)
print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species,main="3",pch=1:3, scales=list(draw=FALSE),zlab="PL",screen=list(z=30,x=-55,y=0)),split=c(1,2,2,2),more=T)
print(cloud(Petal.Width~Sepal.Length*Sepal.Width,data=iris,groups=Species,main="4",pch=1:3, scales=list(draw=FALSE),zlab="PW",screen=list(z=30,x=-55,y=0)),split=c(2,2,2,2),more=T)
data(volcano)
contour(volcano,asp=1,labcex=0.5)
contourplot(volcano)
image(volcano,col=terrain.colors(100),axes=F)
contour(volcano,levels=seq(100,200,by=10),add=T)