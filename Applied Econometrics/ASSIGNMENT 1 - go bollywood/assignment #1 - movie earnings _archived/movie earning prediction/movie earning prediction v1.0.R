#setwd("D:\\Sonali\\000 La Vita Nuova\\NISM\\PGCDS\\TERM 1\\003 Applied Econometrics\\assignment #1 - movie earnings")
getwd()

#rm(list=ls())

data<-read.csv("data\\box_FM.csv",header=T,sep=",")
na.omit(data)
attach(data)
dim(data)
colnames(data)

params<-as.vector(colnames(data))
length(params)
class(params)

##model optimization
##46 data parameters
#for ()
model0 <- lm(Lifetime~larm1+larf1-1)
summ<-summary(model0)
class(summ)
length(summ)
summ[8]

class(model0)
df1<-anova(model0)
dim()
class(df1)
df1[2,5]

p <- 0
q <- 0
for (p in 1:nrow(df1))
{
  for (q in 1:ncol(df1))
  {
    print(df1[p,q])
  }
}
dim(summ$coefficients)

i <- 0
for (i in 1:length(model0))
{
  print(model0[i])
}

#rm(list=ls())
tv_stats <- cbind(tv_weeks,tv_spots,tv_channels,tv_grps,tv_spend)
yt_stats <- cbind(yt_trailerviews,yt_trailerlikes,yt_trailerdislikes,yt_trailercomm,yt_moviesongv,yt_moviesonglikes,yt_moviesongdislikes,yt_moviesongcomm)
fb_stats <- cbind(fb_pagelikes,fb_pageposts,fb_newlikes)
tw_stats <- cbind(tw_follws,tw_twts,tw_faves)
srch_stats <- cbind(search_mvkywd,search_castkywd)
rad_stats <- cbind(rad6_twit,rad6_comm,rad6_news,rad6_agg,rad6_blgs,rad6_vds,rad6_twsentiP,rad6_twsentiN)

model1 <- lm(Lifetime~larm1+search_mvkywd+rad6_twsentiP-1)
summary(model1)


model2<-lm(Lifetime~larm1+tv_spend-1)
summary(model2)
#plot(model1)

model1$model

c1 <- coef(model1)
#residuals(model1)


###generate the linear regression model using combinations of parameters 
##   and identify the top 5 models (least R-squared)
## please note: the first 4 columns in the data file are earnings data and not to be used as params
##   start from column #5
modelstats1 <- c(rep("",length(data)-4))
modelstats2 <- c(rep(0,length(data)-4))
cou <- 0
coucou <- 1
for (cou in 5:ncol(data))
{
  model0 <- lm(Lifetime~data[,cou]-1)
  summ <- summary(model0)
  modelstats1[coucou] <- colnames(data[cou])
  modelstats2[coucou] <- as.numeric(summ[8])
  coucou <- coucou+1
}

##trial
mat <- matrix(nrow=3,ncol=2)
for (i in 1:3)
{
  for (j in 1:2)
  {
    mat[i,j] <- i + j
  }
}

mat[3,2]

class(modelstats2)

which.min(modelstats2)
modelstats1[20]

head(sort(modelstats2,FALSE),5)

n <- 42
r <- 20
combi <- combn(n,r)
ncomb <- factorial(n)/(factorial(r)*factorial(n-r))
class(combi)
dim(combi)

###add combination logic....
numParams <- 0
totNumX <- 10
colOffset <- 4
xCols <- NULL
for (numParams in 1:totNumX)
{
  combi <- combn(totNumX,numParams)
  cou <- 0
  for (cou in 1:ncol(combi))
  {
    i <- 0
    for (i in 1:nrow(combi))
    {
      xCols <- cbind(xCols, data[,colOffset+combi[i,cou]])
    }
    
    model0 <- lm(Lifetime~xCols)
    summary(model0)
  }
}

#rm(xCols)


model0 <- lm(Lifetime~larm1+tv_channels+fb_pagelikes-1)
summary(model0)
















