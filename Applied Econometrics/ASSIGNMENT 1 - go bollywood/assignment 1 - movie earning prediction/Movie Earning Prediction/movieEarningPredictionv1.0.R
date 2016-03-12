#clear the workspace of any residual data
setwd("D:/SONALI - DATA/00000 La Vita Nuova/NISM/PGCDS/TERM 1/003 Applied Econometrics/assignment 1 - movie earning prediction/Movie Earning Prediction")
rm(list=ls())

library(glmulti)

boxOfficeData<-read.csv("./data/boxOfficeData_FM.csv", header=TRUE, sep=",")
#dim(boxOfficeData)
attach(boxOfficeData)
cols<-colnames(boxOfficeData)

#generate a regression model using the box office data to model Lifetime earnings of the movie
mod1<-lm(Lifetime~larm1+search_mvkywd+fg+yt_trailerviews+tw_follws-1)
summary(mod1)
multi_mod1<-glmulti(mod1,crit="aicc",level=1)
summary(multi_mod1)
rpt1<-weightable(multi_mod1)
class(rpt1)
write.csv(rpt1,file="./data/models.csv")
#recommended model
recoMod1<-lm(Lifetime~larm1+search_mvkywd-1)
summary(recoMod1)

####### supplementary models 
##determine what drives search_mvkywd
supMod1<-lm(search_mvkywd~tw_follws+rad6_comm+rad6_news+rad6_blgs+rad6_vds+rad6_twsentiP+rad6_twsentiN-1)
summary(supMod1)
supMod1Multi<-glmulti(supMod1,level=1,crit="aicc")
summary(supMod1Multi)
weightable(supMod1Multi)
recoSupMod1<-lm(search_mvkywd~tw_follws+rad6_comm+rad6_news+rad6_blgs+rad6_vds+rad6_twsentiP)
summary(recoSupMod1)
##stick to supModel1

