#clear the workspace of any residual data
setwd("D:/SONALI - DATA/00000 La Vita Nuova/NISM/PGCDS/TERM 1/003 Applied Econometrics/assignment 1 - movie earning prediction/Movie Earning Prediction")
rm(list=ls())

library(glmulti)

boxOfficeData<-read.csv("./data/boxOfficeData_FM.csv", header=TRUE, sep=",")
#dim(boxOfficeData)
attach(boxOfficeData)
cols<-colnames(boxOfficeData)

#generate a regression model using the box office data to model Lifetime earnings of the movie
#model1<-lm(Lifetime~mvs+holiday1+Genre1+sequel+larm1+larf1+dirr1+tv_grps-1)
model1<-lm(Lifetime~mvs+larm1+tv_grps-1)
summary(model1)
multi_model1<-glmulti(model1,crit="aicc",method="g")
summary(multi_model1)
weightable(multi_model1)



##
str<-cols[5]
for (i in 6:length(cols))
{
  str<-paste(str,"+",cols[i])
}

#str ->
#"mvs + holiday1 + Genre1 + sequel + larm1 + larf1 + dirr1 + fg + imdb + cr + tv_weeks + tv_spots + tv_channels + tv_grps + tv_spend + reach1 + reach3 + reach5 + yt_trailerviews + yt_trailerlikes + yt_trailerdislikes + yt_trailercomm + yt_moviesongv + yt_moviesonglikes + yt_moviesongdislikes + yt_moviesongcomm + fb_pagelikes + fb_pageposts + fb_newlikes + tw_follws + tw_twts + tw_faves + search_mvkywd + search_castkywd + rad6_twit + rad6_comm + rad6_news + rad6_agg + rad6_blgs + rad6_vds + rad6_twsentiP + rad6_twsentiN"
model2<-lm(Lifetime~mvs + holiday1 + Genre1 + sequel + larm1 + larf1 + dirr1 + fg + imdb + cr + tv_weeks + tv_spots + tv_channels + tv_grps + tv_spend + reach1 + reach3 + reach5 + yt_trailerviews + yt_trailerlikes + yt_trailerdislikes + yt_trailercomm + yt_moviesongv + yt_moviesonglikes + yt_moviesongdislikes + yt_moviesongcomm + fb_pagelikes + fb_pageposts + fb_newlikes + tw_follws + tw_twts + tw_faves + search_mvkywd + search_castkywd-1)# + rad6_twit + rad6_comm + rad6_news + rad6_agg + rad6_blgs + rad6_vds + rad6_twsentiP + rad6_twsentiN-1)
summary(model2)
multi_model2<-glmulti(model2,crit="aicc")
summary(multi_model2)
weightable(multi_model2)
#model2 -> too large a set of explanatory variables for glmulti to process


###so now try a set of explanatory variables that makes sense from a movie business perspective


model3<-lm(Lifetime~mvs + holiday1 + Genre1 + sequel + larm1 + larf1 + dirr1 + tv_weeks + tv_spots + tv_channels + tv_grps + tv_spend + yt_trailerlikes + yt_moviesonglikes + fb_pagelikes + fb_pageposts + tw_faves + search_mvkywd -1)
summ<-summary(model3)
summ[9]
multi_model3<-glmulti(model3,crit="aicc",method="g")

###now try a combination from the set (mvs + holiday1 + Genre1 + sequel + larm1 + larf1 + dirr1 + tv_weeks + tv_spots + tv_channels + tv_grps + tv_spend + yt_trailerlikes + yt_moviesonglikes + fb_pagelikes + fb_pageposts + tw_faves + search_mvkywd )
sampleSet<-as.vector(read.csv("./data/sample set.csv",header=TRUE,sep=","))
sampleResids<-c(rep(0,nrow(sampleSet)))

#one parameter
for (i in 1:nrow(sampleSet))
{
	lst<-as.vector(boxOfficeData[toString(sampleSet[i,1])])
	fmla<-as.formula(paste("Lifetime~",colnames(lst),"-1"))
	sampleModel<-lm(fmla)

	sampleSummary<-summary(sampleModel)
	sampleResid[i]<-sampleSummary[9]
}

#two params


write.csv(sampleResid,"./data/residuals.csv")


which.max(sampleResid)


n <- nrow(sampleSet)
#r <- 0
cou<-0
numCombs=NULL
for (k in 1:n)
{
	ncomb <- factorial(n)/(factorial(k)*factorial(n-k))
	cou<-cou+1
	numCombs[cou]<-ncomb
}
combi <- combn(18,9)
ncomb <- factorial(18)/(factorial(9)*factorial(18-9))
dim(combi)
ncomb



mod<-lm(Lifetime~search_mvkywd-1)
summary(mod)

mod11<-lm(Lifetime~larm1+search_mvkywd+fg+yt_trailerviews+tw_follws-1)
summary(mod11)
multi_mod11<-glmulti(mod11,crit="aicc",level=1)
summary(multi_mod11)
weightable(multi_mod11)
mod22<-lm(Lifetime~tw_follws+rad6_comm+rad6_news+rad6_blgs+rad6_vds+rad6_twsentiP+rad6_twsentiN-1)
summary(mod22)

mod<-lm(Lifetime~sequel+larm1+tv_grps+search_mvkywd-1)

multi_mod<-glmulti(mod,crit="aicc",level=1)
summary(multi_mod)
weightable(multi_mod)

mod1<-lm(Lifetime~-1+tv_spend+larm1:sequel+search_mvkywd:larm1)
mod2<-lm(Lifetime~tv_spend+larm1+sequel+search_mvkywd-1)

mod3<-lm(Lifetime~larm1+tv_grps+search_mvkywd+tv_grps:larm1+search_mvkywd:larm1+search_mvkywd:tv_grps-1)
summary(mod3)
multi_mod<-glmulti(mod,crit="aicc",level=1)
summary(multi_mod)
weightable(multi_mod)


summary(mod1)
summary(mod2)

####### supplementary models 
##determine what drives search_mvkywd
supMod1<-lm(search_mvkywd~tw_follws+rad6_comm+rad6_news+rad6_blgs+rad6_vds+rad6_twsentiP+rad6_twsentiN-1)
summary(supMod1)
supMod1Multi<-glmulti(supMod1,level=1,crit="aicc")
summary(supMod1Multi)
weightable(supMod1Multi)


mod<-lm(search_mvkywd ~ 1 + tw_follws + rad6_comm + rad6_news + rad6_blgs + rad6_vds + rad6_twsentiP)
summary(mod)








