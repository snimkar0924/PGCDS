##### this version attempts to optimize the code in any manner possible...


#clear the workspace of any residual data
rm(list=ls())
boxOfficeData<-read.csv("./data/boxOfficeData_2014.csv", header=TRUE, sep=",")
attach(boxOfficeData)
cols<-colnames(boxOfficeData)

#read in the list of representative columns - these cols are a subset of the 
## set of explanatory variables (42 total) - these 18 variables have been  
## chosen as most relevant for the Lifetime earnings..  
sampleSet<-as.vector(read.csv("./data/sample set.csv",header=TRUE,sep=","))
sampleModels<-matrix(ncol=2,data=rep(0))
colnames(sampleModels)<-c("R-squared","model")

#############using combinations of the 18 variables to arrive at the "best" model
## based on the respective R-squared values of the generated models.
i<-1
j<-1
k<-1
totalParams<-nrow(sampleSet)
numCombs<-0
cou<-0
modCou<-0
for (i in 1:totalParams)
{
  combi <- combn(totalParams,i)
  numCombs<-ncol(combi)
  for (l in 1:numCombs)
  {
    sampleModels<-rbind(sampleModels,c(rep(0)))
  }
  for (j in 1:numCombs)
  {
    modCou<-modCou+1
    #here, each column of combi is a set of expl vars for the model
    # the number of vars being the num of rows of combi... 
    str1<-""
    for (k in 1:nrow(combi))
    {
      cou<-cou+1
      str1<-paste(str1,sampleSet[combi[k,j],],sep="+")
      otpt<-paste("cou=",cou,"i=",i,"j=",j,"k=",k,"data=",str1,sep=" ")
      print(otpt)
    }#k
    fmla<-as.formula(paste("Lifetime~",str1,"-1"))
    sampleModel<-lm(fmla)
    sampleSummary<-summary(sampleModel)
    sampleModels[modCou,1]<-sampleSummary$r.squared
    sampleModels[modCou,2]<-toString(sampleSummary$terms)
  }#j
}#i
write.table(sampleModels,file="./data/residuals.csv",append=TRUE,row.names = FALSE,sep=",",qmethod="double")

dim(sampleModels)
#[1] 262144      2
indexBstMod<-which.max(sampleModels[,1])
#[1] 262143
bestModel<-sampleModels[indexBstMod,2]

######moving thro' combinations complete - the following code uses the recommended 
## models and related deviants to arrive at the most useful model for 
## ROI determination and earning prediction
#manually inspect the "bestModel" variable and construct required formula string
bstModFrmla<-"Lifetime ~ +mvs + holiday1 + Genre1 + sequel + larm1 + larf1 + dirr1 + tv_weeks + tv_spots + tv_channels + tv_grps + tv_spend + yt_trailerlikes + yt_moviesonglikes + fb_pagelikes + fb_pageposts + tw_faves + search_mvkywd - 1"
bestModel<-lm(bstModFrmla)
summary(bestModel)

goodModFrmla<-"Lifetime ~ larm1 + larf1 + dirr1 + tv_channels + tw_faves + search_mvkywd - 1"
goodModel<-lm(goodModFrmla)
summary(goodModel)

anoGoodModFrmla<-"Lifetime ~ +mvs + holiday1 + Genre1 + sequel + larm1 + larf1 + dirr1 + tv_weeks + tv_spots + tv_channels + tv_grps + tv_spend + yt_trailerlikes + yt_moviesonglikes + fb_pagelikes + fb_pageposts + tw_faves + search_mvkywd - 1"
anoGoodModel<-lm(anoGoodModFrmla)
summary(anoGoodModel)

yetAnoGoodModel<-lm(Lifetime~larm1+search_mvkywd-1)
summary(yetAnoGoodModel)


##conclusion - use "good model"
#summary(goodModel)

####### supplementary models 
##determine what drives search_mvkywd - since search is of significant importance
## in predicting/ influencing Lifetime earnings - attempting to understand what
## "actionable" variables are driving search itself..
supMod<-lm(search_mvkywd~tw_follws+rad6_comm+rad6_news+rad6_blgs+rad6_vds+rad6_twsentiP+rad6_twsentiN-1)
summary(supMod)
anoSupMod<-lm(search_mvkywd~yt_trailerviews+fb_pagelikes+tw_follws+tw_faves+fg+fb_pageposts-1)
summary(anoSupMod)



##model for prediction - due to lack of data for search mv keyword

coefs<-coef(anoSupMod)
searchVol<-coefs[1]*yt_trailerviews+coefs[2]*fb_pagelikes+coefs[3]*tw_follws+coefs[4]*tw_faves+coefs[5]*fg+coefs[6]*fb_pageposts

coefs<-coef(goodModel)
lifetimeEarnings<-coefs[1]*larm1+coefs[2]*larf1+coefs[3]*dirr1+coefs[4]*tv_channels+coefs[5]*tw_faves+coefs[6]*searchVol














