#setwd("D:\\Sonali\\000 La Vita Nuova\\NISM\\PGCDS\\TERM 1\\003 Applied Econometrics\\assignment #1 - movie earnings")
getwd()

#rm(list=ls())

data<-read.csv("data\\box_FM.csv",header=T,sep=",")
na.omit(data)
attach(data)
dim(data)
colnames(data)

tv_stats <- cbind(tv_weeks,tv_spots,tv_channels,tv_grps,tv_spend)
yt_stats <- cbind(yt_trailerviews,yt_trailerlikes,yt_trailerdislikes,yt_trailercomm,yt_moviesongv,yt_moviesonglikes,yt_moviesongdislikes,yt_moviesongcomm)
fb_stats <- cbind(fb_pagelikes,fb_pageposts,fb_newlikes)
tw_stats <- cbind(tw_follws,tw_twts,tw_faves)
srch_stats <- cbind(search_mvkywd,search_castkywd)
rad_stats <- cbind(rad6_twit,rad6_comm,rad6_news,rad6_agg,rad6_blgs,rad6_vds,rad6_twsentiP,rad6_twsentiN)

model0 <- lm(Lifetime~larm1+Genre1+search_mvkywd-1)
summary(model0)
plot(model0)


model1 <- lm(Lifetime~larm1+Genre1+search_mvkywd-1)
summary(model1)
plot(model1)

