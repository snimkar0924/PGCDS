
dat<-read.csv("./data/train.csv")
dim(dat)
head(dat[,1:3])
str(dat)

N<-round(nrow(dat)*0.7)

#split data into trainig and validation in 70/30 proportion
trnDat<-dat[1:N, ]
valDat<-dat[(N+1):nrow(dat), ]

trnDat<-na.omit(trnDat)
dim(trnDat)
valDat<-na.omit(valDat)
dim(valDat)

vars<-colnames(dat) ##interchangeable with trnDat for this purpose
vars1<-c()
cou1<-1
for (j in 1:length(vars))
{
  if (length(unique(trnDat[,vars[j]])) > 1)
  {
    vars1[cou1] <- vars[j]
    cou1 <- cou1 + 1
  }
}

l<-length(vars1)-1
  
models<-vector()
cou <- 0

for (i in 2:l)
{
  formulaStr<-"TARGET~%s%-1"
  
  formulaStr<-gsub(formulaStr, pattern="%s%", replacement=vars1[i])
  print(formulaStr)

  model<-glm(formulaStr, data=trnDat, 
             family=binomial)
  #summary(model)
  #str(model)
  
  if (as.double(coef(summary(model))[4]) <= 0.05)
  {
    cou<- cou + 1
    models[cou]<-model
  }
  
}#for loop

length(models)

allFormStr<-"TARGET~-1"
for (k in 1:length(models))
{
  allFormStr<-paste(allFormStr, "+", names(models[[k]][1]), sep="")
}


bigmodel<-glm(allFormStr, data=trnDat, 
           family=binomial)
summary(bigmodel)

cfs<-(coef(summary(bigmodel)))

cfs1<-cfs[which(cfs[,4] <= 0.05),]
dim(cfs1)
rownames(cfs1)[1]

allFormStr<-"TARGET~-1"
for (k in 1:nrow(cfs1))
{
  allFormStr<-paste(allFormStr, "+", rownames(cfs1)[k], sep="")
}

model1<-glm(allFormStr, data=trnDat, 
              family=binomial)
summary(model1)



actual<-valDat$TARGET

bmodel<-c()
cfs<-model1$coefficients
names(cfs)
length(cfs)

head(valDat[,names(cfs)])

head(valDat[,names(cfs)])

bmodel<-rowSums(valDat[,names(cfs)]*cfs)
head(bmodel)

bmodel<-1/(1+exp(-1*bmodel))

# bmodel<-1/(1+exp(-1*(cfs[1]*val$Concept.2
#                      +cfs[2]*val$Concept.5
#                      +cfs[3]*val$Concept.6
#                      +cfs[4]*val$Concept.9)))

length(which(actual<threshold))
length(which(actual>=threshold))


length(which(bmodel<threshold))
length(which(bmodel>=threshold))

plot(bmodel, pch=20)
threshold<-0.3
predicted<-c()
predicted[bmodel>=threshold]<-1
predicted[bmodel<threshold]<-0

# plot(predicted, col='lightblue')
# points(actual, col='pink')

##ROC et al
confusion_matrix=matrix(0,nrow=2,ncol=2)
rownames(confusion_matrix)<-c('act 1', 'act 0')
colnames(confusion_matrix)<-c('pre 1', 'pre 0')

confusion_matrix[1,1]<-length(which((predicted==1) 
                                    & (actual==1)))

confusion_matrix[2,2]<-length(which((predicted==0) 
                                    & (actual==0)))

confusion_matrix[1,2]<-length(which((actual==1) 
                                    & (predicted==0)))

confusion_matrix[2,1]<-length(which((actual==0) 
                                    & (predicted==1)))
confusion_matrix
err<-(confusion_matrix[1,2]+confusion_matrix[2,1])/sum(confusion_matrix)
perr<-err*100
sens<-confusion_matrix[1,1]/sum(confusion_matrix[,1])*100
spec<-confusion_matrix[2,2]/sum(confusion_matrix[,2])*100
print(perr)
print(sens)
print(spec)




############################

##Text Mining


# articles<-read.csv('./class work 03_20/article_data.csv')
# dim(articles)
# 
# N<-nrow(articles)
# trnCutOff<-60
# 
# ##in this case, pick up every other row for each set
# cou=0
# s<-seq(1,N)
# idx = c()
# while (T)
# {
#   cou = cou + 1
#   idx <- unique(round(runif(N, s[1], N)))
#   print(cou)
#   if (length(unique(idx))>=trnCutOff) break()
# }
# length(idx)
# d<-length(idx)-trnCutOff
# idx<-idx[1:(length(idx)-d)]
# 
# ##now we have 60 unique elements in idx
# trn<-articles[idx,]
# dim(trn)
# 
# idx1<-which(!(rownames(articles) %in% idx))
# length(idx1)
# 
# val<-articles[idx1,]
# dim(val)
# n<-nrow(val)
# tst<-val[((n/2+1):n),]
# dim(tst)
# 
# val<-val[(1:(n/2)),]
# dim(val)
# 
# colnames(articles)
# 
# model1<-glm(formula=trn$d_type~trn$Concept.1+trn$Concept.2
#             +trn$Concept.3+trn$Concept.4
#             +trn$Concept.5+trn$Concept.6
#             +trn$Concept.7+trn$Concept.8
#             +trn$Concept.9+trn$Concept.10-1
#             ,data=trn, family='binomial')
# summary(model1)
# 
# # model2<-glm(formula=trn$d_type~trn$Concept.2
# #                               +trn$Concept.4
# #                               +trn$Concept.5+trn$Concept.6
# #                               +trn$Concept.9-1
# #             ,data=trn, family='binomial')
# # summary(model2)
# 
# model2<-glm(formula=trn$d_type~trn$Concept.2
#             +trn$Concept.5+trn$Concept.6
#             +trn$Concept.9-1
#             ,data=trn, family='binomial')
# summary(model2)
# 
# actual<-val$d_type
# 
# bmodel<-c()
# cfs<-model2$coefficients
# bmodel<-1/(1+exp(-1*(cfs[1]*val$Concept.2
#                      +cfs[2]*val$Concept.5
#                      +cfs[3]*val$Concept.6
#                      +cfs[4]*val$Concept.9)))
# plot(bmodel)
# threshold<-0.5
# predicted<-c()
# predicted[bmodel>=threshold]<-1
# predicted[bmodel<threshold]<-0
# 
# plot(predicted, col='blue')
# points(actual, col='red')
# 
# ##ROC et al
# confusion_matrix=matrix(0,nrow=2,ncol=2)
# rownames(confusion_matrix)<-c('act 0', 'act 1')
# colnames(confusion_matrix)<-c('pre 0', 'pre 1')
# 
# confusion_matrix[1,1]<-length(which((actual==predicted) & (predicted==0)))
# confusion_matrix[2,2]<-length(which((actual==predicted) & (predicted==1)))
# confusion_matrix[1,2]<-length(which((actual!=predicted) & (predicted==0)))
# confusion_matrix[2,1]<-length(which((actual!=predicted) & (predicted==1)))
# confusion_matrix
# err<-(confusion_matrix[1,2]+confusion_matrix[2,1])/sum(confusion_matrix)
# perr<-err*100
# print(perr)
# 
# ##then iterate thro' the model generation process for the best model 
# # based on data swirling/ randomizing, AIC and ROC values
# 
# ### then use k-means clustering (k=2) to divide the concepts (explanatory variables)
# ## into 2 partitions and determine if the clusters map to Auto and Electronics
# 
# 
# 
# 



