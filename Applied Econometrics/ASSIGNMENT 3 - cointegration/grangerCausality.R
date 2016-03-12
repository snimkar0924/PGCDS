##granger causality from http://www.sayedhossain.com/
## https://www.youtube.com/watch?v=fazPZwCS3bc

library(lmtest)

gold<-read.csv('./data/Gold_price.csv', stringsAsFactors=FALSE)
nifty<-read.csv('./data/Nifty50_price.csv', stringsAsFactors=FALSE)

dim(gold)
dim(nifty)
gold[,'Date'] = as.Date(gold[,'Date'],format='%d-%b-%y')
nifty[,'Date'] = as.Date(nifty[,'Date'],format='%d-%b-%y')

gold<-gold[which(gold[,'Date'] %in% nifty[,'Date']),]

goldTS<-ts(gold[,3],start=c(2015,03,02),freq=247)
niftyTS<-ts(nifty$Close,start=c(2015,03,02),freq=247)

grangertest(x=niftyTS, y=goldTS, order = 1, na.action = na.omit)
# Ho => nifty does not granger cause gold
# Ha => nifty does granger cause gold
## in this case, p-value = 0.02065 < 0.05 => 
##    can reject null => there exists GC


grangertest(y=niftyTS, x=goldTS, order = 1, na.action = na.omit)
# p-value = 0.08021 > 0.05 => cannot reject null
# => no GC 

##conclusion - confirmed 

grangertest(y=diff(niftyTS), x=diff(goldTS), order = 1, na.action = na.omit)

grangertest(x=diff(niftyTS), y=diff(goldTS), order = 1, na.action = na.omit)


