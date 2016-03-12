##

data(UScitiesD)
data(airmiles)
data(faithful)

colnames(faithful)

head(faithful)
str(faithful)
dim(faithful)
getwd()
write.csv(faithful, './data/faitful.csv')

help(faithful)

library(MASS)
data(geyser)
head(geyser)
str(geyser)
dim(geyser)

write.csv(geyser, './data/geyser.csv')

