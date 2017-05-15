# RandomForest redone for manuscript.

#**************************************************



library(randomForest)
transposed=read.csv("transposed.csv", header=T, as.is = T)
transposed[,3]=factor(transposed[,3])


#Full oraganism name is too long, will shorted the names to only show order and beyond (e.g, family,genu)
transposednewname <- sub("^.*o__", "", colnames(transposed[ ,2:44]), perl=TRUE)
#transposednewname

names(transposed)[2:44]=transposednewname
#colnames(transposed[ ,2:44])

#_________________________________________________
#only week 8
#transposed
set.seed(20)
transposed$Treatment_study
transposed[,3]
letrozole=subset(transposed,Treatment_study=="8wk.P"|Treatment_study=="8wk.L" )
#training datase
letrozole$Treatment_study=factor(letrozole$Treatment_study)#when i subset i have to turn y variable into a factor or else rf doesnt work
letrozole$Treatment_study

xlearn=letrozole[,4:46]
i=sample(nrow(letrozole),20, rep=F)
ylearn=letrozole[,3]
xtest=xlearn[i,]
xtrain=xlearn[-i,]
ytest=ylearn[i]
ytrain=ylearn[-i]

letrozole8weekvs4weekrf=randomForest(xtrain, ytrain, xtest, ytest, importance = T, ntree = 500)
letrozole8weekvs4weekrf
varImpPlot(letrozole8weekvs4weekrf,type = 1,n.var=10)
importance(letrozole8weekvs4weekrf)

#-_____________placebo
#transposed[,3]
set.seed(20)
placebo=subset(transposed,  Treatment_study=="4wk.l"|Treatment_study=="4wk.p" )
placebo$Treatment_study
# must make my categories into factors after a subset
placebo$Treatment_study=factor(placebo$Treatment_study)

#random forest
xlearn=placebo[ ,4:46]
i=sample(nrow(placebo),20, rep=F)
ylearn=placebo[,3]
xtest=xlearn[i,]
xtrain=xlearn[-i,]
ytest=ylearn[i]
ytrain=ylearn[-i]

placebo8weekvs4weekrf=randomForest(xtrain, ytrain, xtest, ytest, importance = T, ntree = 500)
placebo8weekvs4weekrf
varImpPlot(placebo8weekvs4weekrf,type = 1,n.var=10)
importance(placebo8weekvs4weekrf)

