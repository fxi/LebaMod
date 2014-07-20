library(caret)
## sample code to extract prob from lebamod object

testMod<-readRDS('../data/models/Alainites_muticus_rf_q50_1_tss_0.87_1401996940')
j<-testMod$lebaData

## extract probabilities from model, for test data
# print data.frame
j$testing
j$training

testProb<-extractProb(list(testMod$caretFit),testX=j$testing[,j$pLoCorr],testY=j$testing$class)
confusionMatrix(testProb$pred,testProb$obs,positive='PRES')
customSummary(testProb, lev=c('PRES','ABS'))



