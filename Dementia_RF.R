# importing csv
data <- read.csv("data.csv")
df <- data
# dropping uncessary columns
df <- df[,c(-1,-2,-3,-4,-5,-6,-8)]
df$CDR <- ifelse(df$CDR >= 1,1,df$CDR)
df$CDR <- as.factor(df$CDR)
df$M.F <- as.factor(ifelse(df$M.F=="F","Female","Male"))

library(ggplot2)
# age vs CDR
ggplot(df, aes(CDR , Age))+
geom_boxplot(col = "blue")+
ggtitle("Degree of CDR by Age")+
xlab("CDR")+
theme(plot.title = element_text(hjust = .5))
# age vs CDR by sex
ggplot(df, aes(CDR, Age, fill = M.F))+
geom_boxplot()+
ggtitle("Degree of CDR by Age")+
xlab("CDR")+
theme(plot.title = element_text(hjust = .5))
# removing outliers, majority of 85+ age show CDR=0.
#So remove two obs. from CDR=1 having age > 85
df[df$CDR==1 & df$Age>85,]
df <- df[c(-176,-177),]
# split data
set.seed(123)
train.data <- df[sample(nrow(df),round(nrow(df)*0.8)) , ]
test.data <- df[-sample(nrow(df),round(nrow(df)*0.8)) ,]
# model
library(randomForest)
set.seed(100)
rf.mod<-randomForest(CDR~., data = train.data, importance =T)
print(rf.mod)
importance(rf.mod, type = 1)
varImpPlot(rf.mod)
# test pred
library(caret)

pred <- predict(rf.mod,test.data)
confusionMatrix(pred,test.data$CDR)
# rf error rate
plot(rf.mod) # taking num trees = 100
# tuning
tune.rf <- tuneRF(train.data[ , -8], train.data[ , 8],
stepFactor = 0.75,
plot=T,
ntreeTry = 100,
trace = T,
improve = 0.05)
# mtry = 3
rf.mod1<-randomForest(CDR~MMSE+Age+nWBV+M.F+ASF+eTIV+EDUC, data = train.data , ntree=100 ,
mtry=3,
importance=T,
proximity=T)
print(rf.mod1)
# test with new mod
pred1 <- predict(rf.mod1,test.data)
confusionMatrix(pred1,test.data$CDR)
# importing another csv to test accuracy
testdata <- read.csv('testdata.csv')
test1 <- testdata
test1$CDR <- ifelse(test1$CDR >= 1,1,test1$CDR)
test1$CDR <- as.factor(test1$CDR)
test1$M.F <- as.factor(ifelse(test1$M.F=="F","Female","Male"))

# testing model
pred.test <- predict(rf.mod1,test1)
confusionMatrix(pred.test,test1$CDR)

saveRDS(rf.mod1,'model_dropSES.RDS')
saveRDS(rf.mod1,'model2.rds')
saveRDS(rf.mod1,'model3.rds')
#setwd('C:\\Users\\Soham\\Documents\\Demrntiaclassifier')
v <- readRDS('model1.rds')
pred.test <- predict(v,test1)
confusionMatrix(pred.test,test1$CDR)

# random forest gave the best accuracy as well as the least
# false negative as compared to SVM, Logistic Regression
# we will save the model RDS file named model_dropSES.RDS
# as this model performed best as compared to
# other combinations in other RF models