library(ROSE)
library(Rcpp)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)
library(ISLR)
library(randomForest)
library(caret)
library(e1071)


#*****************************************************************************************************************
################# RANDOM FOREST ###################################################################################

#install.packages(c('rpart.plot','party','ISLR'))

data <- read.csv('ABCD_states.csv')
View (data)

data$state=as.factor(data$state)
data$no_show <- as.factor(data$no_show)
data$gender=as.factor(data$gender)
data$scholarship <- as.factor(data$scholarship)
data$hipertension <- as.factor(data$hipertension)
data$diabetes <- as.factor(data$diabetes)
data$alcoholism <- as.factor(data$alcoholism)
data$sms_received <- as.factor(data$sms_received)
data$handcap <- as.factor(data$handcap)

class(data$age)
data$age <- cut(data$age, breaks=c(0,18,65,99), labels=c(1,2,3))
data$age <- as.factor(data$age)
str(data)


set.seed(123)
ind <- sample(2,nrow(data),replace=TRUE, prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]
View(training)
class(training$no_show)
class(testing$no_show)
dim(training)
dim(testing)

table(testing$no_show)

table(training$no_show)
#install.packages('randomForest')
library(randomForest)
set.seed(123)
# 
# # Random forest without oversampling does horribly bad performance
# rf <- randomForest(no_show~difference+state+sms_received+age+gender, data=training,ntree=400,mtry=3.464)
# print(rf)
# attributes(rf)
# confusionMatrix(predict(rf, testing),testing$no_show,positive='1')
# #table(training$no_show)

# Random forest with oversampling
overtrain<- ovun.sample(no_show~., data=training, method="under", N=34450)$data
table(overtrain$no_show)
summary(overtrain)
overtest<- ovun.sample(no_show~., data=testing, method="under", N=8812)$data
table(overtest$no_show)

rfover <- randomForest(no_show~difference+state+sms_received+age+gender, data=overtrain,ntree=70,mtry=3.464)
cm <- confusionMatrix(predict(rfover, overtest),overtest$no_show,positive='1')
#summary(rfover)
preds <- predict(rfover, overtest)
class(preds)
head(preds)
act <- overtest$no_show
class(act)
head(act)

attributes(cm)

plot(rfover)
varImpPlot(rfover)

ttest <- t.test(as.numeric(preds), as.numeric(act))
ttest
cm <- confusionMatrix(preds,act)

cm

cm$byClass

f1 <- cm$byClass['F1']
pre <- cm$byClass['Precision']
rec <- cm$byClass['Recall']

cat('Precision is:',pre,'\n')
cat('Recall is:',rec,'\n')
cat('F1 Score is: ',f1,'\n')


getwd()
hist(treesize(rfover),col='green')
print (rfover)

