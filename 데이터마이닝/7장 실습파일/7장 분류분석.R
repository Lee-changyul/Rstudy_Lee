# 7장 분류분석

setwd("C:/R")

install.packages("party")
install.packages("caret")
install.packages("e1071")
library(party)
library(caret)
library(e1071)

##1. 의사결정나무 

# 종속변수가 연속인경우
data("airquality")
str(airquality)

air_tree <-ctree(Temp ~ Solar.R+Wind+Ozone, data=airquality)
air_tree
plot(air_tree)


# 종속변수가 범주형인 경우 
titanic <- read.csv("titanic.csv")
str(titanic)
titanic <- titanic[,-c(3,8)]
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
str(titanic)

set.seed(1234)
inTrain <- createDataPartition(titanic$Survived, p=0.7, list=FALSE)
titanic_train <- titanic[inTrain,]
titanic_test <- titanic[-inTrain,]

titanic_tree <-ctree(Survived ~ .,  data=titanic_train) #.은 모든 변수를 의미함
titanic_tree
plot(titanic_tree)

titanic_pred <- predict(titanic_tree, titanic_test)
confusionMatrix(titanic_pred, titanic_test$Survived)

# 2. 랜덤포레스트

install.packages("randomForest")
library(randomForest)

# 사례: 종속변수가 범주형인 경우
act <- read.csv("activity.csv")
colnames(act)

modFit.rf <- randomForest::randomForest(classe ~ ., data = act)
modFit.rf

varImpPlot(modFit.rf, pch = 20, main = "Importance of Variables")


# 사례: 종속변수가 연속인경우
house <- read.csv("Bostonhouse.csv")
str(house)

house.rf <- randomForest::randomForest(주택가격 ~ ., data=house, importance=T)
house.rf

varImpPlot(house.rf, pch = 20, main = "Importance of Variables")


# 비교사례
titanic <- read.csv("titanic.csv")
colnames(titanic)
str(titanic)
titanic <- titanic[,-c(3,8)]
colnames(titanic)
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
str(titanic)

titanic.rf <- randomForest::randomForest(Survived ~ ., data=titanic, importance=T)
titanic.rf

varImpPlot(titanic.rf, pch = 20, main = "Importance of Variables")


# 3. 로짓분석

#사례 1: 이항로짓분석
job<-read.csv("job.csv") 
str(job)
attach(job)

job$동아리참여여부  <- as.factor(job$동아리참여여부)
job$취업여부  <- as.factor(job$취업여부)
str(job)

logitjob <- glm(취업여부 ~ 취업프로그램참여횟수+동아리참여여부 , binomial, data=job)
summary(logitjob)

exp(logitjob$coef[2:3])
logistic.display(logitjob)


# 사례 2: 다항 로짓분석

install.packages("nnet")
library(nnet)
act <- read.csv("activity.csv")

mlogit_act <- multinom(classe ~ ., data=act)
head(fitted(mlogit_act), 5)
predict(mlogit_act)

predicted <- predict(mlogit_act, newdata = act)
sum(predicted==act$classe)/NROW(act) 

xtabs(~predicted+act$classe)


#비교사례 3 : Titanic
install.packages("reghelper")
library(reghelper) # 표준화 계수 구하기 위한 패키지

titanic <- read.csv("titanic.csv")
str(titanic)

titanic <- titanic[,-3]
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
str(titanic)

logit_titanic <- glm(Survived ~ ., binomial, data = titanic)
summary(logit_titanic)

exp(logit_titanic$coef[2:8]) # 지수함수(exp)를 사용하여 로그오즈비(승산비)를 오즈비(승산비)로 전환함
#install.packages("epiDisplay")
library(epiDisplay)
logistic.display(logit_titanic)


beta(logit_titanic)








