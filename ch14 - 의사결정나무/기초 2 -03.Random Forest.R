#### Ch06.의사결정나무(II)(교재 Ch09)
### package 설치
# install.packages("rpart") : CART구현 패키지
# install.packages("rpart.plot")
# install.packages("randomForest")
# install.packages("adabag")


## 01.Random Forest: Figure 9.15
library(randomForest)

## random forest
# ntree = 트리 갯수
# mtry = 트리에 포함할 변수 갯수
# importance = 중요도 출력
# nodesize = 끝마디(terminal nodes)의 최종개수

rf <- randomForest(Personal.Loan ~ ., 
                   data = train.df, 
                   ntree = 500, 
                   mtry = 4, 
                   nodesize = 5, 
                   importance = TRUE)  

## 02.변수 중요도 도표: variable importance plot
varImpPlot(rf, type = 1)

## 랜덤 포레스트의 정오분류표: confusion matrix
rf.pred <- predict(rf, 
                   valid.df)

confusionMatrix(rf.pred, 
                as.factor(valid.df$Personal.Loan))



##03.Boosted tree 사용: Table 9.5
# install.packages("adabag")
library(adabag)
library(rpart) 
library(caret)

set.seed(1)
boost <- boosting(Personal.Loan ~ ., 
                  data = train.df)
pred <- predict(boost, 
                valid.df)
confusionMatrix(as.factor(pred$class), 
                as.factor(valid.df$Personal.Loan))
