#### Ch06.의사결정나무(II)(교재 Ch09)
### package 설치
# install.packages("rpart") : CART구현 패키지
# install.packages("rpart.plot")
# install.packages("randomForest")
# install.packages("adabag")

## library load
library(rpart) # CART
library(rpart.plot) #CART Tree 그림 
library(caret) #모델성능평가 (ConfusionMatrx)




#### 유니버설 은행 사례 Figure 9.9 ####
## 01.데이터 불러오기
bank.df <- read.csv("UniversalBank.csv", 
                     header=TRUE, 
                     na.strings = ".")



## 02.전치리작업: 범주형 변수(factor)로 인식하게 변환
bank.df$Personal.Loan <- factor(bank.df$Personal.Loan,
                                levels=c(0,1),
                                labels=c("No","Yes"))
# ID, 우편번호 제거
bank.df <- bank.df[ , -c(1, 5)]  
str(bank.df)
head(bank.df)




## 03.훈련용, 검증용 데이터 분리: partition
set.seed(1) # 시드 고정 
train.index <- sample(c(1:dim(bank.df)[1]), 
                      dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
head(train.df)




## 04.분류나무 생성 classification tree
## rpart(): CART
# formula = 목표변수 ~ 입력변수
# data = 사용 셋트
# method = class(분류모형), anova(회귀모형)
# parms = gini(default)
# rpart.control()=정지규칙에 관한 설정

## rpart.control
## help(rpart.control)
# xval = 교차타당성 fold 갯수, default=10
# cp = 오분류율 기준(비용복잡도에 사용할 알파), cp=0 최소값, default=0.01
# 오분류율이 cp값 미만이면 stop
# minsplit = 마디의 최소 관측갯수, default=20
# minbucket =끝마디의 최소 관측개수, minsplit의 1/3
# maxdepth = : 나무의 최대 깊이를 결정(the depth of the tree), default=30

default.ct <- rpart(Personal.Loan ~ ., 
                    data = train.df, 
                    method = "class")
summary(default.ct)
show(default.ct)




##05.분류나무 그래프
# help(prp)
# type = 나무그래프 표현 종류
# extra= 추가 정보 표시, 1=노드의 관측수 표시
# under= box 아래 관측값 표시, default=False
# split.font = 글자 font, default=2(bold) 
# varlen = 변수이름 길이, default=-8, 0=full name
# box.col=box 색깔

prp(default.ct, 
    type = 1, 
    extra = 1, 
    under = TRUE, 
    split.font = 1, 
    varlen = -10,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))




## 06.완전모형생성: Figure 9.10
# cp = 0, minsplit = 1
deeper.ct <- rpart(Personal.Loan ~ ., 
                   data = train.df, 
                   method = "class", 
                   cp = 0, 
                   minsplit = 1)
deeper.ct
#나무모델 노드 개수
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

#분류나무 그래프
prp(deeper.ct, 
    type = 1, 
    extra = 1, 
    under = TRUE, 
    split.font = 1, 
    varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  




## 07.정오행렬표를 이용한 모델검증: Table 9.3
pred.class <- predict(default.ct,
                      valid.df,
                      type = "class")
confusionMatrix(pred.class, 
                valid.df$Personal.Loan)




## 08.가지치기(Pruning the Tree)를 위한 CP계산: Table 9.4
## 교차 타당도 검정
## rpart.control: xval=교차타당성 fold 갯수, default=10
## minsplit = 5

cv.ct <- rpart(Personal.Loan ~ ., 
               data = train.df, 
               method = "class", 
               cp = 0.00001, 
               minsplit = 5, 
               xval = 5)

printcp(cv.ct) # 결과 중에서 CP만 가져오기
plotcp(x=cv.ct) # CP결과를 그래프로 그리기




## 09.CP결과를 이용한 가지치기(prune by lower cp):  Figure 9.12
## 교차검정에러(xerror) 이용 
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable
                   [which.min(cv.ct$cptable[,"xerror"]), "CP"])

length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

prp(pruned.ct, 
    type = 1, 
    extra = 1, 
    split.font = 1, 
    varlen = -10)  





##10.최적 가지치기 결과 Figure 9.13
## 샘플링오차(xerror) 기준(최적) 이용
## minsplit = 1
set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., 
               data = train.df, 
               method = "class", 
               cp = 0.00001, 
               minsplit = 1, 
               xval = 5)  

printcp(cv.ct)

pruned.ct <- prune(cv.ct, 
                   cp = 0.0154639 )
prp(pruned.ct, 
    type = 1, 
    extra = 1, 
    under = TRUE, 
    split.font = 1, 
    varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

pred.class <- predict(pruned.ct,
                      valid.df,
                      type = "class")
confusionMatrix(as.factor(pred.class), 
                as.factor(valid.df$Personal.Loan))



