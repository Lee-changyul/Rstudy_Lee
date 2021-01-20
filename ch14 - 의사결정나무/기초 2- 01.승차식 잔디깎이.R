#### Ch06.의사결정나무(II)(교재 Ch09)
### package 설치
# install.packages("rpart") : CART구현 패키지
# install.packages("rpart.plot")
# install.packages("randomForest")
# install.packages("adabag")

## library load
library(rpart) # CART
library(rpart.plot) #CART Tree 그림 


#### 승차식 잔디깎이 예제 Figure 9.7
# 01.데이터 불러오기

mower.df <- read.csv("RidingMowers.csv", 
                      header=TRUE, 
                      na.strings = ".")
str(mower.df)
head(mower.df)

## 02.분류나무(classification tree)만들기
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


class.tree <- rpart(Ownership ~ ., 
                    data = mower.df, 
                    control = rpart.control(maxdepth = ), 
                    method = "class")
class.tree 
summary(class.tree)

## 03. 분류나무 그래프
## help(prp)
# type = 나무그래프 표현 종류
# extra= 추가 정보 표시, 1=노드의 관측수 표시
# under= box 아래 관측값 표시, default=False
# split.font = 글자 font, default=2(bold) 
# varlen = 변수이름 길이, default=-8, 0=full name

prp(class.tree, 
    type = 4, 
    extra = 1, 
    split.font = 1, 
    varlen = -10)  


