##인공신경망 실습 iris 실습
library(neuralnet)
library(nnet)
library(caret)
library(e1071)


# 01.데이터 불러오기
data(iris)
str(iris)
head(iris)






# 02.데이터 전치리: 범주형 범수를 가변수로 변환
# 수치형 자료: 표준화 (scale)
# 범주형 자료: 범주화 (class.ind)

iris.df <- cbind(scale(iris[-5]), 
                 class.ind(iris[,]$Species))
str(iris.df)
head(iris.df)







# 03.훈련용, 검증용 데이터 분리
set.seed(1) # 시드 고정 
train.index <- sample(c(1:dim(iris.df)[1]), 
                      dim(iris.df)[1]*0.6)  
train.df <- iris.df[train.index, ]
valid.df <- iris.df[-train.index, ]
head(train.df)





# 04.인공신경망 실행
nn <- neuralnet(setosa
                + versicolor
                + virginica ~ 
                  Sepal.Length 
                + Sepal.Width
                + Petal.Length,
                data = train.df, 
                hidden = 3)






# 05.결과확인 및 그래프 그리기
nn$weights # 가중치 점수 확인
plot(nn, 
     rep="best")






# 06.모델평가
# 훈련데이터
train.pred <- compute(nn,
                      train.df[,c(1:4)])

train.class <- apply(train.pred$net.result,
                     1,
                     which.max)-1

train.class <- factor(train.class,
                      level=c(0,1,2),
                      labels=c("setosa","versicolor","virginica"))

confusionMatrix(as.factor(train.class), 
                as.factor(iris[train.index,]$Species))

# 검정 데이터

valid.pred <- compute(nn,
                      valid.df[,(1:4)])

valid.class <- apply(valid.pred$net.result,
                     1,
                     which.max)-1

valid.class <- factor(valid.class,
                      level=c(0,1,2),
                      labels=c("setosa","versicolor","virginica"))

confusionMatrix(as.factor(valid.class), 
                as.factor(iris[-train.index,]$Species))

