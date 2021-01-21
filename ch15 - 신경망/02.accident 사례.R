##인공신경망 실습 Table 11.2
# package 설치
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)
library(nnet)
library(caret)
library(e1071)






# 01.데이터 불러오기
accidents.df <- read.csv("accidentsnn.csv", 
                    header=TRUE, 
                    na.strings = ".")
str(accidents.df)
head(accidents.df)






# 02.데이터 전치리: 범주형 범수를 가변수로 변환
# 범주가 여러개일 경우에 더미변수로 변환
# nnet패키지의 class.ind 사용
vars <- c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")
accidents.ctg <- cbind(accidents.df[,c(vars)],
                       class.ind(accidents.df[,]$SUR_COND),
                       class.ind(accidents.df[,]$MAX_SEV_IR))
names(accidents.ctg) <- c(vars, 
                      paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), 
                      paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))
str(accidents.ctg)
head(accidents.ctg)






# 03.훈련용, 검증용 데이터 분리
set.seed(2) # 시드 고정 
train.index <- sample(c(1:dim(accidents.ctg)[1]), 
                      dim(accidents.ctg)[1]*0.6)  
train.df <- accidents.ctg[train.index, ]
valid.df <- accidents.ctg[-train.index, ]
head(train.df)







# 04.인공신경망 실행
nn <- neuralnet(MAX_SEV_IR_0 
                + MAX_SEV_IR_1 
                + MAX_SEV_IR_2 ~ 
                  ALCHL_I 
                + PROFIL_I_R 
                + VEH_INVL 
                + SUR_COND_1 
                + SUR_COND_2 
                + SUR_COND_3 
                + SUR_COND_4, 
                data = train.df, 
                hidden = 2)






# 05.결과확인 및 그래프 그리기
nn$weights # 가중치 점수 확인
plot(nn, 
     rep="best")






# 06.모델평가
# 훈련데이터
train.pred <- compute(nn,
                      train.df[,-c(8:11)])

train.class <- apply(train.pred$net.result,
                     1,
                     which.max)-1

confusionMatrix(as.factor(train.class), 
                as.factor(accidents.df[train.index,]$MAX_SEV_IR))

# 검정 데이터

valid.pred <- compute(nn,
                      valid.df[,-c(8:11)])

valid.class <- apply(valid.pred$net.result,
                     1,
                     which.max)-1

confusionMatrix(as.factor(valid.class), 
                as.factor(accidents.df[-train.index,]$MAX_SEV_IR))

