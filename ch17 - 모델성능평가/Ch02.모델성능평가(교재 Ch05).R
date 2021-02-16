#### Ch02.모형평가(교재 Ch05)
#### package 설치
# install.packages("forecast") # 모형평가 도구
# install.packages("gains") #gain
# install.packages("caret")
# install.packages("e1071")
# install.packages("pROC")







#### 예측모델의 성능평가 Table 5.1
library(forecast)

# 파일 불러오기
tc.df <- read.csv("ToyotaCorolla.csv", 
                              header=TRUE, 
                              na.strings = "."
)
dim(tc.df) # 데이터와 변수 갯수
str(tc.df) # 데이터 구조확인

library(tidyverse)

tc.df$Model <- str_replace_all(tc.df$Model, "쟕OYOTA", "TOYOTA") 


# 훈련용(training)과 검증용(validation)으로 분리
# sample함수 이용
# setdiff = 차집합, setdiff(x, y)= x에서 y를 제외함
training <- sample(tc.df$Id, 600) 
validation <- sample(setdiff(tc.df$Id, training), 400) 

# 회귀분석(예측모델) 구축
# na.exclude=결측데이터 제거
reg <- lm(Price~., 
          data=tc.df[,-c(1,2,8,11)], 
          subset=training,
          na.action=na.exclude) 

# 학습데이터를 이용한 예측 (predict)
# na.pass=결측데이터 무시(통과)
pred_t <- predict(reg, na.action=na.pass) 

summary(reg)

# 검증데이터를 이용한 예측 (predict)
# newdata=예측을 위한 새로운 데이터
# -c(1,2,8,11)=1,2,8,11번 변수 제외
pred_v <- predict(reg, newdata=tc.df[validation,-c(1,2,8,11)],
                  na.action=na.pass)


## 성능평가(evaluate performance)
# training
accuracy(pred_t, tc.df[training,]$Price)
# validation
accuracy(pred_v, tc.df[validation,]$Price)











#### lift chart (Figure 5.2)
# remove missing Price data
tc.df <-     
  tc.df[!is.na(tc.df[validation,]$Price),]

# generate random Training and Validation sets
training <- sample(tc.df$Id, 600)
validation <- sample(setdiff(tc.df$Id, training), 400) 

# regression model based on all numerical predictors
reg <- lm(Price~., data = tc.df[,-c(1,2,8,11)], subset = training)

# predictions
pred_v <- predict(reg, newdata = tc.df[validation,-c(1,2,8,11)])

# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
gain <- gains(tc.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])

# cumulative lift chart
options(scipen=999) # avoid scientific notation
# we will compute the gain relative to price
price <- tc.df[validation,]$Price[!is.na(tc.df[validation,]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

# baseline
lines(c(0,sum(price))~c(0,dim(tc.df[validation,])[1]), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")










#### 정오행렬(confusion Matrix)(Table 5.5)
library(caret)
library(e1071)
library(tidyverse)

owner.df <- read_csv("ownerExample.csv", 
                      col_names = TRUE,
                      locale=locale('ko', encoding='euc-kr'),
                      na=".") %>% # csv 데이터 읽어오기
    mutate_if(is.character, as.factor)  
  
str(owner.df)

confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                owner.df$Class)











#### 컷오프 전체오차 (Figure 5.4)

# replace data.frame with your own

le.df <- read_csv("liftExample.csv", 
                     col_names = TRUE,
                     locale=locale('ko', encoding='euc-kr'),
                     na=".") %>% # csv 데이터 읽어오기
  mutate_if(is.character, as.factor)  

# create empty accuracy table
accT = c() 

# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(as.factor(1 * (le.df$prob > cut)), as.factor(le.df$actual))
  accT = c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)











#### ROC 커브(Figure 5.5)
library(pROC)
r <- roc(le.df$actual, le.df$prob)
str(le.df)
plot.roc(r)

# auc 계산
auc(r)










#### 향상차트(lift chart)(Figure 5.6)
# 'caret' library:
library(caret)
lift.example <- lift(relevel(as.factor(actual), ref="1") ~ prob, data = df)
xyplot(lift.example, plot = "gain")

# Second option with 'gains' library:
library(gains)
df <- read.csv("liftExample.csv")
gain <- gains(df$actual, df$prob, groups=dim(df)[1])
plot(c(0, gain$cume.pct.of.total*sum(df$actual)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(df$actual))~c(0,dim(df)[1]), col="gray", lty=2)











#### Figure 5.7

# use gains() to compute deciles. 
# when using the caret package, deciles must be computed manually. 
df <- read.csv("liftExample.csv")
gain <- gains(df$actual, df$prob)
str(gain)
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")

