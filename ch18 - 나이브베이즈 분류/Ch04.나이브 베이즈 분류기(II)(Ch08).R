#### Ch03.나이브 베이즈 분류기(교재 Ch08)
#### package 설치
# install.packages("forecast") # 모형평가 도구
# install.packages("gains") #gain
# install.packages("caret")
# install.packages("e1071")
# install.packages("pROC")


#### Table 8.4
#01.파일 불러오기 ####
library(e1071)
library(tidyverse)

delays.df <- read_csv("FlightDelays.csv", 
                    col_names = TRUE,
                    locale=locale('ko', encoding='euc-kr'),
                    na=".") %>% # csv 데이터 읽어오기
  mutate_if(is.character, as.factor)

str(delays.df)

#02.전처리작업: 범주형 변수(factor)로 인식하게 변환 ####

delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK, labels = c("MON", "TUE","WEN","THU","FRI","SAT","SUN"))

head(delays.df)
str(delays.df)

# 출발예정시간변환(18):6 = 6 AM ~ 22 = 10 PM
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

delays.df <- delays.df %>%
  rename(c(Flight_Status = `Flight Status`))
         
str(delays.df)
head(delays.df)

delays.df %>%
  skimr::skim() 

delays.df %>%
  group_by(Flight_Status) %>%
  skimr::skim() 

# base accuracy

delays.df %>% 
  count(Flight_Status) %>% 
  mutate(prop = n/sum(n))


str(delays.df) # 데이터 구조확인


#delays.df <- delays.df %>% 
#  select(10, 1, 8, 4, 2, 13)

#03.훈련용, 검증용 데이터 생성 ####

selected.var <- c(10, 1, 8, 4, 2, 13) # 6개 변수만 사용

set.seed(123)

train.index <- sample(c(1:dim(delays.df)[1]), 
                      dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, 
                      selected.var]
valid.df <- delays.df[-train.index, 
                      selected.var]
str(train.df)

#04.나이브 베이즈 실행(run naive bayes)

delays.nb <- naiveBayes(Flight_Status ~ ., 
                        data = train.df)
                        
delays.nb


#### Table 8.5
#05.비율로 전환: prop.table()
#margin=1 : 행기준 백분율(행을 100%로)
#margin=2 : 열기준 백분율 (열을 100%로)
#margin=NULL : 전체 백분율 (전체를 100%로)

delays.t <- table(train.df$Flight_Status, 
                  train.df$DEST)

delays.p <- prop.table(delays.t, 
                       margin = 1)
delays.p

 #### Table 8.6
#06.검증데이터로 확률 예측(predict probabilities)
# newdata=예측을 위한 새로운 데이터
# type=raw(확률값예측), class(소속 클래스)
# pred.prob <- predict(delays.nb, newdata = valid.df, type = "class")
pred.prob <- predict(delays.nb,
                     newdata = valid.df, 
                     type = "raw")
head(pred.prob)

#07.검증데이터로 소속 class 확인(class membership)
pred.class <- predict(delays.nb, 
                      newdata = valid.df)
head(pred.class)
summary(pred.class)

#08.확률과 class 통합
df <- data.frame(actual = valid.df$Flight_Status, predicted = pred.class, 
                 pred.prob)
str(df)

head(df)

#09.나이브 베이지 모델로 예측
df[valid.df$CARRIER == "DL" & 
     valid.df$DAY_WEEK == 'MON' & 
     valid.df$CRS_DEP_TIME == 10 & 
     valid.df$DEST == "LGA" & 
     valid.df$ORIGIN == "DCA", ]

valid.df[93,]


#### Table 8.7
#10.분류모델성능평가(정오분류표)
library(caret)

# 훈련데이터(training)
pred.class <- predict(delays.nb, 
                      newdata = train.df)

confusionMatrix(pred.class, 
                train.df$Flight_Status)

# 검증데이터(validation)
pred.class <- predict(delays.nb, 
                      newdata = valid.df)

confusionMatrix(pred.class, 
                valid.df$Flight_Status)


str(df)

confusionMatrix(as.factor(ifelse(df$delayed>0.5, 'delayed', 'ontime')), 
                df$actual)



cd <- df %>% 
  mutate(act = ifelse(actual == "delayed", 1, 0))


accT = c() 

# compute accuracy per cutoff

for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(as.factor(1 * (df$delayed > cut)), as.factor(cd$act))
  accT = c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

#### Figure 8.1
#### 11.이익차트
## gains(actual, predicted, groups=10) 
## gains(실제반응값, 예측값,누적그룹수)
# gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[,1], groups=10)
# gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[,1], groups=50)
# gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[,1], groups=100)

library(gains)
gain <- gains(ifelse(valid.df$Flight_Status=="delayed",1,0), 
              pred.prob[,1], 
              groups=100)
gain

#plot (x축~Y축)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Flight_Status=="delayed")) ~ 
       c(0,gain$cume.obs), 
     xlab="# cases", 
     ylab="Cumulative", 
     main="", type="l")

lines(c(0,sum(valid.df$Flight_Status=="delayed")) ~ 
        c(0, dim(valid.df)[1]), 
      lty=2)


#12.나이브 베이지 모델로 예측
df[valid.df$CARRIER == "US" & 
     valid.df$DAY_WEEK == 'SUN' & 
     valid.df$CRS_DEP_TIME == 18 & 
     valid.df$DEST == "LGA" & 
     valid.df$ORIGIN == "DCA",]



# ================== 타이디 모델의 레시피 활용법 ####

# install.packages("tidyverse") 
# install.packages("tidymodels")
# install.packages("skimr")
# install.packages("naniar")
# install.packages("vip")
library(tidyverse)
library(tidymodels)
library(skimr)           # 데이터 요약(EDA)
library(vip)             # 중요한 변수 찾기

#install.packages("naivebayes")

library(naivebayes)

## 01.데이터 불러오기


## 02.data 전처리
# 범주형 변수(factor)로 인식하게 변환
# 결과변수(class)에서 관심있는 변수를 1번으로 세팅
# recipe에서 제거할 수도 있음

## 03.데이터 탐색(EDA)

# 데이터 탐색: 범주형, 연속형 구분
# skimr::skim() - package명을 앞에 써서 구분
# 패키지를 여러개 사용할 경우에 이름이 같은 경우도 있어서
# 구분이 필요할 경우에 [패키지명::]을 사용


## 04.훈련용, 테스트용 데이터 분할: partition ####

# 데이터 partition

set.seed(123) # 시드 고정 

delays.df_split <- 
  initial_split(delays.df,
                prop = 0.6,
                strata = Flight_Status) # 결과변수 비율반영

delays.df_split

# training, test용 분리

train_data <- training(delays.df_split)
test_data  <- testing(delays.df_split)




## 05.Model 만들기 ####

# Model 만들기

# 모델 인자(argument) 확인

# install.packages("discrim")

library(discrim)

args(naive_Bayes) 

bayes_model <- 
  naive_Bayes() %>% 
  set_engine("klaR") 


# recipe 만들기
# step_dummy(all_nominal(), -all_outcomes()) : one-hot-ecoding
# step_log(Gr_Liv_Area, base = 10) : 로그함수로 변환
# step_other(Neighborhood, threshold = 0.01) : 값이 적은 항목을 기타로 변환
# step_upsample(Flight_Status) # 데이터 균형화
# step_zv(all_predictors()) : 단일 고유 값 (예 : 모두 0) 변수 제거. 
# 특히, penalty 사용하는 모델에서 중요(logistic, SVM 등)
# step_normalize(all_numeric()) : 데이터 정규화

bayes_recipe <- 
  recipe(Flight_Status ~ ., data = train_data)

  
summary(bayes_recipe)




# p.39 설명

## 06.workflow 만들기
bayes_workflow <- 
  workflow() %>% 
  add_model(bayes_model) %>% 
  add_recipe(bayes_recipe)

bayes_workflow





## 07.Model 훈련

# 훈련데이터로 모델 훈련하기

bayes_train_fit <- 
  bayes_workflow %>%
  fit(data = train_data)

# 모델 훈련 결과 확인

bayes_train_fit %>%
  pull_workflow_fit()





# p.44 ~ 50 설명

## 08.훈련모델 검정

# 예측결과표 생성

bayes_train_pred <- 
  predict(bayes_train_fit, 
          train_data, 
          type = "prob") %>%
  bind_cols(predict(bayes_train_fit, 
                    train_data)) %>% 
  bind_cols(train_data %>% 
              select(Flight_Status)) %>%
  print()

# 정오분류표(confusion matrix) 만들기

bayes_train_conf <-
  bayes_train_pred  %>%
  conf_mat(truth = Flight_Status, 
           estimate = .pred_class)

bayes_train_conf

autoplot(bayes_train_conf, type = "heatmap") # mosaic
autoplot(bayes_train_conf, type = "mosaic")

summary(bayes_train_conf)

# f1: 재현율(Recall)(↑)과 정밀도(Precision)(↑)
# 재현율(Recall): 실제 Class 중에 잘 맞춘 것(=TPR=민감도)
# 정밀도(Precision): 예측 Class 중에 잘 맞춘 것
# 정확도 (Accuracy) : 클래스 0과 1 모두를 정확하게 분류

# ACU(area under the curve): ROC 정확도

bayes_train_pred %>%
  roc_auc(truth = Flight_Status, 
          .pred_delayed)

# ROC 커브

train_auc <-
  bayes_train_pred %>%
  roc_curve(truth = Flight_Status, 
            estimate = .pred_delayed) %>% 
  mutate(model = "train_auc")

autoplot(train_auc)

# gain 커브

bayes_train_pred %>%
  gain_curve(truth = Flight_Status, 
             estimate = .pred_delayed) %>%
  autoplot()

# lift 커브

bayes_train_pred %>%
  lift_curve(truth = Flight_Status, 
             estimate = .pred_delayed) %>%
  autoplot()


## 09.테스트 데이터 검정

# 구축된 모델에 test data로 검정
# last_fit 사용
# data: bank_split 사용

bayes_test_fit <- 
  bayes_workflow %>%
  last_fit(delays.df_split) 

bayes_test_fit

# 예측결과 자동생성: collect_predictions() 

bayes_test_pred <- 
  bayes_test_fit %>%
  collect_predictions()

bayes_test_pred

# 정오분류표(confusion matrix) 만들기

bayes_test_conf <-
  bayes_test_pred  %>%
  conf_mat(truth = Flight_Status, 
           estimate = .pred_class)

bayes_test_conf

autoplot(bayes_test_conf, type = "heatmap") # mosaic
autoplot(bayes_test_conf, type = "mosaic")

summary(bayes_test_conf)

# f1: 재현율(Recall)(↑)과 정밀도(Precision)(↑)
# 재현율(Recall): 실제 Class 중에 잘 맞춘 것(=TPR=민감도)
# 정밀도(Precision): 예측 Class 중에 잘 맞춘 것
# 정확도 (Accuracy) : 클래스 0과 1 모두를 정확하게 분류

# ACU(area under the curve): ROC 정확도
bayes_test_pred %>%
  roc_auc(truth = Flight_Status, 
          .pred_delayed)

# ROC 커브

test_auc <-
  bayes_test_pred %>%
  roc_curve(truth = Flight_Status, 
            estimate = .pred_delayed) %>% 
  mutate(model = "test_auc")

autoplot(test_auc)

# gain 커브

bayes_test_pred %>%
  gain_curve(truth = Flight_Status, 
             estimate = .pred_delayed) %>%
  autoplot()

# lift 커브

bayes_test_pred %>%
  lift_curve(truth = Flight_Status, 
             estimate = .pred_delayed) %>%
  autoplot()


## 10.train, test 검정결과 비교

# 정오분류표(confusion matrix) 비교

bayes_train_conf
bayes_test_conf
autoplot(bayes_train_conf, type = "mosaic") # mosaic
autoplot(bayes_test_conf, type = "mosaic")

# 검정결과 비교

summary(bayes_train_conf)
summary(bayes_test_conf)

# ROC 커브 비교

bind_rows(train_auc, test_auc) %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1.5) +
  geom_abline(lty = 3) + 
  coord_equal()

