# https://www.tidymodels.org/start/models/
# 유니버설 은행 사례 Figure 9.9 ####
# 최적 모델

#######################################################
# tidyverse: ggplot2, purrr, tibble  3.0.3,           #
#            dplyr, tidyr, stringr, readr, forcats    #
#                           
# tidymodels: broom, recipes, dials, rsample, infer,  #
#             tune, modeldata, workflows, parsnip,    #
#             yardstick                               #
#######################################################

# install.packages("tidyverse") 
# install.packages("tidymodels")
# install.packages("skimr")
# install.packages("naniar")
# install.packages("vip")
library(tidyverse)
library(tidymodels)
library(skimr)           # 데이터 요약(EDA)
library(vip)             # 중요한 변수 찾기




## 01.데이터 불러오기

bank_tb <- read_csv('UniversalBank.csv', 
                    col_names = TRUE,
                    locale=locale('ko', encoding='euc-kr'),
                    na=".") %>% # csv 데이터 읽어오기
  mutate_if(is.character, as.factor)

str(bank_tb)
head(bank_tb)





## 02.data 전처리

# 변수명 수정 (공란이 있을 경우에 변수명 수정)

bank_tb <- bank_tb %>%
  rename(c('Personal_Loan'= 'Personal Loan',
           'CD_Account' = 'CD Account',
           'Securities_Account' = 'Securities Account'))
str(bank_tb)
head(bank_tb)


# 범주형 변수(factor)로 인식하게 변환
# 결과변수(class)에서 관심있는 변수를 1번으로 세팅

bank_tb <- bank_tb %>%
  mutate(Personal_Loan = factor(Personal_Loan, 
                                levels = c(1, 0),              #관심변수=Yes           
                                labels = c("Yes", "No"))) %>%
  mutate(Securities_Account = factor(Securities_Account, 
                                     levels = c(0,1),
                                     labels = c("No", "Yes"))) %>%
  mutate(CD_Account  = factor(CD_Account, 
                              levels = c(0,1),
                              labels = c("No", "Yes"))) %>%
  mutate(Online = factor(Online,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) %>%
  mutate(CreditCard = factor(CreditCard,
                             levels = c(0,1),
                             labels = c("No", "Yes"))) %>%
  mutate(Education  = factor(Education ,
                             levels = c(1:3),
                             labels = c("Undergrad", 
                                        "Graduate", 
                                        "Professional")))
str(bank_tb)
head(bank_tb)

# 필요없는 변수제거: ID, 우편번호 제거
# recipe에서 제거할 수도 있음

bank_tb <- bank_tb %>%
  select(-c(ID, `ZIP Code`))  

str(bank_tb)
head(bank_tb)





## 03.데이터 탐색(EDA)

# 데이터 탐색: 범주형, 연속형 구분
# skimr::skim() - package명을 앞에 써서 구분
# 패키지를 여러개 사용할 경우에 이름이 같은 경우도 있어서
# 구분이 필요할 경우에 [패키지명::]을 사용

bank_tb %>%
  skimr::skim() 

bank_tb %>%
  group_by(Personal_Loan) %>%
  skimr::skim() 

# base accuracy
# yes 기준으로 0.096

bank_tb %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))





## 04.훈련용, 테스트용 데이터 분할: partition

# 데이터 partition

set.seed(123) # 시드 고정 

bank_split <- 
  initial_split(bank_tb,
                strata = Personal_Loan) # 결과변수 비율반영

bank_split

# training, test용 분리

train_data <- training(bank_split)
test_data  <- testing(bank_split)





# p.88 ~94 설명

## 05.Tuning Model 만들기

# model에 TUNING HYPERPARAMETERS 설정

# 모델 인자(argument) 확인

args(decision_tree) 

tree_model <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
    ) %>% 
  set_engine("rpart") %>%  
  set_mode("classification")

tree_model

# 하이퍼파라미터 그리드 만들기
# dials:: grid_regular
# cost_complexity 5개 X tree_depth 5개 = 25세트

tree_grid <- 
  grid_regular(cost_complexity(),
               tree_depth(),
               levels = 5)
tree_grid

# 교차검증(k-fold) 데이터 만들기

set.seed(123)
bank_folds <- 
  vfold_cv(train_data, v=3)

bank_folds

# recipe 만들기
# step_upsample(Personal_Loan)

tree_recipe <- 
  recipe(Personal_Loan ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes())

summary(tree_recipe)



## 06.workflow 만들기
set.seed(123) # 랜덤시드 추가

tree_workflow <- 
  workflow() %>%
  add_model(tree_model) %>% 
  add_recipe(tree_recipe)

tree_workflow





## 07.하이퍼 파라미터 튜닝
# MODEL TUNING WITH A GRID

tree_results <- 
  tree_workflow %>% 
  tune_grid(
    resamples = bank_folds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, accuracy)
  )

tree_results

# 튜닝 결과 확인

tree_results %>% 
  collect_metrics()

# 튜닝 결과 그래프 그리기

tree_results %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)





## 08.final model 세팅(훈련데이터)

# 튜닝 결과 확인

tree_results %>%
  show_best("roc_auc", n=10) %>%
  arrange(desc(mean))

# best model 선택
best_tree <- 
  tree_results %>%
  select_best("roc_auc")

best_tree

# workflow에 best model 파라미터 세팅

final_workflow <- 
  tree_workflow %>%              # 기본 workflow에 세팅
  finalize_workflow(best_tree)   # 기본 workflow에 세팅

final_workflow





## 09.final model 훈련 

# 훈련데이터로 모델 훈련하기

final_train_fit <- 
  final_workflow %>%
  fit(data = train_data)

# 모델 훈련 결과 확인

final_train_fit %>% 
  pull_workflow_fit()





## 10.final model 검정(훈련데이터)

# 예측결과표 생성

final_train_pred <- 
  predict(final_train_fit,                 # 9번에서 세팅한 final model 대입
          train_data, 
          type = "prob") %>%
  bind_cols(predict(final_train_fit, 
                    train_data)) %>% 
  bind_cols(train_data %>% 
              select(Personal_Loan)) %>%
  print()


# 정오분류표(confusion matrix) 만들기

final_train_conf <-
  final_train_pred  %>%
  conf_mat(truth = Personal_Loan, 
           estimate = .pred_class)

final_train_conf

autoplot(final_train_conf, type = "heatmap") # mosaic
autoplot(final_train_conf, type = "mosaic")

summary(final_train_conf)

# f1: 재현율(Recall)(↑)과 정밀도(Precision)(↑)
# 재현율(Recall): 실제 Class 중에 잘 맞춘 것(=TPR=민감도)
# 정밀도(Precision): 예측 Class 중에 잘 맞춘 것
# 정확도 (Accuracy) : 클래스 0과 1 모두를 정확하게 분류

# ACU(area under the curve): ROC 정확도

final_train_pred %>%
  roc_auc(truth = Personal_Loan, 
          .pred_Yes)

# ROC 커브

train_auc <-
  final_train_pred %>%
  roc_curve(truth = Personal_Loan, 
            estimate = .pred_Yes) %>% 
  mutate(model = "train_auc")

autoplot(train_auc)

# gain 커브

final_train_pred %>%
  gain_curve(truth = Personal_Loan, 
             estimate = .pred_Yes) %>%
  autoplot()

# lift 커브

final_train_pred %>%
  lift_curve(truth = Personal_Loan, 
             estimate = .pred_Yes) %>%
  autoplot()

# 중요변수 확인

final_train_fit %>% 
  pull_workflow_fit() %>% 
  vip()






## 11.final model 검정(테스트 데이터)

# 구축된 모델에 test data로 검정
# last_fit 사용
# data: bank_split 사용

final_test_fit <- 
  final_workflow %>%
  last_fit(bank_split) 

final_test_fit


# 예측결과 자동생성: collect_predictions() 

final_test_pred <- 
  final_test_fit %>%
  collect_predictions()

final_test_pred

# 정오분류표(confusion matrix) 만들기

final_test_conf <-
  final_test_pred  %>%
  conf_mat(truth = Personal_Loan, 
           estimate = .pred_class)

final_test_conf

autoplot(final_test_conf, type = "heatmap") # mosaic
autoplot(final_test_conf, type = "mosaic")

summary(final_test_conf)

# f1: 재현율(Recall)(↑)과 정밀도(Precision)(↑)
# 재현율(Recall): 실제 Class 중에 잘 맞춘 것(=TPR=민감도)
# 정밀도(Precision): 예측 Class 중에 잘 맞춘 것
# 정확도 (Accuracy) : 클래스 0과 1 모두를 정확하게 분류

# ACU(area under the curve): ROC 정확도

final_test_pred %>%
  roc_auc(truth = Personal_Loan, 
          .pred_Yes)

# ROC 커브

test_auc <-
  final_test_pred %>%
  roc_curve(truth = Personal_Loan, 
            estimate = .pred_Yes) %>% 
  mutate(model = "test_auc")

autoplot(test_auc)

# gain 커브

final_test_pred %>%
  gain_curve(truth = Personal_Loan, 
             estimate = .pred_Yes) %>%
  autoplot()

# lift 커브

final_test_pred %>%
  lift_curve(truth = Personal_Loan, 
             estimate = .pred_Yes) %>%
  autoplot()

# 중요변수 확인

final_test_fit %>%
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)





## 12.train, test 검정결과 비교

# 정오분류표(confusion matrix) 비교

final_train_conf
final_test_conf

autoplot(final_train_conf, type = "mosaic") # mosaic
autoplot(final_test_conf, type = "mosaic")

# 검정결과 비교

summary(final_train_conf)
summary(final_test_conf)

# ROC 커브 비교

bind_rows(train_auc, test_auc) %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1.5) +
  geom_abline(lty = 3) + 
  coord_equal()




## 13.decision tree 만들기
# install.packages("rpart.plot")
library(rpart.plot)

rpart_fit <- 
  final_train_fit %>% 
  pull_workflow_fit()


# 모형 1
rpart.plot(x = rpart_fit$fit,
           yesno = 2,
           type = 2, 
           extra = 1, 
           split.font = 1, 
           varlen = -10)

# 모형 2
prp(x = rpart_fit$fit, 
    type = 1, 
    extra = 1, 
    under = TRUE, 
    split.font = 1, 
    varlen = -10,
    box.col=ifelse(rpart_fit$fit$frame$var == "<leaf>", 'gray', 'white'))

