## II.개별 모델 만들기

## II-2.Logistic Regression

## 05.Tuning Model 만들기

# 모델 인자(argument) 확인
# install.packages("glmnet")
library(tidyverse)
library(glmnet)

args(logistic_reg) 

lr_model <- 
  logistic_reg(penalty = tune(), # 인자(argument) 설정
               mixture = 1) %>% # L1 regularization(lasso model)
  set_engine("glmnet") %>%
  set_mode("classification")

lr_model

# 하이퍼파라미터 그리드 만들기
# dials:: grid_regular
# cost_complexity 5개 X tree_depth 5개 = 25세트

lr_reg_grid <- 
  tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid 

options(scipen = 20)


# recipe 만들기
# 랜덤 포레스트 모델에는 더미 또는 정규화 된 예측 변수가 
# 필요없음

lr_recipe <- 
  recipe(Personal_Loan ~ ., data = train_data) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric())

summary(lr_recipe)




## 06.workflow 만들기
set.seed(123) # 랜덤시드 추가

lr_workflow <- 
  workflow() %>%
  
  
  add_model(lr_model) %>% 
  add_recipe(lr_recipe)

lr_workflow





## 07.하이퍼 파라미터 튜닝
# MODEL TUNING WITH A GRID

lr_results <- 
  lr_workflow %>% 
  tune_grid(
    resamples = bank_folds,
    grid = lr_reg_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, accuracy)
  )


lr_results

# 튜닝 결과 확인

lr_results %>% 
  collect_metrics()

# 튜닝 결과 그래프 그리기

autoplot(lr_results)





## 08.final model 세팅(훈련데이터)

# 튜닝 결과 확인

lr_results %>%
  show_best("roc_auc", n=10) %>%
  arrange(desc(mean))

# best model 선택
lr_best <- 
  lr_results %>%
  select_best("roc_auc")

lr_best


lr_results %>% 
  collect_predictions()


lr_auc <- 
  lr_results %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(Personal_Loan, .pred_Yes) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)
