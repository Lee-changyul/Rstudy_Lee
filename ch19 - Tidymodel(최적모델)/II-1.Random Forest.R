## II.개별 모델 만들기

## II-1.Random Forest

# 랜덤 포레스트와 같은 트리 기반 모델은 전처리가 거의 필요하지 않으며 
# 다양한 유형의 예측 변수 (희소, 치우침, 연속, 범주 등)를 효과적으로 처리
# 랜덤 포레스트는 더미 또는 정규화 된 예측 변수가 필요없음


## 05.Tuning Model 만들기

# install.packages("ranger")
library(ranger)

# 모델 인자(argument) 확인

args(ranger) 

# CPU가 여러개 있을 경우에 활용

cores <- parallel::detectCores()
cores

rf_model <-
  rand_forest(mtry = tune(), 
              min_n = tune(), 
              # minimal node size 노드에 분류되는 최소 개수 - 노드 하나에 몇개의 데이터가 분류될 것인가?  
              trees = 100) %>% 
  set_engine("ranger", 
             num.threads = cores,
             importance = "impurity") %>%  # 분석에서 변수의 중요도를 불순도를 기준으로 하라 (gini)
            # 불순도를 낮추는 것을 중요하게 생각해서 모델을 만들어 보라. 
  set_mode("classification")


rf_model


# 하이퍼파라미터 그리드 만들기
# dials:: grid_regular
# m 5개 X tree_depth 5개 = 25세트

rf_grid <- 
  grid_regular(mtry(c(1L, 11L)), # L 부호는 정수형으로 데이터를 저장하도록 명령
               min_n(),
               levels = 5)

rf_grid %>% print(n=25)


# recipe 만들기
# 랜덤 포레스트 모델에는 더미 또는 정규화 된 예측 변수가 
# 필요없음

rf_recipe <- 
  recipe(Personal_Loan ~ ., data = train_data) 
 
summary(rf_recipe)




## 06.workflow 만들기
set.seed(123) # 랜덤시드 추가

rf_workflow <- 
  workflow() %>%
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

rf_workflow





## 07.하이퍼 파라미터 튜닝
# MODEL TUNING WITH A GRID

rf_results <- 
  rf_workflow %>% 
  tune_grid(
    resamples = bank_folds,
    grid = rf_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, accuracy)
  )


rf_results

# 튜닝 결과 확인

rf_results %>% 
  collect_metrics()

# 튜닝 결과 그래프 그리기

autoplot(rf_results)


## 08.final model 세팅(훈련데이터)

# 튜닝 결과 확인

rf_results %>%
  show_best("roc_auc", n=10) %>%
  arrange(desc(mean))

# best model 선택
rf_best <- 
  rf_results %>%
  select_best("roc_auc")

rf_best

rf_results %>% 
  collect_predictions()


rf_auc <- 
  rf_results %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(Personal_Loan, .pred_Yes) %>% 
  mutate(model = "Random Forest")

autoplot(rf_auc)
