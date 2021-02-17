## II.개별 모델 만들기

## II-3.Neural Network

## 05.Tuning Model 만들기

nnet_model <- 
  mlp(
    hidden_units = tune(), 
    penalty = tune(), 
    activation = "relu") %>%
  set_engine("nnet") %>%
  set_mode("classification")

nnet_model

# 하이퍼파라미터 그리드 만들기
# dials:: grid_regular
# cost_complexity 5개 X tree_depth 5개 = 25세트

nnet_grid <- 
  nnet_model %>%
  parameters() %>%
  grid_max_entropy(size = 10)

nnet_grid

# recipe 만들기
# 랜덤 포레스트 모델에는 더미 또는 정규화 된 예측 변수가 
# 필요없음

nnet_recipe <- 
  recipe(Personal_Loan ~ ., data = train_data) %>%
  step_BoxCox(all_numeric())%>%
  step_normalize(all_numeric()) %>%
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

summary(nnet_recipe)





## 06.workflow 만들기
set.seed(123) # 랜덤시드 추가



nnet_workflow <- 
  workflow() %>%
  add_recipe(nnet_recipe) %>%
  add_model(nnet_model)

nnet_workflow





## 07.하이퍼 파라미터 튜닝
# MODEL TUNING WITH A GRID

nnet_results <- 
  nnet_workflow %>% 
  tune_grid(
    resamples = bank_folds,
    grid      = nnet_grid,
    control   = control_grid(save_pred = TRUE),
    metrics   = metric_set(roc_auc, accuracy)
  )

nnet_results

# 튜닝 결과 확인

nnet_results %>% 
  collect_metrics()

# 튜닝 결과 그래프 그리기

autoplot(nnet_results)





## 08.final model 세팅(훈련데이터)

# 튜닝 결과 확인

nnet_results %>%
  show_best("roc_auc", n=10) %>%
  arrange(desc(mean))

# best model 선택
nnet_best <- 
  nnet_results %>%
  select_best("roc_auc")

nnet_best

nnet_results %>% 
  collect_predictions()


nnet_auc <- 
  nnet_results %>% 
  collect_predictions(parameters = nnet_best) %>% 
  roc_curve(Personal_Loan, .pred_Yes) %>% 
  mutate(model = "Neural Network")

autoplot(nnet_auc)


nnet_plot <- 
  nnet_results %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean, color=.metric)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

nnet_plot
