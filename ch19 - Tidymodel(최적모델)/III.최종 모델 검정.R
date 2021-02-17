## III.최종 모델 검정

# 09.초종모델 결정 및 훈련


# best model 비교

bind_rows(rf_auc, lr_auc, nnet_auc) %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             col = model)) + 
  geom_path(lwd = 1, alpha = 1) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)

# workflow에 best model 파라미터 세팅

final_workflow <- 
  rf_workflow %>%              # best_model을 기본 workflow에 세팅
  finalize_workflow(rf_best)   # best_model을 기본 workflow에 세팅

final_workflow

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
  vip(num_features = 10)





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
