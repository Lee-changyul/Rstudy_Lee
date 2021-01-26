##인공신경망 실습 Table 11.2
# package 설치
# install.packages("neuralnet") 다양한 패키지가 있다. 
library(neuralnet)
library(caret)







# 01.데이터 불러오기
tiny.df <- read.csv("TinyData.csv", 
                      header=TRUE, 
                      na.strings = ".")
str(tiny.df)
head(tiny.df)







# 02.전처리작업: output layer의 노드로 변환
tiny.df$Like <- tiny.df$Acceptance=="like" # 변수 추가 like 이면 True로 반환
tiny.df$Dislike <- tiny.df$Acceptance=="dislike" # 변수 추가 dislike 이면 False로 변환
str(tiny.df)
head(tiny.df)






# 훈련용, 검증용 데이터 분리는 pass
# 03.인공신경망 실행
# help(neuralnet)
# neuralnet(formula             : 출력변수(종속변수) ~ 입력변수(독립변수)
#          data                 :
#          hidden = 1           : hidden layer(hl)와 노드수 (2,3) hl1=2, hl2=3개 노드, 
                                  # 첫번째 히든 레이어에 2개 노드, 두번째 히든레이어에 3개 노드 
#          rep = 1              : 모형의 적합 횟수 
#          algorithm = "rprop+" : 역전파 알고리즘 "backprop" , rprop+, rprop-
#          err.fct = "sse"      : 오차총합, sse(오차 제곱 합) or ce(교차 엔트로피)
#          act.fct = "logistic" : 활성화 함수, logistic(로지스틱) or tanh(hyperbolic tangent)
#          linear.output = FALSE: 출력노드에서 선형 활성화 비적용
#          likelihood = TRUE)   : AIC(에러감소 체크, 떨어져야 좋음), BIC 산출

set.seed(1)
nn <- neuralnet(Like + Dislike ~ Salt + Fat,
                data = tiny.df, 
                linear.output = F, # 범주형이니 선이 없다. 
                hidden = 3) # 히든 레이어 1개에 노드 3개 지정해라 c(2,3), c(4,2)






# 04.결과확인 및 그래프 그리기
nn$weights # 가중치 점수 확인

# 첫번째 값 : bias 값(절편값)
# 두번째 값 : 가중치 1의 값
# 세번째 값 : 가중치 2의 값


prediction(nn) # 예측결과 확인
plot(nn, 
     rep="best")





# 05.모델평가 Table 11.3

# compute(net, covariate): 데이터셋의 각 개체별로 출력값 산출
# apply: 행(Row) 또는 열(Column) 단위의 연산을 쉽게 처리
# apply(data,     :
#       margine=1,: 1(행), 2(열)
#       mean)     :
# which.max       : 최대값 색인 번호

predict <- compute(nn,  #뉴런신경망 모형에 데이터를 넣어서 예측치 모델을 만들어라 
                   data.frame(tiny.df$Salt, tiny.df$Fat))

head(predict)

predicted.class = apply(predict$net.result, 
                        1, 
                        which.max)-1 

# 예측값의 결과를 바꿈 1번 값이면 0을 출력하고, 2번값이면 1을 출력하라
# 아래 결과 해석에 활용하기 위해서 변환해 준 것임

head(predicted.class)

confusionMatrix(as.factor(ifelse(predicted.class=="1", 
                       "dislike", 
                       "like")), 
                as.factor(tiny.df$Acceptance))
