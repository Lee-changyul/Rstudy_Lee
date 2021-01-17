#### 03.다중 회귀분석(Regression) #####

# 01.데이터 불러오기 
mreg.df <- read.csv("Ch1103.다중회귀분석(MREG).csv", 
                    header=TRUE, 
                    na.strings = ".")
str(mreg.df)







# 02.기본통계치 확인
library(psych)
describe(mreg.df)
pairs.panels(mreg.df)







# 03.다중 회귀분석

# 전체변수 일괄입력
# backword: 변수제거 (변수가 적을 때, 많으면 시간이 많이 걸림 )
# forward: 변수추가  (변수가 많을 때, 하나씩 추가 )
# stepwise: backward와 forward 동시 
# AIC (Akaike information criterion), BIC (Bayesian ...) : 모델 적합성에 대한 기준 

# 전체변수 일괄입력 : 이 방법으로 하여서 모델을 돌림

# mreg.df <- mreg.df[-c(1)] id를 제거하고 입력하라 
# mreg.model <- lm(flow ~ ., data=mreg.df) , flow를 제이 한 모든 데이터를 독립변수로 입력하라 

library(car)
mreg.model <- lm(flow ~ design+info+comm+op+fb, # 변수가 적을 경우 항목을 그냥 입력하자. 
                 data=mreg.df)
anova(mreg.model)
summary(mreg.model) # 제일 하단의 F-statistic이 분산분석 값임 / 여기서는 표준화된 베타값이 나오지 않음
 # 별도의 작업을 통해 표준화된 베타값 확인 

vif(mreg.model) # 10보다 크면 다중공선성 문제 있음 






# 표준화 회귀계수 - 영향력을 확인하기 위함 . standarized 
# install.packages("lm.beta")
library(lm.beta)
lm.beta <- lm.beta(mreg.model)
summary(lm.beta)




# 참고 : backward, forward 방법 

# backword: 변수제거
# 모든 변수 투입확인
mreg.model.b <- lm(flow ~ design + info + comm + op + fb, mreg.df)
summary(mreg.model.b) 
# 제거된 변수있는지 확인
# AIC값이 줄어들어야 의미가 있음
mreg.model.b <- step(mreg.model.b, 
                     direction = "backward", 
                     trace = T) # trace step별로 모두 보여주기 

# 여기서는 5개의 변수가 의미가 있으므로 줄일 필요가 없음, 한번에 끝






# forward: 변수제거
mreg.model.f <- lm(flow ~1, mreg.df) # 아무 것도 없는 것 모델 만들고, 
summary(mreg.model.f) 
mreg.model.f <- step(mreg.model.f, direction = "forward", # 5개까지 계속 투입해봐라 
                   scope =(flow ~ design+info+comm+op+fb),
                   trace = T)

# 초기 AIC 기준점에서 AIC가 계속 내려가는 변수를 추가해서 모델을 만들어 나감 





# 04. 회귀분석 가정 검정

# 등분산성: Scale-Location, ncvTest
# 정규성: Nomal Q-Q, shapiro.test
# 선형성: Residuals vs Fitted, 
# 독립성: durbinWatsonTest
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1

# 그림으로 가정 검정

opar <- par(no.readonly = TRUE)
  par(mfrow=c(2,2))
  plot(mreg.model.f)
par(opar)






# 수치로 가정 검정
library(car)

# 등분산성 검정 
car::ncvTest(mreg.model.f)

# 정규성 검정 
shapiro.test(mreg.model.f$residuals)

# 독립성 
car::durbinWatsonTest(mreg.model.f) # 잔차는 독립적이어야 함 0-4의 값을 가지며 2에 가까울 수록 좋음 

#이상치 검정, sd, hat, d 통합검정
library(car)
influencePlot(mreg.model.f, id.method="identify")







# 부록: 모든 경우의 수 고려 ?? 설명 안해줌 
# install.packages("leaps")
library(leaps)

leap <- regsubsets(flow ~ ., mreg.df, nbest = 5)   # size당 5개의 최적 모형 저장
summary(leap)
plot(leap)
