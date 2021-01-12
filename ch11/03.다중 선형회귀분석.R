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
# backword: 변수제거
# forward: 변수추가
# stepwise: backward와 forwoard 동시
# AIC (Akaike information criterion), BIC (Bayesian ...)

# 전체변수 일괄입력
# mreg.df <- mreg.df[-c(1)]
# mreg.model <- lm(flow ~ ., data=mreg.df)

library(car)
mreg.model <- lm(flow ~ design+info+comm+op+fb, 
                 data=mreg.df)
anova(mreg.model)
summary(mreg.model) # 제일 하단의 F-statistic이 분산분석 값임 
vif(mreg.model)






# 표준화 회귀계수
# install.packages("lm.beta")
library(lm.beta)
lm.beta <- lm.beta(mreg.model)
summary(lm.beta)






# backword: 변수제거
# 모든 변수 투입확인
mreg.model.b <- lm(flow ~ ., mreg.df)
summary(mreg.model.b) 
# 제거된 변수있는지 확인
# AIC값이 줄어들어야 의미가 있음
mreg.model.b <- step(mreg.model.b, 
                     direction = "backward", 
                     trace = T) # trace step별로 모두 보여주기 







# forword: 변수제거
mreg.model.f <- lm(flow ~1, mreg.df)
summary(mreg.model.f) 
mreg.model.f <- step(mreg.model.f, direction = "forward", 
                   scope =(flow ~ design+info+comm+op+fb),
                   trace = T)







# 04. 회귀분석 가정 검정
# 등분산성: Scale-Location, ncvTest
# 정규성: Nomal Q-Q, shapiro.test
# 선형성: Residuals vs Fitted, 
# 독립성: durbinWatsonTest
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1

# 그림으로 가정 검정
opar <- par(no.readonly = TRUE)
  par(mfrow=c(2,2))
  plot(mreg.model)
par(opar)






# 수치로 가정 검정
library(car)

# 등분산성 검정 
car::ncvTest(mreg.model)

# 정규성 검정 
shapiro.test(mreg.model$residuals)

# 독립성 
car::durbinWatsonTest(mreg.model)

#이상치 검정, sd, hat, d 통합검정
library(car)
influencePlot(mreg.model, id.method="identify")







# 부록: 모든 경우의 수 고려
install.packages("leaps")
library(leaps)

leap <- regsubsets(flow ~ ., mreg.df, nbest = 5)   # size당 5개의 최적 모형 저장
summary(leap)
plot(leap)
