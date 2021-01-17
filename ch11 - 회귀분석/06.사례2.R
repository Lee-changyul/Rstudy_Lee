#### 사례2 #####
# 문제의 정의
# K대학에서는 재학생을 대상으로 교육수요자 만족도조사를 실시하였다. 
# 전공, 교양 등 독립변수 중에서 만족도에 영향을 주는 변수는 무엇인가? 
# 가장 중요한 변수는 무엇인가?



# 01.데이터 불러오기 
mreg.df <- read.csv("Ch1106.교육수요자 만족도.csv", 
                    header=TRUE, 
                    na.strings = ".")
str(mreg.df)







# 02.기본통계치 확인
library(psych)
describe(mreg.df)
pairs.panels(mreg.df)







# 03.다중 회귀분석
library(car)
mreg.model <- lm(전반적만족도  ~ ., 
                 data=mreg.df)
anova(mreg.model)
summary(mreg.model)
vif(mreg.model)






# 표준화 회귀계수
# install.packages("lm.beta")
library(lm.beta)
lm.beta <- lm.beta(mreg.model)
summary(lm.beta)






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

# 이상치 검정, sd, hat, d 통합검정
# 2sd 이상되는 자료
library(car)
influencePlot(mreg.model, id.method="identify")







