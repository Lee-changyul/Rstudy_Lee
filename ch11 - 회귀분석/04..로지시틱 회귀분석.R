#### 04.로지스틱 회귀분석(Regression) #####

# 01.데이터 불러오기 
lreg.df <- read.csv("Ch1104.로지스틱회귀분석(LREG).csv", 
                     header=TRUE, 
                     na.strings = ".")
lreg.df$exp <- factor(lreg.df$exp,
                       levels=c(0:1),
                       labels=c("No","Yes"))
lreg.df$chun <- factor(lreg.df$chun,
                          levels=c(0:1),
                          labels=c("No","Yes"))
str(lreg.df)








# 02.기본통계치 확인
library(psych)
describe(lreg.df)
pairs.panels(lreg.df)







# 03.로지스틱 회귀분석
lreg.model <- glm(chun ~ phy+psy+cmmt+exp, 
                  family = binomial, 
                  data=lreg.df)
options(scipen=10) # 소숫점 아래 확인 
summary(lreg.model)

# Odds 계산
odds <- data.frame(summary(lreg.model)$coefficients, 
                   odds = exp(coef(lreg.model))) 
round(odds, 5)

# 오즈비 해석 1을 기준으로 조직몰입도가 1단위 증가하면 이직의도는 0.547배 증가한다. 
# => 다른 표현으로는 이직의도가 45.3% 감소한다. (1-0.547)
