#### 02.단순 선형회귀분석(Regression) #####

# 01.데이터 불러오기 
reg.df <- read.csv("Ch1102.단순 선형회귀분석(REG).csv", 
                     header=TRUE, 
                     na.strings = ".")
str(reg.df)







# 02.단순회귀분석
# reg.df <- reg.df[c(-61:-62),]
plot(fat ~ col, data=reg.df)
abline(lm(fat ~ col, data = reg.df), col = "red", lty = 4)

reg.model <- lm(fat ~ col, 
                data = reg.df)
anova(reg.model)
summary(reg.model)







# 03. 회귀분석 가정 검정
# 등분산성: Scale-Location, ncvTest
# 정규성: Nomal Q-Q, shapiro.test
# 선형성: Residuals vs Fitted, 
# 독립성: durbinWatsonTest
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1
# 그림으로 가정 검정
opar <- par(no.readonly = TRUE)
  par(mfrow=c(2,2))
  plot(reg.model)
par(opar)






# 수치로 가정 검정
library(car)

# 잔차의 등분산성 검정 
car::ncvTest(reg.model)

# 잔차의 정규분포 검정 
shapiro.test(reg.model$residuals)

#이상치 검정, sd, hat, d 통합검정
influencePlot(reg.model, id.method="identify")







# 04.모델을 이용한 예측: 콜레스테롤이 130, 150일 경우 예측값
reg.new <- data.frame(col=c(130,150))
predict(reg.model, newdata = reg.new)

