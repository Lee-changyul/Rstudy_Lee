#### 사례 3 #####

# 01.데이터 불러오기 
lreg.df <- read.csv("Ch1107.유니버셜은행.csv", 
                    header=TRUE, 
                    na.strings = ".")
lreg.df$Loan <- factor(lreg.df$Loan,
                       levels=c(0:1),
                       labels=c("No","Yes"))






# 02.기본통계치 확인
library(psych)
describe(lreg.df)
pairs.panels(lreg.df)







# 03.로지스틱 회귀분석
lreg.model <- glm(Loan ~ ., 
                  family = binomial, 
                  data=lreg.df)
options(scipen=10) # 소숫점 아래 확인 
summary(lreg.model)

# Odds 계산
odds <- data.frame(summary(lreg.model)$coefficients, 
                   odds = exp(coef(lreg.model))) 
round(odds, 5)

