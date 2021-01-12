#### 상관분석#####

# 01.데이터 불러오기 
corr.df <- read.csv("Ch1101.상관분석(CORR).csv", 
                   header=TRUE, 
                   na.strings = ".")
str(corr.df)







# 02.기본통계치 확인
library(psych)
describe(corr.df)
pairs.panels(corr.df)

plot(키 ~ 몸무게, data=corr.df)
abline(lm(키 ~ 몸무게, data = corr.df), col = "red", lty = 4)







# 03.상관분석
cor(corr.df, use="complete.obs", method=c("pearson"))

cor.test(corr.df$키,
         corr.df$몸무게,
         method=c("pearson"))

