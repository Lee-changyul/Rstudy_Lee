## 실습2
# K식품에서는 햄버거의 칼로리를 연구하고 있다. 
# 햄버거의 칼로리가 500kcal 보다 작다고 말 할 수 있는가?
  

# 01.데이터 불러오기 
calorie.df <- read.csv("Ch0703.calorie.csv", 
                            header=TRUE, 
                            na.strings = ".")
str(calorie.df)

# 02.기술통계분석
library(psych)
describe(calorie.df)

# 03.그래프 그리기
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2)) # 화면분할
boxplot(calorie.df$cal) 
hist(calorie.df$cal, 
     breaks=10, 
     col="red",
     xlab="점수", ylab="개수",
     ylim = c(0,25),
     main="만족도 점수") 
par(opar)

# 04.통계분석
# two-sided test: alternative = c("two.sided") 
# right-sided test: alternative = c("greater")
# left-sided test: alternative = c("less")
options("scipen" = 20) #지수 표기법 수정 : 2.2e-4=0.00022
t.test(calorie.df$cal,
       alternative = c("less"),
       mu = 500, 
       conf.level = 0.95)

# 05.통계결과 그래프

mu=500
se=1.01 # 표본이므로 sd대신에 se 사용
inter = qt(p=0.05, df=39,lower.tail=T) # 95% 신뢰구간

data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, dnorm(data, mu, se), type='l', 
     main="칼로리 (Mu<=500) 검정", 
     xlim=c(495,505))
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=498.175, col="red", lty=5)

