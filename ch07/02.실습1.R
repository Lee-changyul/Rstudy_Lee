## 실습1
## K대학에서는 재학생 만족도 조사를 실시하였다.
## 재학생 만족도가 50점이라고 할 수 있는가? (만족도가 50점이다. => 만족도가 50점이 아니다(귀무가설)
  

# 01.데이터 불러오기 
satisfaction.df <- read.csv("Ch0702.satisfaction.csv", 
                header=TRUE, 
                na.strings = ".")
str(satisfaction.df)

# 02.기술통계분석
library(psych)
describe(satisfaction.df)

# 03.그래프 그리기 
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2)) # 화면분할
boxplot(satisfaction.df$satis) # attach를 하지 않았기 때문에 이렇게 쓴다. 
hist(satisfaction.df$satis, 
     breaks=10, 
     col="red",
     xlab="점수", ylab="개수",
     main="만족도 점수") 
par(opar)

# 04.통계분석
options("scipen" = 20) #지수 표기법 수정 : 2.2e-4=0.00022
t.test(satisfaction.df$satis,
       alternative = c("two.sided"),
       mu = 50, 
       conf.level = 0.95)

# 05.통계결과 그래프

mu=50
se=4.57 
inter = qt(p=0.025, df=199)
data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, 
     dnorm(data, mu, se), 
     type='l', 
     main="만족도점수(Mu=50) 검정", 
     xlim=c(30,70))
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=mu-inter*se, col="blue", lty=5)
abline(v=49.98, col="red", lty=5)