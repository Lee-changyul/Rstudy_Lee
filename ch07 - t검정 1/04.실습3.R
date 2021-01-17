## 실습2
# K제약회사에서는 새로운 진통제를 개발하였다. 
# 새로운 진통제의 지속효과가 5시간 이상이라고 말할 수 있는가?
  

# 01.데이터 불러오기 
painkiller.df <- read.csv("Ch0704.painkiller.csv", 
                       header=TRUE, 
                       na.strings = ".")
str(painkiller.df)

# 02.기술통계분석
library(psych)
describe(painkiller.df)

# 03.그래프 그리기
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2)) # 화면분할
boxplot(painkiller.df$time) 
hist(painkiller.df$time, 
     breaks=10, 
     col="red",
     xlab="지속시간", ylab="개수",
     ylim = c(0,25),
     main="진통제 지속시간") 
par(opar)

# 04.통계분석
# two-sided test: alternative = c("two.sided") 
# right-sided test: alternative = c("greater")
# left-sided test: alternative = c("less")
options("scipen" = 20) #지수 표기법 수정 : 2.2e-4=0.00022
t.test(painkiller.df$time,
       alternative = c("greater"),
       mu = 5, 
       conf.level = 0.95)

# 05.통계결과 그래프

mu=5
se=0.0411
inter = qt(p=0.05, df=39,lower.tail=F) # 95% 신뢰구간
data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, dnorm(data, mu, se), type='l', 
     main="진통제 지속시간 (Mu>=5) 검정정", 
     xlim=c(4.95,5.1))
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5)
abline(v=5.0675, col="red", lty=5)
