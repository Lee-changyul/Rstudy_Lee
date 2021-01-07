## 실습1
# 다음은 호흡과 뇌파와의 관계를 연구한 자료이다. 
# 총 4개 채널이 있는데, 채널별로 알파파(al)와 베타파(be) 간에는 각각 차이가 있는가?
  

# 01.데이터 불러오기 
breath <- read.csv("Ch0804.호흡과 뇌파.csv", 
                header=TRUE, 
                na.strings = ".")
str(breath)


# 02.기술통계분석
library(psych)
describe(breath)
dif <- c(breath$ch1be-breath$ch1al) # 몸무게 차이
describe(dif)


# 03.그래프 그리기(박스그래프,히스토그램)

opar <- par(no.readonly = TRUE)
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE)) # 화면분할
hist(breath$ch1al, main="ch1al")
hist(breath$ch1be, main="ch1be")
boxplot(dif, main="뇌파 차이") 
par(opar)

# 04.통계분석
t.test(breath$ch1be, breath$ch1al, 
       alternative = c("two.sided"),
       paired = TRUE,
       conf.level = 0.95)


# 05.통계결과 그래프

mu=0
se=0.05/sqrt(143) 
inter = qt(p=0.025, df=143)
data <-rnorm(1000, mu, se)
data <- sort(data)
plot(data, 
     dnorm(data, mu, se), 
     type='l', 
     main="뇌파 차이 검정", 
     xlim=c(-0.09,0.03))
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=mu-inter*se, col="blue", lty=5)
abline(v=-0.08, col="red", lty=5)
