#### 대응표본 (Paired Sample) t-test #####

# 01.데이터 불러오기 
pst <- read.csv("Ch0802.PST.csv", 
                header=TRUE, 
                na.strings = "."
)
str(pst)


# 02.기술통계분석
library(psych)
describe(pst)
dif <- c(pst$post-pst$pre) # 몸무게 차이
describe(dif)


# 03.그래프 그리기(박스그래프,히스토그램)

opar <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE)) # 화면분할
  hist(pst$pre, main="사전 몸무게")
  hist(pst$post, main="사후 몸무게")
  boxplot(dif, main="몸무게 차이") 
par(opar)

# 04.통계분석
t.test(pst$post, pst$pre, 
       alternative = c("two.sided"),
       paired = TRUE,
       conf.level = 0.95)


# 05.통계결과 그래프

mu=0
se=0.7 
inter = qt(p=0.025, df=19)
data <-rnorm(1000, mu, se)
data <- sort(data)
plot(data, 
     dnorm(data, mu, se), 
     type='l', 
     main="몸무게 차이 검정", 
     xlim=c(-3,3))
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=mu-inter*se, col="blue", lty=5)
abline(v=-2.55, col="red", lty=5)
