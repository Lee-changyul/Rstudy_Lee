#03. 지수분포
# 확률밀도함수: d~
# 누적분포함수(점수): p~
# 누적분포함수(확률): q~

# 지수분포 누적확률계산
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# pexp(q=대기시간, rate=평균발생건수(λ))
# 서비스센터 사례
# 평균발생건수: 5분에 1.5회
# 1분 P(X<=1/5) 이내로 받을 확률

pexp(q=1/5, rate=1.5, lower.tail = TRUE)

# 95%확률로 받을 수 있는 시간
qexp(p=0.95, rate=1.5, lower.tail = TRUE)

# 지수분포 누적확률 graph
x <- seq(0, 3, length = 500) 
plot(x, dexp(x, 1.5), 
     type = "l", 
     ylim = c(0,1.5))

abline(v=0.2, 
       col="red", 
       lty=3)

# 10분 P(X<=3/10) 이내로 받을 확률
pexp(q=3/10, rate=2, lower.tail = TRUE)

# 95%확률로 받을 수 있는 시간
qexp(p=0.95, rate=2, lower.tail = TRUE)


# 부록 ggplot2 이용
library(ggplot2)
ggplot(data.frame(x=c(0,3)), aes(x=x)) + 
  stat_function(fun=dexp, 
                args=list(rate=1.5), 
                colour="red", size=1.5) + 
  ggtitle("Exponential Distribution") 

