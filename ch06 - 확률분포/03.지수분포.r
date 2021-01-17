#03. 지수분포
# 확률밀도함수: d~
# 누적분포함수(점수): p~ 건수로 확률 추출
# 누적분포함수(확률): q~ 확률로 건수(시간) 추출

# 지수분포 누적확률계산
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# pexp(q=대기시간, rate=평균발생건수(λ))
# 서비스센터 사례 - 평균발생건수: 5분에 1.5회
# 1분 P(X<=1/5) 이내로 받을 확률

pexp(q=1/5, rate=1.5, lower.tail = TRUE)

# 95%확률로 받을 수 있는 시간
qexp(p=0.95, rate=1.5, lower.tail = TRUE) # 누적하면 95% 이하이므로 (10분에 1콜이 무조건 온다.)


# 지수분포 누적확률 gra ph
x <- seq(0, 3, length = 500) # 0부터 3까지 500로 쪼개서 넣어라. 여기서는 단위가 5분이니까 숫자 1 = 5분이 된다. 
plot(x, 
     dexp(x, 1.5), # x 각각의 값을 y축에 뿌려주어라.  
     type = "l", 
     ylim = c(0,1.5))

# 그래프의 의미 : 대기시간이 x에 도달 했을 때까지 전화를 못받을 확률을 알아보는 것. 

abline(h=0.2,  # 그래프에 선을 그려주는 함수. h= 가로선이 그려짐
       col="red", 
       lty=1) # line 유형 
 
# 10분에 평균 2회 전화, 3분 이내에 전화를 받을 확률 P(X<=3/10) 이하의 개념 이므로 lower.tail = TRUE
pexp(q=3/10, rate=2, lower.tail = TRUE)

# 95%확률로 받을 수 있는 시간 : 15분 이내에 무조건 전화 한번은 받는다. (95% 확률로)
qexp(p=0.95, rate=2, lower.tail = TRUE)


# 부록 ggplot2 이용
library(ggplot2)
ggplot(data.frame(x=c(0,3)), aes(x=x)) + 
  stat_function(fun=dexp, 
                args=list(rate=1.5), 
                colour="red", size=1.5) + 
  ggtitle("Exponential Distribution") 

