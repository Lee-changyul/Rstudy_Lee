# 04. 정규분포

# 정규분포 : 평균과 분산으로 표시 
# 난수함수: rnorm(n, mean=0, sd=1)
# 확률밀도함수: d~
# 누적분포함수(점수): p~
# 누적분포함수(확률): q~

# 난수함수: rnorm(n, mean=0, sd=1)
x <-rnorm(100,50,20) # 숫자 100개를 무작위로 뽑아라. 그런데 평균이 50이고 표준편차가 20이다. 랜덤으로 추출

x # 무작위 추출

x <- sort(x) # 순서대로 정리 

x <- round(x,2)

plot(x, 
     dnorm(x,50,20), 
     type='l', # 선을 연결하는 옵션 
     main="정규분포, X~N(50,20)")
abline(v=50, # 평균이 50이니까 보기좋게 선 그린 것  
       col="blue", 
       lty=2) # 라인 스타일 

# 정규분포 누적분포함수(점수)
# pnorm(q, mean=0, sd=1, lower.tail=FALSE)
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# 만약 60점이라면 
pnorm(60, mean=50, sd=20, lower.tail=TRUE) # 60점 이하 => 60점이면 아래에 전체의 몇 %가 있는가? 아래료 70%
pnorm(60, mean=50, sd=20, lower.tail=FALSE) # 60점 초과 => 60점을 초과하는 학생은 전체의 몇 % 인가? 상위 몇 %에 속하는가? 30%
abline(v=60, col="red", lty=3)


# 누적분포함수(확률) => 몇 % 일때 점수를 알고 싶다. 
# qnorm(p, mean=0, sd=1, lower.tail=FALSE)
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# 상위 35% 안에 들어가려면 
qnorm(0.35, mean=50, sd=20, lower.tail=FALSE) # 상위 35%의 점수를 알려주어라. 위에서 부터 재는 것
qnorm(0.35, mean=50, sd=20, lower.tail=TRUE) # 하위 35%의 점수를 알려주어라. 
abline(v=57.7, col="green", lty=3) 

# 정규분포 그래프
plot(x, 
     dnorm(x, 50,20), 
     type='l', 
     main="정규분포, X~N(50,20)")
abline(v=50, col="blue", lty=3)
abline(v=50+1.96*20, col="red", lty=3) # 신뢰수준 95% 구간에 표시하라. 
abline(v=50-1.96*20, col="red", lty=3)

# 표준정규분포 그래프 <- 평균을 0로 하여 그린 그래프 
x <- seq(-3, 3, length=200) 
plot(x, 
     dnorm(x, mean=0, sd=1), 
     type='l', 
     main="Normal distribution, X~N(0,1)") 


# 퀴즈
#대학 경영통계 수업 듣는 학생의 시험점수가 평균 70, 표준편차가 20점이라고 한다. 
#상위 35%에 들기 위한 점수는?

qnorm(0.35, 70, 20, lower.tail = FALSE)
qnorm(0.35, mean=70, sd=20, lower.tail=FALSE)


#B이상(80%)에 들기 위한 점수는?7
qnorm(0.2, mean=70, sd=20, lower.tail=TRUE)
