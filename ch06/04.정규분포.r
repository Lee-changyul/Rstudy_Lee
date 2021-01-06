# 04. 정규분포
# 난수함수: rnorm(n, mean=0, sd=1)
# 확률밀도함수: d~
# 누적분포함수(점수): p~
# 누적분포함수(확률): q~

# 난수함수: rnorm(n, mean=0, sd=1)
x <-rnorm(100, 50,20)
x <- sort(x)
plot(x, 
     dnorm(x, 50,20), 
     type='l', 
     main="정규분포, 
     X~N(50,20)")
abline(v=50, 
       col="blue", 
       lty=3)

# 정규분포 누적분포함수(점수)
# pnorm(q, mean=0, sd=1, lower.tail=FALSE)
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# 만약 60점이라면 
pnorm(60, mean=50, sd=20, lower.tail=TRUE)
pnorm(60, mean=50, sd=20, lower.tail=FALSE)
abline(v=60, col="red", lty=3)


# 누적분포함수(확률)
# qnorm(p, mean=0, sd=1, lower.tail=FALSE)
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# 상위 35% 안에 들어가려면 
qnorm(0.35, mean=50, sd=20, lower.tail=FALSE)
qnorm(0.35, mean=50, sd=20, lower.tail=TRUE)
abline(v=57.7, col="green", lty=3)

# 정규분포 그래프
plot(x, 
     dnorm(x, 50,20), 
     type='l', 
     main="정규분포, 
     X~N(50,20)")
abline(v=50, col="blue", lty=3)
abline(v=50+1.96*20, col="red", lty=3)
abline(v=50-1.96*20, col="red", lty=3)

# 표준정규분포 그래프
x <- seq(-3, 3, length=200) 
plot(x, 
     dnorm(x, mean=0, sd=1), 
     type='l', 
     main="Normal distribution, X~N(0,1)") 


# 퀴즈
#대학 경영통계 수업 듣는 학생의 시험점수가 평균 70, 표준편차가 20점이라고 한다. 
#상위 35%에 들기 위한 점수는?
qnorm(0.35, mean=70, sd=20, lower.tail=FALSE)

#B이상(80%)에 들기 위한 점수는?7
qnorm(0.80, mean=70, sd=20, lower.tail=FALSE)
