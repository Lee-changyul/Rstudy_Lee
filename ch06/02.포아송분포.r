#02. 포아송분포(Poisson distribution) : 램덤하게 선택한 일정단위시간(시, 분, 초)나 공간 등 내에 발생하는 사건의 개수를 설명
  # 보통 단위시간 당 도착에 대한 모델이 많이 사용되므로 시간이 주로 사용 / 하루 사고 건수, 10분간 민원 전화 건수 등 
  # <=> 지수분포 : 도착에 따른 시간 측정에 활용 : 건과 건 사이의 시간

# 확률밀도함수: d~
# 누적분포함수(점수): p~
# 누적분포함수(확률): q~

# 포아송분포 확률밀도함수 
# dpois(발생건수, lambda(단위시간당 평균발생 건수)
# 서비스센터 사례
# 서비스 센터에서 단위 시간 당(시간은 임의로 정하는 것) 평균발생건수: 1.5회
# P(X = a) 확률 계산
dpois(x=0, lambda = 1.5)
dpois(x=1, lambda = 1.5)
dpois(x=2, lambda = 1.5)
dpois(x=3, lambda = 1.5)
dpois(x=4, lambda = 1.5)
dpois(x=5, lambda = 1.5)

# 포아송분포 확률밀도함수 graph 
plot(dpois(x=c(0:10), lambda = 1.5), # 평균을 기준으로 가장 높고 평균을 넘어가면 확률이 낮아지는 개념 
     type='h',
     lwd=10, 
     col="red", 
     xlab="성공확률 X",
     main = "포아송분포")


# 포아송분포 누적확률계산 - 사건을 알때 확률 계산
# 이하 : P(X <= a) lower.tail = TRUE
# 초과: P(X > a) lower.tail = FALSE
# 2회이상(x>=2) 받을 확률: P(X > 1) 확률 계산 -> R에서는 이상이 없으니까 초과 개념으로 해서 2회 이상이면 1을 입력한다. 
ppois(q=1, lambda = 1.5, lower.tail = FALSE)

# 포아송분포 누적확률 graph
plot(ppois(0:10, lambda = 1.5), 
     type='h', 
     lwd=10, 
     col="red", 
     main = "누적포아송분포")

## 퀴즈 K 서비스 센터 10분에 평균 2회 전화, 10분 동안 2회 이하로 전화 받을 확률?

# P(X <= 2)이하로 받을 확률
ppois(q=2, lambda = 2, lower.tail = TRUE)



