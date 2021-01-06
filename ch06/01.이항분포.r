## Ch06.확률분포 : 도수분포(frequency table)의 %

abs <- c(0:4)
stu.n <- c(40,35,15,7,3)

pro.stu <- data.frame(abs,stu.n)

prop.stu1 <- prop.table(stu.n)

barplot(prop.stu1 ~ abs,
        main ="확률분포",
        xlab = "결석일수",
        ylab = "학생수")


# 이산확률변수와 연속확률변수의 차이 이해 (특정수치(사건 수, 방문자 수 등) vs. 연속변수(몸무게, 체온, 통근시간 등)

# 확률밀도함수: d~ 개별 건수에 대한 확률 확인
# 누적분포함수(점수): p~ 점수(숫자)를 가지고 몇 %에 해당하는지를 파악
# 누적분포함수(확률): q~ 확률을 가지고 몇 점에 해당하는지를 파악 

## 01.이항분포
# 이항분포 확률밀도 : 확률이 두 가지로 구분되어 있을 경우 활용하는 방법 
# dbinom(성공회수, size(시행횟수(n)), prob(성공확률)

# fortune 쿠키 성공사례 : 식당에서 쿠키를 나누어줌
# 시행횟수: 3명
# 성공확률: 90% (쿠키가 들어있을 확률)
# P(X = a) 확률 계산
dbinom(0, size=3, prob=0.9)
dbinom(1, size=3, prob=0.9)
dbinom(2, size=3, prob=0.9)
dbinom(3, size=3, prob=0.9) 

# 이항분포 확률밀도 graph
y <- dbinom(0:3, size=3, prob=0.9)
plot(0:3, y,    # x를 0부터 3까지 4개로 쪼개라 
     type='h',  #   유형이 달라지면, 그래프의 모양도 달라짐. 옵션은 찾아봐야 함 
     lwd=10, 
     col="red", 
     ylab="확률", 
     xlab="성공확률 X",
     main = "이항분포")


# 이항분포 누적확률계산
# 이하 : P(X <= a) lower.tail = TRUE * 여기가 옵션 넣을 때 매우 중요한 부분임. R의 기본 옵션은 이하와 초과로 구분
# 초과: P(X > a) lower.tail = FALSE
# P(X <= 1)이하로 받을 확률 => 3번 중에 1번 이하로 성공할 확률(누적확률이므로 pbinom사용 )
pbinom(1, size=3, prob=0.9, lower.tail = TRUE)

# 1명이상 받을 확률: P(X >= 1) 확률 계산, 초과이므로 최소 0을 넣어야 함. lowe.tail 옵션은 FALSE
pbinom(0, size=3, prob=0.9, lower.tail = FALSE)

# 누적이항분포  graph

plot(pbinom(0:3, size=3, prob=0.9), 
     type='h', 
     lwd=10, 
     col="red", 
     main = "누적이항분포")


## 퀴즈
# 성공확률 0.7, 10명중 7명 이상: P(X >= 7) 확률 계산

pbinom(6, size=10, prob=0.7, lower.tail = FALSE)


# 확률밀도 graph
y <- dbinom(0:10, size=10, prob=0.7)
plot(0:10, y, 
     type='h', 
     lwd=10, 
     col="red", 
     ylab="확률", 
     xlab="성공확률 X",
     main = "이항분포")

# 누적이항분포  graph
plot(pbinom(0:10, size=10, prob=0.7), 
     type='h', 
     lwd=10, 
     col="red", 
     main = "누적이항분포")
