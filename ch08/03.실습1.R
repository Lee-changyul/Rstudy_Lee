## 실습1
# K대학에서는 재학생(1)과 교원(2)을 대상으로 교육과정에 대한 현황조사를 실시하였다.
# 교육과정, 강의 및 학습, 행정서비스, 교육환경 및 시설, 종합점수가 재학생과 교원이 차이가 있는 가?
# 아래는 종합점수만 분석함
# 다른 변수는 각자 알아서 해보세요.

# 01.데이터 불러오기 
edu <- read.csv("Ch0803.edu.csv", 
                header=TRUE, 
                na.strings = "."
)
edu$구분 <- factor(edu$구분,
                      levels=c(1,2),
                      labels=c("재학생","교원")
)
str(edu)

# 02.기술통계분석
library(psych)
describeBy(edu$종합, 
           edu$구분, mat=T)


# 03.그래프 그리기
opar <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)) # 화면분할
boxplot(edu$종합 ~ edu$구분)
hist(edu$종합[edu$구분=="재학생"])
hist(edu$종합[edu$구분=="교원"])
par(opar)

# 04.통계분석
# 등분산 검정
var.test(edu$종합 ~ edu$구분, data = edu) 

# t-검정
# 등분산이면 var.equal=TRUE, 이분산이면 var.equal=FALSE
t.test(edu$종합 ~ edu$구분, 
       data = edu, 
       alternative = c("greater"), 
       var.equal = TRUE, 
       conf.level = 0.95)


# 05.통계결과 그래프
# 재학생
x=61.76
se=19.82069/(sqrt(50))
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     col="blue",
     type='l', 
     main="재학생 교원간 종합점수 차이",
     xlim=c(55,80), 
     ylim=c(0,0.20))
abline(v=x, col="blue", lty=3)

# 그래프를 겹쳐서 표현하기
par(new=T) 

# 교원
x=74.72
se=16.52733/(sqrt(50)) # 표준편차 / 루트 n
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     col="blue",
     type='l',
     xlim=c(55,80), 
     ylim=c(0,0.20))
abline(v=x, col="blue", lty=3)
