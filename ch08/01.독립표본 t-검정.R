#### 독립표본(Independent Sample) t-test #####

# 01.데이터 불러오기 
ist <- read.csv("Ch0801.IST.csv", 
                header=TRUE, 
                na.strings = "."
)
ist$t_group <- factor(ist$t_group,
                      levels=c(1,2),
                      labels=c("A자동차","B자동차")
)
str(ist)

# 02.기술통계분석
library(psych)
describeBy(ist$t_time, ist$t_group, mat=T)


# 03.그래프 그리기
opar <- par(no.readonly = TRUE)
  layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)) # 화면분할
  boxplot(ist$t_time ~ ist$t_group)
  hist(ist$t_time[ist$t_group=="A자동차"])
  hist(ist$t_time[ist$t_group=="B자동차"])
par(opar)

# 04.통계분석
# 등분산 검정
var.test(ist$t_time ~ ist$t_group, data = ist) 

# t-검정
# 등분산이면 var.equal=TRUE, 이분산이면 var.equal=FALSE
t.test(ist$t_time ~ ist$t_group, 
       data = ist, 
       alternative = c("greater"), 
       var.equal = TRUE, 
       conf.level = 0.95)


# 05.통계결과 그래프
# A자동차 회사
x=48670.57
se=658.5
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     col="blue",
     type='l', 
     main="자동차회사별 타이어수명", 
     xlim=c(45000,55000), 
     ylim=c(0,0.0006))
abline(v=x, col="blue", lty=3)

# 그래프를 겹쳐서 표현하기
par(new=T) 

# B자동차 회사
x=51377.6
se=766.37
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     type='l', 
     col="red",
     xlim=c(45000,55000), 
     ylim=c(0,0.0006))
abline(v=x, col="red", lty=3)


# 부록. 비모수통계분석
shapiro.test(ist$t_time[ist$t_group=="A자동차"]) #정규분포검정 p>0.05일때
shapiro.test(ist$t_time[ist$t_group=="B자동차"]) #정규분포검정 p>0.05일때
wilcox.test(t_time ~ t_group, data = ist) # 비모수(p<0.05)일때 검정방법

# 부록 : ggpot사용하여 그래프 그리기
library(ggplot2)
ggplot(ist, aes(x=ist$t_time)) + 
  geom_histogram(binwidth=5000) + 
  ggtitle("자동차 회사별 타이어수명 밀도함수") + 
  facet_grid(. ~ ist$t_group)

ggplot(ist, aes(x=ist$t_time, colour = ist$t_group)) + 
  geom_density(fill = NA) + 
  geom_line(stat = "density") + 
  expand_limits(y = 0) + 
  ggtitle("자동차 회사별 타이어수명 밀도함수")

