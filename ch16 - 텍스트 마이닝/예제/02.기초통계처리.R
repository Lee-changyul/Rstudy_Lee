## 02.기초통계처리 : 독립표본(Independent Sample) t-test 

#####

# 01.데이터 불러오기(데이터 프레임형식) 
ist <- read.csv("02.ist.csv", 
                header=TRUE, 
                na.strings = ".")
ist$t_group <- factor(ist$t_group,
                      levels=c(1,2),
                      labels=c("A자동차","B자동차"))
str(ist)

# 02.통계분석
# 등분산이면 var.equal=TRUE, 이분산이면 var.equal=FALSE
t.test(ist$t_time ~ ist$t_group, 
       data = ist, 
       alternative = c("two.sided"), 
       var.equal = TRUE, 
       conf.level = 0.95)

