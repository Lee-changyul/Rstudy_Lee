# 사전실험설계 분할표 만들기(교차분석)

#표를만들 때 위치 설정이 중요 왼쪽에 실험군과 대조군을 놓는다(행열지정과 관련된 논의)

pre <- read.csv("0302.pre.csv",
                header = TRUE,
                na.strings = ".")

# 범주를 지정해야 함

pre$treat <- factor(pre$treat, 
                    levels = c(1,2),
                    labels = c("비타민","Palcebo"))
pre$cold <- factor(pre$cold,
                   levels = c(1,2),
                   labels = c("cold","nocold"))
str(pre)

# 잘 불러와서 쉽게 작업하려면 데이터 프레임을 attch()

attach(pre) # 그리고 나서 테이블을 만든다. 

# pre.n <- xtabs(~treat+cold, data=pre)

pre.n <- table(treat, cold)

pre.n

pre.p <- prop.table(pre.n)

pre.p

pre.t <- cbind(pre.n, pre.p)

pre.t

pre.a <- addmargins(pre.t) # 기본 세팅은 행열합계 3(아래로 붙이는 것)

pre.a

detach(pre)


# 교차분석용 라이브러리 이용 (수업에서는 설명하지 않음)

# install.packages("gmodels")

library(gmodels)

pre <- CrossTable(pre$treat, pre$cold)
