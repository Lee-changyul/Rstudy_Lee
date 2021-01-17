# 사후 설계 분할표 - 데이터이 형식이 가공되어 있을 때

post <- read.csv("0303.post.csv",
                 header = T, 
                 na.strings = ".")

str(post)

post$smoking <- factor(post$smoking,
                       levels = c(1,2),
                       labels = c("smok","nsmok"))
post$cancer <- factor(post$cancer,
                      levels = c(1,2),
                      labels = c("cancer","health"))

str(post)

# 여기서 observation 을 숫자가 아닌 데이터로 인식식켜줄 필요가 있다. 

attach(post)

post.n <- xtabs(observation ~ cancer+smoking, data=post)

library("gmodels")

CrossTable(post.n) # 중간에 대문자임 

post.n


detch(post)
