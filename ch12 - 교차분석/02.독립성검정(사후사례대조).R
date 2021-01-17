#### 02.독립성 검정: 사후사례대조 #####
### 표로 정리되어 있는 2차 데이터로 처리



# 01.데이터 불러오기 
postcro.df <- read.csv("Ch1202.사후설계교차분석(PostCH).csv", 
                       header=TRUE, 
                       na.strings = ".")
postcro.df$cancer <- factor(postcro.df$cancer,
                             levels=c(1,2),
                             labels=c("No","Yes"))
postcro.df$smoking <- factor(postcro.df$smoking,
                             levels=c(1:5),
                             labels=c("비흡연군",
                                      "장기금연군",
                                      "단기금연군",
                                      "재흡연군",
                                      "흡연군"))

str(postcro.df)








#02.카이스케어 분석
# install.packages("gmodels")
library(gmodels)
postcro.table <- xtabs(count ~ cancer + smoking , # 2차 데이터를 가지고 계산할 때 활용 xtabs 
                       data=postcro.df)
postcro.result <- CrossTable(postcro.table,
                            expected=TRUE,
                            chisq=TRUE,
                            asresid=F)







#03.수정된 표준잔차
postcro.result$chisq$stdres








# 04.오즈비(odds ratio)
# 위험요인과 질병 발생간의 연관성을 1을 기준으로 나타낸 척도
# 흡연을 하면 폐암에 걸릴 확률이 몇 배나 높아질 것인지?
# 담배를 피는 사람(흡연자)가 암에 걸릴 확률 / 담배를 피지 않는 사람(비흡연자)이 암에 걸릴 확률

odds.smok <- 504/27784
odds.norm <- 723/170867
odds.smok/odds.norm



