#### 01.동질성 검정:사전실험설계 #####



# 01.데이터 불러오기 
precro.df <- read.csv("Ch1201.사전설계교차분석(PreCH).csv", 
                     header=TRUE, 
                     na.strings = ".")
precro.df$group <- factor(precro.df$group,
                      levels=c(1,2),
                      labels=c("비타민","Placebo"))
precro.df$cold <- factor(precro.df$cold,
                           levels=c(1,2),
                           labels=c("noCold","Cold"))
str(precro.df)







#02.카이스케어 분석
# install.packages("gmodels")
library(gmodels)
precro.table <- table(precro.df$group, 
                      precro.df$cold)
precro.result <- CrossTable(precro.table,
                           expected=TRUE,
                           chisq=TRUE,
                           asresid=F)






#03.수정된 표준잔차
precro.result$chisq$stdres

# 1.96보다 높으면 차이가 있음. 






#04.상대위험율 계산
# 비타민은 복용하면 감기에 안걸릴 확률이 
# 그렇지 않은 사람에 비해 2.57배 높게 나타남
# 감기에 비타민을 먹은 사람이 안먹은 사람에 비하여 

0.660/0.240








#부록.chisq.test 함수로 계산
result <- chisq.test(precro.table)
result$expected
result$stdres
