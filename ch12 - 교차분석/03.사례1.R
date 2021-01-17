#### 사례1 #####


# 01.데이터 불러오기 
postcro.df <- read.csv("Ch1203.호흡과 열한.csv", 
                       header=TRUE, 
                       na.strings = ".")
postcro.df$성별 <- factor(postcro.df$성별,
                            levels=c(1,2),
                            labels=c("남자","여자"))
postcro.df$열한구분 <- factor(postcro.df$열한구분,
                             levels=c(1:4),
                             labels=c("열소한소",
                                      "열소한대",
                                      "열대한소",
                                      "열대한대"))

str(postcro.df)








#02.카이스케어 분석
# install.packages("gmodels")
library(gmodels)
postcro.table <- table(postcro.df$성별, 
                       postcro.df$열한구분)
postcro.result <- CrossTable(postcro.table,
                             expected=TRUE,
                             chisq=TRUE,
                             asresid=F)







#03.수정된 표준잔차
postcro.result$chisq$stdres






