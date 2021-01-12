#### 사례1 #####
# 문제의 정의
# 다음은 호흡과 뇌파와의 관계를 연구한 자료이다. 
# Channel별로 호흡을 3번(흡호=1:1, 7:3, 3:7)측정하였다. 
# 뇌파간에는 어떤 상관관계가 있는가?
  

# 01.데이터 불러오기 
corr.df <- read.csv("Ch1105.호흡과 뇌파.csv", 
                    header=TRUE, 
                    na.strings = ".")
str(corr.df)







# 02.기본통계치 확인
library(psych)
describe(corr.df)
pairs.panels(corr.df)








# 03.상관분석
cor(corr.df, use="complete.obs", method=c("pearson"))
