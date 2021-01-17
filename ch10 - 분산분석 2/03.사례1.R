#### 사례분석 1 #####
# 문제의 정의
# K대학에서는 재학생을 대상으로 교육과정에 대한 현황조사를 실시하였다.
# 학부(4개학부)와 학년에 따라 
# 교양강의운영, 전공강의운영, 비교과운영, 종합점수가 차이가 있는 가?
# 학부와 학년간에는 비교과운영의 상호작용효과가 있는지 검증하세요.








# 01.데이터 불러오기 
edu.df <- read.csv("Ch1003.교육역량분석.csv", 
                   header=TRUE, 
                   na.strings = ".")
edu.df$학부 <- factor(edu.df$학부,
                    levels=c(1:4),
                    labels=c("신학","사회복지","아동상담","경영"))
edu.df$학년 <- factor(edu.df$학년,
                    levels=c(1:4),
                    labels=c("1학년","2학년","3학년","4학년"))

str(edu.df)

head(edu.df)




# 02.기본통계치 확인
library(psych)
describeBy(edu.df$비교과운영, edu.df$학부:edu.df$학년, mat=T)








# 03.그래프 그리기(박스그래프,히스토그램)
plot(비교과운영 ~ 학부 + 학년, data=edu.df)
boxplot(비교과운영 ~ 학부*학년, data=edu.df)






library(ggplot2)
ggplot(edu.df, aes(x = 학년, y = 비교과운영)) + 
  geom_boxplot(outlier.colour="red") +
  facet_wrap(~학부) +
  ggtitle("학년*학부에 따른 점수")







# 04.통계분석
twa.result <- aov(비교과운영 ~ 학부 + 학년 + 학부:학년, data=edu.df) 
summary(twa.result)

# 상호작효과가 의미가 없어서 제거해주고 다시 계산
twa.result <- aov(비교과운영 ~ 학부 + 학년, data=edu.df) 
summary(twa.result)






#상호작용효과 그래프
interaction.plot(edu.df$학부, edu.df$학년, edu.df$비교과운영)


# 사후검정 : 그룹이 3개 이상이므로 

TukeyHSD(twa.result)
tukeyPlot <- TukeyHSD(twa.result)
plot(tukeyPlot)
