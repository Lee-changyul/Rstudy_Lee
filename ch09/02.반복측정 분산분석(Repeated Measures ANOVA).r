#### 반복측정 분산분석(Repeated Measures ANOVA) #####

# 01.데이터 불러오기 
rma.df <- read.csv("Ch0902.RMA.csv", 
                    header=TRUE, 
                    na.strings = ".")
str(rma.df)
View(rma.df)
rma.df$time <- factor(rma.df$time,
                        levels=c(1:3),
                        labels=c("사전","3개월","6개월")                       )







# 02.기본통계치 확인
library(psych)
describeBy(rma.df$score, rma.df$time, mat=T)







# 03.그래프 그리기(박스그래프,히스토그램)
boxplot(score ~ time, 
        data=rma.df, 
        ylab="score", 
        xlab="time")








# 04.통계분석
# 다변량 분석을 이용한 구형성(sphericity)검정: Mauchly’s test. 분산의 동질성 검정 방법 레빈 기법 활용한 것과 같은 논리 
  # 매트릭스를 만들어서 해야 한다. 어려우니 잘 따라서 하자. (두번하면 대응표본이지만, 세번하면 분산분석임) 
  # 현재 데이터 구조를 바꾸어야 하기 때문에

library(car)
rma.matrix <- cbind(rma.df$score[rma.df$time=="사전"], # 따로따로 뽑아서 옆으로 합쳐라
                    rma.df$score[rma.df$time=="3개월"], 
                    rma.df$score[rma.df$time=="6개월"])
head(rma.matrix)

rma.model.lm <- lm(rma.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
time.f <- factor(c("사전","3개월","6개월")) # 시간을 적자 (실제 사용할 때에는  여기만 변경해 주면 된다)
options(contrasts=c("contr.sum", "contr.poly"))
rma.result.mt <- Anova(rma.model.lm, #Anova 대문자
                       idata=data.frame(time.f),
                       idesign=~time.f, 
                       type="III")

summary(rma.result.mt, multivariate=F)






# 일변량 ANOVA 검정
rma.result <- aov(score ~ time+Error(id/time), 
                  data=rma.df)
summary(rma.result)






# 다중비교 (Multicamparison test) - t-value 포함
# install.packages("multcomp")
library(multcomp)
result.lm <- lm(score ~ time, data=rma.df)
tukey.result <- glht(result.lm, linfct=mcp(time='Tukey'))
summary(tukey.result)
plot(tukey.result)







# 부록: 사후검정(기본함수사용): Tukey HSD
tukey.result <- aov(rma.df$score ~ rma.df$time, data=rma.df)
TukeyHSD(tukey.result)
plot(TukeyHSD(tukey.result))

