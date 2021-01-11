#### 이원 분산분석(Two-way ANOVA) #####

# 01.데이터 불러오기 
twa.df <- read.csv("Ch1001.TWA.csv", 
                    header=TRUE, 
                    na.strings = ".")
twa.df$meth <- factor(twa.df$meth,
                        levels=c(1:2),
                        labels=c("오븐","기름"))
twa.df$temp <- factor(twa.df$temp,
                       levels=c(1:2),
                       labels=c("200도","300도"))
str(twa.df)







# 02.기본통계치 확인
library(psych)
describeBy(twa.df$taste, twa.df$meth:twa.df$temp, mat=T) # 변수 두개를 비교하면서 해라 : 표시








# 03.그래프 그리기(박스그래프,히스토그램)
plot(taste ~ meth + temp, data=twa.df) # 두개를 보여주는 방법 + 기호 사용
boxplot(taste ~ meth*temp, data=twa.df) # 두개를 보여주는 방법 * 기호 사용






library(ggplot2)
ggplot(twa.df, aes(x = temp, y = taste)) + 
  geom_boxplot(outlier.colour="red") +
  facet_wrap(~meth) +
  ggtitle("방법*온도에 따른 맛")







# 04.통계분석
twa.result <- aov(taste ~ meth + temp + meth:temp, data=twa.df) # 방법에 대하여, 온도에 대하여, 상호작용에 대하여 분석

# 상호작용 효과가 유의하다 : 그대로 사용, 유의하지 않다. 오차항에 다시 포함하여 통계를 다시 계산함  


summary(twa.result)

options("scipen" = 20)





#상호작용효과 그래프
interaction.plot(twa.df$meth, twa.df$temp, twa.df$taste)
                 
# 그래프 순서에 유의, 독립변수가 먼저 오고 종속변수가 나중에 옴 (방법/온도 - 맛)
# x축과 y축을 목적에 따라 지정하면 됨



# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
# 상호작용이 없을 경우 : 각 변수별 주효과 분석(t-test, ANOVA분석)


# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
tw1 <- twa.df[twa.df$meth=="오븐",]  # 방법을 먼저 쪼개로 온도에 따른 차이가 있는지를 보도록하겠다. 
tw2 <- twa.df[twa.df$meth=="기름",]

t.test(taste ~ temp, 
       data=tw1, 
       alternative = c("two.sided"), 
       var.equal = TRUE, 
       conf.level = 0.95)
t.test(taste ~ temp, 
       data=tw2, 
       alternative = c("two.sided"), 
       var.equal = TRUE, 
       conf.level = 0.95)









# 부록 : 일반 분석 TukeyHSD 결과 해석
TukeyHSD(twa.result)
tukeyPlot <- TukeyHSD(twa.result)
plot(tukeyPlot)
