#### 이원 반복측정 분산분석(Two-way Repeated Measures ANOVA) #####

 # 실험군과 대조군에 대한 통증 변화 관찰 
 # 사전 검사와 사후 검사의 차이발생, 상호작용이 발생하였는가?
 # 사람의 데이터 ID가 반드시 들어가야 한다. 

# 01.데이터 불러오기
twrma.df <- read.csv("Ch1002.TWRMA.csv", 
                    header=TRUE, 
                    na.strings = ".")
twrma.df$group <- factor(twrma.df$group,
                       levels=c(1,2),
                       labels=c("대조군","실험군"))
twrma.df$time <- factor(twrma.df$time,
                        levels=c(1:2),
                        labels=c("사전","사후"))
str(twrma.df)

# [참고] 롱테일 그래프 

library(tidyr)

# gather() # melt()와 같음 Wide -> long
twrma.df_wide <- tidyr::spread(twrma.df, "time","painscore")

twrma.df_wide

str(twrma.df_wide)

head(twrma.df_wide)

# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describeBy(twrma.df$painscore, 
           twrma.df$time:twrma.df$group, 
           mat=T)

describeBy(painscore ~ time:group, data = twrma.df, mat=T) # 데이터 사용하는 방식에 따라서 표시되는 방식에 약간의 차이가 있음 





# 03.그래프 그리기(박스그래프,히스토그램)
boxplot(painscore ~ group + time, # 가운데에 + * 모두 상관없이 그래프가 나온다. 
        data=twrma.df)

library(ggplot2)
ggplot(twrma.df, 
       aes(x = group, y = painscore)) + 
  geom_boxplot(outlier.colour="red") +
  facet_wrap(~time)
  ggtitle("실험군과 대조군의 실험전후 통증") +
  theme_classic() + # ggplot2 테마

    
    
    
    
    
    
# 04.통계분석

# 구형성(sphericity)검정: Mauchly’s test.
# 본 예제는 변수가 2개 이기 때문에 구형성 검정 없음
require(car)
twrma.matrix <- cbind(twrma.df$painscore[twrma.df$time=="사전"], 
                     twrma.df$painscore[twrma.df$time=="사후"])
twrma.model.lm <- lm(twrma.matrix ~ 1)
time.f <- factor(c("사전","사후"))
options(contrasts=c("contr.sum", "contr.poly"))
twrma.result.mt <- Anova(twrma.model.lm, 
                         idata=data.frame(time.f), #Anova 대문자
                         idesign=~time.f, 
                         type="III")
summary(twrma.result.mt, multivariate=T)


# 먄약 내가 만든 데이터를 입력한다면? 
twrma.model.lm <- lm(twrma.df_wide ~ 1)
time.f <- factor(c("사전","사후"))
options(contrasts=c("contr.sum", "contr.poly"))
twrma.result.mt <- Anova(twrma.model.lm, 
                         idata=data.frame(time.f), #Anova 대문자
                         idesign=~time.f, 
                         type="III")
summary(twrma.result.mt, multivariate=T)



# ANOVA 검정 (방법1=방법2)
twrma.result <- aov(painscore ~ time*group + Error(id), data=twrma.df)
summary(twrma.result)

twrma.result <- aov(painscore ~ time + group + time:group, data=twrma.df) 
summary(twrma.result)






#상호작용효과 그래프
interaction.plot(twrma.df$time, twrma.df$group, twrma.df$painscore)






# 사후검정(Multicamparison test )
# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
# 상호작용이 없을 경우 : 각 변수별 주효과 분석(t-test, ANOVA분석)
# 사전-사후에 따른 그룹 비교
pre <- twrma.df[twrma.df$time=="사전",]
post <- twrma.df[twrma.df$time=="사후",]

t.test(painscore ~ group, 
       data = pre, 
       alternative = c("two.sided"), 
       var.equal = TRUE,
       conf.level = 0.95)
t.test(painscore ~ group, 
       data = post, 
       alternative = c("two.sided"), 
       var.equal = TRUE,
       conf.level = 0.95)








# 그룹에 따른 사전-사후 비교
controlG <- twrma.df[twrma.df$group=="대조군",]
treatG <- twrma.df[twrma.df$group=="실험군",]

t.test(painscore ~ time, 
       data = controlG, 
       alternative = c("two.sided"), 
       var.equal = TRUE,
       conf.level = 0.95)
t.test(painscore ~ time, 
       data = treatG, 
       alternative = c("two.sided"), 
       var.equal = TRUE,
       conf.level = 0.95)








# 부록 : Tukey방법 - t-test와 결과는 같음
#install.packages("multcomp")
library(multcomp)
resultLm <- lm(painscore ~ group, data=pre)
TukeyResult <- glht(resultLm, linfct=mcp(group='Tukey'))
summary(TukeyResult)

resultLm <- lm(painscore ~ group, data=post)
TukeyResult <- glht(resultLm, linfct=mcp(group='Tukey'))
summary(TukeyResult)
