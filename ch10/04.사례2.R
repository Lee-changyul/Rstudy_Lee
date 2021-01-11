#### 사례분석 2 #####
# 문제의 정의
# 다음은 호흡과 뇌파와의 관계를 연구한 자료이다. 
# 호흡을 3번(흡호=1:1, 7:3, 3:7)측정하였다. 
# 성별에 따라 호흡이 뇌파간 차이가 있는 가?
# Ch1al에 대해서만 검증하세요.
  
  
  



# 01.데이터 불러오기
twrma.df <- read.csv("Ch1004.호흡과 뇌파.csv", 
                     header=TRUE, 
                     na.strings = ".")
twrma.df$sex <- factor(twrma.df$sex,
                         levels=c(1,2),
                         labels=c("남자","여자"))
twrma.df$breath <- factor(twrma.df$breath,
                        levels=c(1:3),
                        labels=c("1:1","7:3","3:7"))
str(twrma.df)







# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describeBy(twrma.df$ch1al, 
           twrma.df$breath:twrma.df$sex, 
           mat=T)







# 03.그래프 그리기(박스그래프,히스토그램)
boxplot(ch1al ~ sex*breath, 
        data=twrma.df)

library(ggplot2)
ggplot(twrma.df, 
       aes(x = sex, y = ch1al)) + 
  geom_boxplot(outlier.colour="red") +
  facet_wrap(~breath)
ggtitle("성별에 따른 호흡의 뇌파변화") +
  theme_classic() + # ggplot2 테마
  
  
  
  
  
  
  
# 04.통계분석

# 구형성(sphericity)검정: Mauchly’s test.
# 본 예제는 변수가 2개 이기 때문에 구형성 검정 없음
require(car)
twrma.matrix <- cbind(twrma.df$ch1al[twrma.df$breath=="1:1"], 
                      twrma.df$ch1al[twrma.df$breath=="7:3"],
                      twrma.df$ch1al[twrma.df$breath=="3:7"])
twrma.model.lm <- lm(twrma.matrix ~ 1)
breath.f <- factor(c("1:1","7:3","3:7"))
options(contrasts=c("contr.sum", "contr.poly"))
twrma.result.mt <- Anova(twrma.model.lm, 
                         idata=data.frame(breath.f), #Anova 대문자
                         idesign=~breath.f, 
                         type="III")
summary(twrma.result.mt, multivariate=T)







# ANOVA 검정 (방법1=방법2)
twrma.result <- aov(ch1al ~ breath*sex + Error(id), data=twrma.df)
summary(twrma.result)

twrma.result <- aov(ch1al ~ breath + sex, data=twrma.df) 
summary(twrma.result)






#상호작용효과 그래프
interaction.plot(twrma.df$breath, twrma.df$sex, twrma.df$ch1al)






