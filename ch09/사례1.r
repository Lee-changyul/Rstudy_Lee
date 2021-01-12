#### 일원 분산분석(One-way ANOVA) #####

# 01.데이터 불러오기 
owa.df <- read.csv("Ch0903.교육역량분석.csv", 
                   header=TRUE, 
                   na.strings = ".")

owa.df$학년 <- factor(owa.df$학년,
                       levels = c(1:4),
                       labels = c("1학년","2학년","3학년","4학년"))

str(owa.df)
head(owa.df)

# 02.기본통계치 확인
library(psych)
describeBy(owa.df$비교과운영, 
           owa.df$학년, 
           mat=T) # 합쳐서 하나의 표로 보여줄지, 따로 그룹별로 보여줄지 나타내는 옵션 


# 03.그래프 그리기(박스그래프,히스토그램)
# install.packages("ggplot2")

library(ggplot2)

ggplot(owa.df, aes(x = 학년, y = 비교과운영)) + #aes 축
  geom_boxplot(outlier.colour="red") + #이상치를 레드로 하라
  ggtitle("학년별 점수 차이") +
  theme_classic() + # ggplot2 테마
  theme(title = element_text(color="darkblue", size=20))

# facet_grid(); 그룹으로 구분- 범주형 변수
# facet_grid(.~); 수직
# facet_grid(~.); 가로

ggplot(owa.df, aes(x=비교과운영)) + 
  geom_histogram(binwidth=10) + 
  facet_grid(.~ 학년) +
  ggtitle("매장별 만족도") + 
  theme_classic()

# 04.통계분석
# 등분산 검증
# 이분산일때는 하단의 부록(Welch's ANOVA test) 참조
bartlett.test(비교과운영 ~ 학년, data=owa.df)

# barlett test는 정규분포에 민감하기 때문에 leveneTest 많이 사용
# install.packages("car")
library(car)
car::leveneTest(비교과운영 ~ 학년, data=owa.df) # 레벤의 검증은 library(car)에 있다. 

# 등분산일때 ANOVA분석
owa.result <- aov(비교과운영 ~ 학년, data=owa.df) 

owa.result

summary(owa.result)

# 부록: 이분산일때 Welch's ANOVA test
oneway.test(owa.df$비교과운영 ~ owa.df$학년, data=owa.df, 
            var.equal = FALSE)


# 사후검정(Multicamparison test )
# Fisher LSD
pairwise.t.test(owa.df$비교과운영, 
                owa.df$학년, 
                data=owa.df, 
                p.adj="non") # 많이 않쓰임. 오차가 크기 때문에 

# Bonferroni, Tukey HSD, Duncan LSR
pairwise.t.test(owa.df$비교과운영, 
                owa.df$학년, 
                data=owa.df, 
                p.adj="bonf") # 여기서비교하는 방법을 알려줌. 이것은 유의확률을 보여줌. 강남-강동 차이 있음 

# Tukey HSD, Duncan LSR
TukeyHSD(owa.result) # 결과값으로 바로 보여줌 (분산분석 결과치를 데이타프레임으로 저장하고 입력)




# 학년으로 표현
# install.packages("agricolae")
library(agricolae)

# console=TRUE: 결과를 화면에 표시
# 학년=TRUE: 그룹으로 묶어서 표시, FALSE: 1:1로 비교하여 다 보여줌, 다른 것끼리 구분되는 것 
LSD.test(owa.result, 
         "학년", 
         console = T, 
         p.adj="bonf")
duncan.test(owa.result, 
            "학년", 
            group=T, 
            console = T)
scheffe.test(owa.result, 
             "학년", 
             group=T, 
             console = T)




# 05.통계결과 그래프
tukeyPlot <- TukeyHSD(owa.result) # 그룹간 차이 비교
plot(tukeyPlot) # 0을 거치고 있다는 것은 차이가 없다는 뜻임




# 부록: 비모수통계분석 Kruskal Wallis H test 
kruskal.test(owa.df$비교과운영 ~ owa.df$학년, 
             data=owa.df)
# install.packages("userfriendlyscience")
library(userfriendlyscience)
posthocTGH(owa.df$비교과운영, 
           owa.df$학년)


# install.packages("nparcomp")  # 비모수 검정에서 다중 비교 방식 
library(nparcomp)
result = mctp(owa.df$비교과운영 ~ owa.df$학년, 
              data=owa.df)
summary(result)
