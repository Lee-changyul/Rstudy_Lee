#### 일원 분산분석(One-way ANOVA) #####

# 01.데이터 불러오기 
owa.df <- read.csv("Ch0901.OWA.csv", 
                    header=TRUE, 
                    na.strings = ".")

owa.df$group <- factor(owa.df$group,
                       levels = c(1:4),
                       labels = c("강남","강서","강동","강북"))

str(owa.df)
head(owa.df)

# 02.기본통계치 확인
library(psych)
describeBy(owa.df$score, 
           owa.df$group, 
           mat=T)


# 03.그래프 그리기(박스그래프,히스토그램)
# install.packages("ggplot2")

library(ggplot2)

ggplot(owa.df, aes(x = group, y = score)) + #aes 축
  geom_boxplot(outlier.colour="red") + #이상치를 레드로 하라
  ggtitle("매장별 만족도") +
  theme_classic() + # ggplot2 테마
  theme(title = element_text(color="darkblue", size=20))

# facet_grid(); 그룹으로 구분- 범주형 변수
# facet_grid(.~); 수직
# facet_grid(~.); 가로

ggplot(owa.df, aes(x=score)) + 
  geom_histogram(binwidth=10) + 
  facet_grid(. ~ group) +
  ggtitle("매장별 만족도") + 
  theme_classic()

# 04.통계분석
# 등분산 검증
# 이분산일때는 하단의 부록(Welch's ANOVA test) 참조
bartlett.test(score ~ group, data=owa.df)

# barlett test는 정규분포에 민감하기 때문에 leveneTest 많이 사용
# install.packages("car")
library(car)
leveneTest(score ~ group, data=owa.df) # 레벤의 검증은 library(car)에 있다. 

# 등분산일때 ANOVA분석
owa.result <- aov(score ~ group, data=owa.df) 

owa.result

summary(owa.result)

# 부록: 이분산일때 Welch's ANOVA test
oneway.test(owa.df$score ~ owa.df$group, data=owa.df, 
            var.equal = FALSE)


# 사후검정(Multicamparison test )
# Fisher LSD
pairwise.t.test(owa.df$score, 
                owa.df$group, 
                data=owa.df, 
                p.adj="non") # 많이 않쓰임

# Bonferroni, Tukey HSD, Duncan LSR
pairwise.t.test(owa.df$score, 
                owa.df$group, 
                data=owa.df, 
                p.adj="bonf") # 여기서비교하는 방법을 알려줌. 이것은 유의확률을 보여줌. 강남-강동 차이 있음 

# Tukey HSD, Duncan LSR
TukeyHSD(owa.result) # 결과값으로 바로 보여줌




# group으로 표현
# install.packages("agricolae")
library(agricolae)

# console=TRUE: 결과를 화면에 표시
# group=TRUE: 그룹으로 묶어서 표시, FALSE: 1:1로 비교하여 다 보여줌, 다른 것끼리 구분되는 것 
LSD.test(owa.result, 
         "group", 
         console = T, 
         p.adj="bonf")
duncan.test(owa.result, 
            "group", 
            group=T, 
            console = T)
scheffe.test(owa.result, 
             "group", 
             group=T, 
             console = T)







# 05.통계결과 그래프
tukeyPlot <- TukeyHSD(owa.result) # 그룹간 차이 비교
plot(tukeyPlot) # 0을 거치고 있다는 것은 차이가 없다는 뜻임







# 정규분포로 표시(강남)
x=88.87 
se=1.34
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     col="blue",
     type='l', 
     main="매장별 고객만족도", 
     xlim=c(75, 95), 
     ylim=c(0,0.3))
abline(v=x, col="blue", lty=3)


# 그래프를 겹쳐서 표현하기 
par(new=T) 


# 정규분포로 표시(강서)
x=88.19 
se=1.33
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     type='l', 
     col="red",
     xlim=c(75, 95), 
     ylim=c(0,0.3))
abline(v=x, col="red", lty=3)


par(new=T)

# 정규분포로 표시(강북)
x=86.05 
se=1.58
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     type='l', 
     col="black",
     xlim=c(75, 95), 
     ylim=c(0,0.3))
abline(v=x, col="black", lty=3)


par(new=T)


# 정규분포로 표시(강동)
x=82 
se=2.05
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, 
     dnorm(data, x, se), 
     type='l', 
     col="green",
     xlim=c(75, 95), 
     ylim=c(0,0.3))
abline(v=x, col="green", lty=3)



# install.packages("nparcomp")
library(nparcomp)
result = mctp(owa.df$score ~ owa.df$group, 
              data=owa.df)
summary(result)


# 부록: 비모수통계분석 Kruskal Wallis H test 
kruskal.test(owa.df$score ~ owa.df$group, 
             data=owa.df)
install.packages("userfriendlyscience")
library(userfriendlyscience)
posthocTGH(owa.df$score, 
           owa.df$group)



