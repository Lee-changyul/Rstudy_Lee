setwd("C:/R")

# 2. 자료 불러오기
fat<-read.csv('fat.csv', header=T, stringsAsFactors = T, na.strings="999")


# 3. 자료 살펴보기와 변환(encoding과 recode 등)

colnames(fat)
str(fat)

install.packages("car")
library(car)
fat$EDU <- recode(fat$edu, "1=6; 2=9; 3=12; 4=16; else='NA'")
View(fat)

fat$region <- factor(fat$region, levels = c(1,2), labels = c("도시", "농촌")) 
table(fat$region)
table(fat$sex)
table(fat$sex, fat$region)

fat$rQ4 <- recode(fat$Q4, "1=5; 2=4; 3=3; 4=2; 5=1")
table(fat$Q4)
table(fat$rQ4)

install.packages("psych")
library(psych)  # alpha()함수 패키지
library(dplyr)  # select(), mutate()

factor1 <- select(fat, Q1, Q2, Q3)
alpha(factor1)

factor2 <- select(fat, rQ4, Q5, Q6)
alpha(factor2)

fat <- mutate(fat, 건강관리=(Q1+Q2+Q3)/3, 삶의질=(rQ4+Q5+Q6)/3) 
colnames(fat)

par(mfrow=c(1,2))  #화면을 1행 2열로 분할

hist(fat$bmi)
boxplot(fat$bmi)

par(mfrow=c(1,1))  # 초기화면으로 복귀
plot(fat$fat, fat$bmi)

plot(fat[,c("edu", "age", "bmi", "fat", "pressure")])

#4. 결측값 확인 처리

summary(fat) 
colSums(is.na(fat))

install.packages("DMwR")
library(DMwR)

fat2 <- centralImputation(fat)
#fat2 <- knnImputation(fat) #최근접이웃대체

colSums(is.na(fat2)) 


#5. 이상치 탐색 제거
library(car)
#fat2 <- read.csv("fat2.csv")

par(mfrow=c(1,2)) 
Boxplot(fat2$건강관리,id=list(n=5))  # car 패키지의 Boxplot() 함수
Boxplot(fat2$삶의질, id=list(n=5))

par(mfrow=c(1,1))  # 초기화면으로 복귀
scatterplot(삶의질~건강관리, data=fat2, id=list(n=5)) 


fat2 <- fat2[-c(8,531,529,396),]
dim(fat2)  #dimension

#library(dplyr)
fat3<- select(fat2, sex, edu, age, region, 건강관리, 삶의질)

colnames(fat3)

# 회귀분석
fit <- lm(삶의질 ~ 건강관리, data=fat3)
summary(fit)

fit2 <- lm(삶의질 ~ 건강관리+edu+age+sex, data=fat3)
summary(fit2)













