freq <- read.csv("0301.frequency.csv",
                 header = T,
                 na.strings = ".")
str(freq)
freq$grade <- factor(freq$grade,
                     levels = c(5:1),
                     labels = c("A","B","C","D","F"))
                              
str(freq$grade)

str(freq)

attach(freq)

# 일변량 범주형 자료 ####

barplot(grade)

# 그래프로 된 것만 barplot 으로 그릴 수 있다. table로 정리한 후에 그려야 한다. - 원자료 사용못한다. 
# 기본 막대그래프 

grade <- table(grade)
barplot(grade)

# 그래프 옵션

barplot(grade, # 원데이터가 아니라 테이블로 만든 데이터 
        main = "학점별 분포",
        xlab = "학점",
        ylab = "명",
        ylim = c(0,30),
        legend =rownames(grade)) # 범례, 행네임을 grade에 있는 A - F로 붙여 주어라. 

# 수평 막대 그래프

barplot(grade,  # 원데이터가 아니라 테이블로 만든 데이터 
       main = "학점별 분포", 
       horiz = TRUE, # 가로형태로 만들어라 
       xlab = "명",
       ylab = "학점",
       xlim = c(0,30), 
       col = rainbow(length(grade)), # heat.colors 로 해도됨 
       legend = rownames(grade))

# 원그래프 ####

pie(grade,
    main = "학점별 분포",
    init.angle = 90, # 처음 각도를 0도에서 시작하라. 
    col = rainbow(length(grade))) # grade 테이블의 행의 갯수 

legend(1 ,1, # 범례지정함수는 그래프와 별도로 입력할 수 있음 1,1은 위치지정 ()
       rownames(grade),
       cex = 1, # 범례 글자 크기 
       fill = rainbow(length(grade))) # 색깔입력 

# 3D 원그래프 옵션 ####

# install.packages("plotrix")
library(plotrix)

pie3D(grade,
      main = "학점별 분포",
      labels=grade,
      explode=0.1,
      col=heat.colors(5))  # 파이가 쪼개지는 정도
legend(0.8 ,1, # 범례지정함수는 그래프와 별도로 입력할 수 있음 1,1은 위치지정 ()
       rownames(grade),
       cex = 0.8, # 범례 글자 크기 
       fill = heat.colors(5)) # 색깔입력

detach(freq)

# 누적 막대그래프와 모자이크 ####

pre <- read.csv("0302.pre.csv",
                header = T,
                na.strings = ".")

str(pre)

pre$treat <- factor(pre$treat,
                    levels = c(1,2),
                    labels = c("비타민","Placebo"))
pre$cold <- factor(pre$cold,
                   levels = c(1,2),
                   labels = c("Cold","noCold"))
str(pre)

attach(pre)

# 누적막대 그래프(barplot), 범주가 2개일 때
# 데이터를 테이블로 정리

pre.t <- table(treat, cold)

pre.t

barplot(pre.t,
        main = "비타민 섭취에 따른 감기유병률",
        xlab = "집단",
        ylab = "명",
        ylim = c(0,80),
        col = c("darkblue","red"),
        legend = rownames(pre.t))

barplot(pre.t,
        main = "비타민 섭취에 따른 감기유병률",
        xlab = "집단",
        ylab = "명",
        col = c("darkblue","red"),
        legend = rownames(pre.t),
        beside = T) # 범주별로 보여주는 옵션임 

# 모자이크 그래프 ####

mosaicplot(pre.t, # 뭔가 구체적인 설명이 필요할 것 같은데... 더 찾아봐야 함. 그래프는 더 보기 좋게 나타남 
           shade = T,
           xlab = "treat",
           ylab = "감기유무",
           main = "비타민섭취에 따른 감기유병률")

detach(pre)

# 히스토그램 그리기 ####
 # 범주형 자료가 아니라 연속형 자료일 때 활용하는 그래프(칸과 칸 사이가 붙어있음, 막대그래프는 떨어짐)

wgt <- read.csv("0401.wgt.csv",
                header = T,
                na.strings = ".")
str(wgt)

wgt$sex <- factor(wgt$sex,
                  levels = c(1,2),
                  labels = c("남자","여자"))
str(wgt)

attach(wgt)

hist(weight)
boxplot(weight)
stem(weight)

# 수치형 자료를 범주형으로 구분하여 막대그래프 그리기. 
 # 미만 - 이상 으로 구분하여 개수 세고 그림 그리기 
 # if 를 쓰는 방법과 cut을 활용하는 방법이 있음. 이 것은 cut이 편함

wgt <- transform(wgt,
                 wgt.cut = cut(weight,
                               breaks=c(0,45,50,55,60,65,70,100),
                               right=FALSE, # right = FALSE : a <= x < b (이상 - 미만) / right = TRUE : a < x <= b 초과 - 이하 
                               labels=c("~45미만", "45~50미만","50~55미만","55~60미만",
                                        "60~65미만","65~70미만","70이상~")))

# 범주형 자료로 변환 (ifelse이용)
wgt<- transform(wgt,
                wgt.if = ifelse(weight < 45, "~45",
                                ifelse(weight >= 45 & weight < 50, "45~50미만",
                                       ifelse(weight >= 50 & weight < 55, "50~55미만",
                                              ifelse(weight >= 55 & weight < 60, "55~60미만",
                                                     ifelse(weight >= 60 & weight < 65, "60~65미만",
                                                            ifelse(weight >= 65 & weight < 70, "65~70미만","70~"
                                                            )
                                                     )
                                              )
                                       )
                                )
                )
)

# 데이터프레임을 새롭게 수정했을 경우에는 detach한후에 다시 attach해줘야 함

attach(wgt)
table(wgt.cut)
barplot(table(wgt.cut))

# 히스토그램 bin 설정
hist(weight, 
     main ="학생들의 체중",
     breaks=15, # 구분점을 몇개로 할지 정해라 
     col= "blue",
     min(weight),
     max(weight))


detach(wgt)

# 다변량 수치형 자료 ####
 # 그룹간 연속변수 특성 비교 (박스표)

boxplot(weight ~ sex, # 왼쪽에 있는 것이 종속변수(수치형 분석), 뒷쪽이 독립변수(구분해 주는 변수)
        data = wgt,
        main = "성별에 따른 몸무게 분포",
        xlab = "성별",
        ylab = "kg")

# 여러 연속자료의 비교 ####

load("game.RData")
str(game)

detach(wgt)
attach(game)

o1 <- mean(o1)
o2 <- mean(o2)
fb1 <- mean(fb1)
fb2 <- mean(fb2)
fb3 <- mean(fb3)

game.t <- cbind(o1,o2,fb1,fb2,fb3) # 옆으로 저장됨
game.t

barplot(game.t,
        col=c("darkblue"),
        main = "연속변수 평균",
        names.arg = c(colnames(game.t)), # 이거 의미를 확인해야함. 이름을 넣는데, 컬럼 순서대로 넣으라는 뜻?
        ylim = c(0,4))

detach(game)

# 고급 그래프 그리기 ####

library(ggplot2)

ggplot(wgt, aes(x=sex)) + 
  geom_bar()

ggplot(wgt, aes(x=sex, y=weight)) + geom_boxplot(col="red") +
  ggtitle("성별 체중 상자도표")
