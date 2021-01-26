#5 시각화

#1절. 기본 산점도

##1. 데이터 내보내기와 불러오기

data("iris")
write.csv(iris, 
          file="iris.csv", 
          row.names = F)

iris <- read.csv(file = "iris.csv", stringsAsFactors = F)

head(iris)


##2. 산점도

plot(x=iris$Sepal.Length, y=iris$Petal.Length)


attach(iris)
plot(x=Sepal.Length, y=Petal.width)


plot(Petal.Width ~ Sepal.Length, ylab="꽃잎길이", xlab="꽃받침길이", main="(a) 산점도",  xlim = c(4, 8), ylim = c(0, 3), col = c("red", "blue", "green")[Species])


layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), widths = c(2, 1), heights = c(1, 1))
plot(Sepal.Length, Petal.Width, main="(a) 산점도")
hist(Sepal.Length, main="(b) 꽃받침 히스토그램")
boxplot(Sepal.Length, main="(c) 꽃받침 박스플롯")


#2절. ggplot2

##1. ggplot2

install.packages("ggplot2")
install.packages("ggpubr")

library(ggplot2)
library(ggpubr)

head(diamonds); str(diamonds)

##2.geom_그래프 계열

p1 <- ggplot(data = diamonds, mapping = aes(x = carat, y=price, colour=clarity))  +  geom_point() + geom_smooth() + ggtitle("(a) 투명도별 산포도")

p2 <- ggplot(data = diamonds, mapping = aes(x =  carat, y=price))  +  geom_point(colour="blue", pch=1, size=0.5) +  geom_smooth()+ ggtitle("(b) 산포도와 평활그래프")

p3 <- ggplot(data = diamonds, mapping = aes(x = carat, y=price))  +  geom_smooth() + ggtitle("(c) 평활그래프")

p4 <- ggplot(data = diamonds, mapping = aes(x = carat, y=price))  +  geom_smooth(aes(group=clarity)) + ggtitle("(d) 투명도별 평활그래프")

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)



b1 <- ggplot(data=diamonds, aes(x=cut)) + geom_bar(stat="count") + ggtitle("(a) 기본 막대그래프")
b1
# stat은 통계적 처리방법을 의미한다.여기에서는 빈도(count)처리를 지시하고 있다. 막대색을 blue로 채운다. 

b2 <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) + geom_bar(stat="count") + ggtitle("(b) 변수 추가 막대그래프")
b2
# 막대를 투명도 변수로 채운다.

b3 <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) + geom_bar(stat="count", position = "dodge")+ ggtitle("(c) 묶은 세로막대 그래프 ")
b3
# position = "dodge"는 묶은 세로막대형 그래프, 즉 개별막대를 의미한다. 

ggarrange(b1,ggarrange(b2, b3, ncol = 2), nrow = 2)


h1 <- ggplot(diamonds, aes(price)) + geom_histogram()+ ggtitle("(a) 일반 히스토그램")
h1

h2 <- ggplot(diamonds, aes(price, fill=cut)) + geom_histogram(binwidth = 500)+ ggtitle("(b) 컷팅기술포함 히스토그램")
h2

h3 <- ggplot(diamonds, aes(price, colour = cut)) +
  geom_density()+ ggtitle("(c) 집단별 밀도사용 히스토그램")
h3

h4 <- ggplot(diamonds, aes(price, fill= cut)) + geom_freqpoly()+ ggtitle("(d) 집단별 직선사용 히스토그램")
h4 

ggarrange(h1, h2, h3, h4, nrow = 2, ncol = 2)


ggplot(data = diamonds, mapping = aes(x = cut, y= price))  +  geom_boxplot(width=0.8, outlier.size = 0.1, outlier.shape = 16, outlier.colour = "red") + stat_summary(fun.y = "mean", geom = "point", shape=21, size=0.1, fill="blue") + ggtitle("박스플롯")

# 기타

library(ggpubr)
library(ggplot2)
library(grid)

data("iris")
attach(iris)

sc1 <- ggplot(iris, aes(x=Petal.Width, y=Petal.Length, fill=Species)) +
  geom_point(colour="blue", shape=21, size=3) +
  scale_fill_brewer(palette = "Reds")+
  geom_hline(yintercept = 2.6, colour="grey", lty="dashed", size=1)+geom_vline(xintercept = 0.8, colour="grey", lty="dashed", size=1) +
  geom_hline(yintercept = 4.9, colour="grey", lty="dashed", size=1)+geom_vline(xintercept = 1.75, colour="grey", lty="dashed", size=1) +
  ggtitle("(a) 수평선과 수직선")

sc1

sc2 <- sc1 +
  annotate("text", x=0.3, y=2.5, label="Setosa", size=5) +
  annotate("text", x=1.25, y=3, label="Versicolor", size=5) +
  annotate("text", x=1.5, y=6.5, label="Virginica", size=5) +
  annotate("rect", xmin=0, xmax=0.8, ymin=0, ymax=2.6, alpha=0.1, fill="purple") +
  annotate("rect", xmin=0.8, xmax=1.75, ymin=2.6, ymax=4.9, alpha=0.2, fill="purple") +
  annotate("rect", xmin=1.3, xmax=2.7, ymin=4.3, ymax=7.2, alpha=0.3, fill="purple") +  
  ggtitle("(b) 주석달기와 음영")

sc2

sc3 <- sc2 +   geom_smooth(method = "lm", se=F) +ggtitle("(c) 개별 회귀선")

sc3

# 회귀식 구하기
coef(lm(Petal.Length~Petal.Width, data = iris))
# y=1.08+2.23x

sc4 <- ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) + geom_point(colour="red", shape=19, size=3) +  scale_fill_brewer(palette = "Reds") + geom_smooth(method = "lm", level=0.95)+
  annotate("segment", x=1.5, xend = 1.25, y=2, yend = 3.8, size=1.2, colour="black", arrow=arrow())+
  annotate("text", x=1.5, y=1.8, size=4, colour="black",label="y=1.08+2.23x") +  
  ggtitle("(d) 전체 회귀선")

sc4

ggarrange(sc1, sc2, sc3, sc4, nrow = 2, ncol = 2)



#3절: 모자이크 함수

install.packages("vcd")
library(vcd)
library(MASS)

data("Titanic")
str(Titanic)
attach(Titanic)

mosaic(~ Class + Survived, data = Titanic,
       main = "(a) 객실등급과 생존 간 관계", shade = TRUE, legend = TRUE)

mosaic(~ Sex + Survived, data = Titanic,
       main = "(b) 성별과 생존 간 관계", shade = TRUE, legend = TRUE)

mosaic(~ Sex + Class + Age + Survived, data = Titanic, main = "(c) 생존과 기타 변수들 간 관계", shade = TRUE, legend = TRUE)

mosaic(Survived ~ ., data = Titanic)


