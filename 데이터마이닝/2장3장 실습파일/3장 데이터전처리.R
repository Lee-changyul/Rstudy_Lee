#3장 데이터 구조와 전처리  

##제 1절 R의 데이터 구조  

nvector <- c(1,2,3,4,5)  
nvector

is(nvector)    # 데이터 형태가 무엇인지 확인하는 함수는 is( )

cvector <- c("국어","영어","수학")
cvector

is(cvector)

cvector <- c("남자","여자","남자","여자")
is(cvector)

cvar <- as.factor(cvector)  # 요인으로 변환시켜주는 함수
is(cvar)

cvar

#as.numeric   # 숫자형 데이터로 변환
#as.character   # 문자형 데이터로 변환
#as.factor       # 요인형 데이터로 변환
#as.data.frame   # 데이터프레임 형태로 변환(뒤에서 살펴봄)

m <- matrix(1:9, ncol = 3)
m

no<-c(1,2,3,4)
name<-c("Lee", "Kim", "Park", "Jung")
gender<-c("M", "F", "M", "F")
dfrm1<-data.frame(no, name, gender)
dfrm1


##제 2절 데이터프레임 편집  

###1) cbind( )와 rbind( )

height<-c(180, 170, 185, 168)
weight<-c(90, 54, 100, 52)
age<-c(20, 18, 23, 19)


dfrm2<-data.frame(age, height, weight)
dfrm2
cbind(dfrm1, dfrm2)

no<-c(5,6,7,8)
name<-c("Cheon", "Rhoh", "Moon", "Choi")
gender<-c("F", "F", "F", "M")
dfrm3<-data.frame(no, name, gender)
rbind(dfrm1, dfrm3)

###2) merge( )  
dfrm4<-data.frame(name=c("Lee", "Kim", "Park"), age=c(20,18,23))
dfrm4
dfrm5<-data.frame(name=c("Lee", "Jung", "Park"), height=c(180, 168, 185))
dfrm5
merge(dfrm4, dfrm5, by="name")

dfrm6<-merge(dfrm4, dfrm5, all=T)
dfrm6

###3) 결측값(NA) 
Na_cleaning<-na.omit(dfrm6)
Na_cleaning

###4) subset( ) 

subset(dfrm1, gender == "F")
subset(dfrm1, name == "Lee")
subset(dfrm1, no == 2)
dfrm7<-subset(dfrm1, select="name")
dfrm7

###5) colnames( )  
dfrm8<-data.frame(name=c("Lee", "Kim", "Park"), age=c(20,18,23))
dfrm8
colnames(dfrm8)

colnames(dfrm8)<- c("이름", "나이")
dfrm8
colnames(dfrm8)[2]<- "나이"
dfrm8

###6) 인텍싱(indexing)

dfrm1
dfrm1[1,]
dfrm1[,1]
dfrm1[2,1]
dfrm1[1,c(1,3)]


##제 3절 apply 함수  

###1) apply 함수 : apply(x, d, f)  

head(iris)
apply(iris[,1:4], 2, sum)

###2) lapply 함수 
lapply(iris[,1:4],mean, na.rm=T)

###3) sapply 함수

sapply(iris[1,1:4], mean, na.rm=T)

###4) tapply(출력값,기준컬럼,적용함수): 요인의 수준별로 특정 벡터에 함수 적용

#attach(iris)
#tapply(Sepal.Length, Species, mean)
with(iris, tapply(Sepal.Length, Species, mean))  #with 함수로 attach 함수를 대체함

##제 4절 dplyr 활용 

setwd("C:/R")  # 작업디렉토리 설정
#install.packages("dplyr")  
library(dplyr)
data <- read.csv("./data/raw_data/age.csv")  # . 는 작업디렉토리를 의미한다.
head(data,5)

###1) filter 함수 
data1 <- filter(data, 남자 > 42 & 여자 > 43)
head(data1)
str(data1)

data2 <- select(data, 자치구, 동)
head(data2); str(data2)  # head와 str 명령어를 동시에 수행할 경우 “;”를 사용함

###3) arrange 함수 
data4 <- arrange(data, 평균연령)  #오름차순 정렬
head(data4); str(data4)

###4) mutate 함수  
data6<-mutate(data, 남자여자 = 남자 + 여자) 
head(data6); str(data6)

###5) summarise 함수  
data7<- data %>% group_by(자치구) %>% summarise(average = mean(평균연령, na.rm=T)) # nr.rm 은 결측치 제거함수임      
head(data7); str(data7)

