#spss와 같은 상업용 프로그램은 어지간한 컴퓨터에서도 잘 구동되어서 사용하는데 스트레스가 없습니다. 그러나 R은 무료이지만 스트레스가 상당하다는 것을 감내하셔야 합니다. 특히 컴퓨터의 사양과 컴퓨터의 친숙도에 따라 정말 많이 달라집니다. 부탁드리고 싶은 것은 설치가 안되고 자꾸 컴퓨터 오류가 발생하면 컴퓨터를 한번 초기화하실 것을 권합니다. 초기화 방법은 온라인을 통해 얼마든지 알 수 있으나 이것도 부담스럽다면 비용을 들여서라도 컴퓨터 정리를 깨끗이 하시고 시작하실 것을 강력히 권장드립니다. 


# 먼저 기억해주세요. Rstudio는 한글 입력이 불편해서 가끔씩 한영전환을 해도 한글 입력이 안되는 경우가 발생합니다. 그러할 때는 윈도우 검색(화면 좌하단 돋보기 클릭)에서 ctfmon을 입력해서 실행시키고 그 다음에 한영전환 키를 활용해서 한글입력을 하시면 됩니다. 포캣몬이 아닙니다^^


# 2장 설치


a <- 1

a = 2

b <- sum(1,8,4,5,9)
b

num <- c(1,2,3,4)
num

fruit <- c("apple", "Leeato", "banana")
fruit

name <- c("철수", "영희", "민수")
name

melon <- "melon"
melon

total <- c(num, fruit, name, melon)
total

a <- 1
b <- 2
a == 2

a <- 1
b <- 2
a == b

a != b

a < b

a <= b

a > b

a >= b

total

total[7]

getwd()

setwd("C:/R")
setwd("C:\R)    # 잘못된 예


setwd("C:/R")
mydata <- read.csv("./data/raw_data/example.csv", header=T, na.strings=".")  # . 는 작업디렉토리를 의미한다.

View(mydata)  # V는 대문자임
edit(mydata)   # 편집가능

scan1 <-scan(“example.txt”, what=“character”,sep=“\n”,encoding=“UTF-8”)
read <- readLines(“example.txt”,  encoding = “UTF-8”)

write.csv(객체명, “파일이름.csv”, row.names=F)

save.image(“파일이름.RData”)  # 작업공간 저장
load(“파일이름.RData”)  # 작업공간 불러오기

library(패키지명)
search()  # 현재 작업공간에 있는 모든 데이터나 패키지를 알려준다.
detach(“package:패키지명”)  # 나열되어 있는 패키지 중 원하는 패키지 제거


