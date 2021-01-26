#8장 연관분석

install.packages("arules")
install.packages("arulesViz") 

setwd("C:/R")
library(arules)
library(arulesViz)

#사례분석 1: 마트 물건 판매

# 원자료 탐색
mart <- read.csv("groceries.csv", header = F)
head(mart, 5)
rm(list=ls())  # 모든 객체 삭제

# 데이터프레임 자료를 transaction 자료로 변환하기
mart <- read.transactions("groceries.csv", sep=",")
summary(mart)

# 자료 사전탐색
itemFrequency(mart[ , 1:10])
itemFrequencyPlot(mart, topN=10,  type='relative') # type='absolute'는 절대 빈도

exp <- eclat(mart, parameter=list(support=2/9827,minlen=2)) # maxlen은 최대 거래 물건 수
exp

inspect(sort(exp)[1:10])
summary(exp)

# 연관분석
rules<-apriori(mart, parameter=list(support=0.001, confidence=0.01, minlen=2))
rules
#summary(rules)

rules <- sort(rules, by='lift')
inspect(rules[1:10])

## 특정 품목 제외 
rules_out<-apriori(mart, parameter=list(support=0.001, confidence=0.01, minlen=2),appearance=list(none=c("beer","liquor")))
rules_out <- sort(rules_out, by='lift')
inspect(rules_out[1:10])

##특정 품목 포함
rules_Lsoju<-apriori(mart, parameter=list(support=0.001, confidence=0.01, minlen=2),appearance=list(lhs='soju',default='rhs'))
rules_Lsoju <- sort(rules_Lsoju, by='lift')
inspect(rules_Lsoju[1:10])

rules_Rsoju<-apriori(mart, parameter=list(support=0.001, confidence=0.01, minlen=2),appearance=list(rhs='soju',default='lhs'))
rules_Rsoju <- sort(rules_Rsoju, by='lift')
inspect(rules_Rsoju[1:10])

plot(rules_Rsoju[1:10], method="grouped")
plot(rules_Rsoju[1:10], method="graph")


# 사례분석 2; 도서관 대출

rm(list=ls())  # 앞에서 연습했던 모든 객체 삭제
gc()   # 메모리 청소
setwd("C:/R")

library(arules)
library(arulesViz)

booksub<-read.csv("booksubtitle.csv")
str(booksub)
head(booksub, 7)
table(booksub$id) # 몇명의 대출자가 몇권씩 대출해갔을까?

# 위와 같은 데이터프레임은 아래의 함수로 transaction data로 변환해준다. 
booksub.list<-split(booksub$booksubtitle, booksub$id)
booksub.transaction<-as(booksub.list, "transactions")
summary(booksub.transaction)

# 자료 사전탐색
exp <- eclat(booksub.transaction, parameter=list(support=2/20,minlen=2)) 
exp

inspect(sort(exp)[1:10])
summary(exp)

#연관분석
rules<-apriori(booksub.transaction, parameter=list(support=0.1, confidence=0.08))
summary(rules)

rules <- sort(rules, by='lift')
inspect(rules[1:10])

# 시각화
plot(rules[1:15], method = "grouped")
plot(rules[1:10], method = "graph")

# 특정도서만 추천
rules_int <- subset(rules, lhs %in% "오베라는 남자") #%in%는 매칭을 의미함
rules_int <- sort(rules_int, by='lift')
inspect(rules_int[1:5])

plot(rules_int[1:5], method = "graph")