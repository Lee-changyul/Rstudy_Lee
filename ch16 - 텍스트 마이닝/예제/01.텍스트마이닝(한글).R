## 텍스트마이닝 한글

## 한글 텍스트마이닝 package 설치: KoNLP
# install.packages("rJava")
# install.packages("KoNLP")
# install.packages("stringr")
# install.packages("wordcloud")
# install.packages("RColorBrewer")

library(rJava)
library(KoNLP)
library(stringr)
library(wordcloud)
library(RColorBrewer)

# 02.데이터 불러오기
kclibrary.df <- read.csv("data.csv", stringsAsFactors=F)
str(kclibrary.df)

#03. 한글을 사용하기 위해 세종사전을 이용
#세종사전이 세팅되지 않을 때는 JAVA jdk를 설치해야 함
#NIA에서 개발한 사전: useNIADic()
useSejongDic()

#04.텍스트에서 명사단어를 추출함
word <- sapply(kclibrary.df$text, extractNoun, USE.NAMES = F)
head(word)
# 리스트를 단어로 풀어서 저장
word <-unlist(word)
word


#05.2~5글자 사이 단어만 추출
word1 <- Filter(function(x){nchar(x)>=2&nchar(x)<=5},word)
head(word1,20)


#06.단어 카운트
wordcount <-table(unlist(word1))
head(sort(wordcount, decreasing = T),50)

#07.필요 없는 단어 제거
word1 <- gsub("\\n","",word1)
word1 <- gsub("\\d+","",word1)
word1 <- gsub("\\.","",word1)
word1 <- gsub("도서관","",word1)
word1 <- gsub("학교","",word1)
word1 <- gsub("학생","",word1)
word1 <- gsub("들이","",word1)


#08.정제된 내용을 txt에 저장
write(unlist(word1),"word.txt")
wordlist <-read.table("word.txt")
nrow(wordlist)

#09.데이터 카운트 및 csv에 저장
wordcount2 <- table(wordlist)
write.csv(wordcount2, "frequency.csv")
head(sort(wordcount2, decreasing = T),50)


#10.wordcloud 만들기
palete<-brewer.pal(6,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))

wordcloud(names(wordcount2), 
          freq=wordcount2, 
          scale=c(7,1), 
          min.freq = 3, 
          random.order = F, 
          random.color = T, 
          colors=palete, 
          family="malgun")
