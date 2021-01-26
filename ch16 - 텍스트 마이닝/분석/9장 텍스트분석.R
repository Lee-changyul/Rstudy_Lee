# 9장 텍스트 분석

#1. 리스트의 구조

#1) 리스트 

myvector <- c(1, 2, 3, 4)
myvector


mylist <- list(1, 2, 3, 4)
mylist
myvector[2]
mylist[[2]]
mylist[[2]][1]
mylist[2]


yourvector <- c(1, 2, 3, "a")
yourvector
yourlist <- list(1, 2, 3, "a")
yourlist

mean(yourlist[1:3])
mean(unlist(yourlist[1:3]))

#2) 조건문과 반박문

if (TRUE) {
  print("솔직하게")
  print("어제 술 마셨다")
} else {
  print("아냐")
  print("어제 공부했어")
}


for (i in 1:5) {
  print(i)
}


f <- function(x, y) {
  +   print(x+100)
  +   print(y-100)
}

f(100,200)


#2. 텍스트분석과 자연어처리


# 여러분에 작업폴더에 dictionary라는 하위폴더를 생성하시고 그 안에 stopword.csv, synoym.csv, user_dictionary 파일을 복사해 놓으세요. 마찬가지로 raw_data라는 하위폴더를 만드시고 그 안에 데이터 파일을 복사해 놓으세요. 

setwd("C:/R")

#1. java 설치확인
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_191")  # 해당 디렉토리(C:/Program Files/Java/)에 들어가서 jre 버전이 맞는지 확인함. 여러분 버전은 181? 191? 확인하세요. 
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

# rjava 설치
install.packages("rJava") 
library(rJava)

#2. KNLP4kec 설치 및 필요 패키지 불러오기
# https://github.com/NamyounKim/NLP4kec 에서 여러가지 경고 메시지를 무시하고 windows 용 파일을 다운받아 여러분 작업디렉토리에 옮겨놓으시기 바람
install.packages("NLP4kec_1.2.0.zip" , repos=NULL, type="win.binary")  # rstudio에서도 설치됨
library(NLP4kec)
library(tm)

# 1. 동의어 / 불용어 사전 불러오기
stopWordDic <- read.csv("./dictionary/stopword_ko.csv") #.는 여러분의 작업디렉토리를 의미함
synonymDic <- read.csv("./dictionary/synonym.csv")

# 2. 형태소 분석 및 전처리
p_data = file_parser_r(path = "./raw_data/speech.xlsx", language = "ko", useEn = F, korDicPath = "./dictionary/user_dictionary.txt")

p_data

# 데이터 전처리
install.packages("tm") #텍스트 마이닝을 위한 패키지
library(tm)

# 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], p_data))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    p_data[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], p_data[docNum])
  }
}

## 단어간 스페이스를 하나 더 추가하는 것으로 NLP4kec 패키지의 알고리즘상 필요한 과정으로 특별한 의미는 없다. gsub()함수의 사용 예를 보면 gsub("한국어", "한글", data)이란 코딩은 data 파일에서 한국어를 한글로 교체하라는 의미이다.
p_data <- gsub(" ","  ",p_data)

#Corpus 생성
corp <- VCorpus(VectorSource(p_data)) # 아래 참고) 참조
corp

#특수문자 제거: 예를 들어 @, #, $ 등의 문자를 제거함
corp <- tm_map(corp, removePunctuation)

#숫자 삭제
corp <- tm_map(corp, removeNumbers)

#소문자로 변경: 영문의 경우 대문자를 소문자로 변경함: 영문을 사용하지 않기 때문에 해당사항 없음
corp <- tm_map(corp, tolower)

#특정 단어 삭제: 불용어 사전을 활용하여 해당 문자를 데이터에서 제거함
corp <- tm_map(corp, removeWords, stopWordDic$stopword)

#텍스트문서 형식으로 변환
corp <- tm_map(corp, PlainTextDocument)


# 3. DTM 생성 및 Sparse Term 삭제 --
#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[,nchar(colnames(dtm)) > 1]
dtm

#참고) Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))

#Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어든다.)
dtm <- removeSparseTerms(dtm, as.numeric(0.70))
dtm

#DTM, TDM을 데이터 프레임 형식으로 저장하기
dtm_df <- as.data.frame(as.matrix(dtm))
tdm_df <- as.data.frame(as.matrix(tdm))

# 분석 

#4. 단어 발생 빈도 구하기
freq <- colSums(as.matrix(dtm[4,]))  # 문재인대통령 연설문
length(freq)

freq[head(order(-freq), 20)]
#freq[head(order(freq), 10)]


# 5. 단어 빈도 정보로 시각화 하기 --------------

#단어 빈도 정보 Data Set 만들기
wordDf <- data.frame(word=names(freq), freq=freq)
View(wordDf)

#상위 20개 단어만 바차트로 보여주기
library(ggplot2)
library(dplyr)
ggplot(head(arrange(wordDf,-freq),20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity")

# Word Cloud 그리기
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = wordDf
           , color = "random-dark" #random-light
           , shape = "star"  # circle, cardioid, diamond, triangle-forward
           , size = 0.5
           , fontFamily = "나눔고딕")

#treeMap 그리기
install.packages("treemap")
library(treemap)
treemap(wordDf # 대상 데이터 설정
        ,title = "Word Tree Map"
        ,index = c("word") # 박스 안에 들어갈 변수 설정
        ,vSize = "freq"  # 박스 크기 기준
        ,fontsize.labels = 12 # 폰트 크기 설정
        #,palette = pal # 위에서 만든 팔레트 정보 입력
        ,border.col = "white") # 경계선 색깔 설정

#2. 문재인, 박근혜 비교하기
install.packages("wordcloud")
library(wordcloud)

colnames(tdm_df) <- c("노무현", "이명박", "박근혜","문재인")
tdm_PM <- tdm_df[,3:4]
View(tdm_PM)

comparison.cloud(tdm_PM, colors = c("blue", "red"),scale=c(3,1),rot.per=.02, max.words = 60, random.order=FALSE)

commonality.cloud(tdm_PM,colors = c("purple", "red"), scale=c(2,1),rot.per=.02, max.words = 50, random.order=FALSE)

## 4.4. 피라미드 그래프
install.packages("plotrix")

library(dplyr)
library(plotrix)

par(mfrow=c(1,1))

common_words_25 <- tdm_PM %>% 
  mutate(label = rownames(tdm_PM)) %>% 
  filter(문재인 > 0 & 박근혜 >0) %>% 
  mutate(diff = abs(문재인 - 박근혜)) %>% 
  arrange(desc(diff)) %>% slice(1:25)

pyramid.plot(common_words_25$문재인, common_words_25$박근혜,
             labels = common_words_25$label, gap = 8,
             top.labels = c("문재인", "Words", "박근혜"),
             main = "공통단어", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

# 대통령들간 상관관계 구하기: 가중치 없음

my.assoc.func <- function(tdm, term1, term2) {
  var1 <- as.numeric(tdm[, term1])
  var2 <- as.numeric(tdm[, term2])
  cor.test(var1, var2)
}

my.assoc.func(tdm_df, "노무현", "이명박")
my.assoc.func(tdm_df, "이명박", "박근혜")
my.assoc.func(tdm_df, "박근혜", "문재인")

# 대통령들간 상관관계 구하기: 가중치 주기
tdmw = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf),
                                             weighting = function(x) weightTfIdf(x, normalize = TRUE)))  #Tf-Idf 가중치 주기
tdmw_df = as.data.frame(as.matrix(tdmw))
colnames(tdmw_df) <- c("노무현", "이명박", "박근혜","문재인")

my.assoc.func(tdmw_df, "노무현", "이명박")
my.assoc.func(tdmw_df, "이명박", "박근혜")
my.assoc.func(tdmw_df, "박근혜", "문재인")
