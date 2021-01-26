## 텍스트마이닝 영어

## Student Guerantee

# install.packages("tm") # 텍스트마이닝 도구
# install.packages("SnowballC") # 불용어
# install.packages("wordcloud")

library(tm)
library(SnowballC) 
library(wordcloud)

# 작업디렉트토리 찾기 및 설정
sg.df<-read.csv("Youth Guarantee.csv",header=TRUE,sep=",")

sg.content<-sg.df$text #변수만들어서 설정

# 코퍼스만들기: 코퍼스는 문서가 저장되어 있는 공간
sg.corpus<-VCorpus(VectorSource(sg.content)) 

# 코퍼스의 내용을 보는 함수
inspect(sg.corpus) 

# 구체적인 내용을 보는 함수: 2번째 보기(as.character)
writeLines(as.character(sg.corpus[[2]])) 


# 전처리: 불필요한 단어 제거
# whitespace(공백)제거
sg.corpus <- tm_map(sg.corpus, stripWhitespace)
# 대문자를 소문자로 변경
sg.corpus <- tm_map(sg.corpus, content_transformer(tolower))
# 숫자삭제
sg.corpus <- tm_map(sg.corpus, removeNumbers)
# 구두점 제거
sg.corpus <- tm_map(sg.corpus, removePunctuation)
# stopword(불용어) 제거
sg.corpus <- tm_map(sg.corpus, removeWords, stopwords("english"))

# 기본 불용어 외 불용어로 쓸 단어 추가하기
sword2 <- c(stopwords('en'),"will","students","student","guarantee" ) 
# 불용어 제거하기 (전치사 , 관사 등)
sg.corpus <- tm_map(sg.corpus,removeWords,sword2) 

tm_map(sg.corpus,stemDocument)
writeLines(as.character(sg.corpus[[2]]))

# 도큐먼트 매트릭스 만들기
dtm<-DocumentTermMatrix(sg.corpus)
dtm # 10개 문서, 1150개의 단어
inspect(dtm[1:5,1:5]) # 처음 다섯개의 컬럼만 보기
findFreqTerms(dtm,5) # 5회 이상 단어만 출력
findAssocs(dtm,"student",0.9) # sg이란 단어와 90%이상 연관성있는 단어

# 반대로 텀도큐먼트 매트릭스 만들기
tdm <- TermDocumentMatrix(sg.corpus)
tdm
class(tdm)
findFreqTerms(tdm,10) # 10회 이상 나온 단어 찾기
m<-as.matrix(tdm) # 매트릭스로 변환
m

# sorting 하기
v<-sort(rowSums(m),decreasing = TRUE) # 단어가 나타난 숫자별로 정렬
d<-data.frame(word=names(v),freq=v) # data.frame으로 변환
head(d,10)

#word cloud
set.seed(1234) # 난수가 필요해서 임의로 세팅함
wordcloud(words=d$word, 
          freq=d$freq,
          min.freq=25, 
          max.words=200,
          random.order=FALSE, 
          colors=brewer.pal(8,"Dark2"))

