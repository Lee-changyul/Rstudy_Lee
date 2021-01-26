## 03.Corpus(영어)

# 1.기본 package 설정
## 1.1 package 설치
## 대소문자 구분해야 함

# install.packages("tm")        # 텍스트마이닝 도구
# install.packages("SnowballC") # 어간추출 - 'S', 'C' 대문자

## 1.2 library 로드
library(tm)
library(SnowballC) 



# 2.텍스트 가져오기(Corpus)

## 2.1 데이터가져오기
text_df <- read.csv("03.text.csv", 
                    stringsAsFactors=F, 
                    header=F)
str(text_df)
text_df

## 2.2 Corpus(말뭉치)로 저장: 코퍼스는 문서가 저장되어 있는 공간
## VCorpus(volatile)와 PCorpus(Permanent)
## ?getSources() : 가지고 올 수 있는 데이터 형태를 체크
## tm:: 은 여러 패키지가 혼합되어 있을 경우에 구분을 위해 사용하는 것
## tm이라는 패키지를 사용한다는 것을 의미하 -> 일반적으로 사용안함

corp <- tm::Corpus(VectorSource(text_df$V1))
inspect(corp) # 코퍼스의 내용을 보는 함수
corp[[1]]$meta
corp[[1]]$content



# 3 용어(tocken) 만들기 (tokenization)
## ?getTransformations()

## 3.1 대문자를 소문자로 변경
## tolower은 tm 객체가 아니기 때문에 Corpus에 사용할 수 없음
## tm 함수의 content_transformer()를 이용해서 처리
corp <- tm_map(corp, content_transformer(tolower))
inspect(corp)

## 3.2 숫자삭제
corp <- tm_map(corp, removeNumbers)
inspect(corp)

## 3.3 whitespace(공백)제거: 2개 이상 공란을 1개로 조정
corp  <- tm_map(corp, stripWhitespace) 
inspect(corp)

## 3.4 구두점 제거
corp <- tm_map(corp, removePunctuation) 
inspect(corp)



# 4. 불용어(stopwords) 처리
## 4.1 기본 불용어 사전으로 처리
stopwords("english")
corp <- tm_map(corp, removeWords, stopwords("english"))
inspect(corp)

## 4.2 기본 불용어 외 불용어로 쓸 단어 추가하기
st_word <- c(stopwords('english'),c("aaa","bbb") ) 
corp <- tm_map(corp,removeWords,st_word) 



# 5. 어근 동일화(stemming)
corp <- tm_map(corp, stemDocument) 
inspect(corp)



# 6.단어-문서 행렬 만들기

## 6.1 tdm(Term-Document-Matrix 만들기): 단어중심일때
## ?TermDocumentMatrix
tdm <- TermDocumentMatrix(corp, 
                          control = list(wordLengths=c(2,Inf)))

tfidf_tdm <- weightTfIdf(tdm)
inspect(tfidf_tdm)

## 6.2 dtm(Document-Term-Matrix 만들 기): 문서 중심일때 
dtm <- DocumentTermMatrix(corp)
inspect(dtm)

tfidf_dtm <- DocumentTermMatrix(corp, 
                          control = list(weighting=weightTfIdf))
inspect(tfidf_dtm)

