## 04.Corpus(영어)_실제

## Student Guerantee 데이터 실습



# 1.기본 package 설정

## 1.1 package 설치

# install.packages("tm")        # 텍스트마이닝 도구
# install.packages("SnowballC") # 어간추출 - 'S', 'C' 대문자
# install.packages("dplyr")     # 데이터 처리에 특화된 R 패키지

## 1.2 library 로드
library(tm)        
library(SnowballC)  
library(dplyr)      



# 2.텍스트 가져오기(Corpus)

## 2.1 데이터가져오기
text_df <- read.csv("04.youth_guarantee.csv", 
                    stringsAsFactors=F, 
                    header=TRUE,
                    sep=",")
str(text_df)

## 2.2 Corpus(말뭉치)로 저장: 코퍼스는 문서가 저장되어 있는 공간
corp <- text_df$text  %>%
  VectorSource() %>%
  VCorpus()

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인



# 3 용어(tocken) 만들기 (tokenization)

# 불용어 및 수동으로 처리
st_word1 <- c(stopwords('english'))        # 불용어 사전
st_word2 <- c("youth", "guarantee")        # 불용어 추가

corp <- corp %>%
  tm_map(content_transformer(tolower)) %>% # 소문자로 변경
  tm_map(removeNumbers) %>%                # 숫자삭제
  tm_map(stripWhitespace) %>%              # whitespace(공백)제거
  tm_map(removePunctuation) %>%            # 구두점 제거
  tm_map(removeWords, 
         words=c(st_word1, st_word2)) %>%  # 불용어 제거
  tm_map(stemDocument)                     # 어근 동일화(stemming)

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인



# 4.단어-문서 행렬 만들기

## 4.1 tdm(Term-Document-Matrix 만들기): 단어중심일때
## ?TermDocumentMatrix
tdm <- TermDocumentMatrix(corp, 
                          control = list(wordLengths=c(2,Inf)))

inspect(tdm)

tfidf_tdm <- weightTfIdf(tdm)
inspect(tfidf_tdm)

## 4.2 dtm(Document-Term-Matrix 만들 기): 문서 중심일때 
dtm <- DocumentTermMatrix(corp)
inspect(dtm)


tfidf_dtm <- DocumentTermMatrix(corp, 
                                control = list(weighting=weightTfIdf))
inspect(tfidf_dtm)



# 5.Word Cloud(단어 빈도수 계산)





# 참고.dtm을 tidytext로 변환
## tidytext 이용방법은 tidytext 사용법 참조
library(tidytext)
dtm <- as.data.frame(tidy(dtm))
dtm <- tibble(dtm)

