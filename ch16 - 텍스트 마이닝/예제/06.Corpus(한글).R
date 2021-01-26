## 06.Corpus(한글)

### 한글 텍스트마이닝 KoNLP 패키지 설치 방법 ################################
## 현재 KoNLP 패키지가 cran(R패키지 통합 설치 사이트)에서 제외             ##
## 따라서 기존에 설치방법(install.packages("KoNLP") 방법을 사용할 수 없고  ##
## github 설치 방법을 이용해야 됩니다.                                     ##
## 현재 인터넷에 설치방법이 잘 나와 있지 않으니                            ##
## 아래 설치 방법을 잘 따라해서 설치하세요.                                ##
#############################################################################

# 1.기본 package 설정

## 1.1 package 설치

## 1.1.1 jdk 설치
# install.packages("rJava")
# install.packages("multilinguer")

library(rJava)
library(multilinguer)
install_jdk()

## 1.1.2 KoNLP 설치(github이용)
# install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', 
                        upgrade = "never", 
                        INSTALL_opts=c("--no-multiarch"))

## 1.1.3 텍스트 마이닝 패키지 설치
# install.packages("tm") # 텍스트마이닝 도구
# install.packages("dplyr") 
# install.packages("stringr") 

## 1.2 library 로드
library(KoNLP)
library(tm)
library(dplyr)
library(stringr)

useNIADic() # 단어사전 사용 - useSejongDic()



# 2.텍스트 가져오기(Corpus)

## 2.1 데이터가져오기
text_df <- read.csv("06.개선사항.csv", 
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

corp <- corp %>%
  tm_map(removeNumbers) %>%                # 숫자삭제
  tm_map(stripWhitespace) %>%              # whitespace(공백)제거
  tm_map(removePunctuation)                # 구두점 제거


writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인




# 4. 명사 추출: "R을 이용한 텍스트 마이닝,백영민,한울" p.147 사용
myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext),collapse='  ')
  myNounList
}

for (i in 1:length(corp)) {
  corp[[i]]$content <- myNounFun(corp[[i]]$content)
}
writeLines(as.character(corp[[1]])) 




# 5. 불용어(stopwords) 처리
#지정된 표현들 교체

st_word <- c("것","들", "수", "좋겠습니", 
             "해주", "되", "화", "한", "들이", "좀") 
corp <- tm_map(corp, removeWords, st_word) 

writeLines(as.character(corp[[1]])) 



# 6.단어-문서 행렬 만들기

## 6.1 tdm(Term-Document-Matrix 만들기): 단어중심일때
## ?TermDocumentMatrix
tdm <- TermDocumentMatrix(corp, 
                          control = list(wordLengths=c(2,Inf)))
inspect(tdm)

tfidf_tdm <- weightTfIdf(tdm)
inspect(tfidf_tdm)

## 6.2 dtm(Document-Term-Matrix 만들 기): 문서 중심일때 
dtm <- DocumentTermMatrix(corp)
inspect(dtm)


tfidf_dtm <- DocumentTermMatrix(corp, 
                                control = list(weighting=weightTfIdf))
inspect(tfidf_dtm)



