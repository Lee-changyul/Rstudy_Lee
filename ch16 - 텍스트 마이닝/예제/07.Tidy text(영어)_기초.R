## 05.타이디 텍스트(영어)
## 참고문헌: https://www.tidytextmining.com/tidytext.html



# 1.기본 package 설정

## 1.1 package 설치
# tidytext: tidy문서로 만드는 변환
# tidyverse: tibble, tidyr, dplyr, stringr, ggplot2, forcats,  purrr, readr   
# 등 8개 패키지를 한 곳에 묶은 형태

# install.packages("tidytext") # tidy문서로 만드는 
# install.packages("tidyverse")
# install.packages("SnowballC") # 어간추출 - 'S', 'C' 대문자

## 1.2 library 로드
library(tidytext) 
library(tidyverse)
library(SnowballC)  



# 2.텍스트 가져오기(Corpus)

## 2.1 데이터가져오기
text = c("This is the first first first            sentence!!",
         "                                      This is a second Sentence",
         "The third sentence is here",
         "Forth of all sentences. Forth of all sentences. Forth of all sentences.")

no = c(1,2,3,4)

text_tb <- tibble(no=no, text=text) %>%
  unnest_tokens(output=word, input=text)
  
print(text_tb, n=30)


# 4.용어(tocken) 만들기 (tokenization)
text_tb <- text_tb %>%
  filter(!grepl(pattern="\\d+", word)) %>% # //d+ = 숫자의 정규표현식
  mutate(word=wordStem(word))              # mutate: dplyr함수로 열추가

print(text_tb, n=30)




# 5.단어-문서 행렬 만들기

## 5.1. frequency, tf_dif 값 도출
text_tb <- text_tb %>%
  count(no, word, sort = TRUE) %>%
  bind_tf_idf(no, word, n) %>%
  print(text_tb)

## 5.2 tdm 만들기
tdm <- text_tb %>%
  cast_tdm(term=word, document=no, value=n)

inspect(tdm)

tfidf_tdm <- text_tb %>%
  cast_tdm(document=no, term=word, value=tf_idf)

inspect(tfidf_tdm)

## 5.3 dtm 만들기 
dtm <- text_tb %>%
  cast_dtm(document=no, term=word, value=n)

inspect(dtm)

tfidf_dtm <- text_tb %>%
  cast_dtm(document=no, term=word, value=tf_idf)

inspect(tfidf_dtm)
