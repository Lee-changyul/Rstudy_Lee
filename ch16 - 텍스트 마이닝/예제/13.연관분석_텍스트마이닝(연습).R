##13.연관분석_텍스트마이닝



# 1.기본 package 설정

## 1.1 package 설치
# tidytext: tidy문서로 만드는 변환
# tidyverse: tibble, tidyr, dplyr, stringr, ggplot2, forcats,  purrr, readr   
# 등 8개 패키지를 한 곳에 묶은 형태

# install.packages("tidytext") # tidy문서로 만드는 
# install.packages("tidyverse")
# install.packages("SnowballC") # 어간추출 - 'S', 'C' 대문자

## 1.2 library 로드
library(tm)
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

## 2.2 tibble로 만들기
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
dtm <- text_tb %>%
  cast_dtm(term=word, document=no, value=n)

inspect(dtm)



# 6. 연관분석 
# install.packages("arules") 
library(arules) # 연관분석 패키지 설치




# 전리작업: dtm을  matrix 형식으로 변환
dtm_m <- as.matrix(dtm)
dtm_m <- ifelse(dtm_m > 0, 1, 0) # 단어가 1이상인 것을 0,1로 변환함
head(dtm_m)




# 6.1 바이너리 코드를 transactions으로 변환
dtm_trans <- as(dtm_m, 
                "transactions")
arules::inspect(dtm_trans) 
# inspect는 여러 패키지에서 사용하므로 동시에 
# 사용할때는 특정패키지 명을 지정




# 6.2 연관규칙 실행
# 그래프 확인
itemFrequencyPlot(dtm_trans)
# apriori(data, minimum support, minimum confidence, and target)
rules <- apriori(dtm_trans, 
                 parameter = list(supp= 0.5, 
                                  conf = 0.7, 
                                  target = "rules"))



# 6.3 규칙확인: lift가 높은 순서로 sorting
rules.tbl <- arules::inspect(sort(rules, by = "lift"))
rules.tbl[rules.tbl$support >= 0.5 & 
            rules.tbl$confidence >= 0.7 & 
            rules.tbl$lift >= 1,]
