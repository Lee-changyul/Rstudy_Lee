## 09.Tidy text(한글)

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

## 1.1.3 KoNOP 로드
library(KoNLP)

useNIADic() # 단어사전 사용 - useSejongDic()


## 1.2 텍스트 마이닝 패키지 설치
# tidytext: tidy문서로 만드는 변환
# tidyverse: tibble, tidyr, dplyr, stringr, ggplot2, forcats,  purrr, readr   
# 등 8개 패키지를 한 곳에 묶은 형태

# install.packages("tidytext") # tidy문서로 만드는 
# install.packages("tidyverse")

## 1.2 library 로드
library(tidytext) 
library(tidyverse)



# 2.데이터가져오기
text_df <- read.csv("06.개선사항.csv", 
                    stringsAsFactors=F, 
                    header=TRUE,
                    sep=",")
str(text_df)



# 3.한글 형태소로 분리
# 참고사이트: https://mrchypark.github.io/RKoText101/#58
text_tb <- text_df %>%
  as_tibble() %>%
  unnest_tokens(morp, text, token=SimplePos09) %>% # 형태소로 분리
  filter(str_detect(morp, "/n")) %>%               # 명사만 추출
  mutate(word = str_remove(morp, "/.*$")) %>%      # 형태소 정보 제거
  filter(str_length(word)>=2)

print(text_tb, n=30)  




# 4.불용어 삭제
st_word <- tibble(word=c("저조하다", "학교에대한",
                         "개설해서")) # 불용어 추가

text_tb <- text_tb %>%
  anti_join(st_word, by=c("word"="word")) %>%   # 불용어 추가 삭제
  filter(!grepl(pattern="\\d+", word))          # //d+ = 숫자의 정규표현식

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






