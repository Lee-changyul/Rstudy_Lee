# Tidy-text를 이용한 데이터 분석

library(tidyverse)
library(tidytext)
library(tidymodels)
library(writexl)
library(tm)

# install.packages("readxl")
# install.packages("writexl")

library(readxl)

# 1. 데이터 읽기


texts <- read_excel(".//raw_data//final.xlsx")

head(texts)

str(texts)

print(texts, n=30)


# 2. 토근단위로 분해하고 엑셀에 저장하여 단어 전처리하기  

unnest_tokens(tbl=, output=, input=)
# tbl=데이터프레임 (토큰단위로 분해해서 한행에 한개씩 저장)
# output= 분해한 토큰을 저장할 새로운 열이름
# input= 분해할 텍스트가 저장된 열이름
# text.df

# text.df <- unnest_tokens(text.df, word, 키워드,drop = F)
# write_xlsx(text.df, path=".//raw_data//final.xlsx", col_names = T)


# text_df로 읽은 다음 text_tb 토큰화까지 한번에 처리하기 
#texts <- table %>% as_tibble() %>% print() %>% unnest_tokens(output=word, input=content, drop=F)
#write_xlsx(texts, path=".//raw_data//final.xlsx", col_names = T)

# 3. 단어 빈도 계산하하고 스탑워드 작성 

preq <- texts %>% 
  count(word, sort = TRUE) 

st_word <- filter(preq, nchar(word) <= 1) 

str(st_word)

# 4. 불용어 처리하기 (숫자 등)

texts <- texts %>%
  anti_join(st_word, by="word") %>%        # 불용어 추가 삭제
  filter(!grepl(pattern="\\d+", word)) # //d+ = 숫자의 정규표현식
   

print(texts, n=30)

preq <- texts %>% 
  count(word, sort = TRUE) 


# 5.단어-문서 행렬 만들기

## 5.1. frequency, tf_dif 값 도출
texts <- texts %>%
  count(no, word, sort = TRUE) %>%
  bind_tf_idf(no, word, n) %>%
  print(texts)

## 5.2 tdm 만들기
tdm <- texts %>%
  cast_tdm(term=word, document=no, value=n)
inspect(tdm)

tfidf_tdm <- texts %>%
  cast_tdm(document=no, term=word, value=tf_idf)
inspect(tfidf_tdm)

## 5.3 dtm 만들기 
dtm <- texts %>%
  cast_dtm(document=no, term=word, value=n)
inspect(dtm)

tfidf_dtm <- texts %>%
  cast_dtm(document=no, term=word, value=tf_idf)
inspect(tfidf_dtm)

?inspect

# 6.워드 클라우드와 단어 빈도분석

## 6.1 단어빈도
texts %>%
  count(word, sort = TRUE) %>%
  filter(n >150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## 6.2 tf_idf 중요도
texts %>%
  arrange(desc(tf_idf))%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(40) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


# 6.3 단순 워드 클라우드
texts %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words=word, 
                 freq=n, 
                 max.words=100))


# 6.4 색깔 워드 클라우드

library(wordcloud)
palete<-brewer.pal(6,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))

texts %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words=word,
                 freq=n,
                 scale=c(4,0.5), 
                 min.freq=10, 
                 max.words=100,
                 random.order=FALSE, 
                 colors=palete, 
                 family="malgun"))


=============
  
# 아래 내용은 검증을 해봐야 함
  
corTerms <- tfidf_dtm %>% as.matrix() %>% cor()

# 차원을 확인합니다. 
dim(x = corTerms)
## [1] 398 398 상관계수 행렬로 연관성 높은 단어를 확인합니다.

checkCorTerms <- function(n = 10, keyword) {
  
  # 키워드 유무를 확인합니다.
  corTerms %>% 
    colnames() %>% 
    str_subset(pattern = keyword) %>% 
    print()
  
  # 연관 키워드가 있는 컬럼의 전체 단어를 한꺼번에 출력합니다.
  corRef <- data.frame()
  
  # 상관계수 높은 순서로 정렬합니다.
  corRef <- corTerms[ , keyword] %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    magrittr::set_colnames(c('corr'))
  
  # 미리보기 합니다. 
  head(x = corRef, n = n + 1)
}

# 빈도수 높은 단어 중 관심 있는 명사 위주로 확인합니다. 
checkCorTerms(n = 20, keyword = '재난')  


