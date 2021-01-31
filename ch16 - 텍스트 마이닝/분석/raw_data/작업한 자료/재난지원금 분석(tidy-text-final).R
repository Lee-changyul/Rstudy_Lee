save(list=ls(), file ="재난지원금1.RData")

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

texts <- subset(texts, select = -word)

texts$date <- as.character(texts$date)

class(texts$date)


texts$date <- as.Date(texts$date, format=c("%Y/%m/%d"))


t_freq <- texts %>%
  group_by(date) %>% 
  count(no, word, sort = TRUE) %>%
  bind_tf_idf(no, word, n) %>%
  print(t_freq) %>% 
  ungroup()



#2 .단어-문서 행렬 만들기

## 2.1. frequency, tf_dif 값 도출
t_freq <- texts %>%
  count(no, word, sort = TRUE) %>%
  bind_tf_idf(no, word, n) %>%
  print(t_freq)

library("xlsx")

write.xlsx(t_freq, ".//raw_data//wordfreq.xlsx")


## 2.2 tdm 만들기
tdm <- t_freq %>%
  cast_tdm(term=word, document=no, value=n)
inspect(tdm)

tdm_df <- as.data.frame(as.matrix(tdm))

write.csv(tdm_df,".//tdm.csv")

tdm.99 <- removeSparseTerms(tdm, 0.99)
inspect(tdm.99)

str(tdm.99)

tdm.99 <- as.data.frame(as.matrix(tdm.99))

tfidf_tdm <- t_freq %>%
  cast_tdm(document=no, term=word, value=tf_idf)
inspect(tfidf_tdm)

tfidf_tdm.99 <- removeSparseTerms(tfidf_tdm, 0.99)
inspect(tfidf_tdm.99)


## 2.3 dtm 만들기 
dtm <- t_freq %>%
  cast_dtm(document=no, term=word, value=n)
inspect(dtm)

dtm_df <- as.data.frame(as.matrix(dtm))

dtm.99 <- removeSparseTerms(dtm, 0.99) 
inspect(dtm.99)

dtm_df.99 <- as.data.frame(as.matrix(dtm.99))

wordFreq <- dtm.99 %>% as.matrix() %>% colSums()

wordDf <- data.frame( word = names(x = wordFreq),
                      freq = wordFreq,
                      row.names = NULL) 


# 1- sparsity 전체 중에 0이 아닌 칸의 비율을 보여줌, 99% 이상 0이 있는 열을 제거(다시 말해서 단어를 제거하라) 

tfidf_dtm <- t_freq %>%
  cast_dtm(document=no, term=word, value=tf_idf)
inspect(tfidf_dtm)

tfidf_dtm_df <- as.data.frame(as.matrix(tfidf_dtm))

tfidf_dtm.99 <- removeSparseTerms(tfidf_dtm, 0.99)
inspect(tfidf_dtm.99)

tfidf_dtm_df.99 <- as.data.frame(as.matrix(tfidf_dtm.99))

wordFreq1 <- tfidf_dtm.99 %>% as.matrix() %>% colSums()

wordDf1 <- data.frame( word = names(x = wordFreq1),
                      freq = wordFreq1,
                      row.names = NULL) 


wordDF.A <- cbind(wordDf, wordDf1)

write.csv(wordDF.A,".//핵심단어.csv")

wordDF <- data.frame(word = names(x = freq.spareterm,
                                freq = freq.spareterm,
                                row.names = NULL)) %>% arrange(desc(x = freq))


worddf <- read.csv(".//핵심단어.csv", header=T)

worddf <- tibble(worddf)

str(worddf)
library(RColorBrewer)
library(wordcloud2)
library(htmlwidgets)

# 3.워드 클라우드와 단어 빈도분석

## 3.2 단어빈도


# 6.3 단순 워드 클라우드

library(wordcloud)
palete<-brewer.pal(6,"Dark2")

t_freq %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words=word,
                 freq= n,
                 scale=c(4,0.5), 
                 min.freq=20, 
                 max.words=350,
                 random.order=FALSE, 
                 colors=palete))






# 키워드 간 상관관계 파악

# 아래 내용은 검증을 해봐야 함
  
corTerms <- tfidf_dtm.99 %>% as.matrix() %>% cor()

# 키워드 369 개 중에서 선택 (문서에 공통으로 출현되는 단어를 중심으로 선정)

glimpse(corTerms)

findAssocs(tfidf_dtm.99,'재난',0.1)

# 차원을 확인합니다. 
dim(x = corTerms)
## [1] 398 398 상관계수 행렬로 연관성 높은 단어를 확인합니다.

checkCorTerms <- function(n = 10, keyword) {
  
  # 키워드 유무를 확인합니다.
  corTerms %>% 
    colnames() %>% 
    str_subset(pattern = keyword) %>% 
    print()
  
  # str_subset 은 완전히 매칭되는 단어를 찾아주는 못함. 

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
checkCorTerms(n = 20, keyword = '대통령')  

corTerms[1:10,1:10]

# 네트워크 분석 
# pacman::p_load('tidymodels','tidytext','NLP4kec' ,'stringr','magrittr','tm', 'network','GGally', 'sna', 'RColorBrewer)

#install.packages("network")
#install.packages("GGally")

library(network)
library(GGally)
library(sna)

netTerms <- network(x = corTerms, directed = FALSE)

plot(netTerms, vertex.cex = 1)

corTerms[corTerms <= 0.20] <- 0
netTerms <- network(x = corTerms, directed = FALSE) # 네트워크에 방향성이 있음 
netTerms

plot(netTerms, vertex.cex = 1)

btnTerms <- betweenness(netTerms) 
btnTerms[1:10]

netTerms %v% 'mode' <-
  ifelse(
    test = btnTerms >= quantile(x = btnTerms, probs = 0.90, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest')


nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

set.edge.value(netTerms, attrname = 'edgeSize', value = corTerms * 3) # 상관계수의 3배 

ggnet2(
  net = netTerms,
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.size = sna::degree(dat = netTerms),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')
        
  )

ggnet2(
  net = netTerms,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  labs(title = "매개중심성 반영한 단어-네트워크맵")


# 공빈도 출현 단어 분석 (tdm 활용)

tdm.matrix <- as.matrix(tdm)
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계
word.order <- order(word.count, decreasing=T)  #내림차순 정렬
freq.words <- tdm.matrix[word.order[1:40], ] #상위 20개 단처 추출
co.matrix <- freq.words %*% t(freq.words)  #TDM을 Co-occurence Matrix로 변경


#install.packages("qgraph")
library(qgraph)


qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              
       edge.color='blue',
       vsize=log(diag(co.matrix))*1) 

  
  
  
  