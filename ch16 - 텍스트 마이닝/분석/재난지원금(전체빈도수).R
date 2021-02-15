# Tidy-text를 이용한 데이터 분석

# Orange%>% filter(age %in% c(118, 664, 1582)) 
# 정확히 일치하는 것 찾기 
# filter(Orange, Tree==1 & age==118) 그리고의 의미 
# filter(Orange, Tree==1 | age==118) 또는의 의미 
#"filter(between(나이, 30, 39 ))" 사이값 찾기 
# filter(!word %in% stop_words$word) 단어와 다른 것을 골라내라. 
# ifelse 함수를 이용하여 ID가 짝수이면 A, 홀수이면 B를 부여 
# iris["Group"]<-ifelse(iris$ID%%2==0,"A","B")
# funcif<-function(a){if(a<0){return(0)}else if(a>=1&&a<6){  return(1)} else{return(10)}}funcif(-3)
# ifelse(<조건>, <조건에 부합할 경우>, 
# ifelse(<조건>, <조건에 부합할 경우>, <조건에 부합하지 않을 경우>))
# 공통단어 추출 : mutate(diff= abs(a-b))

# 엑셀 작업을 위해서 저장

library("xlsx")
# write.xlsx(t_freq, ".//raw_data//wordfreq.xlsx")


save(list=ls(), file ="재난지원금.RData")
load("재난지원금.RData")


pacman::p_load('tidymodels','tidytext', 'tidyverse', 'tm', 'network','GGally', 'sna', 'RColorBrewer')

# install.packages("pacman")

library(writexl)
# install.packages("readxl")
#install.packages("writexl")
library(readxl)

# 1. 데이터 읽기 ####

texts <- read_excel(".//raw_data//final.xlsx") # 만약 깨지면 오피스 2003 형식으로 저장 .xls 열림 

texts <- texts %>% 
  select(-word1) %>% 
  filter(no != 1657)

texts$word <- str_replace_all(texts$word, "재난지원금", "긴급재난지원금") 

str(texts)

library(xlsx)

# write.xlsx(texts, ".//raw_data//final(0130).xlsx")

# 2. 빈도분석하기 ####

# st_word <- tibble(word=c("긴급재난지원금", "지급")) # 불용어 추가
# anti_join(st_word, by=c("word"="word"))

t_freq <- texts %>%
  group_by(date) %>%
  count(no, word, sort = TRUE) %>%
  bind_tf_idf(no, word, n) %>%
  print(t_freq) %>% 
  ungroup()


t_freq["Group"] <- ifelse(t_freq$date <= 20200317, "first", # 지급논의
                          ifelse(t_freq$date <= 20200331, "second", #1차추경
                                 ifelse(t_freq$date <= 20200415, "third", "fourth"))) # 총선이전 # 총선이후

str(t_freq)

write.xlsx(t_freq, ".//raw_data//word.xlsx")

head(t_freq, 20)

capture.output(head(t_freq, 20), 
               file = ".//재난지원금.txt",
               append = T) 


# 1) 일반빈도 분석 


t_freq %>% group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-count) %>% # 빈도순으로 정렬하라, 
  head(25) %>% 
  mutate(word = reorder(word, count)) %>% # 그래프에서 단어를 다시 정렬해서 보기 좋게 만들어라. 
  ungroup() %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(count), hjust = -0.4)) +
  coord_flip()

# 2) 가중치부여 빈도

t_freq %>% group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-countwt) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, countwt))%>%
  head(20) %>% 
  ungroup() %>% 
  ggplot(aes(word, countwt)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(countwt, digit = 3), hjust = -0.4)) +
  coord_flip()

# 3) 워드 클라우드 

library(wordcloud)
palete<-brewer.pal(6,"Dark2")

st_word <- tibble(word=c("긴급재난지원금", "지급"))

t_freq %>%
  anti_join(st_word, by=c("word"="word")) %>%
  group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>%
  ungroup() %>% 
  with(wordcloud(words=word,
                 freq= count,
                 scale=c(7,0.3), 
                 min.freq=40, 
                 max.words=7000,
                 random.order=FALSE, 
                 colors=palete))


# 3. 시기별 주요 키워드 분석 ####

# 1. 2월말 - first결정까지 

t_freq %>% filter(Group == "first") %>%  group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-count) %>%
  filter(count > 23) %>%
  head(20) %>% 
  mutate(word = reorder(word, count))%>%
  ungroup() %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(count), hjust = -0.4)) +
  coord_flip()


my.freq.func <- function(data, group, freq=20, top=20){
  
data %>% 
    filter(Group == group) %>%  group_by(word) %>%  
    summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
    arrange(-count) %>%
    filter(count > freq) %>%
    head(top) %>% 
    mutate(word = reorder(word, count))%>%
    ungroup() %>% 
    ggplot(aes(word, count)) +
    geom_col() +
    labs(x = "", y = "") +
    geom_text(aes(label=format(count), hjust = -0.4)) +
    coord_flip()  

    ggsave(file=paste0(".//graph.", group, ".jpg"), width=12, height =6, units = "in") 
         # directory, filename +  width=20, height=15, units=c("cm")) # width, height, units
            
}


my.freq.func(t_freq, "first", 20, 20)

# 반복문을 통해서 한번에 해결 

for(i in unique(t_freq$Group))
  {my.freq.func(t_freq, i , 23 ,20)}


# 잠시 묶어두자 ####

t_freq %>% filter(Group == "first") %>% group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-countwt) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, countwt))%>%
  head(20) %>% 
  ungroup() %>% 
  ggplot(aes(word, countwt)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(countwt, digit = 3), hjust = -0.4)) +
  coord_flip()
  

# 2. 1차 추경 - 긴급재난지원금 결정까지 

t_freq %>% filter(Group == "second") %>%  group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-count) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, count))%>%
  head(20) %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(count), hjust = -0.4)) +
  coord_flip()


t_freq %>% filter(Group == "second") %>% group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-countwt) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, countwt))%>%
  head(20) %>% 
  ungroup() %>% 
  ggplot(aes(word, countwt)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(countwt, digit = 3), hjust = -0.4)) +
  coord_flip()

# 3. 지급결정이후 - 총선까지 

t_freq %>% filter(Group == "third") %>%  group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-count) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, count))%>%
  head(20) %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(count), hjust = -0.4)) +
  coord_flip()


t_freq %>% filter(Group == "third") %>% group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-countwt) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, countwt))%>%
  head(20) %>% 
  ungroup() %>% 
  ggplot(aes(word, countwt)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(countwt, digit = 3), hjust = -0.4)) +
  coord_flip()

# 4. 총선 이후 - 지급 결정까지 

t_freq %>% filter(Group == "fourth") %>%  group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-count) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, count))%>%
  head(20) %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(count), hjust = -0.4)) +
  coord_flip()


t_freq %>% filter(Group == "fourth") %>% group_by(word) %>%  
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  arrange(-countwt) %>%
  filter(count > 23) %>% 
  mutate(word = reorder(word, countwt))%>%
  head(20) %>% 
  ungroup() %>% 
  ggplot(aes(word, countwt)) +
  geom_col() +
  labs(x = "", y = "") +
  geom_text(aes(label=format(countwt, digit = 3), hjust = -0.4)) +
  coord_flip()


# 시기간에 그룹간 용어 등장에 차이가 있는가? 기간별 비교 그래프를 그려보자. ####
# TDM 만들어서 비교하는 것 


t_freq1 <- t_freq %>% group_by(Group, word) %>% 
  summarise(countwt = sum(tf_idf), count =sum(n)) %>% 
  ungroup()

head(t_freq1)

tdm_df <- t_freq1 %>%
  cast_tdm(term=word, document=Group, value=count) %>% 
  as.matrix() %>% 
  as.data.frame() 
  
head(tdm_df)
  
# install.packages("plotrix")

library(dplyr)
library(plotrix)

par(mfrow=c(1,1))

# 공통 단어를 중심 으로 추이를 분석 하는 그래프 

# 1. 지급논의 - 1차추경 사이 

common_words_25 <- tdm_df %>% 
  mutate(label = rownames(tdm_df)) %>% 
  filter(second > 15 & first > 15 & third > 15 & fourth > 15) %>% #시기별로 10회이상 출현한 단어 기준 
  mutate(diff = abs(first-second),
         diff2 = abs(second-third), 
         diff3 = abs(third-fourth)) %>% 
  arrange(desc(diff)) %>% slice(1:25) # 1행부터 20행까지 잘라서 프레임 자체에 넣는 방식 

pyramid.plot(common_words_25$first, common_words_25$second,
             labels = common_words_25$label, gap = 70,
             top.labels = c("지급논의", "Words", "1차추경"),
             main = "공통단어", laxlab = NULL, 
             raxlab = NULL, unit = NULL)


# 2. 1차추경 - 총선사이 


common_words_25 <- tdm_df %>% 
  mutate(label = rownames(tdm_df)) %>% 
  filter(second > 15 & first > 15 & third > 15 & fourth > 15) %>% #시기별로 10회이상 출현한 단어 기준 
  mutate(diff = abs(first-second),
         diff2 = abs(second-third), 
         diff3 = abs(third-fourth)) %>% 
  arrange(desc(diff2)) %>% slice(1:25) # 1행부터 20행까지 잘라서 프레임 자체에 넣는 방식 

pyramid.plot(common_words_25$second, common_words_25$third,
             labels = common_words_25$label, gap = 100,
             top.labels = c("1차추경", "Words", "총선이전"),
             main = "공통단어", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

# 총선이전 - 총선이후 


common_words_25 <- tdm_df %>% 
  mutate(label = rownames(tdm_df)) %>% 
  filter(second > 15 & first > 15 & third > 15 & fourth > 15) %>% #시기별로 10회이상 출현한 단어 기준 
  mutate(diff = abs(first-second),
         diff2 = abs(second-third), 
         diff3 = abs(third-fourth)) %>% 
  arrange(desc(diff3)) %>% slice(1:25) # 1행부터 20행까지 잘라서 프레임 자체에 넣는 방식 

pyramid.plot(common_words_25$third, common_words_25$fourth,
             labels = common_words_25$label, gap = 100,
             top.labels = c("총선이전", "Words", "총선이후"),
             main = "공통단어", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

# 4. 단어간 연관되는 단어 분석 (필요시 사용 - dtm어떤 단어가 연계가 많이 되었는가? 긍정과 부정) ####

corterm <- t_freq %>% cast_dtm(document=no, term=word, value=tf_idf) %>% 
  removeSparseTerms(0.99) 

findAssocs(corterm,"재정건전성", 0.01) # 특정단어랑 연관되는 상관관계 보기 

corTerms <- t_freq %>% cast_dtm(document=no, term=word, value=tf_idf) %>% 
  removeSparseTerms(0.99) %>% as.matrix() %>% cor() 

# 5. 매개중심성을 활용한 그래프(네트워크 분석) ####

# 네트워크 분석 
# pacman::p_load('tidymodels','tidytext','NLP4kec' ,'stringr','magrittr','tm', 'network','GGally', 'sna', 'RColorBrewer)

#install.packages("network")
#install.packages("GGally")

library(network)
library(GGally)
library(sna)

#netTerms <- network(x = corTerms, directed = FALSE) # 기본 함수식
# plot(netTerms, vertex.cex = 1)

corTerms[corTerms <= 0.20] <- 0
netTerms <- network(x = corTerms, directed = FALSE) # 네트워크에 방향성이 없음

plot(netTerms, vertex.cex = 1)

# 매개 중심성 상위 10%에 해당하는 노드를 금색으로 색칠하고 
# 각 노드를 연결하는 엣지는 상관계수의 3배로 설정

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
  node.size = sna::degree(dat = netTerms), # degree centrality 연결중심성(직접연결된 노드 수)을 중심으로 노드사이즈 선정  
  edge.size = 'edgeSize', # 엣지의 굵기를 상관관계에 따라 다르게 하기 
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

# 중심성 기준으로 시기별로 연관되는 단어 흐름 보기(가중치 부여 단어) 

# 1번째 시기(지급논의)

corTerms.first <- t_freq %>% filter(Group == "first") %>% 
  cast_dtm(term=word, document=no, value=tf_idf) %>% 
  removeSparseTerms(0.93) %>% as.matrix() %>% cor()   

corTerms.first[corTerms.first <= 0.10] <- 0
netTerms.first <- network(x = corTerms.first, directed = FALSE) # 네트워크에 방향성이 없음

plot(netTerms.first, vertex.cex = 1)

# 매개 중심성 상위 10%에 해당하는 노드를 금색으로 색칠하고 
# 각 노드를 연결하는 엣지는 상관계수의 3배로 설정

btnTerms.first <- betweenness(netTerms.first) 
btnTerms.first[1:10]

netTerms.first %v% 'mode' <-
  ifelse(
    test = btnTerms.first >= quantile(x = btnTerms.first, probs = 0.90, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest')


nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

set.edge.value(netTerms.first, attrname = 'edgeSize', value = corTerms.first * 3) # 상관계수의 3배 

ggnet2(
  net = netTerms.first,
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.size = sna::degree(dat = netTerms.first), # degree centraity 연결중심성(직접연결된 노드 수)을 중심으로 노드사이즈 선정  
  edge.size = 'edgeSize', # 엣지의 굵기를 상관관계에 따라 다르게 하기 
  family = 'AppleGothic')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')
        
  )

ggnet2(
  net = netTerms.first,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms.first),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  labs(title = "매개중심성 반영한 단어-네트워크맵")

# 2번째 시기(1차추경)


corTerms.second <- t_freq %>% filter(Group == "second") %>% 
  cast_dtm(document=no, term=word, value=tf_idf) %>% 
  removeSparseTerms(0.93) %>% as.matrix() %>% cor()

corTerms.second[corTerms.second <= 0.10] <- 0
netTerms.second <- network(x = corTerms.second, directed = FALSE) # 네트워크에 방향성이 없음

plot(netTerms.second, vertex.cex = 1)

# 매개 중심성 상위 10%에 해당하는 노드를 금색으로 색칠하고 
# 각 노드를 연결하는 엣지는 상관계수의 3배로 설정
 
btnTerms.second <- betweenness(netTerms.second) 
btnTerms.second[1:10]

netTerms.second %v% 'mode' <-
  ifelse(
    test = btnTerms.second >= quantile(x = btnTerms.second, probs = 0.90, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest')


nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

set.edge.value(netTerms.second, attrname = 'edgeSize', value = corTerms.second * 3) # 상관계수의 3배 

ggnet2(
  net = netTerms.second,
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.size = sna::degree(dat = netTerms.second), # degree centraity 연결중심성(직접연결된 노드 수)을 중심으로 노드사이즈 선정  
  edge.size = 'edgeSize', # 엣지의 굵기를 상관관계에 따라 다르게 하기 
  family = 'AppleGothic')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')
        
  )

ggnet2(
  net = netTerms.second,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms.second),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  labs(title = "매개중심성 반영한 단어-네트워크맵")


# 3번째 시기(총선이전)

corTerms.third <- t_freq %>% filter(Group == "third") %>% 
  cast_dtm(document=no, term=word, value=tf_idf) %>% 
  removeSparseTerms(0.94) %>% as.matrix() %>% cor()

corTerms.third[corTerms.third <= 0.10] <- 0
netTerms.third <- network(x = corTerms.third, directed = FALSE) # 네트워크에 방향성이 없음

plot(netTerms.third, vertex.cex = 1)

# 매개 중심성 상위 10%에 해당하는 노드를 금색으로 색칠하고 
# 각 노드를 연결하는 엣지는 상관계수의 3배로 설정

btnTerms.third <- betweenness(netTerms.third) 
btnTerms.third[1:10]

netTerms.third %v% 'mode' <-
  ifelse(
    test = btnTerms.third >= quantile(x = btnTerms.third, probs = 0.90, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest')


nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

set.edge.value(netTerms.third, attrname = 'edgeSize', value = corTerms.third * 3) # 상관계수의 3배 

ggnet2(
  net = netTerms.third,
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.size = sna::degree(dat = netTerms.third), # degree centraity 연결중심성(직접연결된 노드 수)을 중심으로 노드사이즈 선정  
  edge.size = 'edgeSize', # 엣지의 굵기를 상관관계에 따라 다르게 하기 
  family = 'AppleGothic')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')
        
  )

ggnet2(
  net = netTerms.third,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms.third),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  labs(title = "매개중심성 반영한 단어-네트워크맵")


# 4번째 시기(총선이후)

corTerms.fourth <- t_freq %>% filter(Group == "fourth") %>% 
  cast_dtm(document=no, term=word, value=tf_idf) %>% 
  removeSparseTerms(0.95) %>% as.matrix() %>% cor()

corTerms.fourth[corTerms.fourth <= 0.10] <- 0
netTerms.fourth <- network(x = corTerms.fourth, directed = FALSE) # 네트워크에 방향성이 없음

plot(netTerms.fourth, vertex.cex = 1)

# 매개 중심성 상위 10%에 해당하는 노드를 금색으로 색칠하고 
# 각 노드를 연결하는 엣지는 상관계수의 3배로 설정

btnTerms.fourth <- betweenness(netTerms.fourth) 
btnTerms.fourth[1:10]

netTerms.fourth %v% 'mode' <-
  ifelse(
    test = btnTerms.fourth >= quantile(x = btnTerms.fourth, probs = 0.90, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest')


nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

set.edge.value(netTerms.fourth, attrname = 'edgeSize', value = corTerms.fourth * 3) # 상관계수의 3배 

ggnet2(
  net = netTerms.fourth,
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.size = sna::degree(dat = netTerms.fourth), # degree centraity 연결중심성(직접연결된 노드 수)을 중심으로 노드사이즈 선정  
  edge.size = 'edgeSize', # 엣지의 굵기를 상관관계에 따라 다르게 하기 
  family = 'AppleGothic')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')
        
  )

ggnet2(
  net = netTerms.fourth,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms.fourth),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  labs(title = "매개중심성 반영한 단어-네트워크맵")


# 참고 (중심성 구하기 )

word_network <- data.frame(word = rownames(corTerms),
                           centrality = degree(netTerms), #연결 중심성 구하기 
                           betweenness = betweenness(netTerms), #매개 중심성 구하기
                           closeness = closeness(netTerms, cmode="suminvundir"), # 근접 중심성 구하기
                           eigenvector = evcent(netTerms)) # 고유벡터 중심성 구하기


# 6. 공출현빈도를 통한 그래프 그리기 ####


# install.packages("widyr" ,dependencies = T)

library(widyr)

co <- t_freq %>% pairwise_count(word, no, sort=T, upper = T) 

# 사용법 pairwise_count(텍스트 데이터, 그룹 단위 변수, sort = T, upper = F)
# tbl = 대상 데이터,
# item = 갯수를 새어야 할 컬럼,
# feature = 함께 출현했다고 판단할 단위 그룹,
# sort = 출현 횟수 단위로 정렬할지
# upper = F는 반복출력 피해줌 (1,2) (2,1) 중에서 하나만 출력
# 그룹 단위 내에서 단어가 동시에 출현한 횟수를 세어주는 함수. 
# 보통 문장 단위를 그룹으로 처리 - 본 연구에서는 기사 하나가 단위가 될 것  

# 1) 관심단어 동시출현단어 확인하기 

co %>% filter(item1 %in% c("이재명")) %>% 
  top_n(15) %>% 
  mutate(item2 = fct_reorder(item2, n, .desc = TRUE)) %>% 
  ggplot(aes(x = item2, y = n, fill = item1)) +
  geom_bar(stat = "identity")


# 2) 공출현 빈도를 중심으로 전체 네트워크 그리기 

library(igraph)
library(ggraph)
library(networkD3)

co %>% filter(n > 50) %>% 
  with(simpleNetwork(co,
              zoom = TRUE,
              fontSize = 15,
              linkDistance = 75,
              opacity = 0.9))


write.csv(co, ".//co.csv")