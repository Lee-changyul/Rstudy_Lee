# 10장 데이터 수집과 분석

##2. 뉴스기사 수집과 분석
setwd("C:/R")

install.packages("rvest") 
library(rvest) 

# 책 본문에서 예로 든 시사인 기사가 더이상 크롤링이 허용되지 않기 때문에 유사한 논조의 한겨레신문 기사로 대체하여 분석함 

# 뉴스기사(한겨레신문 기사)
html <- read_html("http://www.hani.co.kr/arti/opinion/column/891514.html")
nodes <- html_nodes(html, css = "#a-left-scroll-in")
text1 <- html_text(nodes)
text1

# 뉴스기사 크롤링(중앙일보사설)
html <- read_html("https://news.joins.com/article/22886574")
nodes <- html_nodes(html, css = "#article_body")
text2 <- html_text(nodes)
text2

text <- rbind(text1, text2)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_191")
library(rJava)

library(NLP4kec)
library(tm)
text_p <- r_parser_r(text, language = "ko", useEn =F, korDicPath = "./dictionary/user_dictionary.txt")
text_p

stopWordDic <- read.csv("./dictionary/stopword_ko.csv")
synonymDic <- read.csv("./dictionary/synonym.csv")


for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], text_p))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    text_p[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], text_p[docNum])
  }
}
text_p <- gsub(" ","  ",text_p)
corp <- VCorpus(VectorSource(text_p))
corp
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, removeWords, stopWordDic$stopword)
corp <- tm_map(corp, PlainTextDocument) 
dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[,nchar(colnames(dtm)) > 1]
dtm
dtm <- removeSparseTerms(dtm, as.numeric(0.98))
dtm

dtm_df <- as.data.frame(as.matrix(dtm))
tdm_df <- t(dtm_df)


# 비교하기
#install.packages("wordcloud")
library(wordcloud)

colnames(tdm_df) <- c("한겨레", "중앙")
#View(tdm_df)

# 비교 그림 실행을 반복할 경우에는 "Session - Terminate R" 로 다시 시작하여야 동일한 그림을 얻을 수 있다.    
comparison.cloud(tdm_df, colors = c("blue", "red"),scale=c(2,1),rot.per=.01, max.words = 70, random.order=FALSE, title.bg.colors=c("grey40","grey70"))

commonality.cloud(tdm_df,colors = c("purple", "red"), scale=c(2,1),rot.per=.02, max.words = 50, random.order=FALSE)

# 상관관계 구하기: 가중치 주기
tdmw = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf),
                                             weighting = function(x) weightTfIdf(x, normalize = TRUE)))  #Tf-Idf 가중치 주기
tdmw_df = as.data.frame(as.matrix(tdmw))
colnames(tdmw_df) <- c("한겨례", "중앙")

my.assoc.func <- function(tdm, term1, term2) {
  var1 <- as.numeric(tdm[, term1])
  var2 <- as.numeric(tdm[, term2])
  cor.test(var1, var2)
}

my.assoc.func(tdmw_df, "한겨례", "중앙")


##3. 영화(완벽한 타인) 댓글 크롤링과 흥행 예측분석

setwd("C:/R")

library(rvest)
library(reshape2) #melt
library(dplyr) # filter, %>%

# 완벽한 타인(영화)
urls<-paste0("https://movie.daum.net/moviedb/grade?movieId=117910&type=netizen&page=",c(170:186)) 
urls <- lapply(urls,read_html) # 불러온 댓글을 html 형식으로 읽어옴
#주의: c(1:136)136은 댓글이 늘어나면서 변동될 수 있다. 

# 크롤링을 통해 가져온 자료를 저장하기 위한 변수를 미리 만들어 놓음
other<-NA  # 영화평
other.star<-NA # 별점
other.date<-NA

# 각 댓글마다 별점, 감상평, 날짜/시간 데이터를 불러와서 순서대로 위에서 만들어놓은 변수들에 저장함
for (i in 1:length(urls) ) {
  other[i]<-melt(urls[[i]] %>% html_nodes(".desc_review") %>% html_text())
  other.star[i]<-melt(urls[[i]] %>% html_nodes(".emph_grade") %>% html_text())
  other.date[i]<-melt(urls[[i]] %>% html_nodes(".info_append") %>% html_text())
}
#other.star

other<-melt(other)
other.star<-melt(other.star)
other.date<-melt(other.date)
#other.star

#통합자료생성
other_df<-cbind(other, other.star, other.date)
colnames(other_df)
other_df<-other_df[-c(2,4,6)]
names(other_df)<-c('content','star','date')

other_df <- data.frame(lapply(other_df, trimws), stringsAsFactors = F) # 공백제거
other_df$star<-as.numeric(other_df$star)

other_df$date<-as.POSIXct(other_df$date,  format = "%Y.%m.%d, %H:%M", tz = "EST5EDT") # 시간 데이터 생성
str(other_df)

# 인문사회과학도 기준으로 컴퓨터는 한없이 예민하다. 교재에 나온대로 정확히 코딩을 하여도 에러가 나는 경우가 매우 많다. 때로는 제공되는 스크립트를 사용해서 잘못 입력되는 것을 예방할 수도 있다.


#개봉 후 일줄일간의 댓글과 평점(10월 31일 - 11월 7일)
other_df <- filter(other_df, date <= "2018-11-07" & date > "2018-10-31")

other_df <- other_df[-which(other_df$content==""),] # 빈 행(row) 제거
other_df$id <- 1  # 새로운 ID 생성
other_df$id <- cumsum(other_df$id)
other_df <- other_df[,c(4,1,2,3)] # 열(column) 순서 교체
str(other_df)

#엑셀파일로 저장하고 불러와 사용할 수 있음
library(rJava)
library(writexl)
writexl::write_xlsx(other_df, "./raw_data/other_df.xlsx") # 영화평 전체 데이터 저장
writexl::write_xlsx(other_df[,c(1,2)], "./raw_data/other_dftext.xlsx") # 영화평 댓글 데이터 저장

#형태소 분석
setwd("C:/R")

library(NLP4kec)
library(tm)
library(tidytext)
library(qdap)
library(tidyverse)

clean_text <- function(text){
  text <- tolower(text)
  text <- removeNumbers(text) # 숫자제거
  text <- bracketX(text) #괄호 안 텍스트 제거
  text <- removePunctuation(text) # 특수기호제거
  text <- stripWhitespace(text) #공백제거
  indexes <- which(text == "") #빈 항 제거
  if(length(indexes) > 0){
    text <- text[-indexes]
  } 
  return(text)
}

temp <- clean_text(other_df$content)

p_data <- r_parser_r(temp, language = "ko", useEn =F, korDicPath = "./dictionary/user_dictionary.txt")
p_data 

#만일 저장된 엑셀파일을 사용하기를 원할 경우 file_parser_r 함수를 사용함
#p_data <- file_parser_r(path="./raw_data/other_dftext.xlsx", language = "ko", useEn =F, korDicPath = "./dictionary/user_dictionary.txt")

stopWordDic <- read.csv("./dictionary/stopword_ko2.csv")
synonymDic <- read.csv("./dictionary/synonym2.csv")

for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], p_data))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    p_data[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], p_data[docNum])
  }
}
p_data <- gsub(" ","  ",p_data)
corp <- VCorpus(VectorSource(p_data))
corp
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, removeWords, stopWordDic$stopword)
corp <- tm_map(corp, PlainTextDocument) 
dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
colnames(dtm) <- trimws(colnames(dtm)) # 공백제거
dtm <- dtm[,nchar(colnames(dtm)) > 1]
dtm <- removeSparseTerms(dtm, as.numeric(0.98))


# 단어 빈도 정보 시각화 하기
freq <- colSums(as.matrix(dtm))  
length(freq)

freq[head(order(-freq), 20)]

install.packages("qgraph")
library(qgraph)

tdm.matrix <- as.matrix(t(dtm))
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계
word.order <- order(word.count, decreasing=T)  #내림차순 정렬
freq.words <- tdm.matrix[word.order[1:20], ] #상위 20개 단처 추출
co.matrix <- freq.words %*% t(freq.words)  #TDM을 Co-occurence Matrix로 변경

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              
       edge.color='blue',
       vsize=log(diag(co.matrix))*2) 

#. 별점 정보
hist(other_df$star)
mean(other_df$star)

table(other_df$star)

# 특정 별점에서 추출된 명사 확인
mydf <- data.frame(content = sapply(corp, as.character), stringsAsFactors = FALSE)
mydf$id <- 1  # ID 생성
mydf$id <- cumsum(mydf$id)
View(mydf)
other_df2 <- other_df[,-2]
View(other_df2)
text.noun.star<-merge(mydf,other_df2, c('id')) # 두 파일 병합
View(text.noun.star)

text.noun.star[,c(2,3)] %>% filter(star<=4)  %>% head(10)
text.noun.star[,c(2,3)] %>% filter(star==10) %>% head(10)

# 다른 영화와 비교 언급
library(stringi)
text.noun.star$movie.name<-stri_count_regex(text.noun.star$content, "봄|보헤미안|황소|바울|폴란드")

table(text.noun.star$movie.name)
table(text.noun.star$movie.name)/nrow(text.noun.star)

# 배우, 감독 언급
text.noun.star$act.drt.name<-stri_count_regex(text.noun.star$content, "감독|이서진|유해진|조진웅|김지수")
table(text.noun.star$act.drt.name)/nrow(text.noun.star)


# 영화 내용에 대한 언급 (ex, 이야기, 스토리, 내용)
text.noun.star$contents<-stri_count_regex(text.noun.star$content, "연기|이야기|스토리|내용")
table(text.noun.star$contents)/nrow(text.noun.star)


# Density로 보기 (별점)
library(ggplot2)
ggplot(text.noun.star, aes(star)) + 
  geom_density(data = subset(text.noun.star,movie.name > 0), fill = "red", alpha = 0.2) +
  geom_density(data = subset(text.noun.star,act.drt.name > 0), fill = "blue", alpha = 0.2) +
  geom_density(data = subset(text.noun.star,contents > 0), fill = "green", alpha = 0.2)

# Density로 보기 (날짜)
ggplot(text.noun.star, aes(date)) + 
  geom_density(data = subset(text.noun.star,movie.name > 0), fill = "red", alpha = 0.2) +
  geom_density(data = subset(text.noun.star,act.drt.name > 0), fill = "blue", alpha = 0.2) +
  geom_density(data = subset(text.noun.star,contents > 0), fill = "green", alpha = 0.2)


ggplot(text.noun.star, aes(x=date, y=star)) + geom_line(color="red", size=0.5)


# 4. 다음 뉴스 댓글 분석
setwd("C:/R")

install.packages("remotes")
library(remotes)

remotes::install_github("forkonlp/DNH4", force = TRUE)
#만일 업데이트 선택나오면 21: None의 21을 입력하고 클릭한다
library(DNH4)

# 주한미군 철수를 주장하는 국민주권연대 회원들과 한미동맹 강화를 외치는 태극기 집회 참가자들이 3일 오후 서울 광화문광장에서 열린 집회를 마친 뒤 깃발을 들고 행진하며 서로 엇갈리고 있다. 이날 광화문광장 일대는 각계각층의 시민·사회단체 행사와 집회로 북새통을 이뤘다. 2018.11.3


# 다음 뉴스 댓글 수집
comments <- DNH4::getComment("https://news.v.daum.net/v/20181103165019259", limit = 550)
colnames(comments)
comments <- comments[,c(1, 8, 12)]
names(comments) <- c("id", "content", "like")
str(comments)

#형태소 분석
install.packages("igraph")

library(NLP4kec)
library(tm)
library(tidytext)
library(qdap)
library(tidyverse)

stopWordDic <- read.csv("./dictionary/stopword_ko.csv")
synonymDic <- read.csv("./dictionary/synonym.csv")

clean_text <- function(text){
  text <- tolower(text)
  text <- removeNumbers(text) # 숫자제거
  text <- bracketX(text) #괄호 안 텍스트 제거
  text <- removePunctuation(text) # 특수기호제거
  text <- stripWhitespace(text) #공백제거
  indexes <- which(text == "")
  if(length(indexes) > 0){
    text <- text[-indexes]
  } 
  return(text)
}

temp <- clean_text(comments$content)

p_data <- r_parser_r(temp, language = "ko", useEn =F, korDicPath = "./dictionary/user_dictionary.txt")

for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], p_data))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    p_data[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], p_data[docNum])
  }
}
p_data <- gsub(" ","  ",p_data)
corp <- VCorpus(VectorSource(p_data))
corp
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, removeWords, stopWordDic$stopword)
corp <- tm_map(corp, PlainTextDocument) 
dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[,nchar(colnames(dtm)) > 1]
dtm
dtm <- removeSparseTerms(dtm, as.numeric(0.98))

#Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))

#DTM, TDM을 데이터 프레임 형식으로 저장하기
dtm_df <- as.data.frame(as.matrix(dtm))
tdm_df <- as.data.frame(as.matrix(tdm))

# 분석 

# 단어 빈도 정보 시각화 하기 --------------

# 단어 발생 단순빈도 구하기
freq <- colSums(as.matrix(dtm))  
length(freq)

freq[head(order(-freq), 20)]

# 단어 빈도 정보로 시각화 하기
#TF-IDF 값으로 연관 키워드 추출하기
dtmW <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                              weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
dtmW

colnames(dtmW) <- trimws(colnames(dtmW)) # 단어 양옆 스페이스 제거
dtmW <- dtmW[,nchar(colnames(dtmW)) > 1] # 두 글자 이상만 남기기
dtmW <- removeSparseTerms(dtmW, as.numeric(0.98))  # 중요하지 않은 노드 단어 제거 
dtmW

findAssocs(dtmW, "태극기", corlimit = 0.1) 
findAssocs(dtmW, "철수", corlimit = 0.1) 


# 키워드 네트워크 분석 및 시각화 ----
install.packages(c("network", "sna", "GGally")) #패키지 한꺼번에 설치하기
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtmW_m = as.matrix(dtmW)
cor_termW = cor(dtmW_m)

cor_termW[1:10, 1:10]

#Edge 개수 조절하기
cor_termW[cor_termW < 0.15] = 0  
cor_termW[1:10, 1:10]

# 다른 노드와 연관성이 0인 노드 제거하기
removeTarget = colSums(cor_termW) == 1
cor_termW = cor_termW[!removeTarget, !removeTarget] # ! 이외에는 남겨라

# Network Map을 그리기 위한 객체 만들기
net = network(cor_termW, directed = FALSE) # 무향 의미

# betweenness값 상위 20% 이면서 eigenvector 값이 상위 10%이면 "High" -> 빨강색
# betweenness값 상위 20% 이면서 eigenvector 값이 하위 90%이면 "Medium" -> 노란색
# betweenness값 하위 80% 이면 "Low" -> 회색
# 
quantile(betweenness(net), seq(0,1,0.1))

net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8)
                        ,ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), 
                        "Low")
node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")

# Network edge size 값 설정하기 (단어간 상관계수 값 * 2)
set.edge.value(net, "edgeSize", cor_termW * 2) # 두배로 크기가 커짐

# Network Map 화면에 그리기

ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = "degree" # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
)

# ggnet2 웹사이트: https://briatte.github.io/ggnet/, https://www.r-graph-gallery.com/network/ 


# 중심성 구하기
word_network <- data.frame(word = rownames(cor_termW),
                           centrality = degree(net), #연결 중심성 구하기 
                           betweenness = betweenness(net), #매개 중심성 구하기
                           closeness = closeness(net, cmode="suminvundir"), # 근접 중심성 구하기
                           eigenvector = evcent(net) # 고유벡터 중심성 구하기
)

head(word_network)
word_network %>% arrange(-betweenness)


# 특정 키워드만 석택한 네트워크 맵 
keyword <- c("중국", "미군", "철수")

cor_termW <- cor(dtmW_m)
cor_termW[cor_termW < 0.01] <- 0

sub_cor_term <- cor_termW[,keyword] # keworkd만 남기기 
sub_cor_term <- sub_cor_term[!(rownames(sub_cor_term) %in% keyword),] #!=not
sub_cor_term <- sub_cor_term[rowSums(sub_cor_term)>0,]

net2 <- network(sub_cor_term, directed = FALSE, matrix.type="bipartite")

ggnet2(net2 # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       #,edge.size = sub_cor_term[sub_cor_term>0] * 2
       ,size = degree(net2)
)

head(comments, 5)


#참고)
# 2. 네이버 뉴스 댓글 분석
setwd("C:/R")

# 네이버 뉴스와 댓글 수집
#remotes::install_github("forkonlp/N2H4")
library(N2H4)
comments <- N2H4::getComment("https://news.naver.com/main/ranking/read.nhn?sid1=&rankingType=popular_memo&oid=001&aid=0010496021&date=20181129&type=1&rankingSectionId=100&rankingSeq", pageSize = 50, page = 1, sort = c("favorite", "reply"))

colnames(comments)
comments <- comments[,c(5, 21, 35)]
names(comments) <- c("id", "content", "like")
str(comments)
# 50대가 댓글의 41%임 

#형태소 분석
library(NLP4kec)
library(tm)
library(tidytext)
library(qdap)
library(tidyverse)

clean_text <- function(text){
  text <- tolower(text)
  text <- removeNumbers(text) # 숫자제거
  text <- bracketX(text) #괄호 안 텍스트 제거
  text <- removePunctuation(text) # 특수기호제거
  text <- stripWhitespace(text) #공백제거
  indexes <- which(text == "")
  if(length(indexes) > 0){
    text <- text[-indexes]
  } 
  return(text)
}

temp <- clean_text(comments$content)

p_data <- r_parser_r(temp, language = "ko", useEn =F, korDicPath = "./dictionary/user_dictionary.txt")


stopWordDic <- read.csv("./dictionary/stopword_ko.csv")
synonymDic <- read.csv("./dictionary/synonym.csv")

for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], p_data))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    p_data[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], p_data[docNum])
  }
}
p_data <- gsub(" ","  ",p_data)
corp <- VCorpus(VectorSource(p_data))
corp
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, removeWords, stopWordDic$stopword)
corp <- tm_map(corp, PlainTextDocument) 
dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[,nchar(colnames(dtm)) > 1]
dtm
dtm <- removeSparseTerms(dtm, as.numeric(0.98))
dtm

# 단어 빈도 정보로 시각화 하기 --------------

#install.packages("qgraph")
library(qgraph)

tdm.matrix <- as.matrix(t(dtm))
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계
word.order <- order(word.count, decreasing=T)  #내림차순 정렬
freq.words <- tdm.matrix[word.order[1:20], ] #상위 20개 단처 추출
co.matrix <- freq.words %*% t(freq.words)  #TDM을 Co-occurence Matrix로 변경

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              
       edge.color='blue',
       vsize=log(diag(co.matrix))*2)  


# 네이버 홈쇼핑 댓글 크롤링


setwd("C:/R")

install.packages("RSelenium")
install.packages("wdman")

library(wdman) #이 패키에서 chrome, firefox, phantomJs 등의 웹드라이버를 제공함
library(RSelenium)
library(rvest) 
library(httr) 
library(stringr)


robot <- "https://search.shopping.naver.com/detail/detail.nhn?nv_mid=12896830416&cat_id=50002352&frm=NVSHAKW&query=%EC%83%A4%EC%98%A4%EB%AF%B8+%EB%A1%9C%EB%B4%87%EC%B2%AD%EC%86%8C%EA%B8%B0&NaPm=ct%3Djqjtqhvs%7Cci%3D94f42d8c86e923b91375fc17c3d1eb53331221a6%7Ctr%3Dslsl%7Csn%3D95694%7Chk%3D28b926d13cc9b43019c80d095a4eb38dbff2cb9f"


# 가끔 "Selenium message:session not created: This version of ChromeDriver only supports Chrome version 74(or 75 등)" 등과 같은 오류 메시지가 발생한다.이는 wdman에서 제공하는 최신 크롬드라이버와 내가 사용하는 크롬 버전이 서로 호환되지 않아서 발생하는 문제이다. 이 경우에는 먼저 아래와 같이 크롬 드라이버 버전을 확인한다.
binman::list_versions("chromedriver") # 크롬 드라이버 버전을 알수가 있음

#$win32
#[1] "2.45"         "2.46"         "71.0.3578.80" "72.0.3626.7"  "73.0.3683.20" "73.0.3683.68" "74.0.3729.6"  "75.0.3770.8"

# 필자의 경우 크롬 버전은 74인데 크롬 드라이버는 위의 결과에서 보이듯이 "75.0.3770.8" 까지이다. 이러할 경우 호환문제가 발생한다. 그래서 아래와 같이 크롬 드라이버를 version="74.0.3729.6"로 지정해서 필자가 사용하는 크롬 버전과 일치시켜야 한다. 

ch <- wdman::chrome(port=4567L, version="74.0.3729.6") #chrome드라이버를 포트 4567번에 배정함
remDr <- remoteDriver(port=4567L, browserName='chrome') #remote설정

remDr$open() #크롬드라이버 실행
remDr$navigate(robot)

All_review <- c() #텍스트를 모으기 위한 백터공간

for(i in 1:69) {  #페이지 수만큼 반복
  a <- i #페이지 번호
  first <- "shop.detail.ReviewHandler.page(" #i 앞 부분
  secon <- ", '_review_paging'); return false;" #i 뒤 부분
  script <- paste(first,a,secon,sep='') 
  pagemove <- remDr$executeScript(script, args = 1:2)
  source<-remDr$getPageSource()[[1]] #페이지 소스 가져오기
  main <- read_html(source) 
  mainfo <- html_nodes(main,css='.atc')
  review <- mainfo%>%html_text() #텍스트만 가져오기
  All_review <- c(All_review,review) #텍스트 저장
}

review <- data.frame(All_review)
View(review)

review$id <- 1  # 새로운 ID 생성
review$id <- cumsum(review$id)
review <- review[,c(2,1)] # 열(column) 순서 교체
names(review)<-c('id','content')
View(review)

str(review)

# 11번가 홈쇼핑 댓글 크롤링

setwd("C:/R")

library(httr)
library(dplyr)
library(rvest)
library(RSelenium)


coff <- "http://www.11st.co.kr/product/SellerProductDetail.tmall?method=getSellerProductDetail&prdNo=143745076&trTypeCd=21&trCtgrNo=585021"

# 크롬 드라이버는 호환성 등의 문제가 발생하기 때문에 저자는 아래의 phantomjs를 추천하는 편이다.
ch <- wdman::phantomjs(port=4567L) #phantomjs 드라이버를 포트 4567번에 배정하고 실행준비함

remDr <- remoteDriver(port=4567L, browserName='phantomjs') #remote설정
remDr$open() #phantomjs 드라이버 실행
remDr$navigate(coff)
remDr$screenshot(display = T)

remDr$getPageSource()[[1]] %>%
  read_html() %>%
  html_nodes("iframe#ifrmReview") %>%
  html_attr("src") -> coff_url  #src에서 주소를 가져와서 저장한다.

coff_url


# 그러나 coff_url 앞 부분이 절단되어 있기 때문에 이어 붙이는 작업수행
root <- "http://deal.11st.co.kr"

#나누기
coff_urls <- strsplit(coff_url, "&page=[0-9]+")[[1]] # [0-9]는 모든 숫자를 의미하는 정규표현식이고 +는 11과 같이 연속된 숫자를 의미한다. 
coff_urls

#모두 합하여 review 데이터 만들기
i <- 1
coff <- paste0(root, coff_urls[1], "&page=", i, coff_urls[2])
remDr$navigate(coff)
remDr$screenshot(display = T)

remDr$getPageSource()[[1]] %>%
  read_html() %>%
  html_nodes("a#reviewContTxt") %>%
  html_text() %>%
  trimws() -> reviews #띄어쓰기 등의 빈공간 삭제

review_all <- c()
#while문 사용해서 페이지 넘기기
while(!identical(reviews, character(0))) {
  i <- i + 1
  coff <- paste0(root, coff_urls[1], "&page=", i, coff_urls[2])
  remDr$navigate(coff)
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes("a#reviewContTxt") %>%
    html_text() %>%
    trimws() -> reviews
  
  review_all <-c(review_all,reviews)
}

review_all %>% length()

review <- data.frame(review_all)

review$id <- 1  # 새로운 ID 생성
review$id <- cumsum(review$id)
review <- review[,c(2,1)] # 열(column) 순서 교체
names(review)<-c('id','content')
View(review)

write.csv(review, "review_11.csv", row.names = F) 





