#12장 감성분석

# 구글에서 받은 키를 분석를 수행하는 디렉토리에 저장한다.  
GL_AUTH = path.expand(file.path("~", ".Renviron")) # R환경파일 만들기
file.edit(file.path("~", ".Renviron")) #R환경파일 열기
# 환경파일이 열리면 GL_AUTH = sentiment-230709-e9f047111cfe.json  를 저장한다, sentiment-230709-e9f047111cfe.json는 저자가 받은 키 파일명


setwd("C:/R")

text <- readLines("sentiment_ex.txt", encoding="UTF-8") #readLines: 텍스트 파일을 불러오는 함수
library(NLP4kec)
library(tm)
library(tidytext)
library(qdap)
library(tidyverse)

clean_text <- function(text){
  text <- tolower(text) 
  text <- removeNumbers(text) #숫자제거 
  text <- bracketX(text) #괄호 안 텍스트 제거
  text <- removePunctuation(text) #특수기호제거
  text <- stripWhitespace(text) #공백제거
  indexes <- which(text == " ")
  if(length(indexes) > 0){
    text <- text[-indexes]
  }
  return(text)
}

temp <- clean_text(text)
text_p <- r_parser_r(temp, language = "ko", useEn =F)
text_p

text_p <- gsub(" ", " ", text_p)
corp <- VCorpus(VectorSource(text_p))
corp

corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)
text <- data.frame(content = sapply(corp, as.character), stringsAsFactors = F)

#install.packages("googleLanguageR")
library(googleLanguageR)

nlp <- gl_nlp(text, nlp_type = c("analyzeSentiment"), language = "ko")
summary(nlp)

str(nlp)

#영화평 감상분석
setwd("C:/R")

library(rvest)
library(reshape2) #melt
library(dplyr) #filter, %>%

#마약왕(영화)
urls <- paste0("http://movie.daum.net/moviedb/grade?movieId=108037&type=netizen&page=",c(1:178))
urls <- lapply(urls,read_html)

drug <- NA #영화평
drug.star <- NA #별점
drug.date <- NA

for (i in 1:length(urls) ) {
  drug[i]<-melt(urls[[i]] %>% html_nodes(".desc_review") %>% html_text())
  drug.star[i]<-melt(urls[[i]] %>% html_nodes(".emph_grade") %>% html_text())
  drug.date[i]<-melt(urls[[i]] %>% html_nodes(".info_append") %>% html_text())
}


drug<-melt(drug)
drug.star<-melt(drug.star)
drug.date<-melt(drug.date)


#통합자료생성
drug_df<-cbind(drug, drug.star, drug.date)
colnames(drug_df)
drug_df<-drug_df[-c(2,4,6)]
names(drug_df)<-c('content','star','date')

drug_df <- data.frame(lapply(drug_df, trimws), stringsAsFactors = F) # 공백제거
drug_df$star<-as.numeric(drug_df$star)

drug_df$date<-as.POSIXct(drug_df$date,  format = "%Y.%m.%d", tz = "EST5EDT") # 시간 데이터 생성
str(drug_df)

drug_df <- drug_df[-which(drug_df$content==""),]
drug_df$id <- 1
drug_df$id <- cumsum(drug_df$id)
drug_df <- drug_df[,c(4,1,2,3)]

str(drug_df)

#형태소 분석
#install.packages("tidytext")
#install.packages("qdap")
#install.packages("tidyverse")
library(tidytext)
library(qdap)
library(tidyverse)
library(NLP4kec)
library(tm)

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

temp <- clean_text(drug_df$content)

p_data <- r_parser_r(temp, language = "ko")

p_data <- gsub(" ","  ",p_data)
corp <- VCorpus(VectorSource(p_data))
corp

corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, PlainTextDocument) 

mydf <- data.frame(content = sapply(corp, as.character), stringsAsFactors = FALSE)
mydf$id <- 1
mydf$id <- cumsum(mydf$id)
drug_df2 <- drug_df[, -2]
text <- merge(mydf, drug_df2, c('id')) #두 파일 병합
text <- na.omit(text) #결측지 제거
text[,1] <- NULL #id 제

#크롤링을 최근 순서부터 하였기에 순서를 과거부터 시작하는 것으로 변경함
text <- text[nrow(text):1,]

library(googleLanguageR)
#install.packages("scales")
library(scales)
library(ggplot2)
nlp <- gl_nlp(text$content, nlp_type = c("analyzeSentiment"), language = "ko")

nlp

str(nlp)

tobText <- text %>% add_column(magnitude = nlp$documentSentiment$magnitude, score = nlp$documentSentiment$score) #add_column 은 새로운 열을 추가해주는 tidyverse패키지 함수

tobText <- na.omit(tobText) #결측지 제거

summary(tobText)

mean(tobText$score)

hist(tobText$score,main = "영화 감성분석", xlab = "감성 점수", ylab = "빈도", col = "red")

hist(tobText$star, main = "영화평 분석", xlab = "영화평 점수", ylab = "빈도", col = "red")

ggplot(tobText, aes(x = date, y = score)) + geom_line() + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%m, %d")) + labs(title="영화 감성분석", x = "날짜", y = "감성 점수")

#install.packages("syuzhet")
library(syuzhet)

#백분율 기반 평균 그래프
pct_values <- get_percentage_values(tobText$score)

plot(pct_values,
     type = "l",
     main = "영화 감성분석",
     xlab = "시간",
     ylab = "감성 점수",
     col = "red")

#DCT 기반 그래프
dct_values <- get_dct_transform(
  tobText$score,
  low_pass_size =5,
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T)

plot(dct_values,
     type = "l", #h:histogram
     main = "영화 감성분석",
     xlab = "시간",
     ylab = "감성 점수",
     col = "red")

#simple_plot
simple_plot(tobText$score)

#simple_plot
simple_plot(tobText$star)
