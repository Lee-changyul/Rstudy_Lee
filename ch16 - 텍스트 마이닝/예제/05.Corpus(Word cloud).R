## 05.Corpus(Word cloud)

# 1.기본 package 설정
## 1.1 package 설치

# install.packages("wordcloud") # 워드 클라우드

## 1.2 library 로드
library(wordcloud)



# 5.Word cloud

## 5.1 Frequence Table 만들기

## 행렬로 전환
tdm_m <- as.matrix(tdm)
tdm_m

## 단어별 합계 구하기
tdm_sum <- sort(rowSums(tdm_m), 
                decreasing = TRUE) # 단어가 나타난 숫자별로 정렬
head(tdm_sum,10)

## 데이터 프레임으로 저장
tdm_df <- data.frame(word=names(tdm_sum),
                     freq=tdm_sum) # data.frame으로 변환
head(tdm_df,10)

## 필요시 테이블로 저장하기: 상대빈도(%)추가
#tdm_freq <- data.frame(word=names(tdm_sum),
#                       freq=tdm_sum,
#                       prop=prop.table(tdm_df$freq)) # data.frame으로 변환
#head(tdm_freq,10)
#write.csv(tdm_freq, "frequency.csv")


## 5.2 Word cloud
palete<-brewer.pal(6,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))

wordcloud(words=tdm_df$word, 
          freq=tdm_df$freq,
          scale=c(4,0.5), 
          min.freq=20, 
          max.words=200,
          random.order=FALSE, 
          colors=palete, 
          family="malgun")

