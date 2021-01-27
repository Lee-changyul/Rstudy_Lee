# 텍스트분석과 자연어처리 ####

 
# 여러분에 작업폴더에 dictionary라는 하위폴더를 생성하시고 그 안에 stopword.csv, synoym.csv, user_dictionary 파일을 복사해 놓으세요. 마찬가지로 raw_data라는 하위폴더를 만드시고 그 안에 데이터 파일을 복사해 놓으세요. 

# rjava 설치
# install.packages("rJava") 
library(rJava)

#2. KNLP4kec 설치 및 필요 패키지 불러오기
# https://github.com/NamyounKim/NLP4kec 에서 여러가지 경고 메시지를 무시하고 windows 용 파일을 다운받아 여러분 작업디렉토리에 옮겨놓으시기 바람
# ininstall.packages("NLP4kec_1.4.0.zip", repos=NULL))  # rstudio에서도 설치됨
library(NLP4kec)
library(tm)
library(tidyverse)
library(tidytext)

# 0. 사용데이터 정렬 ####

texts <- read.csv(".//raw_data//finaldata.csv", header = T)

dimnames(texts)

names(texts)[names(texts)=="키워드"]="content"

names(texts)

# 1. 형태소 분석 및 전처리 : NLP4kec 활용 (패키지 활용법은 생략)

parsed <- texts$content

parsed[1:10]

# 말뭉치 생성 : tm::VCorpus()
# 다양한 소스로부터 읽어들인 텍스트를 코퍼스 형식으로 변환해주는 함수
# 텍스트가 저장되어 있는 소스에 대응되어 있는 함수를 사용해야 함
# 소스별 대응 함수는 getSources()를 이용해서 확인
# getSources()
# [1] "DataframeSource" 
# [2] "DirSource"  
# [3] "URISource"      
# [4] "VectorSource"   
# [5] "XMLSource"      
# [6] "ZipSource"

#Corpus 생성

corp <- VCorpus(VectorSource(parsed)) # 아래 참고) 참조

print(corp)

str(object = corp[[1]])


# 데이터 전처리 중 동의어 / 불용어 사전 불러오기

stopWordDic <- read.csv("./dictionary/stopword_ko.csv") 

# 동의어 처리방법 

# 이방식 실패 

# 코퍼스 처리를 위한 tm 패키지 로딩 후 이어서 데이터 전처리 완료 

library(tm)

corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopWordDic$stopword)
corp <- tm_map(x = corp, FUN = stripWhitespace)

#텍스트문서 형식으로 변환
corp <- tm_map(corp, PlainTextDocument)

inspect(corp[[1]])

# 3. DTM 생성 및 Sparse Term 삭제 --

#Document Term Matrix 생성 (단어 Length는 2로 세팅)

dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기

colnames(x = dtm) <- trimws(x = colnames(x = dtm), which = 'both')
dtm <- dtm[,nchar(colnames(dtm)) > 1]

# 차원을 확인합니다.
dim(x = dtm) # 2270개 행(문서)에서 5798 개 열(단어)로 구성


#참고) Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))

dtm$dimnames$Terms[1:100]

# dtm에 언급된 단어(term)별 빈도수를 생성합니다.
wordsFreq <- dtm %>% as.matrix() %>% colSums()

# 사용된 단어의 총 개수를 확인합니다.
length(x = wordsFreq)

wordsFreq <- wordsFreq[order(wordsFreq, decreasing = TRUE)]

# 데이터 프레임으로 전환한 후 엑셀로 데이터 저장 

library(xlsx)

wordDf <- data.frame(
  word = names(x = wordsFreq),
  freq = wordsFreq,
  row.names = NULL) %>% 
  arrange(desc(x = freq))

write.xlsx(wordDf,                # R데이터명
           file=".//raw_data//wordDF.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new3", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=TRUE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저









# ==========================

#Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어든다.) 차원을 축소하는 것. 
dtm <- removeSparseTerms(dtm, as.numeric(0.99)) 
dtm

dim(x = dtm) # 2270개 행에서398 열(단어)로 차원이 축소됨 


rowSums(x = dtm %>% as.matrix()) %>% table() #행의 합이 0인 건수를 확인(의미를 찾아봐야 함)

# 문서 이름(row name)을 지정합니다. 나중에 생성할 parsedDf와 병합하기 위함입니다.
dtm$dimnames$Docs <- generateIDs(obj = dtm$dimnames$Docs, index = 'doc')

# dtm 객체를 육안으로 확인합니다. 
dtm$dimnames$Docs[1:40]
dtm$dimnames$Terms[1:40]

# 단어 간 가중치를 부여한 DTM 생성 (TF-IDF)

# 문서단어행렬의 원소가 TF-IDF인 dtmTfIdf 객체를 생성합니다.
dtmTfIdf <- DocumentTermMatrix(
  x = corp,
  control = list(
    removeNumbers = TRUE,
    wordLengths = c(2, Inf),
    weighting = function(x) weightTfIdf(x, normalize = TRUE) ))

# 단어(=컬럼명) 양옆의 공백을 제거합니다.
colnames(x = dtmTfIdf) <- trimws(x = colnames(x = dtmTfIdf))

# 차원을 확인합니다.
dim(x = dtmTfIdf) 

# 2270개 행(문서)에서 5626개 열(단어)로 구성, 만약 원 데이터에서 숫자를 삭제하지 않았다면
# 여기 옵션에서는 숫자가 삭제되었기 때문에 단어수가 달라질 수 있음 

# 네트워크의 크기를 줄이기 위해 sparse가 큰 컬럼을 제거합니다.
# 역시 이 과정을 통해 DTM의 단어 개수가 크게 줄었습니다.
dtmTfIdf <- removeSparseTerms(x =  dtmTfIdf, sparse = as.numeric(x = 0.99))

# 차원을 확인합니다.
dim(x = dtmTfIdf)

rowSums(x = dtmTfIdf %>% as.matrix() %>% round(digits = 1L)) %>% table()

# 문서 이름(row name)을 지정합니다. 나중에 생성할 parsedDf와 병합하기 위함입니다.
dtmTfIdf$dimnames$Docs <- generateIDs(obj = dtmTfIdf$dimnames$Docs, index = 'doc')


# 3. 단어시각화 

# dtm에 언급된 단어(term)별 빈도수를 생성합니다.
wordsFreq <- dtm %>% as.matrix() %>% colSums()

# 사용된 단어의 총 개수를 확인합니다.
length(x = wordsFreq)

wordsFreq <- wordsFreq[order(wordsFreq, decreasing = TRUE)]

# 단어 빈도를 막대그래프로 그리기 위해 데이터 프레임으로 변환한 다음 
# 내림차순으로 정렬합니다.
wordDf <- data.frame(
  word = names(x = wordsFreq),
  freq = wordsFreq,
  row.names = NULL) %>% 
  arrange(desc(x = freq))

# 건수를 확인합니다.
nrow(x = wordDf)
## [1] 101






#DTM, TDM을 데이터 프레임 형식으로 저장하기
dtm_df <- as.data.frame(as.matrix(dtm))
tdm_df <- as.data.frame(as.matrix(tdm))




# 5. 단어 빈도 정보로 시각화 하기 

#단어 빈도 정보 Data Set 만들기


# ggplot() 함수에 적용할 나만의 테마(mytheme)를 설정합니다.
mytheme <- theme(
  plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
  axis.title.x = element_text(color = 'blue', size = 12, face = 'bold'),
  axis.title.y = element_text(color = '#993333', size = 12, face = 'bold'),
  axis.text.x = element_text(family = 'NanumGothic', size = 10, face = 'bold'),
  axis.text.y = element_blank(), 
  axis.ticks.length = unit(0, 'cm'),
  panel.background = element_blank(),
  panel.grid = element_blank() )


ggplot(
  data = head(x = wordDf, n = 20L), 
  mapping = aes(
    x = reorder(word, -freq), 
    y = freq)) + 
  geom_bar(
    stat = 'identity', 
    fill = c(rep(x = 'gray30', times = 5), rep(x = 'gray80', times = 15))) +
  geom_text(
    mapping = aes(label = freq), 
    size = 4, 
    vjust = -0.5) + 
  labs(
    x = '고빈도 단어', 
    y = '빈도수', 
    title = '고빈도 단어 현황_전체') + 
  mytheme 


# Word Cloud 그리기
# install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = wordDf
           , color = "random-dark" #random-light
           , shape = "circle"  # circle, cardioid, diamond, triangle-forward
           , size = 0.5
           , fontFamily = "나눔고딕")

#treeMap 그리기
install.packages("treemap")
library(treemap)
treemap(wordDf # 대상 데이터 설정
        ,title = "Word Tree Map"
        ,index = c("word") # 박스 안에 들어갈 변수 설정
        ,vSize = "freq"  # 박스 크기 기준
        ,fontsize.labels = 12 # 폰트 크기 설정
        #,palette = pal # 위에서 만든 팔레트 정보 입력
        ,border.col = "white") # 경계선 색깔 설정


# 6. 단어 연관성 분석

# dtmTfIdf 객체를 행렬로 변환한 다음 상관(계수) 행렬을 구함. 
# 그리고 나서 특정 단어가 속한 컬럼을 뽑아 행렬 원소를 내림차순으로 정렬하면 끝납니다. 
# 고빈도 단어 중 명사 위주로 관심 있는 몇 가지에 대해 상위 10개씩 확인해본 것입니다.

# 상관계수 행렬을 직접 생성합니다.
corTerms <- dtmTfIdf %>% as.matrix() %>% cor()

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
checkCorTerms(n = 20, keyword = '지급')  


# 7. 네트워크 맵 그리기

# 추가적인 함수에 대한 이해가 필요 
