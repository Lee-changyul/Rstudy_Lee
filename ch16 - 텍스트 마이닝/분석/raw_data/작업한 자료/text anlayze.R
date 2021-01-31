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

# 데이터 불러오기 

texts <- read.csv(".//raw_data//finaldata.csv", header = T)

head(texts)

text.df <- tibble(texts)

text.df

unnest_tokens(tbl=, output=, input=)
# tbl=데이터프레임 (토큰단위로 분해해서 한행에 한개씩 저장)
# output= 분해한 토큰을 저장할 새로운 열이름
# input= 분해할 텍스트가 저장된 열이름
text.df

text.df <- unnest_tokens(text.df, word, 키워드)

text.df$word

# 33개 행 = 전체 단어 개수




# NA(결측값)이 포함된 데이터 제거하기 

texts <- texts[complete.cases(texts), ]
nrow(x = texts)

# 중복된 행이 있을 경우 제거

texts <- unique(x = texts)
nrow(x = texts)

# 데이터 원본과 나중에 생성할 DTM을 서로 결합할 때 기준변수로 사용할 id 컬럼을 생성합니다.

# 객체에 id를 추가하는 함수를 생성합니다.

generateIDs <- function(obj, index = 'id') {
  
  # 객체의 종류에 따라 길이를 계산합니다. 
  if (obj %>% class() == 'data.frame') {
    n <- nrow(x = obj)
  } else {
    n <- length(x = obj)
  }
  
  # id를 생성합니다. 
  id <- str_c(
    index, 
    str_pad(
      string = 1:n, 
      width = ceiling(x = log10(x = n)), 
      side = 'left', 
      pad = '0') )
  
  # 결과를 반환합니다. 
  return(id)
}

# texts 객체에 id 컬럼을 추가합니다. doc 대신에 다른 이름을 써도 괜찮다. 
texts$id <- generateIDs(obj = texts, index = 'doc')

head(texts)


# 여기서 두가지 방식으로 데이터 전처리를 수행할 수 있다. 

# 1) 문장을 다 합친 후에 재정리하는 방법
# 2) 어느 정도 정돈된 데이터를 입력하고 이를 토대로 정리하는 방법 

# => 아래에서는 먼저 기존 어느 정도 정돈된 데이터가 있으므로 두번째 방법을 사용해 본다. 


# 다음에 논의할 내용은 

# 1) 모든 데이터를 다 쓸 것인가?
# 2) 일부 데이터를 걸러낼 것인가? 길이에 따라 가중치가 결정되므로 골라 내는 것도 방법 
  # => 이 경우 텍스트 길이 확인하는 법

# 열이름 확인

dimnames(texts)

# 쉽게 바꿀 수 있는 팁은?

names(texts)[names(texts)=="일자"]="date"
names(texts)[names(texts)=="인용문"]="cites"
names(texts)[names(texts)=="키워드"]="content"

names(texts)

# 각 행(컬럼) 글자수 확인

textRange <- texts$content %>% nchar() %>% range()
print(x = textRange)



# 1. 형태소 분석 및 전처리 : NLP4kec 활용 


# texts = file_parser_r(path = "./raw_data/speech.xlsx", language = "ko", useEn = F, korDicPath = "./dictionary/user_dictionary.txt")
# 이번 데이터는 기본적으로 excel 에서 기초 전처리 작업을 하고 왔기 때문에 이 과정 생략

parsed <- texts$content

texts$content[1:10]

# 글자수 확인 

parsed %>% nchar() %>% table()

# 중복되는 것은 없는지 확인하고 중복 값 제거 (내용이 같은 것을 제거해 주는 것인지 추후 검증 필요)

duplicated(x = parsed) %>% sum()

parsed <- unique(x = parsed)

length(parsed)


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

# 첫 번째 결과를 출력하여 확인합니다. 

str(object = corp[[1]])


# 사전 만들어서 내용 합치기 : N-gram 활용 (여기서는 원리만 확인하고, 동의어 사전으로 대체하여 편성)

# 필요한 패키지를 불러옵니다. 
library(RWeka)


# 인접한 2개의 단어를 결합한 bigram을 생성합니다. 
# min과 max에 할당할 숫자를 바꾸면 원하는 N-gram을 만들 수 있습니다. 
bigram <- function(x) {
  NGramTokenizer(x = x, control = Weka_control(min = 2, max = 4))
}

# 단어문서행렬(Term-Document matrix)을 생성합니다. 

bigramList <- corp %>% 
  TermDocumentMatrix(control = list(tokenize = bigram)) %>% 
  apply(MARGIN = 1, FUN = sum) %>% 
  sort(decreasing = TRUE)

# bigram의 길이를 확인합니다. 

length(bigramList)

# 문서 개수의 1% 이상 발생하는 bigram만 남깁니다. 

# 빈도수가 작은 것은 굳이 관심을 가지지 않아도 됩니다. 

bigramList <- bigramList[bigramList >= (nrow(x = texts) * 0.01)]
length(x = bigramList)


# bigram의 컬럼명(글자)만 따로 추출하여 bigramNames에 할당합니다. 
bigramNames <- names(bigramList)

# bigramNames을 육안으로 확인하기 위해 최대 100개까지 출력합니다. 
top <- if (length(x = bigramNames) >= 100) bigramNames[1:100] else bigramNames
print(top)

# 텍스트로 저장하고 텍스트에서 편집.

write.table(
  x = top, 
  quote = FALSE, 
  file = './dictionary/spacing.txt', 
  row.names = FALSE, 
  col.names = FALSE)

# 데이터 전처리 중 동의어 / 불용어 사전 불러오기

stopWordDic <- read.csv("./dictionary/stopword_ko.csv") #.는 여러분의 작업디렉토리를 의미함
synonymDic <- read.csv("./dictionary/synonym.csv")


# 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], texts$content))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    texts$content[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], texts$content[docNum])
  }
}



## 단어간 스페이스를 하나 더 추가하는 것으로 NLP4kec 패키지의 알고리즘상 필요한 과정으로 특별한 의미는 없다. gsub()함수의 사용 예를 보면 gsub("한국어", "한글", data)이란 코딩은 data 파일에서 한국어를 한글로 교체하라는 의미이다.
texts <- gsub(" ","  ",texts)


## 여기까지 기본 전처리 방법 1 습득

# 코퍼스 처리를 위한 tm 패키지 로딩 후 이어서 데이터 전처리 완료 

#install.packages("tm") #텍스트 마이닝을 위한 패키지
library(tm)


#특수문자 제거: 예를 들어 @, #, $ 등의 문자를 제거함
corp <- tm_map(corp, removePunctuation)

#숫자 삭제
corp <- tm_map(corp, removeNumbers)

#소문자로 변경: 영문의 경우 대문자를 소문자로 변경함: 영문을 사용하지 않기 때문에 해당사항 없음
corp <- tm_map(corp, tolower)

#특정 단어 삭제: 불용어 사전을 활용하여 해당 문자를 데이터에서 제거함
corp <- tm_map(corp, removeWords, stopWordDic$stopword)

# whitespace를 제거합니다.
corp <- tm_map(x = corp, FUN = stripWhitespace)


#텍스트문서 형식으로 변환
corp <- tm_map(corp, PlainTextDocument)

inspect(corp[[1]])

# 문서번호를 새로운 컬럼에 만든 후 데이터 프레임으로 저장합니다.
parsedDf <- data.frame(
  id = generateIDs(obj = parsed, index = 'doc'),
  parsedContent = parsed, 
  corpusContent = sapply(X = corp, FUN = `[[`, 'content'))

# 3. DTM 생성 및 Sparse Term 삭제 --

#Document Term Matrix 생성 (단어 Length는 2로 세팅)

dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기

colnames(x = dtm) <- trimws(x = colnames(x = dtm), which = 'both')
dtm <- dtm[,nchar(colnames(dtm)) > 1]

# 차원을 확인합니다.
dim(x = dtm) # 2270개 행(문서)에서 5626 개 열(단어)로 구성


#참고) Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))

dtm$dimnames$Docs <- generateIDs(obj = dtm$dimnames$Docs, index = 'doc')

dtm$dimnames$Docs[1:40]
dtm$dimnames$Terms[1:40]




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


# 만든 단어를 엑셀로 저장해 봅니다. 

# install.packages("xlsx")

library(xlsx)

write.xlsx(wordDf,                # R데이터명
           file=".//raw_data//wordDF.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new2", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
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
