## 10.Logistic 회귀분석


# 1.기본 package 설정
## 1.1 package 설치

# install.packages("tm") # 텍스트마이닝 도구
# install.packages("SnowballC") # 불용어 처리

## 1.2 library 로드
library(tm)
library(SnowballC)

# 2.텍스트 가져오기(Corpus)

## 2.1 데이터가져오기
## 2.2 Corpus(말뭉치)로 저장: 코퍼스는 문서가 저장되어 있는 공간
corp <- ZipSource("AutoAndElectronics.zip", recursive = T)  %>%
  tm::Corpus()

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인


# 3 용어(tocken) 만들기 (tokenization)
corp <- tm_map(corp, content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english"))

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인

# 4. 불용어(stopwords) 처리

# 제거해 주고 싶은 단어가 있으면 아래의 removeWords를 이용해서 제거
st_word <- c(stopwords('en'),c("and", "will")) 
corp <- tm_map(corp, removeWords, st_word) 

# 5. 어근 동일화(stemming)
corp <- tm_map(corp, stemDocument)


# 6.LSA를 이용한 회귀분석

## 6.1 tdm(Term-Document-Matrix 만들기): 단어중심일때
## dtm(Document-Term-Matrix 만들기): 문서 중심일때 
tdm <- TermDocumentMatrix(corp)
tfidf <- weightTfIdf(tdm)
inspect(tdm)
inspect(tfidf)

# extract (20) concepts 
# install.packages("lsa")
library(lsa)
lsa_tfidf <- lsa(tfidf, dim = 5)
head(lsa_tfidf$dk)

words_df <- as.data.frame(as.matrix(lsa_tfidf$dk)) 
head(words_df,10)
str(words_df)

## 행렬로 전환
lsa_tfidf_m <- as.matrix(lsa_tfidf$dk)
lsa_tfidf_m

## 데이터 프레임으로 저장
lsa_tfidf_df <- as.data.frame(lsa_tfidf_m) # data.frame으로 변환
head(lsa_tfidf_m,10)

# create an array of records labels
label <- c(rep(1, 100), rep(0, 100))

lsa_tfidf_df = cbind(label, words_df)
head(lsa_tfidf_df)


## 6.2 훈련용, 검증용 데이터 분리
set.seed(2) # 시드 고정 
train_index <- sample(c(1:dim(lsa_tfidf_df)[1]), 
                      dim(lsa_tfidf_df)[1]*0.6)  
train_df <- lsa_tfidf_df[train_index, ]
valid_df <- lsa_tfidf_df[-train_index, ]
head(train_df)

# 6.3 로지스틱 회귀분석 실행
# glm() : family = "binomial"
logit.reg <- glm(label ~ ., 
                 data = train_df, 
                 family = "binomial") 
options(scipen=10) # 소숫점 아래 확인 
summary(logit.reg)

# Odds 계산
odds <- data.frame(summary(logit.reg)$coefficients, 
                   odds = exp(coef(logit.reg))) 

round(odds, 5)

## 6.4 모델성능평가 (ConfusionMatrx)
## install.packages("caret")
library(caret)
pred <- predict(logit.reg, valid_df)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), 
                as.factor(valid_df$label))

