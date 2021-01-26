## 12.Topic분석_IDM(Intertopic Distance Map)

# A topic model for movie reviews
# 참고사이트: http://textmining.kr/?p=432

# install.packages('tm')
# install.packages('stringr')
# install.packages('topicmodels')
# install.packages('lda')
# install.packages('LDAvis')
# install.packages('servr')
# install.packages('LDAvisData')

# install.packages("devtools")
# devtools::install_github("cpsievert/LDAvisData")

library(tm)
library(stringr)
library(topicmodels)
library(lda)
library(LDAvis)
library(servr)
library(LDAvisData)


# 7. LDA모형(토픽분석)

## 7.1 LDA분석용 포멧 만들기
## tidy text -> list로 변환

doc <- text_tb %>%
  select(word) %>%
  as.character() %>%
  unlist(text_tb)
head(doc)

## 토큰화
doc.list <- strsplit(doc, "[[:space:]]+")
head(doc.list)

## the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

## 단어명 저장
vocab <- names(term.table)
head(vocab)
class(vocab)

## LDA용으로 저장 
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)


# Compute some statistics related to the data set:
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus

## 7.2 LDA모형 분석
## 파라미터 설정

K <- 5
G <- 100
alpha <- 0.02
eta <- 0.02

# 모델 생성
set.seed(1234)

lda_model <- lda.collapsed.gibbs.sampler(documents = documents,
                                          K = K, 
                                          vocab = vocab, 
                                          num.iterations = G, 
                                          alpha = alpha, 
                                          eta = eta, 
                                          initial = NULL, 
                                          burnin = 0,
                                          compute.log.likelihood = TRUE)

theta <- t(apply(lda_model$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(lda_model$topics) + eta, 2, function(x) x/sum(x)))

# 모델 생성
idm_model <- list(phi = phi,
                  theta = theta,
                  doc.length = doc.length,
                  vocab = vocab,
                  term.frequency = term.frequency)

#한글로 결과 보기
options(encoding = 'UTF-8') 

## 7.3 시각화(IDM: Intertopic Distance Map)
## 시각화 변수 세팅

json <- createJSON(phi = idm_model$phi, 
                   theta = idm_model$theta, 
                   doc.length = idm_model$doc.length, 
                   vocab = idm_model$vocab, 
                   term.frequency = idm_model$term.frequency, encoding='UTF-8')

## 시각화 프로그램
serVis(json, out.dir = 'vis', open.browser = TRUE)


## 7.4 테이블 만들기
dim(lda_model$topics)  #5개 토픽에 1770개의 단어 출현   
top.topic.words(lda_model$topics) #5개 각 토픽 별 상위 20개 단어
lda_topics <- data.frame(top.topic.words(lda_model$topics))
head(lda_topics, 10)

