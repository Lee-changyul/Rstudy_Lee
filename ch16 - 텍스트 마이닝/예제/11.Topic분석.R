## 08.Topic분석

## "R을 이용한 텍스트 마이닝,백영민,한울" 사용

# 7. LDA모형(토픽분석)
# install.packages("topicmodels") # 토픽분석을 위해 추가 설치
library(topicmodels)

## 7.2 LDA
lda_dtm <- LDA(dtm, control=list(seed=1234), k=5)


####### 에러발생 #########
# Error in LDA(dtm, control = list(seed = 1234), k = 5) : 
#  Each row of the input matrix needs to contain at least one non-zero entry
###############

## 에러 해결
# ui = unique(dtm$i)
# dtm = dtm[ui,]


dim(lda_dtm@gamma)  # 문서X토픽 행렬의 구조
head(lda_dtm@gamma) # 문서X토픽 행렬 내용
dim(lda_dtm@beta)   # 토픽X단어 행렬 
head(lda_dtm@beta)  # 토픽X단어 행렬 내용


## 7.3 5개의 잠재토픽으로 분류된 상위12개 단어구조  
terms(lda_dtm,12)


## 7.4 특정 문서가 특정 토픽을 반영할 확률
lda_posterior <- posterior(lda_dtm)
lda_topics <- data.frame(round(lda_posterior$topics,3))
head(lda_topics, 10)

# 문서별 토픽 등장 확률의 총합
apply(lda_topics,2,sum)

