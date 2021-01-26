# 13장 WORD2VEC

setwd("C:/R")

library(tm)
library(slam)
library(dplyr)
library(NLP4kec)
library(readxl)

# corpus 만들기

# 1. 동의어 / 불용어 사전 불러오기
stopWordDic <- read.csv("./dictionary/stopword_ko.csv")
synonymDic <- read.csv("./dictionary/synonym.csv")


# 2. 형태소 분석 및 전처리
install.packages('readxl')
library(readxl)
textData <- read_excel("./data/patent.xlsx")
parsedData <- file_parser_r(path = "./data/patent.xlsx", language = "ko", useEn =T, korDicPath = "./dictionary/user_dictionary.txt")

# 3. 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], parsedData))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    parsedData[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedData[docNum])
  }
}

parsedData <- gsub(" ","  ",parsedData)
corp <- VCorpus(VectorSource(parsedData))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeWords, stopWordDic$stopword)
corp <- tm_map(corp, PlainTextDocument)

# word2vec로 연관 키워드 추출하기

#library(devtools)
#devtools::install_github("bmschmidt/wordVectors") # 설치시에 다른 패키지를 같이 설치할 것인지를 묻는 화면이 뜨면 그냥 엔터를 눌러서 무시하는 것이 좋다.
#install.packages("tsne")

library(wordVectors)
library(tsne) # 차원 축소 그래프 생성

targetData <- NULL
for(i in 1:length(corp)){
  temp = corp[[i]]$content
  targetData[i] = temp
}

#word2vec 학습 text 파일 만들기
write.table(targetData, file = "trainTxt.txt", row.names = FALSE, col.names = FALSE, quote = F)

#모델 학습
model <- train_word2vec(train_file = "trainTxt.txt"
                       , threads=4 #쿼드코어
                       , vectors=50 #50차원
                       , force = T #기존모델 덮어쓰기
                       , window = 6 #window size
                       , output_file = "trainTxt.bin")

# cbow=1이면 continuous-bag-of-words model을 사용함. min_count 는 최소등장빈도를 지정함. negative_samples는 skip-gram model 사용시에 학습시간 단축에 사용함. 큰 코퍼스의 경우 2-5, 작은 코퍼스의 경우 5-15가 적당하며 모든 데이터를 사용할 경우는 0임. 

model <- model[rownames(model)!="</s>",] # </s> 삭제하기
model <- model[nchar(rownames(model))>1,] # 한 글자 단어 삭제하기

# 생성된 word2vector model 확인
model
model[["사차산업혁명"]]

#연관 키워드 추출하기
closest_to(model,model[["사차산업혁명"]], 20)
closest_to(model,model[["정부"]], 20)
closest_to(model,model[["특허청"]], 20)

#단어간 벡터연산
model %>% closest_to(~"사차산업혁명" - "정부" + "특허청")


#전체 단어 관계 시각화
#install.packages("extrafont")
#library(extrafont) 
#par

plot(model)


# 단어간 거리 구하기

#Euclidean Distance
dist(model[(row.names(model)=="사차산업혁명" | row.names(model)=="정부"),])

#Cosine 거리
cosineDist(model[["사차산업혁명"]], model[["정부"]])

#Cosine 유사도 (= 1 - Cosine거리)
cosineSimilarity(model[["사차산업혁명"]], model[["정부"]])


# 연관네트워크 

library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

# 단어간 코사인 유사도 구하기
temp <- cosineSimilarity(model, model)
temp[1:10, 1:10]

# Edge 개수 조절하기 (0~1 사이 값으로 세팅)
temp[temp < 0.9] <- 0

sparseRatio <- colSums(temp == 0) / nrow(temp) # 0의 갯수 파악

# sparse ratio 값 분포 확인
quantile(sparseRatio, seq(0,1,0.1))
temp <- temp[sparseRatio<0.9997, sparseRatio<0.9997]

# Node 개수 조절하기 (0인 값 제외)
temp <- temp[,colSums(temp)!=0]
temp <- temp[rowSums(temp)!=0,]

# Network Map을 그리기 위한 객체 만들기
net <- network(temp, directed = FALSE)

# Network의 betweenness값을 구하여 상위 10% 이상인 node에는 노란색 
net %v% "mode" <- ifelse(betweenness(net) > quantile(betweenness(net), 0.9), "big", "small")
node_color <- c("small" = "grey", "big" = "gold")

# Network edge size 값 설정하기 (단어간 상관계수 값 * 0.8)
set.edge.value(net, "edgeSize", temp * 0.8)

# Network Map 화면에 그리기
ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = "degree" # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
) 

# 문서분류

library(wordVectors)
library(tm)

# 속성명 정하기
attKeyword <- c("사차산업혁명","정부","특허청")

# 단어간 코사인 유사도 행렬 구하기
simMat <- cosineSimilarity(model, model)

# 속성별 가중치 행렬 만들기
weightMat <- simMat[row.names(simMat) %in% attKeyword,] # 매칭되는것만 남김

# TDM 만들기 (TF-IDF기준)
tdmW <- TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)
                                             ,weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

# TDM의 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
rownames(tdmW) <- trimws(rownames(tdmW))
tdmW <- tdmW[nchar(rownames(tdmW)) > 1,]

# TDM의 sparse term 삭제하기
tdmW <- removeSparseTerms(tdmW, as.numeric(0.98))

# TDM을 행렬로 변환하기
tdmW_mat <- as.matrix(tdmW)

# 가중치 행렬의 단어와 TDM 단어 맞추기
weightMat <- weightMat[,colnames(weightMat) %in% rownames(tdmW_mat)] # 매칭되는것만 남김
weightMat <- weightMat[,sort(colnames(weightMat))] # 순서 매칭

# 단어 순서가 맞는지 확인
weightMat[,1:10]
tdmW_mat[1:10,1:10]

## TDM과 가중치 행렬의 내적(inner product) 구하기 - 각 문서에 속성 점수 만들기
attScore <- weightMat %*% tdmW_mat
attScore <- as.data.frame(attScore)
attScore <- t(attScore)

# 각 문서별 스코어 확인 하기
attScore[187,]
textData[187,"content"]

# 각 문서별 속성 스코어 붙이기
textData2 <- cbind(textData, attScore)

# 각 문서별로 MAX 스코어와 속성값 붙이기
maxAttName <- NULL
maxAttVal <- NULL
for(i in 1:nrow(attScore)){
  maxAttName[i] = names(which.max(attScore[i,]))
  maxAttVal[i] = max(attScore[i,])
}
textData2 <- cbind(textData2, maxAttName, maxAttVal)

# 전체 속성별 분류 개수 확인하기
tapply(textData2$id, textData2$maxAttName, length)

# 속성별 상위 점수 문장 확인하기
library(dplyr)
textData2 %>% filter(maxAttName == "사차산업혁명") %>% select(content, maxAttVal) %>% arrange(desc(maxAttVal))%>% head(7)

textData2 %>% filter(maxAttName == "정부") %>% select(content, maxAttVal) %>% arrange(desc(maxAttVal))%>% head(7)

textData2 %>% filter(maxAttName == "특허청") %>% select(content, maxAttVal) %>% arrange(desc(maxAttVal))%>% head(7)


