# 11장 토픽모형: LDA

setwd("C:/R")

# 1) 형태소 분석과 전처리
library(NLP4kec)
library(tm)

library(readxl)
forthR <- read_excel("raw_data/forthR.xlsx")

p_data <- r_parser_r(forthR$content, language = "ko", useEn =T, korDicPath = "./dictionary/user_dictionary.txt")

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

#install.packages("topicmodels")
#install.packages("ldatuning")
#install.packages("LDAvis")
#install.packages("servr")

library(topicmodels)
library(ldatuning)
library(LDAvis)
library(servr)
library(tm)
library(slam)
library(dplyr)

# sparse term을 제거하면서 특정 문서의 경우 모든 토픽의 빈도가 0인 경우가 발생하므로 해당 문서를 제거해야 LDA 실행이 가능함
new_dtm <- dtm[row_sums(dtm) > 0,] 

# 2) 토픽수 결정하기
# 토픽수 결정은 빅데이터가 아닌 경우에는 지금 상황에서는 분석자 본인이 결정하는 것이 더 맞는 것으로 보인다. 아래 패키지는 보조적 수단으로 활용할 것을 권장한다. 왜냐하면 본서에서 사용하고 있는 자료처럼 빅데이터가 아닌 작은 데이터의 경우에는 패키지에 의한 권장 토픽수가 매번 달라지기 때문이다. 본서에서 제시한 토픽수는 저자의 경우에 한정되고 다른 사용자는 다르게 나올 수 있다는 것을 미리 밝힌다. 이런 측면에서 텍스트 분석은 아직 가야할 길이 멀다.


#ldatuning 패키지 활용
optimal.topics <- FindTopicsNumber(new_dtm, topics = 2:20, metrics = "Griffiths2004", method = "Gibbs", control = list(), mc.cores = 4L) # metrics에 "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014" 등이 있으나 Griffiths2004를 권장함
optimal.topics
FindTopicsNumber_plot(optimal.topics)

# harmonic Mean 방법
harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}


k = 2
burnin = 1000
iter = 1000
keep = 50

fitted <- LDA(new_dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
harmonicMean(logLiks)

sequ <- seq(2, 30, 1)  # 토픽수를 2에서 30까지 설정함
fitted_many <- lapply(sequ, function(k) LDA(new_dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

plot(sequ, hm_many, type = "l") # 결과 그래프 
sequ[which.max(hm_many)] # 최적 토픽수 결정


# 3) LDA 실행
name <- "4차산업혁명대응"
k <- 13
lda_tm <- LDA(new_dtm, control=list(seed=1111), k)

lda_tm@gamma  # 문서ⅹ토픽 행렬
dim(lda_tm@gamma )

lda_tm@beta  # 토픽ⅹ단어 행렬
dim(lda_tm@beta)


#토픽ⅹ단어 행렬 출력
term_topic <- terms(lda_tm, 10)
term_topic


# 토픽ⅹ단어 그래픽 표현
library(tidytext)
library(ggplot2)

q_topics <- tidy(lda_tm, matrix="beta")

q_top_terms <- q_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

q_top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour = "black"))


#문서ⅹ토픽 행렬 확률값 계산
doc_topic <- topics(lda_tm, 1)
doc_topic_df <- as.data.frame(doc_topic)
doc_topic_df$rown <- as.numeric(row.names(doc_topic_df))
head(doc_topic_df,10)

doc_Prob = posterior(lda_tm)$topics
doc_Prob_df = as.data.frame(doc_Prob)
head(doc_Prob_df,10)

#최대 확률값 찾기
doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)

#문서별 토픽번호 및 확률값 추출하기
doc_Prob_df$rown <- doc_topic_df$rown
p_data <- as.data.frame(p_data)
p_data$rown <- as.numeric(row.names(p_data))
id_topic <- merge(doc_topic_df, doc_Prob_df, by="rown")
id_topic <- merge(id_topic, p_data, by="rown", all.y = TRUE)
id_topic <- subset(id_topic,select=c("rown","p_data","doc_topic","maxProb"))

#단어별 토픽 확률값 출력하기
posterior(lda_tm)$terms

# 4) LDA결과 시각화

# phi는 각 단어별 토픽에 포함될 확률값
phi <- posterior(lda_tm)$terms %>% as.matrix

# theta는 각 문서별 토픽에 포함될 확률값
theta <- posterior(lda_tm)$topics %>% as.matrix

# vocab는 전체 단어 리스트
vocab <- colnames(phi)

# 각 문서별 문서 길이
doc_length <- vector()
doc_topic_df <- as.data.frame(doc_topic)

for( i in as.numeric(row.names(doc_topic_df))){
  temp = corp[[i]]$content
  doc_length = c(doc_length, nchar(temp[1]))
}

# 각 단어별 빈도수
new_dtm_m <- as.matrix(new_dtm)
freq_matrix <- data.frame(ST = colnames(new_dtm_m),
                         Freq = colSums(new_dtm_m))

# 시각화하기
product_reviews <- list(phi = phi, 
                       theta = theta,
                       vocab = vocab,
                       doc.length = doc_length,
                       term.frequency = freq_matrix$Freq)

options(encoding = 'UTF-8') #한글로 결과 보기

json <- createJSON(phi = product_reviews$phi, 
                   theta = product_reviews$theta, 
                   doc.length = product_reviews$doc.length, 
                   vocab = product_reviews$vocab, 
                   term.frequency = product_reviews$term.frequency, encoding='UTF-8')

serVis(json, out.dir = 'vis', open.browser = TRUE) #사전에 vis 디렉토리를 만들지 않음


#remDr$close # 종료

# 11장 토픽모형: STM

setwd("C:/R")

# 1) 한글 형태소 분석 및 전처리
library(NLP4kec)
library(tm)

library(readxl)
forthR <- read_excel("raw_data/forthR.xlsx")
forthR$id <- NULL # id 제거
forthR <- forthR[!is.na(forthR$content),] # 무응답인 빈 행(row) 제거
forthR$id <- 1  # 새로운 ID 생성
forthR$id <- cumsum(forthR$id)

p_data <- r_parser_r(forthR$content, language = "ko", useEn =T, korDicPath = "./dictionary/user_dictionary.txt")
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

mydf <- data.frame(content = sapply(corp, as.character), stringsAsFactors = FALSE)
mydf$id <- 1  # ID 생성
mydf$id <- cumsum(mydf$id)

forthR<- forthR[,-c(3)]
data <- merge(forthR,mydf, c('id')) # 두 파일 병합
data <- data[,-c(1)] # id 제거


#2)	데이터 투입(Ingest)과 구조변환(Prepare)

#install.packages("stm")
library(stm)

myprocess <- textProcessor(data$content, metadata = data)
myprocess$documents[1]

out <- prepDocuments(myprocess$documents, myprocess$vocab, myprocess$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
docs
vocab
meta

#3) 모형선정 및 평가

#kresult <- searchK(out$documents, out$vocab, K = c(5:20), prevalence =~ year + age, data = out$meta)
#plot(kresult)


# 4)	추정

##(1) 관련 단어 보여주기  
stm.mod1 <- stm(documents = out$documents, vocab = out$vocab,K = 13, prevalence =~ year + age, max.em.its = 70, data = out$meta,init.type = "Spectral") # set to run for a maximum of 70 EM iterations
summary(stm.mod1)
labelTopics(stm.mod1)
plot.STM(stm.mod1, type="labels", n=9)


thoughts <- findThoughts(stm.mod1, texts=meta$content, n=2)
thoughts
#plot(thoughts, 2:3)
#plotQuote(thoughts$docs[[1]])

##(2) 효과 추정하기  
# Topic prevalence
out$meta$age <- as.factor(out$meta$age)
prep <- estimateEffect(1:13 ~ year + age, stm.mod1, meta = out$meta, uncertainty = "Global")
plot.STM(stm.mod1, type="summary", labeltype="frex", topics=c(1:13), n=10)


cloud(stm.mod1, topic=9)
plot(prep, "year", model=stm.mod1, method="continuous", topics = 9, xlab = "근무경력")
plot(prep, covariate = "age", topics = 9, model = stm.mod1, xlim = c(-.1, .1))
summary(prep, topics=9)

# Topic content
stm.mod2 <- stm(out$documents, out$vocab, K = 13,
                prevalence =~ year + age, content =~ age,
                max.em.its = 75, data = out$meta, init.type = "Spectral")
summary(stm.mod2)
plot(stm.mod2, type = "perspectives", topics = 9)
plot(stm.mod2, type = "perspectives", topics = c(9,10))

# 근무기간과 나이의 상호작용
Interaction <- stm(out$documents, out$vocab, K = 13,
                   prevalence =~ age * year, max.em.its = 75,
                   data = out$meta, init.type = "Spectral")

prep2 <- estimateEffect(c(9) ~ age * year, Interaction,
                        metadata = out$meta, uncertainty = "None")

plot(prep2, covariate = "year", model = Interaction,
     method = "continuous", xlab = "근무기간", moderator = "age",
     moderator.value = 0, linecol = "blue", ylim = c(0, 0.20), printlegend = F)

plot(prep2, covariate = "year", model = Interaction,
     method = "continuous", xlab = "근무기간", moderator = "age",
     moderator.value = 1, linecol = "red",  add = T,
     printlegend = F)

legend(7, 0.15, c("청년등", "장년층"), lwd = 2, col = c("blue", "red"))


##(3) 토픽들 간 상관관계

mod.out.corr <- topicCorr(stm.mod1)
plot(mod.out.corr)


