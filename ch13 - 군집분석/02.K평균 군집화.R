# 02.K-평균 군집화: Table 15.9

# 01.데이터 불러오기 ####
utilities.df <- read.csv("Utilities.csv",
                         header=TRUE,
                         na.strings = ".")
str(utilities.df)
head(utilities.df)







# 02.회사이름을 row 이름으로 변경 ####
# 회사이름 제거
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]
head(utilities.df)






 
# 03.데이터 정규화 ####
# sapply: 주어진 함수 연산을 특정 단위로 쉽게 할 수 있도록 지원하는 함수 군
# 연산결과를 벡터형태로 출력
utilities.df.norm <- sapply(utilities.df, scale) # 데이터 전체를 정규화
row.names(utilities.df.norm) <- row.names(utilities.df) # 회사이름을 row 이름으로 변경
head(utilities.df.norm)

str(utilities.df.norm)








# 04.K-평균 군집화 ####
# 초기 시드 설정(기준점을 적어주는 것, 랜덤결정), 군집수 6개
set.seed(2) # 결과를 똑같이 보기 위한 것, 국제 표준 난수표의 번호를 지정 

?kmeans

km <- kmeans(utilities.df.norm, 6)
km 
km$centers
km$cluster

# 다양한 옵션을 넣어서 하는 방법 

utilities.df.norm <- sapply(utilities.df, scale) # 정제된 data를 표준화
 # scale(utilities.df)와 동일한 결과 / 8번 항목 먼저 확인하고 와도 된다. 
 
km1 <- kmeans(utilities.df.norm, 4, nstart = 25) # 4는 클러스터 수, nstart 는 함수 권장 옵션
print(km1)




# 05.군집별 특성 파악: Figure 15.5 ####

# 화면틀 생성
plot(c(0), 
     xaxt = 'n', 
     ylab = "", 
     type = "l", 
     ylim = c(min(km$centers), 
              max(km$centers)), 
     xlim = c(0, 8))

# x축 라벨
axis(1, at = c(1:8), labels = names(utilities.df))

# centroids(중심값)출력
for (i in c(1:6)) # 1번부터 6번까지 반복 
  lines(km$centers[i,], 
        lty = i, # line type
        lwd = 4, # line 굵기 
        col = ifelse(i %in% c(1, 3, 5),
                     "black", "dark grey"))

# 클러스터 이름 넣기
text(x = 0.5, 
     y = km$centers[, 1], 
     labels = paste("Cluster", c(1:6)))


# 다른 모양의 그림 그리기

library("factoextra")

fviz_cluster(km1, data = utilities.df.norm)


 
 # 06.군집별 거리 측정: Table 15.11 ####
dist(km$centers)








# 07.최적 군집수를 찾는 엘보우 챠트(Elbow Method) ####
# 군집 6개일때의 에러값
km$withinss
km$tot.withinss

# 엘보우 차트 계산: k = 2 to k = 6.
set.seed(2)
k.max <- 10
elow.m <- sapply(1:k.max, 
              function(k){kmeans(utilities.df.norm, 
                                 k, 
                                 nstart=50,
                                 iter.max = 15 )$tot.withinss})
elow.m # 숫자가 줄어들면 에러가 줄어 든다. 

plot(1:k.max, 
     elow.m,
     type="b", 
     pch = 19, 
     frame = FALSE, 
     xlab="군집 수",
     ylab="군집내 거리 제곱합")

# 08. 군집수의 결정 : 타당성 측정 방법 ####
# 인문사회과학을 위한 빅데이터 분석방법론 중에서
 # 이 방법은 표준화 할 때 방식이 달라진다. 
 

# install.packages("factoextra")
# install.packages("cluster")
# install.packages("NbClust")

library(factoextra)
library(cluster)
library(NbClust)

res.nbclust <- NbClust(utilities.df.norm, 
                       distance = "euclidean", 
                       method = "complete",
                       min.n = 2, max.nc = 8) # 예제에서 complete 사용

fviz_nbclust(res.nbclust)

res <- get_clust_tendency(utilities.df.norm, nrow(utilities.df.norm)-1) # 의미가 무엇일까? 군집이 될 수 있는가? 의미가 있는가?
res$hopkins_stat 
