#### 계층적 군집화: Table 15.2


# 01.데이터 불러오기
utilities.df <- read.csv("Utilities.csv",
                         header=TRUE,
                         na.strings = ".")
str(utilities.df)
head(utilities.df)

# 정수와 숫자의 차이가 있는 것일까? num(값), int(갯수)
# length 함수 등에서 차이가 남

 # class(length(utilities.df$Fixed_charge)) # 개수가 나옴 / 이 함수는 출력물을 갯수로 인식 
 # class(utilities.df$Fixed_charge)


# 02.회사이름을 row 이름으로 변경
# 회사이름 제거
row.names(utilities.df) <- utilities.df[,1] # 열 이름 붙여주기 (1열의 이름을 제목으로 붙여라)
utilities.df <- utilities.df[,-1] # 첫번째 열을 제외하고 추출해서 다시 저장하라. 
head(utilities.df)








# 03.데이터 정규화
# sapply: 주어진 함수 연산을 특정 단위로 쉽게 할 수 있도록 지원하는 함수 군(반복함수 안해도 됨)
# 연산결과를 벡터형태로 출력
utilities.df.norm <- sapply(utilities.df, scale) # 데이터 전체를 정규화하여 저장 
 # sapply 행렬과 백터의 형식으로 결과값을 보여주는 함수, 결과값 자체를 다른 데이터로 저장하라는 의미 

utilities.df.norm1 <- scale(utilities.df)

head(utilities.df.norm)
head(utilities.df.norm1)

row.names(utilities.df.norm) <- row.names(utilities.df) # 회사이름을 row 이름으로 변경
head(utilities.df.norm)








# 04.유클리드 거리 측정: Euclidean distance
# 거리 측정 방법을 바꾸려면 method = 
# "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
d.norm <- dist(utilities.df.norm, method = "euclidean")
d.norm








# 05.계층적 군집화
# hclust() "single", "complete", "average", "median", "centroid", "ward.D"
# 단일연결법
hc.s <- hclust(d.norm, method = "single")
plot(hc.s, hang = -1, ann = FALSE) # hang =-1로 하면 최상위 부터 출력
# 평균연결법
hc.a <- hclust(d.norm, method = "average")
plot(hc.a, hang = -1, ann = FALSE)



# 06.군집수 결정: Table 15.6
memb.s <- cutree(hc.s, k = 3) # 단일거리법
memb.s
memb.a <- cutree(hc.a, k = 3) # 평균법
memb.a


# 기법에 따라 결과가 달라진다. 

# 덴드로그램으로 예쁘게 표현 

library("factoextra")
fviz_dend(hc.s, cex = 1, # 계층을 계산한 자료, 글자크기 
          rect= T, # 군집에 사각형 그리기
          k=6, # 군집수
          palette = "jco", # color
          data = utilities.df.norm) # 정규화(표준화 한 자료)





# 07.군집별 특성 파악: Figure 15.4
# 회사명과 군집번호 일치
row.names(utilities.df.norm) <- paste(memb.s, ": ", row.names(utilities.df), sep = "")
head(utilities.df.norm)
# heatmap으로 표현
# rev(): 값이 크면 dark로 역변환
heatmap(as.matrix(utilities.df.norm), 
        Colv = NA, 
        hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

