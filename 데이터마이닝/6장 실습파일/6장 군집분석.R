#6 장 군집분석 1

## 군집분석 기초

install.packages("factoextra")
install.packages("cluster")

library("cluster")
library("factoextra")

# 분할적 군집분석 사례: 폭력범죄

# 1. 올바른 속성 또는 변수 선정
setwd("D:/magic/Bigdata/clustering")
arrest <- read.csv("arrest.csv")
head(arrest, n = 3)
View(arrest)

# 2.자료의 전처리
str(arrest)
summary(arrest)
colSums(is.na(arrest))
#arrest <- na.omit(arrest) # 결측치 발생할 경우

rownames(arrest) <- arrest[,1] # 첫번째 열에 rownames 특성을 부여함
arrest[,1] <- NULL  # 열을 삭제함, 즉 column 0:rownames 로 만듦
s_arrest <- scale(arrest)
View(s_arrest)


#4. 군집수 결정
library(NbClust)
res.nbclust <- NbClust(s_arrest, distance = "euclidean", method = "complete")

library(factoextra)
fviz_nbclust(res.nbclust)

#5. 분석 및 시각화
set.seed(123) # 난수생성(random number generator)을 함으로써 반복 계산하더라도 동일결과물 생성유도
km.res <- kmeans(s_arrest, 2, nstart = 25) #nstart는 처음 시작하는 분할 또는 센터의 숫자로서, 보다 안정적인 결과를 유도하기 위해 25개의 분할(센터)로 시작함
print(km.res)

# 시각화
library("factoextra")
fviz_cluster(km.res, data = s_arrest)


#6. 군집경향 평가하기
library(factoextra)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(s_arrest, n = nrow(s_arrest)-1)
res$hopkins_stat


# 계층적 군집분석 사례: 단백질 음식 소비

#1. 자료불러오기
food <- read.csv("food.csv")
head(food, n = 3)

# 2.자료의 전처리
str(food)
summary(food)
colSums(is.na(food))

rownames(food) <- food[,1] # 첫번째 열에 rownames 특성을 부여함
food[,1] <- NULL  # 열을 삭제함, 즉 column 0:rownames 로 만듦
s_food <- scale(food)

#3. 거리계산
d <- dist(s_food, method="euclidean")

#4. 군집수 결정
library(NbClust)
res.hc <- NbClust(s_food, distance = "euclidean", method = "complete")

library(factoextra)
fviz_nbclust(res.hc)


#5. 분석 및 시각화
fit <- hclust(d, method="average")
print(fit)

# 시각화
library("factoextra")
fviz_dend(fit, cex = 0.5, #글자크기
          rect = T, #군집에 사각형 그리기
          k=2, #군집 수  
          palette = "jco", #color
          data = s_food)


# 6. 군집경향 평가하기
library(factoextra)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(s_food, n = nrow(s_food)-1)
res$hopkins_stat

fviz_silhouette(res.hc)


##4절 시장세분화

setwd("C:/R")

# 엑셀 파일 가져오기: Rstudiodml Import Dataset 기능 활용
library(readxl)
online <- read_excel("Online Retail.xlsx")
data <- online # 작업을 하다보면 별도로 데이터를 만들어놓는것이 반복 작업에 편리하다. 


# 결측치 제거
colSums(is.na(data))
data <- na.omit(data)

# 기간 제한
data <- subset(data, InvoiceDate >= "2010-12-09")
range(data$InvoiceDate)

# 고객 국적 제한
table(data$Country)
data <- subset(data, Country == "United Kingdom")
str(data)

# 반품 확인
table(data$InvoiceNo)
data$item.return <- grepl("C", data$InvoiceNo, fixed=TRUE) #반품된 InvoiceNo는 앞에 "c"가 붙어있으므로 c가 붙어있는 InvoiceNO를 찾아내서 item.return 변수 생성함. 이때 c가 포함되면 TRUE로 표기한다. 

table(data$item.return) # 반품건수는 7358건이다. 
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1) # TRUE이면 0 fALSE이면 1
table(data$purchase.invoice)


# 새로운 데이터셋 생성
customers <- as.data.frame(unique(data$CustomerID)) # 중복된 CustomerID 제거하고 새로운 데이터 생성
str(customers)
names(customers) <- "CustomerID"
str(customers)

###########################
## 최근구매후 기간 변수 생성: 오늘 날짜에서 마지막 구입한 날짜를 빼면 recency 변수 생성 
###########################
range(data$InvoiceDate)
data$recency <- as.Date("2011-12-10") - as.Date(data$InvoiceDate)
temp <- subset(data, purchase.invoice == 1) #반품 안한 구매만 채택
View(temp)

# 고객별로 최근 구매후 최소 recency를 구함
recency <- aggregate(recency ~ CustomerID, data=temp, min, na.rm=TRUE)  #min=minimum
View(recency)
remove(temp)  # 필요한 recency를 구하였으니 제거함

# 객체 합하기: 앞에서 만든 customer와 recency를 CustomerId 변수를 중심으로 병합
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE) 
remove(recency)
customers$recency <- as.numeric(customers$recency)
str(customers)

#############
# 구매 빈도 변수 생성
#############
# invoices 데이터 만들기
customer.invoices <- subset(data, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ] # 중복제거
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),] #CustomerID순서대로 정렬
row.names(customer.invoices) <- NULL
View(customer.invoices)

# 고객별 인보이스 만들기
annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE) #고객별로 인보이스 계산하기
str(annual.invoices)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency" # 변수이름 바꾸기
str(annual.invoices) 

# 기존 customers 데0이터에 고객별 인보이스변수 합하기
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE) 
remove(customer.invoices, annual.invoices)

table(customers$frequency)
customers <- subset(customers, frequency > 0) # 구매빈도 0인 고객 제거하기


###############################
# 고객들의 지출액 변수 생성
###############################

# 개별 구매물건 지출 금액 계산하기
data$Amount <- data$Quantity * data$UnitPrice

# 고객별로 전체 구매물건의 총지출금액 계산하기
annual.sales <- aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary" #변수 이름 바꾸기

# 기존 custormer 데이터에 총지출금액변수 합하기
customers <- merge(customers, annual.sales, by="CustomerID", all.x = TRUE, sort=TRUE)
remove(annual.sales)
str(customers)

# 반품으로 인해 발생한 음의 총지출금액 발생 고객 
range(customers$monetary)
customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) # 음의 고객은 지출 0 으로 변환
range(customers$monetary)

##############
# 2: 8 법칙
##############

customers <- customers[order(-customers$monetary),] # monetary변수를 내림차순으로 정리
head(customers, 5)

# 80/20의 파레토 패턴 적용하기
pareto.cutoff <- 0.8 * sum(customers$monetary)  # monetary의 80분위가 5233903임
pareto.cutoff

customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%") # 80분위보다 같거나 작지 않다면 top 20%, 나머지는 하위 80%임
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE) # pareto 변수를 팩터로 변환함
head(customers, 5)

levels(customers$pareto)

table(customers$pareto)
prop.table(table(customers$pareto)) # 각 집단의 비율
round(prop.table(table(customers$pareto)), 2) # 소수점 둘째자리까지 비율 표시

remove(pareto.cutoff)

customers <- customers[order(customers$CustomerID),] #다시 CustomerID 순서대로 정렬
str(customers)
colSums(is.na(customers))
View(customers)

# 표본수 줄이기
customers_o <- customers
str(customers_o)

customers <- customers[sample(nrow(customers),550),]
write.csv(customers, "customers.csv", row.names = F)

## 여기서부터 

# 1.자료의 전처리

setwd("D:/magic/Bigdata/clustering")

customers <- read.csv("customers.csv")
str(customers)
colSums(is.na(customers))

rownames(customers) <- customers[,1] # 첫번째 열에 rownames 특성을 부여함
customers[,1] <- NULL  # 열을 삭제함, 즉 column 0:rownames 로 만듦

par(mfrow=c(1,2))
hist(customers$recency)
hist(customers$frequency)
hist(customers$monetary)
par(mfrow=c(1,1))

customers$recency <- log(customers$recency)
customers$frequency <- log(customers$frequency)
customers$monetary <- customers$monetary+1
customers$monetary <- log(customers$monetary)

customers$recency <- scale(customers$recency)
customers$frequency <- scale(customers$frequency)
customers$monetary <- scale(customers$monetary)

str(customers)
summary(customers)

##################
# Visualize data #
##################

library(ggplot2)
library(scales)

scatter <- ggplot(customers, aes(x = frequency, y = monetary)) + 
  geom_point(aes(colour = recency, shape = pareto)) +
  scale_shape_manual(name = "80/20 Designation", values=c(17, 16)) + 
  scale_colour_gradient(name="Recency") + 
  xlab("구매빈도") + 
  ylab("구매 총지출")

scatter

#############################################
# 군집수 결정#
#############################################

#4. 군집수 결정
library(NbClust)
customers <- customers[,-4] # pareto 변수 제외
colnames(customers)
res.nbclust <- NbClust(customers, distance = "euclidean", min.nc=2, max.nc=8, method = "complete") 


library(factoextra)
fviz_nbclust(res.nbclust)

#5. 분석 및 시각화
set.seed(1234)
km.res2 <- kmeans(customers, 2, nstart = 25)
km.res2
fviz_cluster(km.res2, data = customers)

set.seed(1234)
km.res3 <- kmeans(customers, 3, nstart = 25)
km.res3
fviz_cluster(km.res3, data = customers)

set.seed(1234)
km.res4 <- kmeans(customers, 4, nstart = 25)
km.res4
fviz_cluster(km.res4, data = customers)

fviz_cluster(km.res4, choose.vars = c("frequency", "monetary"), data = customers)
fviz_cluster(km.res4, choose.vars = c("recency", "monetary"), data = customers)
fviz_cluster(km.res4, choose.vars = c("recency", "frequency"), data = customers)

km.res4

#6. 군집경향 평가하기
library(factoextra)
res <- get_clust_tendency(customers, n = nrow(customers)-1)
res$hopkins_stat

# 3D 그래프 보기: 
#install.packages("rgl")
library(car)
library(rgl)
library(mgcv)

var.name <- paste("cluster", 4, sep="_")
customers[,(var.name)] <- km.res4$cluster
customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:4))

colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')

scatter3d(x = customers$frequency,
          y = customers$monetary,
          z = customers$recency,
          groups = customers$cluster_4,
          xlab = "Frequency",
          ylab = "Monetary",
          zlab = "Recency",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, 
          fit = "smooth",
          grid = TRUE,
          axis.col = c("black", "black", "black"))

remove(colors)






