#### 사례1 #####
# 문제의 정의
# 다음은 호흡과 뇌파와의 관계를 연구한 자료이다. 
# Channel별로 호흡을 3번(흡호=1:1, 7:3, 3:7)측정하였다. 
# 뇌파간에는 어떤 상관관계가 있는가?
  

# 01.데이터 불러오기 ####
corr.df <- read.csv("Ch1105.호흡과 뇌파.csv", 
                    header=TRUE, 
                    na.strings = ".")
str(corr.df)

head(corr.df)






# 02.기본통계치 확인####
library(psych)
describe(corr.df)
pairs.panels(corr.df)








# 03.상관분석####
cor.df.r <- cor(corr.df, use="complete.obs", method=c("pearson"))

head(cor.df.r)

 # 상관관계 및 p-value 한번에 알아보기 Hmisc 패키지 사용하기 

# install.packages("Hmisc")

Hmisc::rcorr(as.matrix(corr.df), type="pearson") 

cor.df.r

# 04. P-value에 따라서 상관계수에 ** 넣어보기 ####


# 상관계수 구하기 => 상관계수 소수점 (둘째, 또는 셋째 자리까지 반환하기) => 함수에 넣어서 별표 보기 
 # result : corr.df.r 데이터(상관관계 데이터)를 보기 좋게, 소수점 셋째짜리까지 표시해 둔 것    
 # 이 부분은 함수를 사용할 때 마다, 변경되야 함 


# 상관계수 : 
# 상관계수 소수점 셋째짜리까지 압축



# 아래 식에서 위에서 지정한 데이터 명만 바꾸어줌 

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(corr.df, 0.95) # 원본 데이터를 넣는다. 

str(res1)

res1[[1]] # 리스트의 첫번째 원소를 보여달라라


# lowCI : 신뢰구간의 하단, uppCI : 신뢰구간의 상단

result <- round(cor.df.r, 3) # 상관관계 데이터를 소수점 셋째짜리까지 정렬


l=length(corr.df[1,])
for(i in 1:l)
{
  for(j in 1 :l)
  {
    result[i,j]=ifelse(res1[[1]][i,j]>=0.05,paste(result[i,j],"",sep=""),
                       ifelse(res1[[1]][i,j]>=0.01,paste(result[i,j],"*",sep=""),
                              paste(result[i,j],"**",sep="")))
    
  }
}
result # 이데이터에서 "" 를 없애고 다시 저장하려면 어떻게 해야 할까?


