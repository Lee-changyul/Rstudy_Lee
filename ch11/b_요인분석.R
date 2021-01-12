#### 요인분석 #####

# 01.데이터 불러오기 
faData <- read.csv("./data/09.fa.csv", 
                    header=TRUE, 
                    na.strings = "."
)
str(faData)

# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describe(faData)
names(faData)

# 03.요인분석
# 1차 요인분석 : 요인갯수 확인하기
faResult <- principal(faData[,c(2:17)], rotate="varimax")
faResult$values
plot(faResult$values, type="b", sub = "Scree Plot")

### plot(type)
###값	설명
#“p”	점으로
#“l”	선으로
#“b”	점과 선 둘다 동시에
#“o”	점과 선 둘다 동시에 (단 겹쳐짐 : overplotted)
#“h”	히스토그램과 비슷한 형태로 (histogram)
#“s”	계단모양으로 (stair steps)
#“S”	계단모양으로 (upper stair steps)
#“n”	좌표찍지 않음

# 2차 요인분석 : 요인적재량 확인
faResult <- principal(faData[,c(2:17)], nfactors = 5, rotate="varimax")
print(faResult, digits=3, cutoff=.3, sort=TRUE)

# 3차 요인분석 : 요인적재량 재거 (C4, fb4) 및 요인값 저장
faResult <- principal(faData[,c(2:5,7:16)], nfactors = 5, rotate="varimax", scores=T)
print(faResult, digits=3, cutoff=.3, sort=TRUE)
faResult$scores

# 종속변수 요인분석
faResult1 <- principal(faData[,c(18:21)], 
                       rotate="varimax")
faResult1$values
faResult1 <- principal(faData[,c(18:21)], 
                       nfactors = 1, rotate="varimax")
print(faResult1, digits=3, cutoff=.3, sort=TRUE)
faResult1 <- principal(faData[,c(18,19,21)], 
                      nfactors = 1, rotate="varimax", scores=T)
print(faResult1, digits=3, cutoff=.3, sort=TRUE)
faResult1$scores

# 4.요인값 데이터구조로 저장
faResultData <- data.frame(faResult$scores, faResult1$scores)
str(faResultData)
names(faResultData) <- c("design","info","comm","op","fb", "flow")

attach(faResultData)
lm(flow ~ design+info+comm+op+fb)
detach(faResultData)

# R 데이터로 저장. sava.image는 결과전에(객체전체)를 저장 
save(faResultData, file="./faResultData.RData")
load(file="./faResultData.RData")


# 부록 : 최우추정법(maximum likelihood method) 사용
faResult <- factanal(faData[,c(2:16)], 
                     factors = 5, rotation = "varimax", 
                     scores="regression")
print(faResult, digits=3, cutoff=.3, sort=TRUE)


