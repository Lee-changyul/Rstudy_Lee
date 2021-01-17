# 스칼라

c(1) # c는 소문자로 쓴다
C(1)

# 벡터 (c)- 숫자형

num <- c(1,2,3,4) # c(1:4)
numT <- t(num) # 열벡터를 행벡터로 변환
View(num) # 데이터 보기
View(numT)
num %*% numT # 벡터곱셈(4*1 x 1*4) = 4*4
numT %*% num # 벡터곱셈(1*4 x 4*1) = 1*1) 

# 문자형, 논리형

c("m","F","F","M")
c(TRUE, FALSE, FALSE, TRUE)


m <- 1:12
m <- c(1,2,3,4,5,6,7,8,9,10,11,12)
m

# 행렬 (Matrix 4X3)
mtx <- matrix(m, nrow=4)  #  (m을 가지고 하는데, 4개 열을 만들겠다.  nrow, ncol)
mtx

mtx[3,2] #3열 2행을 출력하라

# 배열 (array 2X3X2) # 열, 행, 깊이 

arr <- array(m, c(2,3,2))
arr

# 데이터 프레임 (Data Frame)

var1 <- c(1,2,3,4)
var2 <- factor(c("M","F","F","M"))  # factor로 해야 그룹변수로 인식한다. 나중에 lable 명령어도 있다. 
df = data.frame(id = var1, sex = var2)
df
str(df)


# 나중에 데이터에서 변수를 뽑아올 때에는 $표시를 한다. date.frame의 변수를 읽는 기본 구조 


# 리스트 

v1 <- c(1,2,3,4)
v2 <- matrix(1:12, nrow=4)
v3 <- array(1:12, c(2,3,2))
v4 <- data.frame(id = c(1,2,3,4), sex = c("M","F","F","M"))
lt <- list(v1, v2, v3, v4)
lt

str(lt) #str : 데이터 구조를 보여주어라 . [] 벡터와 행렬, [[]] 리스트 형태의 데이터이다. 


