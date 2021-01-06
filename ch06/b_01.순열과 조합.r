# 기본함수
n <- 6 
r <- 3 
factorial(n)
factorial(n)/factorial(n-r)
choose(n,r)


# 패키지 이용
install.packages("gtools")
library(gtools)

nrow(permutations(6,3))
nrow(combinations(6,3))

# 야구경기 (9명-9개)
factorial(9)

# 집배원(4군데)
factorial(4)

# 12명중 8개
nrow(permutations(12,8))

n <- 12 
r <- 8 
factorial(n)/factorial(n-r)

# 20명중 2명씩 짝
choose(20,2)

# 45개 숫자중 6개 고름
choose(45,6)

# 1등 당첨확률
# 지수표기법 환원 1.2e+10->0.00000000012
options("scipen" = 20)
lot <- choose(45,6)
1/lot

# 5등 당첨확률(3개만)
options("scipen" = 20)
lot5 <- (choose(6,3)*choose(39,3))
lot5/lot

