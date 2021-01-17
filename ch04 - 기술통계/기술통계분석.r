## ch04. 기술통계분석(수치형)

## 산술평균 ####

## 데이터 가져오기 

wgt <- read.csv("0401.wgt.csv",
                header = T,
                na.strings = ".")
str(wgt)

wgt$sex <- factor(wgt$sex,
                  levels = c(1,2),
                  labels = c("남자","여자"))
str(wgt)

str(wgt$sex)

attach(wgt)

# 기본함수 이용
# na.rm = TRUE 결측값을 통계 분석 시 제외(미포함) 실제로 이 옵션을 붙여서 활용한다. 

mean(weight)
mean(weight, na.rm=T)
median(weight)
summary(wgt)

# 절단평균(Trimmed Mean, pshych) 양옆의 일정비율을 삭제하고 통계 계산
# median, mean 의 경향을 확인하고 파악

mean(weight)
mean(weight, trim = 0.1) # 양쪽을 0.05씩 떼주어라. 

detach(wgt)

# 가중평균 #### 각 항의 수치에 그 중요도에 비례하는 계수를 곱한다음 산출

class <- c("A","B","C")
class.n <- c(10, 50, 40)
class.avg <- c(60,70,80)
class.t <- data.frame("학급"=class, "학생수"=class.n, "학급평균"=class.avg)

View(class.t)

mean(class.avg)
weighted.mean(class.avg, class.n) #학급에 10명, 50명, 40명이 있으니, 가중치를 주는 것

# 기하평균 #### 
  # 곱의 형태로 변화하는 자료 (비율의 평균계산 - 물가상승률, 인구변동률, 연평균 성장률(CAGR)) 
  # 증가율은 전년도 대비임. 원년대비가 아님 / 평균의 의미 매년 똑같이 증가한다는 의미 


table <- data.frame("연도"=c(2010:2014),
                    "수익"=c(635,998,1265,1701,2362),
                    "증가율"=c(0,0.572,0.268,0.345,0.389))
)

# 식으로 계산

(2363/635)^(1/4) -1

# 비율로 계산(각각의 증가율로 계산)

cagr <- c(1.5716, 1.2675, 1.3446, 1.3885)

# cagr1 <- c((998/635), (1265/998), (1701/1265), (2362/1701)) , 괄호를 해도되고 안해도 됨

prod(cagr)^(1/length(cagr))-1 #각각의 항목을 모두 곱한 다음(prod), 곱한 만큼 나누어주고, 자신을 뺀다.

# psych 통계 패키지 사용

# install.packages("psych")
library(psych)

# geometric.mean 함수 사용 : 입력은 비율자료이어야 함
  # 비율만 알때 사용하기 좋은 방식 - 위의 것이 복잡하기 때문에 실제로는 아래방식으로 사용할 것을 추천(원데이터 알면 상관없지만..)
  
cagr <- c(1.5716, 1.2675, 1.3446, 1.3885)
geometric.mean(cagr) - 1

# 조화평균 ####
 # 속도 등과 같이 여러 단위가 결합되어 있을 때에 계산 (속도와 시간의 결합 등 km/h) harmonic mean

 # 질문 : 서울-> 부산 갈때 400km, 1시간 / 올때, 부산 -> 서울 100km, 4시간 = 평균 시속은?

hm <- c(400,100)
1/mean(1/hm) 

 # 패키지이용(psych)

harmonic.mean(hm)

# 분포 ####
  # 4분위 : 자료를 동일한 비율로 4등분  box plot에서 활용
  # 표준편차, 분산, 왜도, 첨도 등 
attach(wgt)

# 기본변수 R 사용하여 확인하는 법

min(weight)
max(weight)
diff(range(weight)) # 최소값과 최대값의 차이 
quantile(weight, c(0.25, 0.5, 0.75, 1.0))
var(weight)
sd(weight)
skew(weight) 왜도 (오른쪽으로 길다)
kurtosi(weight) 첨도 

# 표준화 ####
 # 측정단위 등과 관계없이 자료를 표준화시킨 값 : 절대 비교가 가능 / 평균에서 멀어진 정도 

wgt <- transform(wgt,
                 weight.z = scale(weight)) #scale 표준화하라는 명령어

wgt

detach(wgt)

# 기술통계 패키지 활용법 ####

attach(wgt)

summary(weight)

library(psych)

describe(weight)

?describe


detach(wgt)

# 이상치 제거 ####

boxplot(wgt$weight)

attach(wgt)

boxplot(weight)

# 80Kg 이상인 데이터 제거 

wgt <- wgt[!(weight > 80),] # ! 아닌거 => 80kg 아닌 거 골라서 다시 집어 넣어라 
describe(weight) # 패키지 psych 읽고나서 사용

 # 이러면 수치가 바뀌어야 하는데, 수정되지 않은 것을 볼 수 있다.  
 # attcah를 사용하고 나서 데이터 프레임 수정후에는 반드시 detach 하고 다시 프레임을 읽어와야 한다. (매우 중요)

detach(wgt)

attach(wgt)

describe(weight)

detach(wgt)

# 그룹간 연속변수 특성비교 ####

attach(wgt)

tapply(weight, sex, mean)
tapply(weight, sex, summary)
library(psych)
describeBy(wgt[c("weight")], sex, mat=TRUE) # mat = TRUE, mat = FALSE 동시에 묶어서 보여줄 것인가? 구분해서 보여줄 것인가?
describeBy(wgt[c("weight")], sex, mat=FALSE) 

detach(wgt)

