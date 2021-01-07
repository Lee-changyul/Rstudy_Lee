#### 일표본(One Sample) t-test #####

# 01.데이터 불러오기 
ost.df <- read.csv("Ch0701.ost.csv", 
                header=TRUE, 
                na.strings = ".")
str(ost.df)

ost.df

ost.df <- round(ost.df, 2) # 소수점 2자리로 반올림(2자리까지 보여주어라. )

ost.df

sort(ost.df$weight)

# 이상치 확인 및 제거 

# boxplot.stats(ost.df$weight)$out
# ost.df[!(ost.df$weight <= 242),]



# 02.기본통계치 확인: describe(psych패키지 이용)
attach(ost.df) # 객체연결 (attach  -> detach)
library(psych)
describe(ost.df)

# 03.그래프 그리기(박스그래프,히스토그램)
opar <- par(no.readonly = TRUE) #디폴트 par 값을 미리 할당--> par(opar)
par(mfrow=c(1,2)) # 화면분할 (열을 반으로 쪼개라)
boxplot(weight) 
hist(weight, 
     breaks=10, 
     col="red",
     xlab="무게", ylab="개수",
     ylim = c(0,25),
     main="아이스크림 무게에 대한 히스토그램 및 정규분포") 
par(opar)

# 04.통계분석
# two-sided test: alternative = c("two.sided") 같다(이다, 아니다)의 경우 - 보통 이것을 만이 활용한다. 
# right-sided test: alternative = c("greater") 크다 검증일 경우 검증
# left-sided test: alternative = c("less") 작다 검증일 경우
options("scipen" = 20) #지수 표기법 수정 / 소수점 20자리까지는 그냥 풀어주어라. : 2.2e-4=0.00022
t.test(ost.df,
       alternative = c("two.sided"), # 대부분 같다. 
       mu = 320.0, 
       conf.level = 0.95)

# 05.통계결과 그래프

mu=320
se=2 # 표본이므로 sd대신에 se 사용 (describe에서 가져오는 것)
inter = qt(p=0.025, df=99) # 95% 신뢰구간, df=조사대상 수 -1 
data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, 
     dnorm(data, mu, se), 
     type='l', 
     main="아이스크림 무게(Mu=320) 검정", 
     xlim=c(290,330))
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=mu-inter*se, col="blue", lty=5)
abline(v=295.44, col="red", lty=5)

detach(ost.df)

