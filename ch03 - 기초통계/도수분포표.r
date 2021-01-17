# 도수분포표 : 빈도수(table), 비율(prop.table), 합계구하기(addmargins) 누적비율 등 제시하고 그래프 그리기 

freq <- read.csv("0301.frequency.csv",
                 header = TRUE,
                 na.strings = ".")
str(freq)

freq$grade <- factor(freq$grade,
                     levels = c(1:5),
                     labels = c("F","D","C","B","A"))

str(freq)

###################################
# 돗수분포표 만들기(일변량)

# attach(시작) ~ detach(끝) : 데이터 프레임 활성화 
# freq라는 데이터 프레임을 고정적으로 인식하고 그 안의 변수만을 가지고 작업하고 싶을 때, 이 함수를 사용 하여 작업량 단축
# 항상 attach() detach() 함께 입력하고 데이터 작업 수행 - 에러메시지 뜨면 detach 후 다시 입력 


attach(freq)

# table을 이용한 빈도수 확인, 그래프 만들 때에는 원데이터를 사용하는 것이 아니라 필요한 부분을 table로 인식해야 한다. 

# grade.n <- table(freq$grade) # attach 없을때

grade.n <- table(grade)

table(grade) 

#상대빈도(%) : 데이블을 만들어 놓고, 테이블에서 빈도를 입력하여 결정한다. 

grade.p <- prop.table(grade.n)
grade.p

#빈도 + 상대빈도 : 빈도와 비율을 합치고자 할때 사용 bind

grade.t <- cbind(grade.n,grade.p) # cbind 옆으로 나열하라 (옆에 열추가)
grade.t


grade.t1 <- rbind(grade.n,grade.p) #  rbind 아래로 나열하라 (아래 행추가)
grade.t1

#빈도 + 상대빈도 + 합계


grade.a <- addmargins(grade.t, 1) # cbind에서는 기본 세팅으로 1을 하면 좋을 것 같다. 
# grade.a <- addmargins(grade.t, margin =1) 그냥 1로해도 나온다. 옵션을 알 때 유용하겠군
grade.a

#margin=1 : 열 합계
#margin=2 : 행 합계
#margin=NULL : 전체 합계

rm(grade.t1)

detach(freq) # 항상 마무리 




