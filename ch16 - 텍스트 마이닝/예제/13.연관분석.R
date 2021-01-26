## 13.연관분석_텍스트마이닝



# install.packages("arules") 
library(arules) # 연관분석 패키지 설치




# 6.전리작업: dtm을  matrix 형식으로 변환
dtm_m <- as.matrix(dtm)
dtm_m <- ifelse(dtm_m > 0, 1, 0) # 단어가 1이상인 것을 0,1로 변환함
head(dtm_m)




# 6.1 바이너리 코드를 transactions으로 변환
dtm_trans <- as(dtm_m, 
                  "transactions")
arules::inspect(dtm_trans) 
# inspect는 여러 패키지에서 사용하므로 동시에 
# 사용할때는 특정패키지 명을 지정




# 6.2 연관규칙 실행
# 그래프 확인
itemFrequencyPlot(dtm_trans)
# apriori(data, minimum support, minimum confidence, and target)
rules <- apriori(dtm_trans, 
                 parameter = list(supp= 0.5, 
                                  conf = 0.7, 
                                  target = "rules"))




# 6.3 규칙확인: lift가 높은 순서로 sorting
rules.tbl <- arules::inspect(sort(rules, by = "lift"))
rules.tbl[rules.tbl$support >= 0.5 & 
            rules.tbl$confidence >= 0.7 & 
            rules.tbl$lift >= 1,]


