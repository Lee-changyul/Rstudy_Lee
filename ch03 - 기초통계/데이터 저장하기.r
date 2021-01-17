gradecsv <- read.csv("0202.grade.csv",
                    header=TRUE, # T만 적어도 됨. FALSE 는 F만 적어도 됨
                    sep = ",", # 기본값이 R에서는 ,로 인식하기 때문에 빼주어도 됨
                    na.strings = ".")

str(gradecsv)

gradecsv$csex <- factor(gradecsv$csex,
                        levels = c(1,2),
                        labels = c("남자","여자")) 
# 변수를 수정하는 것(업데이트) : 똑같은 변수를 왼쪽과 오른쪽에 입력
 
str(gradecsv)

# 데이터 내보내기

write.csv(gradecsv,
          file = "gradecsv1.csv",
          row.names = F,
          na="")

# R 데이터로 저장하기 

save(gradecsv, file="grade.Rdata")

load(file ="grade.Rdata")
