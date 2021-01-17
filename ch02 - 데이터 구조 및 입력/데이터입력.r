# 데이터 가져오기 Read.table(txt 파일, tab으로 구분된 파일), 
# Read.csv (csv 파일 ","로 구분된 파일)
# Read_excel (excel 파일 가져오기, library 기능 추가 필요)
# 그외 spss, json 등의 파일도 연결 가능 

gradetxt <- read.table("0201.grade.txt",
                     header=FALSE,
                     sep = "\t",
                     stringsAsFactor = FALSE,
                     na.strings = ".")

# Header = FALSE (변수명이 없을 때) 변수명을 인식할 것인가? default = true 
# Sep(구분자) : "," "", ":" Wt(tab) 마지막 것은 탭으로 구분하는 것
# StringsAsFactor = 문자데이터를 요인으로 인식 (TRUE) 대부분은 숫자로 저장되어 있기 때문에 의미가 없다. 
# na.strings = "", ".", "NA" 등이 있음 => 값이 안들어와 있으면 어떻게 넣을 것인가? .을 찍으면 missing 값으로 인식
# str(game) 속성보기 

str(gradetxt) 

# 데이터 가져오기 (read.csv)
gradecsv <- read.csv("0202.grade.csv",
                       header=TRUE, # T만 적어도 됨. FALSE 는 F만 적어도 됨
                       sep = ",", # 기본값이 R에서는 ,로 인식하기 때문에 빼주어도 됨
                       na.strings = ".")
str(gradecsv)

# 데이터 가져오기(read_excel)

# install.packages("readxl") # 패키지 : 데이터 프로그램을 미리 만들어 놓은 것
library(readxl)

gradexls <- read_excel("0203.grade.xlsx",
                       sheet = "grade",
                       col_names = TRUE,
                       na = "NA")

str(gradexls)

# 데이터 구조 보기

str(gradexls)
dim(gradexls) # 전체 데이터 개수(50개 행), 그중에 변수가 몇개인지 보여줌(열) 
summary(gradexls) # 기술통계 분석석
summary(gradexls$msex) # $ 뒤에 붙는 것은 변수명, 즉, msex 변수만 요약해서 보여주어라라 

# R에서 class(숫자일 경우 어떻게 해석하는지를 보여줌) mode (문자, 숫자, 함수, 리스트 등)의 차이  

# factor (질적(범주형) 변수 처리 : 문자형을 숫자로 

gradetxt <- read.table("0201.grade.txt",
                       header=FALSE,
                       sep = "\t",
                       stringsAsFactor = FALSE,
                       na.strings = "")

str(gradetxt) # 여기서 2번째 변수는 문자 이것을 그림으로 찍어보자

plot(x = gradetxt$V2, y = gradetxt$V4,
     levels=c("Male","Female")) # factor 형태로 바꾸지 않았기 때문에 오류가 발생함. factor로 바꾸어 주어야 함

gradetxt$V2 <- factor(gradetxt$V2,
                      levels=c("Male","Female"))
str(gradetxt$V2)
table(gradetxt$V2)
plot(x=gradetxt$V2, y=gradetxt$V4) # plot 함수는 다음시간에... 


# factor 변수처리 2 : 숫자형을 문자로 


gradetxt$V3 <- factor(gradetxt$V3,
                      levels = c(1,2))

str(gradetxt$V3)

plot(x = gradetxt$V3, y = gradetxt$V4,
     levels=c(1,2))


gradetxt$V3 <- factor(gradetxt$V3,
                      levels = c(1,2), 
                      labels=c("남자","여자")) #factor 로 바꾸어주고 이름을 붙여라

str(gradetxt$V3)

# 실제 활용 하는 방법 : 위의 예제에서 숫자 1,2를 요인으로인식하게 하고 요인명을 붙인다음에 그래프를 그리는 작업
# 이것을 텍스트 파일로부터 읽어와서 처리하는 방식으로 하여 순서대로 해보자. 

gradetxt <- read.table("0201.grade.txt",
                       header=FALSE,
                       sep = "\t",
                       stringsAsFactor = FALSE,
                       na.strings = "")

gradetxt$V3 <- factor(gradetxt$V3,
                      levels = c(1,2),
                      labels = c("남자","여자"))

str(gradetxt$V3)

table(gradetxt$V3)

plot(x = gradetxt$V3, y =  gradetxt$V4)


 