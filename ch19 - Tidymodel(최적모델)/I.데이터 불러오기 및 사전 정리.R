# https://www.tidymodels.org
# 유니버설 은행 사례
# 모델 비교 분석

#######################################################
# tidyverse: ggplot2, purrr, tibble  3.0.3,           #
#            dplyr, tidyr, stringr, readr, forcats    #
#                           
# tidymodels: broom, recipes, dials, rsample, infer,  #
#             tune, modeldata, workflows, parsnip,    #
#             yardstick                               #
#######################################################

# install.packages("tidyverse") 
# install.packages("tidymodels")
# install.packages("skimr")
# install.packages("vip")
library(tidyverse)
library(tidymodels)
library(skimr)           # 데이터 요약(EDA)
library(vip)             # 중요한 변수 찾기





## I. 데이터 불러오기 및 사전 정리

## 01.데이터 불러오기

bank_tb <- read_csv('UniversalBank.csv', 
                    col_names = TRUE,
                    locale=locale('ko', encoding='euc-kr'),
                    na=".") %>% # csv 데이터 읽어오기
  mutate_if(is.character, as.factor)

str(bank_tb)
head(bank_tb)





## 02.data 전처리

# 변수명 수정 (공란이 있을 경우에 변수명 수정)

bank_tb <- bank_tb %>%
  rename(c('Personal_Loan'= 'Personal Loan',
           'CD_Account' = 'CD Account',
           'Securities_Account' = 'Securities Account'))
str(bank_tb)
head(bank_tb)


# 범주형 변수(factor)로 인식하게 변환
# 결과변수(class)에서 관심있는 변수를 1번으로 세팅

bank_tb <- bank_tb %>%
  mutate(Personal_Loan = factor(Personal_Loan, 
                                levels = c(1, 0),              #관심변수=Yes           
                                labels = c("Yes", "No"))) %>%
  mutate(Securities_Account = factor(Securities_Account, 
                                     levels = c(0,1),
                                     labels = c("No", "Yes"))) %>%
  mutate(CD_Account  = factor(CD_Account, 
                              levels = c(0,1),
                              labels = c("No", "Yes"))) %>%
  mutate(Online = factor(Online,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) %>%
  mutate(CreditCard = factor(CreditCard,
                             levels = c(0,1),
                             labels = c("No", "Yes"))) %>%
  mutate(Education  = factor(Education ,
                             levels = c(1:3),
                             labels = c("Undergrad", 
                                        "Graduate", 
                                        "Professional")))
str(bank_tb)
head(bank_tb)

# 필요없는 변수제거: ID, 우편번호 제거
# recipe에서 제거할 수도 있음

bank_tb <- bank_tb %>%
  select(-c(ID, `ZIP Code`))  

str(bank_tb)
head(bank_tb)





## 03.데이터 탐색(EDA)

# 데이터 탐색: 범주형, 연속형 구분
# skimr::skim() - package명을 앞에 써서 구분
# 패키지를 여러개 사용할 경우에 이름이 같은 경우도 있어서
# 구분이 필요할 경우에 [패키지명::]을 사용

bank_tb %>%
  skimr::skim() 

bank_tb %>%
  group_by(Personal_Loan) %>%
  skimr::skim() 

# base accuracy
# yes 기준으로 0.096

bank_tb %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))





## 04.훈련용, 테스트용 데이터 분할: partition

# 데이터 partition

set.seed(123) # 시드 고정 

bank_split <- 
  initial_split(bank_tb,
                strata = Personal_Loan)

bank_split

# training, test용 분리

train_data <- training(bank_split)
test_data  <- testing(bank_split)

# 교차검증(k-fold) 데이터 만들기

set.seed(123)
bank_folds <- 
  vfold_cv(train_data, v=3)

bank_folds

