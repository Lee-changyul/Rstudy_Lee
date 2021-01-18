# https://www.tidymodels.org/learn/statistics/k-means/
# 02.K-평균 군집화: Table 15.9 공부 더 해야할 분야. 최신 방식 


#######################################################
# tidyverse: ggplot2, purrr, tibble  3.0.3,           #
#            dplyr, tidyr, stringr, readr, forcats    #
#######################################################

# install.packages("tidyverse") 
# install.packages("tidymodels")
library(tidyverse)
library(tidymodels)





## 01.데이터 불러오기

utilities_tb <- read_csv('Utilities.csv', 
                    col_names = TRUE,
                    locale=locale('ko', encoding='euc-kr'),
                    na=".") %>% # csv 데이터 읽어오기 / 이부분이 달라지고, 보기에도 편해졌다. 
  mutate_if(is.character, as.factor)

str(utilities_tb)
head(utilities_tb)

# 데이터 분포 확인 - 데이터 수가 적을 확인 

ggplot(utilities_tb, 
       aes(x = Sales, 
           y = Fuel_Cost)) +
  geom_point(alpha = 0.3)





# 02.데이터 정규화
# 데이터 정규화: mutate_if, 수치형 변수만 정규화
# 회사이름을 row 이름으로 변경

utilities_tb <- 
  utilities_tb %>%
  mutate_if(is.numeric, funs(scale(.))) %>%
  column_to_rownames(var = "Company")

utilities_tb





# 03.최적 군집수 찾기
# 최적 군집수를 찾는 엘보우(Elbow) 챠트

# 군집 9개 (알아서 세어보고 비교해봐라)

kclusts <- 
  tibble(k = 2:9) %>%
  mutate(
    kclust = map(k, ~kmeans(utilities_tb, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, utilities_tb)
  )

kclusts

# kclusts 에 저장된 값들을 확인하는 방법 

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))


clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# 엘보우(Elbow) 챠트

ggplot(clusterings, 
       aes(k, tot.withinss)) +
  geom_line() +
  geom_point()


# 군집별 그래프

ggplot(assignments, 
       aes(x = Sales, 
           y = Fuel_Cost)) +
  geom_point(aes(color = .cluster), 
             alpha = 0.8) + 
  facet_wrap(~ k) +
  geom_point(data = clusters, 
             size = 5, 
             shape = "x") # x는 랜덤시드를 주고 묶은 것 



# 04.best K-mean clustering 결과

#  best model 구축

set.seed(123)

kclust_best <- 
  kmeans(utilities_tb, # center에 군집수를 지정했을 때 요약문을 제시해라.  
         centers = 5)

# 군집분석 결과 확인

tidy(kclust_best)





# 05.군집별 특성 파악

kclust_cl <- 
  tidy(kclust_best) %>%
  select(-c(size, withinss))

kclust_cl <- 
  kclust_cl %>%
  pivot_longer(c("Fixed_charge", #c("1999, 2000")에러남
                 "Cost",
                 "RoR",
                 "Load_factor",
                 "Demand_growth",
                 "Sales",
                 "Nuclear",
                 "Fuel_Cost"),  
               names_to = "type",
               values_to = "cases")

ggplot(kclust_cl) + 
  geom_line(aes(x = type,
                y = cases,
                group = cluster,
                color = cluster))

