library(tidyverse)
library(tidytext)
library(tidymodels)
library(writexl)
library(tm)

# install.packages("readxl")
# install.packages("writexl")

library(readxl)

# 1. 데이터 읽기

texts <- read_excel(".//raw_data//final_setting.xlsx")

str(texts)

texts$date <- mydates


count(texts)

text <- subset(texts, select = -word1)

distinct(text)

t_freq <- text %>%
  group_by(date) %>% 
  count(no, word, sort = TRUE) %>%
  bind_tf_idf(no, word, n) %>%
  print(t_freq) %>% 
  ungroup()

t_freq$date <- as.Date(as.character(t_freq$date))


#month와 type을 기준으로 그룹화
  
  
a <- t_freq %>% 
  group_by(date) %>%
  filter(word == "재난지원금")

b <- t_freq %>% 
  group_by(date) %>%
  filter(word == "미국")

c <- t_freq %>% 
  group_by(date) %>%
  filter(word == "재정건전성")


ggplot(a) +
  aes(x = date, weight = n) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "재난지원금") +
  theme_minimal() 

  ggplot(c) +
  aes(x = date, weight = n) +
  geom_bar(fill = "#fa9e3b") +
  labs(title = "현금") +
  theme_minimal()
