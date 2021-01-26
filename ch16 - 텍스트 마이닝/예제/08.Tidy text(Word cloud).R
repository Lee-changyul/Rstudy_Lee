# 08.Tidy text(Word cloud)


# 6.워드 클라우드와 단어 빈도분석

## 6.1 단어빈도
text_tb %>%
  count(word, sort = TRUE) %>%
  filter(n >50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## 6.2 tf_idf 중요도
text_tb %>%
  arrange(desc(tf_idf))%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(15) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


# 6.3 단순 워드 클라우드
text_tb %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words=word, 
                 freq=n, 
                 max.words=100))


# 6.4 색깔 워드 클라우드

library(wordcloud)
palete<-brewer.pal(6,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))

text_tb %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words=word,
                 freq=n,
                 scale=c(4,0.5), 
                 min.freq=10, 
                 max.words=100,
                 random.order=FALSE, 
                 colors=palete, 
                 family="malgun"))
