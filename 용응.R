library(KoNLP)
library(dplyr)

useSejongDic()

#데이터 준비--------------------
naver = readLines("naver.txt")
naver
View(naver)

#데이터 정제-----------------
library(stringr)
text_extracted = unique(naver)
text_extracted = gsub("[[:punct:]]","",text_extracted)
noun.list = sapply(text_extracted,USE.NAMES = F,FUN = extractNoun)
noun.list = lapply(noun.list, function (x) return(x[str_length(x) >=2 ]))

head(noun.list)

#단어 빈도 막대 그래프 만들기----------------
wordcount = table(unlist(noun.list))
wordcount

df_word = as.data.frame(wordcount, stringsAsFactors = F)

df_word = rename(df_word,
                 word = Var1,
                 freq = Freq)

df_word = filter(df_word, nchar(word) >= 2)
top_30 = df_word %>% 
  arrange(desc(freq)) %>% 
  head(30)
top_30

library(ggplot2)
order = arrange(top_30, freq)$word

ggplot(data = top_30, aes(x = word, y = freq)) +
  ylim(0, 30) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) + 
  geom_text(aes(label = freq), hjust = -0.3) 

#워드 클라우드 만들기----------------------------------------
library(wordcloud) 
library(RColorBrewer)

pal = brewer.pal(8, "Dark2") 

set.seed(1234) 
wordcloud(words = df_word$word, 
          freq = df_word$freq, 
          min.freq = 2, 
          max.words = 200, 
          random.order = F, 
          rot.per = .1, 
          scale = c(4, 0.3), 
          colors = pal) 
View(top_30)
#ggplotly 만들기-------------------------------------------
library(plotly)

top_7 = df_word %>% 
  arrange(desc(freq)) %>% 
  head(7)

p = ggplot(data = top_7, aes(x = word, y = freq)) +
  ylim(0, 25) +
  geom_bar(stat = "identity")
#stat = "identity"는 내가 y축에 원하는 변수 설정 하고 싶을 때 이용
ggplotly(p)
