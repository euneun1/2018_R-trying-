##텍스트 마이닝--------------------------------------------------
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")

library(KoNLP)
library(dplyr)

#패키지 로드 에러 발생할 경우: java 설치 경로 확인후 경로 설정 바꾸기. (111를 181로 바꾸기)
#그래도 안되면 R을 uninstall 한뒤 C:에 다시 깔기
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_111/")

useNIADic() #사전 설정

txt = readLines("hiphop.txt") #데이터 불러오기
head(txt)
txt

install.packages("stringr") #특수문자 제거
library(stringr)
txt = str_replace_all(txt, "\\W", " ")

extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다") #명사 추출

nouns = extractNoun(txt) #가사에서 명사추출

wordcount = table(unlist(nouns)) #추출한 명사 list를 문자열 벡터로 바꾸고 단어별 빈도표 만들기
wordcount

#자주 사용된 단어 빈도표 만들기
df_word = as.data.frame(wordcount, stringsAsFactors = F)

df_word = rename(df_word, #변수명 수정
                 word = Var1,
                 freq = Freq)

df_word = filter(df_word, nchar(word) >= 2) #두글자 이상의 단어 저장
top_20 = df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

install.packages("wordcloud")
library(wordcloud) #워드 클라우드 만들기
library(RColorBrewer)

#단어 색상 목록
pal = brewer.pal(8, "Dark2") #Dark2 색상 목록에서 8개 색상 추출

set.seed(1234) #난수 고정
wordcloud(words = df_word$word, #단어
          freq = df_word$freq, #빈도
          min.freq = 2, #최소 단어 빈도
          max.words = 200, #표현 단어 수
          random.order = F, #고빈도 단어 중앙 배치
          rot.per = .1, #회전 단어 비율
          scale = c(4, 0.3), #단어 크기 범위
          colors = pal) #색깔 목록


pal <- brewer.pal(9,"Blues")[5:9] # 파랑색으로 표현
set.seed(1234) 
wordcloud(words = df_word$word, 
          freq = df_word$freq, 
          min.freq = 2, 
          max.words = 200,
          random.order = F, 
          rot.per = .1, 
          scale = c(4, 0.3), 
          colors = pal) 