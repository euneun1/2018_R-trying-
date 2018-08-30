sessionInfo() #R에 대한 환경 보여주기
##############################
#트위터를 csv로 저장하여 텍스트 마이닝하기-------
##############################
twitter = read.csv("twitter.csv",
                   header = T,
                   stringsAsFactors = F,
                   fileEncoding = "UTF-8")

twitter = rename(twitter,
                 no = 번호,
                 id = 계정이름,
                 date = 작성일,
                 tw = 내용)

twitter$tw = str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

nouns = extractNoun(twitter$tw)
wordcount = table(unlist(nouns))
df_word = as.data.frame(wordcount, stringsAsFactors = F)
df_word = rename(df_word,
                 word = Var1,
                 freq = Freq)

df_word = filter(df_word, nchar(word) >= 2)

top20 = df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

library(ggplot2)
order = arrange(top20, freq)$word

ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.3)

pal = brewer.pal(8, "Dark2")
set.seed(1234) #색깔이 돌아가면서 칠해지는데, 색깔을 4개로만 제한.

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)

###########################
##트위터에서 긁어온 파일로 하기---------------------------------------
###########################
# 1. Twitter 회원 가입  www.twitter.com
# 2. 개발자 회원 인증   apps.twitter.com
# 3. 개발 앱 등록 ( 키 4개 받기 )
install.packages("RCurl")
install.packages("base64enc")
install.packages("twitteR")
install.packages("ROAuth")

library(RCurl)
library(base64enc)
library(twitteR)
library(ROAuth)

reqURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/accsess_token"
authURL = "https://api.twitter.com/oauth/authorize"

#트위터로 받는 키
consumerKey = "?"
consumerSecret = "?"
accesstoken = "?"
accesstokensecret = "?"

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="https://curl.haxx.se/ca/cacert.pem", destfile = "carcert.pem")
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesstokensecret)
1

#특정 키워드로 검색하여 가져오기
string = enc2utf8("축구") #한글로 찾을때는 enc2utf8이용

tweets = searchTwitter(string, n = 3000, lang = "ko", retryOnRateLimit = 10000) #트위터 가서 string을 긁어와라. 3000개를 뒤져라. 한국말로(lang = "ko", 만약 멈출경우 만번만 재시도 해라.)
tweets

text_extracted = sapply(tweets, function(t) t$getText()) #쓸데 없는 것 날리기
text_extracted

getCurRateLimitInfo() #트위터와 인증이 잘 되었는지 확인
#retryOnRateLimit - 부하가 걸리면 몇번 정도 재시도할지 체크

string = enc2utf8("너의 결혼")
tweets = searchTwitter(searchString = string, n = 1000, lang = "ko", retryOnRateLimit =  10000)
tweets

#불필요한 기호 등을 삭제
text_extracted = sapply(tweets, function(t) t$getText())
text_extracted

#불필요한 글자 삭제
tweets_df = twListToDF(tweets)
tweets_df$text

#겹치는 것 없애기
text_extracted = unique(text_extracted)

#여러개의 키워드로 들어간 것 찾기
string = enc2utf8("한국+베트남")  # 한글 검색

tweets = searchTwitter(searchString = string, n = 100, lang = "ko", retryOnRateLimit =  100)


# 영어로 된것 찾기
tweets = searchTwitter(searchString = 'korea+love', n = 100)
tweets

#보기좋게 나타내기
tweets = unlist(tweets) 
str(tweets)


# 특정 글자 삭제하기
tweets = gsub("사랑","",tweets)

tweets = table(tweets)

###########################
#트위터에서 가져와서 텍스트 마이닝하기----------------------
###########################
install.packages("RCurl")
install.packages("base64enc")
install.packages("twitteR")
install.packages("ROAuth")

library(RCurl) #연결
library(base64enc) #연결
library(twitteR) #트위터 문법
library(ROAuth) #인증담당

# options(httr_oauth_cache = T) 문제가 생길 때만

reqURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/accsess_token"
authURL = "https://api.twitter.com/oauth/authorize"


consumerKey = "?"
consumerSecret = "?"
accesstoken = "?"
accesstokensecret = "?"

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="https://curl.haxx.se/ca/cacert.pem", destfile = "carcert.pem")
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesstokensecret) #여기서 안되면 새 키 넣기
1

#가져오기------------
string = enc2utf8("축구")

tweets = searchTwitter(string, n = 300, lang="ko", retryOnRateLimit = 10000)
tweets
text_extracted = sapply(tweets, function(t) t$getText()) #읽어온걸 글자를 R에서 다루기 편하게 거치기. sapply거치면 벡터입력으로 바꿔줌.
text_extracted

getCurRateLimitInfo() #인증이 트위터와 잘 연결되어있는지 확인

#처리하기------------
# 문제점 많음 
# 자바 설치 jdk  7 
#  http://www.oracle.com/technetwork/java/javase/downloads/index-jsp-138363.html

# java 폴더 경로 설정 
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_111/")
install.packages("rJava")
install.packages("KoNLP")
install.packagesI("stringr")

library(rJava)
library(KoNLP)
library(stringr)

useSejongDic() #rJAva에 들어있는 사전을 쓴다.

#분석하기------------
head(text_extracted) #앞에서 수집한 텍스트 확인

text_extracted = unique(text_extracted) # 중복 삭제
text_extracted = gsub("관람객","",text_extracted) # 관람객 단어 제거
text_extracted = gsub("[ㄱ-ㅣ]","",text_extracted) # ㅋㅋㅋ ,ㅜㅠ 등 제거
text_extracted = gsub("[[:punct:]]","",text_extracted) # 구두점 제거

# 2글자 이상 단어만 추출 
noun.list = sapply(text_extracted,USE.NAMES = F,FUN = extractNoun)
noun.list = lapply(noun.list, function (x) return(x[str_length(x) >=2 ]))
head(noun.list)

#워드 클라우드
install.packages("wordcloud2")
library(wordcloud2)

allnoun = unlist(noun.list) #목록을 해제하기, 벡터형으로 바꾸기. 
head(allnoun, 20)

#단어별 개수 정리
allnoun.table = table(allnoun) #테이블로 만들
allnoun.table = sort(allnoun.table, decreasing = T)
head(allnoun.table, 30)

#그림 그리기
wordcloud2(allnoun.table, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
wordcloud2(data = allnoun.table) #옵션 안주고 했을때.

# ?wordcloud2 하면 wordcloud2에 대한 도움말이 나옴. 밑에 예제 있으니깐 참고해서 적용해보기.

#--------------------------------------------------------
install.packages("d3Network") #좀더 세련된 그림이 나옴.
library(d3Network)