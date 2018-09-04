kakaotalk = readLines("KakaoTalkChats2.txt", encoding = "CP949")
#CP949로도 안되면 인코딩을 ANSI로 하기
kakaotalk

#전처리하기
kakaotalk = gsub("(2017년).+(:)", "", kakaotalk) 
kakaotalk = gsub("(2018년).+(:)", "", kakaotalk)
kakaotalk = gsub("[ㄱ - ㅎ]", "", kakaotalk)
kakaotalk = gsub("\\(이모티콘\\)", "", kakaotalk)
kakaotalk = gsub("[~!@#$%^&*()_+=?]<>", "", kakaotalk)
kakaotalk = gsub("^ㅋ", "", kakaotalk)

kakaotalk

library(KoNLP)
noun.list = sapply(kakaotalk, USE.NAMES = F, FUN = extractNoun)
noun.list = lapply(noun.list, function (x) return(x[str_length(x) >=2 ]))
noun.list

library(wordcloud2)
allnoun = unlist(noun.list)
head(allnoun, 20)

allnoun.table = table(allnoun)
allnoun.table = sort(allnoun.table, decreasing = T)
head(allnoun.table, 30)

wordcloud2(allnoun.table, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
