##########################
#미국 주별 강력 범죄율 단계 구분도 만들기
##########################
install.packages("ggiraphExtra")
library(ggiraphExtra)

#데이터 준비
str(USArrests)
USArrests
head(USArrests)

library(tibble) 

crime = rownames_to_column(USArrests, var = "state") #행 이름을 state 변수로 바꿔 데이터 프레임으로 생성하기.
crime

crime$state = tolower(crime$state) #지도 데이터와 동일하게 맞추기 위해 state 값을 소문자로 바꾸기
crime
str(crime)

library(ggplot2)
states_map = map_data("state") #state라는 map_data 읽어오기
str(states_map)

#단계 구분도 만들기
ggChoropleth(data = crime, #지도에 표현할 데이터
             aes(fill = Murder, #색깔로 표현할 변수
                 map_id = state), #지역 기준 변수
             map = states_map, #지도 데이터
             interactive = T) #인터랙티브 : 터마우스 올리면 값도 나오고 색깔별로 나옴.

ggChoropleth(data = crime,
             aes(fill = c(UrbanPop, Murder),
                 map_id = state),
             map = states_map,
             interactive = T)

################################
#대한민국 시도별 인구 수 단계 구분도 만들기----------------
################################
install.packages("stringi")

install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014") #깃허브에서 야매로 끌어다 설치

library(kormaps2014)

str(changeCode(korpop1)) #한글 가져올 때 문제가 생기면 changeCode 적용해보기

library(dplyr)
korpop1 = rename(korpop1, #필드명은 가능한 영어로 바꾸기 
                 pop = 총인구_명,
                 name = 행정구역별_읍면동)
str(changeCode(kormap1))

#단계구분도 만들기
ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

options(encoding = "CP949") #한국말로 처리하기. (그래도 안됨.)

###################################
#대한민국 시도별 결핵 환자수 단계 구분도 만들기-----------------
###################################
str(changeCode(tbc))
ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name,
                 map = kormap1,
                 interactive = T))

######################
#미국 주별 인구수----------------------------------------
######################
library(rvest)
library(ggplot2)
library(ggmap)
library(xml2)
library(dplyr)

html.states_pop = read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States")

usa_data = html_table(html_nodes(html.states_pop, "table")[[1]], fill = TRUE)
head(df)
View(df)

colnames(df)[6] = "pop" 
colnames(df)[1] = "state"
usa_data$state = tolower(df$state)

install.packages("ggiraphExtra")
library(ggiraphExtra)
library(tibble)

states_map = map_data("state")
ggChoropleth(data = df,
             aes(fill = pop,
                 map_id = state),
             map = states_map,
             interactive = T)

#csv로 할꺼면 표 긁어와서 엑셀에 넣고 정리 한뒤 csv로 저장--------
usa = read.csv("usa.csv")
View(usa)
colnames(usa)[1] = "state"
colnames(usa)[2] ="pop"

usa$state = tolower(usa$state)

states_map = map_data("state")
ggChoropleth(data = usa,
             aes(fill = pop,
                 map_id = state),
             map = states_map)
