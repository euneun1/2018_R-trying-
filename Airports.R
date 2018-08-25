##세계공항 이용률 나타내기--------------------------------
install.packages("rvest")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("xml2")

library(xml2)
library(ggplot2)
library(rvest)
library(ggmap)

#html파일 가져오기
html.airports = read_html("https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic")

#fill = TRUE는 빈칸 채워라. 가져온 파일에서 첫 번째 데이터만 추출
df = html_table(html_nodes(html.airports, "table")[[1]], fill = TRUE)

head(df)
View(df)

##전처리
colnames(df)[6] ="total" #df의 6번째 이름을 total로 바꾸기
df$total
df$total = gsub(',','',df$total) #필요없는 부분(,) 제거하기

df$total = as.numeric(df$total) #문자를 숫자로 바꾸기

gc = geocode(df$Airport) #공항들의 위도와 경도값을 알아오기
gc

df = cbind(df, gc) #위도 경도 값과 원래 자료와 합치기
df

world = map_data("world") #세계지도 부르기

#지도 위에 그리기
ggplot(df, aes(x = lon, y = lat)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey70") +
  geom_point(color = "dark red", alpha = .25, aes(seize = total)) +
  geom_point(color = "dark red", alpha = .75, shape = 1, aes(size = total)) +
  theme(legend.position = 'none')
#경도는 lon, 위도는 lat으로 표시.
#shape = 1은 가운데 비우기. alpha는 투명도. .25는 25%의 비율로.
#geom_polygon은 지도를 도형 모양으로 표현. polygon은 점을 크게 생각하고 안쪽에 표현하는 것. 값별로 원의 크기 처리. 입체감있게 표현됨.

##한국 항공사 공항이용률 나타내기------------------------------
library(xml2)
library(ggplot2)
library(rvest)
library(ggmap)

html.airports_korea = read_html("https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_South_Korea")

df_new = html_table(html_nodes(html.airports_korea, "table")[[1]], fill = TRUE)

head(df_new)
View(df_new)

colnames(df_new)[4] = "total"

df_new$total
df_new$total = gsub(',', '',df_new$total)
df_new$total = as.numeric(df_new$total)

gc = geocode(df_new$Airport)
gc

df_new = cbind(df_new, gc)
df_new
korea = map_data("world", region = "South Korea")

ggplot(df_new, aes(x = lon, y = lat)) +
  geom_polygon(data = korea, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey70") +
  geom_point(color = "dark red", alpha = .25, aes(seize = total)) +
  geom_point(color = "dark red", alpha = .75, shape = 1, aes(size = total)) +
  theme(legend.position = 'none') +
  geom_text(data = df_new,
            aes(x = lon, y = lat + 0.2, label = Airport ),
            size = 2.8)

