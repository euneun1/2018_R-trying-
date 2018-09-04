#plot.ly/ggplot2 참고
install.packages("plotly")
library(plotly)

#ggplot으로 그래프 만들기--------------------------------
library(ggplot2)
p = ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point()

#인터랙티브 그래프 만들기
ggplotly(p)

#인터랙티브 막대 그래프 만들기
p = ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")

ggplotly(p)

p = ggplot(data = midwest, aes(x = state, y = poptotal, col = county)) +
  ylim(3000, 1500000) +
  geom_point()

ggplotly(p)

#인터랙티브 시계열 그래프 만들기----------------------
install.packages("dygraphs")
library(dygraphs)

economics = ggplot2::economics #데이터 준비
head(economics)

library(xts) 
eco = xts(economics$unemploy, order.by = economics$date) #시간 순서 속성을 지니는 xts 데이터 타입으로 변경
head(eco)

dygraph(eco) #그래프 생성

dygraph(eco) %>% dyRangeSelector() #날짜 범위 선택 가능

#여러 값 표현하기
eco_a = xts(economics$psavert, order.by = economics$date) #저축률
eco_b = xts(economics$unemploy/1000, order.by = economics$date) #실업자 수

#합치기
eco2 = cbind(eco_a, eco_b) #데이터 결합
colnames(eco2) = c("psavert", "unemploy") #변수명 바꾸기
head(eco2)

#그래프 만들기
dygraph(eco2) %>% dyRangeSelector()

eco_c = xts(economics$pop/10, order.by = economics$date)
eco_d = xts(economics$pce, order.by = economics$date)

eco3 = cbind(eco_c, eco_d)
colnames(eco3) = c("pop", "pce")

dygraph(eco3) %>% dyRangeSelector()

#----------------------------------------------------------------
install.packages("googleVis") #구글에서 만든 차트
library(googleVis)

data() #내장 데이터셋 목록 보기

name = c("a", "b", "C", "d")
wt = c(71, 64, 80, 74)
ht = c(175, 178, 180, 183)

friend = data.frame(name=name, wt=wt, ht=ht) #데이터프레임 만들기
friend

fri1 = gvisColumnChart(friend, options = list(title = "Friends",
                                              height = 400,
                                              weight = 500)) #구글비주얼컬럼함수. 
plot(fri1) #저장할 필요없이 구글웹으로 보여줌.

fri2 = gvisAreaChart(friend, options = list(title = "Friends",
                                            height = 400,
                                            weight = 500))
plot(fri2)

?gvisColumnChart

df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))

Col1 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Col1)

Col2 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
                        options=list(isStacked=TRUE))
plot(Col2)

Col3 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
                        options=list(title="Hello World",
                                     titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                                     bar="{groupWidth:'100%'}"))
plot(Col3)

#----------------------------------------------------
name = c("BMW", "Hyundai")
Fire = c(1.9, 0.5)
car = data.frame(name=name, Fire= Fire)

car1 = gvisGauge(car, options = list(min = 0, max = 2,
                                     redFrom = 0, redTo = 0.9,
                                     yellowFrom = 0.9, yellowTo = 1.1,
                                     greenFrom = 1.1, greenTo = 2.0,
                                     width = 700,height = 500)) #색깔별로 경로 만들기
plot(car1) #게이지로 표현됨.
