#p값은 작은게 좋은것.
mpg = as.data.frame(ggplot2::mpg)

library(dplyr)
mpg_diff = mpg %>%
  select(class, cty) %>% 
  filter(class %in% c("compact", "suv"))

head(mpg_diff)

table(mpg_diff$class)

t.test(data = mpg_diff, cty~class, var.equal = T) #보고 싶은 것 ~ 기준점.
#---------------------------------------------------------------
mpg_diff2 = mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c("r", "p"))

table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty~fl, var.equal = T)
#-----------------------------------------------------------------
economics = as.data.frame(ggplot2::economics)

cor.test(economics$unemploy, economics$pce)

head(mtcars)

car_cor = cor(mtcars)
round(car_cor, 2)

library(corrplot)

corrplot(car_cor)
corrplot(car_cor, method = "number")

#분석한뒤 보고서 만들기. 일목요연하게 깔끔하게.
#워드로 만들기에 번잡하고, 복붙도 복잡해서 긁기로 함.
#Markdown은 다른 곳에서도 많이 씀

#코드 청크 안에 코드 입력하기. 

#(echo = FALSE) 는 코드 감추고 결과만 나옴
#위에 knit찾기

#파워포인트 만들기
library(devtools)
install_github(c('slidify','slidifyLibraries'), 'ramnathv')
library(slidify)

author("Rclass3")
getwd()

#다 적고 Knit
#log 옆은 브라우저로 함.

#shiny 쓰면 interactive 하게 쓸수 있다.