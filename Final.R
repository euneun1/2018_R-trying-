############
#1. 비만도
############

#데이터 가져오기
install.packages("openxlsx")
library(openxlsx)
t = read.xlsx("size.xlsx")

#변수명 확인
str(t)

library(dplyr)
t2 = t %>% select(c(1,2,8,11,13,18,46,59))

t2 = t[c(1,2,8,11,13,18,46,59)]

colnames(t2) = c("gender", "age", "bmi", "fat", "basal", "height", "weight", "abdo")
str(t2)

library(ggplot2)
ggplot(data = t2, aes(x = height, y = weight, colour = gender)) +
  geom_point(size = 0.5) +
  stat_ellipse(geom = "polygon", alpha = 0.5, aes(fill = gender)) #분포를 타원으로 표현됨.

table(t2$fat)

#NA와 0.9제거
t3 = t2[(t2$fat != '0.9') & (!is.na(t2$fat)),] 
table(t3$fat)

ggplot(data = t3, aes(x = height, y = weight, colour = fat)) +
  geom_point(size = 0.5) +
  stat_ellipse(geom = "polygon", alpha - 0.5, aes(fill = gender))

cor.test(t2$height, t2$weight)

t3 = t2[c("age", "bmi", "height", "weight")]

class(t3)

t4 = na.omit(t3)

t5 = round(cor(t4), 2)
t5

library(corrplot)
corrplot(t5)

corrplot(t5, method = "number")

t6 = t4[,c("age", "height")]
t6 = t6 %>% mutate(ages = ifelse(age<10, "10-",
                                 ifelse(age<20, "10",
                                        ifelse(age<30, "20",
                                               ifelse(age<40, "30",
                                                      ifelse(age<50,"40",
                                                             ifelse(age<60,"50",
                                                                    ifelse(age<70,"60","70+"))))))))
head(t6)

library(ggplot2)
library(plotly)
p = ggplot(data = t6, aes(x = ages, y = height)) + geom_point()
ggplotly(p)

#########
##2.땅값
#########
t = read.csv("jiga-2017.csv")

#JIGA를 기준으로 최고가 100개 출력하기
library(dplyr)
t %>% arrange(desc(JIGA)) %>% 
  head(100)

t[order(-t$JIGA), ][1:100,]
#최저가 10개
t[order(t$JIGA), ][1:10,]

summary(t$JIGA)

library(ggplot2)
ggplot(t, aes(x = JIGA)) +
  geom_histogram()

install.packages("treemap")
install.packages("plyr")
library(treemap)
library(plyr)

#"SIGUNGU_NM", "BJDONG_NM"를 기준으로 합쳐서 JIGA 계산
t2 = ddply(t, c("SIGUNGU_NM", "BJDONG_NM"), summarise, total = sum(JIGA))

#treemap를 "SIGUNGU_NM", "BJDONG_NM" 기준으로 하여 total 값으로 작성
treemap(t2, index = c("SIGUNGU_NM", "BJDONG_NM"), vSize = "total")

??RMarkdown
