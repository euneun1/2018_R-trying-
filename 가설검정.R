#유의하다: 의미가 있다
#p 값이 크다는게 안좋은것.
#p 값이 작은게 좋은것(통계적으로 유의하다) 기준: 0.05. 5%
#귀무가설(변화가 없을 것이다) 대안가설(변화가 있을것이다)
#귀무가설을 기각하면 대립가설이 산다

###################
#t-test(t검정): 두집단의 평균에 통계적으로 유의한 차이가 있는지
####################
#일반 휘발유와 고급 휘발유의 도시 연비 t 검정---------------------------------
mpg = as.data.frame(ggplot2::mpg)
View(mpg)

library(dplyr)
mpg_diff = mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c("compact", "suv"))

head(mpg_diff)
table(mpg_diff$class)
t.test(data = mpg_diff, cty ~ class, var.equal = T) #var.equal = T는 퍼져 있는 정도, 분산이 동일하다고 가정. 대부분 T라고 함.
#p-value가 중요. 2.2e-16를 복사해서 엑셀에 넣으면 값을 바꿔줌(0.00000000000000022) 0.05보다 작으니 의미가 있다. 
#cty ~ class 에서 cty는 정말 알고 싶은것. class는 그거에 따른 변화

#혼자서 해보기-----------------------------------------
View(midwest)
midwest = as.data.frame(ggplot2::midwest)

midwest_diff = midwest %>% 
  filter(state %in% c("IL", "MI")) %>% 
  select(state, poptotal)

head(midwest_diff)
table(midwest_diff)

t.test(data = midwest_diff, poptotal ~ state, var.equal = T)
#p-value 가 0.9991이므로 유의미하다. 약 99%이므로 전혀 상관없다.

#혼자서 해보기2--------------------------------------------------------------
before = c(52,60,63,43,46,56,62,50)
after = c(58,62,62,48,50,55,68,57)

t.test(before, after, paired = TRUE) #paired TRUE의 뜻: 짝꿍. 일반얘들 나중에 똑같이 또 시험.짝꿍 그대로 일치시키기.
#p-value가 0.0166이므로 0.05보다 작다. 유의하므로 의미 있다. 규모가설을 기각한다.
#대부분 paired가 많이 쓰이니 이걸 많이 기억하기.

##############
#상관관계분석: 두 변수가 서로 관련이 있는지.
##############
#1에 가까울 수록 관련성이 큼.
#상관계수가 양수면 정비례, 음수면 반비례 관계
#실업자 수와 개인 소비 지출의 상관관계-----------------------------
economics = as.data.frame(ggplot2::economics)

#상관분석
cor.test(economics$unemploy, economics$pce)
#약자를 모르는데 아는 방법은 help를 이용. 예를들어 pce의 뜻을 모르면 ?economics를 치기
#p-value가 2.2e-16이므로  0.05보다 작아 유의하다.

#####################
#상관행렬 히트맵 만들기: 여러개 비교해보기
######################
head(mtcars)

car_cor = cor(mtcars) #상관행렬 생성
round(car_cor, 2) $소수점 셋째 자리에서 반올림해서 출력
#1이면 관계 100%

#히트맵(heat map) : 값의 크기를 색깔로 표현한 그래프
install.packages("corrplot")
library(corrplot)

corrplot(car_cor)
corrplot(car_cor, method = "number") #원 대신 상관계수 표시

#다양한 파라미터 지정하기
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
        method = "color", # 색깔로 표현
        col = col(200), # 색상 200 개 선정
        type = "lower", # 왼쪽 아래 행렬만 표시
        order = "hclust", # 유사한 상관계수끼리 군집화
        addCoef.col = "black", # 상관계수 색깔
        tl.col = "black", # 변수명 색깔
        tl.srt = 45, # 변수명 45 도 기울임
        diag = F) # 대각 행렬 제외

#혼자서 해보기-------------------------------------
library(carData)
head(Wool)

?corrplot
cor.test(Wool$len, Wool$cycles)
#p-value 가 0.0005244이므로 0.05보다 작아서 유의미하다.

car_cor = cor(Wool)
round(car_cor, 2)

corrplot(car_cor, method = "square")

corrplot(car_cor,
         method = "square",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)

