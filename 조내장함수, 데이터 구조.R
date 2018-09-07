##############
## R 내장 함수로 데이터 추출하기
##############
#행 번호로 행 추출하기
exam = read.csv("csv_exam.csv")

#대괄호안 쉼표 기준, 왼쪽에 행 번호(인덱스) 입력
#인덱스: 데이터의 위치 또는 순서 의미
#인덱싱: 인덱스를 이용해 데이터를 추추하는 작업
exam[] #조건 없이 전체 데이터 출력
#exam[행,열]

exam[1,] #1행 추출
exam[2,]

#조건을 충촉하는 행 추출하기
exam[exam$class==1,] #class가 1인 행 추출

exam[exam$math >= 80,]
exam[exam$class == 1 & exam $math >= 50,]
exam[exam$english > 90 | exam$science < 50,]

#열 변호로 변수 추출하기
exam[,1] #첫 번째 열 추출
exam[,2]
exam[,3]

#변수명으로 변수 추출하기
exam[,"class"] #class 변수 추출
exam[, "math"]
exam[,c("class", "math", "english")]

#행, 변수 동시 추출하기
exam[1,3]
exam[5, "english"]
exam[exam$math >= 50, "english"]
exam[exam$math >= 50, c("english", "science")]

#내장 함수 코드
exam$tot = (exam$math + exam$english + exam$science)/3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean)
#dplyr 코드
library(dplyr)
exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate(tot = (math + english + science)/3) %>% 
  group_by(class) %>%
  summarise(mean = mean(tot))

#혼자서 해보기----------------------------------------
mpg = as.data.frame(ggplot2::mpg)
View(mpg)

#내장함수
mpg$tot = (mpg$cty + mpg$hwy)/2 #통합 연비 변수 만들기
df_comp = mpg[mpg$class == "compact",] #compact 추출
df_suv = mpg[mpg$class == "suv",]
mean(df_comp$tot) #compact의 tot 평균 산출
mean(df_suv$tot)

#dplyr
mpg %>% 
  mutate(tot = (cty + hwy)/2) %>% 
  filter(class == c("suv", "compact")) %>% 
  group_by(class) %>% 
  summarise(mean = mean(tot))

#################
##변수 타입
################
#함수에 따라 적용 가능한 변수 타입 다름
#분석 전에 변수 타입이 무엇인지 확인 필요
#함수 실행했을 때 오류 발생 또는 예상과 다른 결과가 출력되면 변수 타입 확인 후 함수에 맞게 변경

#변수 종류: 연속변수, 범주변수
#연속 변수(Numeric 타입): 값이 연속적, 크기를 의미. 산술 가능 ex)키, 몸무게
#범주변수(Factor 타입) : 값이 대상을 분류하는 의미. 산술 불가능 ex)성별, 거주지

#변수 타입 간 차이 알아보기
var1 = c(1,2,3,1,2) #numeric 변수 생성
var2 = factor(c(1,2,3,1,2)) #factor 변수 생성

var1
var2 #Levels는 형태를 의미.

var1+2 #numeric 변수로 연산 _ 가능
var2+2 #factor 변수로 연산 _ 불가능

#변수 타입 확인하기
class(var1)
class(var2)

#factor 변수의 구성 범주 확인하기
levels(var1) #NULL이라고 나옴
levels(var2)

#문자로 구성된 factor 변수
var3 = c("a", "b", "b", "c") #문자 변수 생성
var4 = factor(c("a", "b", "b", "c")) #문자로 된 factor 변수 생성

var3
var4

class(var3) #character라고 나옴
class(var4) #factor라고 나옴

#함수마다 적용 가능한 변수 타입이 다름
mean(var1)
mean(var2) #불가능

#변수 타입 바꾸기
var2 = as.numeric(var2) #numeric타입으로 변환
mean(var2) 
class(var2) #타입확인
levels(var2) #번주확인 _ NULL이라고 뜸

#변환함수
#as.numeric, as.factor, as.charactor, as.Date, as.data.frame

#혼자서 해보기-----------------------------------
mpg = as.data.frame(ggplot2::mpg)
View(mpg)

class(mpg$drv)
drv = as.factor(mpg$drv)
class(drv)
levels(drv)

###############
##데이터 구조
###############
#데이터 프레임외에도 다양한 데이터 구조 존재.

#벡터 : 하나 또는 여러 개의 값으로 구성된 데이터 구조
#여러 타입을 섞을 수 없고, 한가지 타입으로만 구성 가능
#벡터 만들기
a = 1
a
b = "hello"
b
#데이터 구조 확인
class(a) #numeric이라고 나옴
class(b) #character이라고 나옴

#데이터 프레임 : 행과 열로 구성된 2차원 데이터 구조
#다양한 변수 타입으로 구성 가능
x1 = data.frame(var1 = c(1,2,3),
                var2 = c("a", "b", "c"))
x1
class(x1) #data.frame이라고 나옴

#매트릭스(Matrix) : 행과 열로 구성된 2차원 데이터 구조
#한 가지 타입으로만 구성 가능
# 매트릭스 만들기 - 1~12 로 2 열
x2 = matrix(c(1:12), ncol = 2)
x2
class(x2) #matrix라고 나옴

#어레이(Array) : 2차원 이상으로 구성된 매트릭스
#한 가지 타입으로만 구성 가능
# array 만들기 - 1~20 으로 2 행 x 5 열 x 2 차원
x3 = array(1:20, dim = c(2, 5, 2))
x3

#리스트(List) : 모든 데이터 구조를 포함하는 데이터 구조
#여러 데이터 구조를 합해 하나의 리스트로 구성 가능
#리스트 생성 - 앞에서 생성한 데이터 구조 활용
x4 = list(f1 = a, #벡
          f2 = x1, #데이터 프레임
          f3 = x2, #매트릭스
          f4 = x3) #어레이
x4

#리스트 활용
#함수의 결과물이 리스트 형태로 반환되는 경우 많음
#리스트를 활용하면 함수의 결과물에서 특정 값을 추출 가능
#boxplot() 출력 결과물에서 값 추출하기
mpg = ggplot2::mpg
x = boxplot(mpg$cty)
x

x$stats[,1] #요약 통계량 추출
x$stats[,1][3] #중앙값 추출
x$stats[,1][2] #1분위수 추출

#혼자서 해보기--------------------------------
BOD
class(BOD)
a = boxplot(BOD$demand)
a
a$stats[,1]
a$stats[,1][3]
a$stats[,1][4]

