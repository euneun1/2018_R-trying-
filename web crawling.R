##############
##실시간 검색어 가져오기--------------------------------------
##############
library(rvest)
library(ggplot2)
library(xml2)

#크롤링 연습: naver_html로 읽어오기
naver_html = read_html("http://www.naver.com")

#검색어순위 끌어오기(어떤 페이지에 있는 무언가). 특정 테그나 특정 요소의 값만 추출
#node라는 변수를 만듦. html_node는 함수 이름. 뒤에는 s가 붙음. s붙으면 여러개 찾음. node는 하나만 잡음. naver_html에서 .ah_1을 1개만 찾아라. %>%  그 찾은 것들 중에 .ah_k라는 것들을 찾아라. 앞부분은 글자만 잡는게 아니라 글자를 잡는 모든 요소(코드들)를 잡기
naver_node = html_node(naver_html, ".ah_l") %>% html_nodes(".ah_k")
naver_node

#글자만 추출하기
html_text(naver_node)

##정리
#read_html() 을 통해서 xml문서 읽기
#html_nodes()를 통해서 특정 태그나 속성 값을 갖는 요소만 추출. 다른 곳에 그 요소들이 있을 수 있어서 앞에 html_node 써야됨.
#html_text()를 통해 위 요소 내 text만 추출

##############
##영화 리플 읽어오기-----------------------------------------
###############
#(자기가 자동으로 페이지를 바꿔옴. 위는 한페이지만 긁어옴.)
#읽어올 페이지 파악
#구글 크롬 권장 개발자 도구

movie_url = "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=140652&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
#맨뒤 page = 2인데 2 지우기

movie_url1 = paste0(movie_url, "2")
movie_url1

#페이지 자동 변환해서 읽어오기
#페이지 번호 생성
page = c(1:100)

#변수 만들기
alltxt = c()

#페이지 자동으로 읽어오기
for (i in page){
  movie_url1 = paste0(movie_url, i)
  movie_html = read_html(movie_url1)
  movie_node = html_nodes(movie_html, css=".score_result") %>% 
    html_nodes("p")
  movie_txt = html_text(movie_node)
  movie_alltxt = append(alltxt,movie_txt)
  print(i)
} #i가 바뀜. i가 page 1부터 100까지 바뀜. 

head(movie_alltxt)

###분석하기
#문제점 많음
#자바 설치 jdk 7

#KoNLP은 한글을 분석하는 패키지. 이건 자바를 이용함. 옛날꺼라서 java7을 깜.
#oracle.com/java

