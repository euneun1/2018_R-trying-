###########################
#d3 네트워크를 통해 관련성을 그림으로 나타내줌.
##########################
install.packages("devtools")
library(devtools)
install_github("christophergandrud/d3Network") #깃허브에 야매로 올려놓은 앱


library(RCurl) #URI를 가져 오는 등, 도착 및 게시물 양식 및 웹 서버에 의해 반환 된 결과를 처리하는 편리한 기능을 제공
library(d3Network)

Source<-c("A","A","A","A","B","B","C","C","D")
Target<-c("B","C","D","J","E","F","G","H","I")
NetworkData<-data.frame(Source,Target) #source와 Target 데이터프레임 생성

NetworkData

# NetworkData는 네트워크의 연결정보를 저장하고 있는 data.frame형태이다. Source열에서 Target열로 노드가 연결되어있음을 나타낸다.

# setwd("사용자지정경로")

# 이 함수는 작업디렉토리를 지정하는 함수로 밑에 d3SimpleNetwork()에서 생성될 파일의 경로를 지정하려고 사용한다.

d3SimpleNetwork(NetworkData, width=400, height=250, file="test1.html") #오른쪽 Files 에서 오른쪽 마우스 클릭후 View in WebBrowser하면 창 새로 뜨면서 표현됨.(거미줄처럼)

d3SimpleNetwork(NetworkData, width = 400, height = 250, textColour = "orange", linkColour = "red", nodeColour = "orange", opacity = 0.9, file="test2.html")

d3SimpleNetwork(NetworkData, width = 400, height = 250, textColour = "#D95F0E", linkColour = "#FEC44F",
                nodeColour = "#D95F0E", opacity = 0.9, charge = -50, fontsize = 12, file="test3.html")

d3SimpleNetwork(NetworkData, height = 300, width = 700, fontsize = 15, file="test4.html")