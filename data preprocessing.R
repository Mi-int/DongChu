library(readxl)
library(dplyr)

a = read_xlsx("구병원.xlsx") #시군구코드명
b = read_xlsx("구공원.xlsx") #자치구별(2)
c = read_xlsx("구경찰서.xlsx")
d = read_xlsx("구도서관.xlsx")
e = read_xlsx("구지하철.xlsx")
f = read_xlsx("구별 집값 데이터.xlsx")
g = read_xlsx("동별 집값 데이터.xlsx")
h = read_xlsx("구범죄.xlsx")

i = read_xlsx("동대형마트.xlsx")
j = read_xlsx("동편의점.xlsx")
k=read_xlsx("보건소.xlsx")
l=read_xlsx("일.xlsx")
m=read_xlsx("구 cctv.xlsx")
all=read_xlsx("총괄파일1.xlsx")

#--------------------------------------#
aa = a[,-2]
aaa = table(aa$시군구코드명)
hospital = data.frame(aaa)
colnames(hospital) = c("자치구별","병원개수")

park = b[-c(1,2),c(2,4)]
colnames(park) = c("자치구별","공원개수")

c1 = c$`2020...180`
c2 = c$`2020...181`
c3 = c$`자치구별(2)`
cc = data.frame(c3,c1,c2)
police = cc[-c(1,2,28),]
police=l[,c(1,4)]
colnames(police) = c("자치구별","경찰서수")


dd = subset(d, select = c("자치구별(2)","2020"))
dd = dd[-1,]
library = na.omit(dd)
colnames(library) = c("자치구별","도서관개수")

subway = e[-c(26:30),-c(1,3)]
colnames(subway) = c("자치구별","역개수")

price = f[,-c(3,4)]
colnames(price) = c("자치구별","평당가")

crime = h[-c(1:4),c(2:3)]
colnames(crime) = c("자치구별","범죄발생")




#-마트
i<-i[,16]
i1<-as.character(i)
i2<-strsplit(i1, " " )
i2<-as.data.frame(i2)
i3<-i2[grep("구$",i2$c..c.NA.......서울특별시....양천구....신정동....1297......),]
i4<-as.data.frame(i3)
mart<-table(as.data.frame(i4$i3))
mart<-as.data.frame(mart)
colnames(mart)<-c("자치구별","대형마트수")

#-편의점
j<-j[,2]
j1<-as.character(j)
j2<-strsplit(j1," ")
j2<-as.data.frame(j2)
j3<-j2[grep("구$",j2$c..c...서울특별시....송파구....방이동....89....올림픽선수기자촌아파트......),]
j4<-as.data.frame(j3)
j5<-table(as.data.frame(j4$j3))
j5<-as.data.frame(j5)
store<-j5[-c(1,6,16),]
colnames(store)<-c("자치구별","편의점수")
#-보건소

health<-k[-1,c(2,6)]
colnames(health)<-c("자치구별","보건소수")

m<-table(m[,1])
as.numeric(m)
cctv<-as.data.frame(m)
colnames(cctv)<-c("자치구별","cctv수")
#----------

all = full_join(library,subway,by='자치구별')%>%
  full_join(.,park,by='자치구별')%>%
  full_join(.,police,by='자치구별')%>%
  full_join(.,hospital,by='자치구별')%>%
  full_join(.,crime,by='자치구별')%>%
  full_join(.,price,by='자치구별')%>%
  full_join(.,mart,by='자치구별')%>%
  full_join(.,store,by='자치구별')%>%
  full_join(.,health,by='자치구별')%>%
  full_join(.,cctv,by='자치구별')

dat2 <- max(all$범죄발생) - all$범죄발생

install.packages("writexl")
library(writexl)
write_xlsx(all,path = "/cloud/project/dongchu/총괄파일1.xlsx")


# 도서관 비율 

dat <- read_excel("dongchu.xlsx")
View(dat)

table(dat$도서관개수)
dat$도서관개수
dat<-full_join(dat,cctv,by='자치구별')%>%
  full_join(.,cctv,b)

prop.table(dat$공원개수)*100


#------------------평균평단가 추가가

dat<-read_xlsx("dong.xlsx")


mean(dat$평당가)
mee<-ifelse(dat$평당가>=2808.68,'1','0')
mee<-as.numeric(mee)
dat$평균평당가<-mee
