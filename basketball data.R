library(xml2)
library(rvest)
library(tidyverse)
site1 <- "https://www.basketball-reference.com/leagues/NBA_"
site2 <- "_games-"
site3 <- ".html"
year <- c(2013:2018)
month <- c("january","february","march","april","may","june","october","november","december")
month_index<-c(1:9)
year_index<-c(1:6)
name1<-c()
name2<-c()
total.date<-c()
total.attdence<-c()
for (i in year_index){
  for (j in month_index){
    site<-paste(site1,year[i],site2,month[j],site3,sep="")
    webpage<-read_html(site)
    name1<- webpage %>% html_nodes('.left:nth-child(1)') %>% html_attrs()
    name2<- webpage %>% html_nodes('.center+.right') %>% html_text()
    total.date <- c(total.date,name1)
    total.attdence <- c(total.attdence,name2)
    j<-j+1
  }
  i<-i+1
}
total.date1 <- t(data.frame((total.date)))[,4]
total.attdence1 <- data.frame(total.attdence)
b_data<-cbind(total.date1,total.attdence1)[-1,]
colnames(b_data)<-c("date","attendance")
rownames(b_data)<-rep(1:7934)
host<-substr(b_data$date, 10, 12)
b_data$date<-substr(b_data$date,1,9)
b_data3<-cbind(b_data,host)
write.csv(x = b_data3,file = "basketball.csv")

