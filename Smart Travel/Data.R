library(jsonlite)
library(curl)
library(readr)
library(shiny)
library(leaflet)
library(magrittr)
library(rgdal)
library(rgeos)
library(rvest)

# WIFI spot information
wifi_spot=read_csv("wifi.csv")
# WIFI icon
wifi_spot_icon=makeIcon(
  iconUrl="wifi_icon.png"
)

# Charging station information
charge_spot=read_csv("charge.csv")
# Charging station icon
charge_spot_icon=makeIcon(
  iconUrl="charge_icon.png"
)

# Activity information
activity <- fromJSON("https://gis.taiwan.net.tw/XMLReleaseALL_public/activity_C_f.json")
activity <- activity[["XML_Head"]][["Infos"]][["Info"]]
activity <- fromJSON("http://gis.taiwan.net.tw/XMLReleaseALL_public/activity_C_f.json")
activity <- activity[["XML_Head"]][["Infos"]][["Info"]]
activity$Participation[activity$Participation==""] <- "一般民眾" 
activity$Add <- paste(activity$Add, activity$Location)
activity <- activity[substr(activity$Start, 1, 4)=="2019",] 
activity$Start <- substr(activity$Start, 1, 10)    
activity.s <- data.frame(Name=activity$Name, Add=activity$Add, Tel=activity$Tel, 
                         Participation=activity$Participation, Start=activity$Start, Org=activity$Org, 
                         Description=activity$Description, lng=activity$Px, lat=activity$Py, 
                         Website=activity$Website)
activity.s=data.frame(lapply(activity.s,as.character),stringsAsFactors=FALSE)
for (i in 1:length(activity.s$Website)){
  if(activity.s$Website[i]==""){
    activity.s$Website[i]="www.google.com.tw"
  }
}
for (i in 1:length(activity.s$Tel)){
  if(activity.s$Tel[i]==""){
    activity.s$Tel[i]="無"
  }
}
# Activity icon
event_spot_icon=makeIcon(
  iconUrl="events.png"
)
# Spot information
spot <- fromJSON("https://gis.taiwan.net.tw/XMLReleaseALL_public/scenic_spot_C_f.json")
spot <- spot[["XML_Head"]][["Infos"]][["Info"]]
spot.s <- data.frame(Name=spot$Name, Add=spot$Add, Tel=spot$Tel, Ticketinfo=spot$Ticketinfo,                     
                     Opentime=spot$Opentime, Description=spot$Description, lng=spot$Px, 
                     lat=spot$Py, Class=spot$Class1) 

spot.s=data.frame(lapply(spot.s,as.character),stringsAsFactors=FALSE)
for (i in 1:length(spot.s$Class)){
  if(spot.s$Class[i]=="01"){
    spot.s$Class[i]=1
  }else if(spot.s$Class[i]=="02"){
    spot.s$Class[i]=2
  }else if(spot.s$Class[i]=="03"){
    spot.s$Class[i]=1
  }else if(spot.s$Class[i]=="04"){
    spot.s$Class[i]=3
  }else if(spot.s$Class[i]=="05"){
    spot.s$Class[i]=4
  }else if(spot.s$Class[i]=="06"){
    spot.s$Class[i]=5
  }else if(spot.s$Class[i]=="07"){
    spot.s$Class[i]=2
  }else if(spot.s$Class[i]=="08"){
    spot.s$Class[i]=2
  }else if(spot.s$Class[i]=="09"){
    spot.s$Class[i]=6
  }else if(spot.s$Class[i]=="10"){
    spot.s$Class[i]=7
  }else if(spot.s$Class[i]=="11"){
    spot.s$Class[i]=2
  }else if(spot.s$Class[i]=="12"){
    spot.s$Class[i]=8
  }else if(spot.s$Class[i]=="13"){
    spot.s$Class[i]=9
  }else if(spot.s$Class[i]=="14"){
    spot.s$Class[i]=10
  }else if(spot.s$Class[i]=="15"){
    spot.s$Class[i]=11
  }else{
    spot.s$Class[i]=12
  }
}


for (i in 1:length(spot.s$Class)){
  if((as.numeric(spot.s$Class[i])==1)){
    spot.s$Class[i]="Cultural monuments"
  }else if((as.numeric(spot.s$Class[i])==2)){
    spot.s$Class[i]="Ecology"
  }else if(as.numeric(spot.s$Class[i])==3){
    spot.s$Class[i]="Temple"
  }else if(as.numeric(spot.s$Class[i])==4){
    spot.s$Class[i]="Crafting"
  }else if(as.numeric(spot.s$Class[i])==5){
    spot.s$Class[i]="Shopping area/Night market"
  }else if(as.numeric(spot.s$Class[i])==6){
    spot.s$Class[i]="Farm"
  }else if(as.numeric(spot.s$Class[i])==7){
    spot.s$Class[i]="Spring"
  }else if(as.numeric(spot.s$Class[i])==8){
    spot.s$Class[i]="Entertainment"
  }else if(as.numeric(spot.s$Class[i])==9){
    spot.s$Class[i]="Sports"
  }else if(as.numeric(spot.s$Class[i])==10){
    spot.s$Class[i]="Tourist"
  }else if(as.numeric(spot.s$Class[i])==11){
    spot.s$Class[i]="Memorial Park"
  }else{
    spot.s$Class[i]="Others"
  }
}

spot.s[is.na(spot.s)] <- "None"
for (i in 1:4546) {
  if(spot.s[i,5]=="")
    spot.s[i,5]="None"
}
# Spot icon
tour_spot_icon=makeIcon(
  iconUrl="tour_spot.png"
)

# Restaurant information
food <-fromJSON("https://gis.taiwan.net.tw/XMLReleaseALL_public/restaurant_C_f.json")
food <- food[["XML_Head"]][["Infos"]][["Info"]]
fooduse <- data.frame(food$Name,food$Add,food$Tel,food$Opentime,food$Px,food$Py,food$Description)
colnames(fooduse) <- c("Name","Add","Tel","Opentime","lng","lat","Description")
fooduse <- fooduse[-(142:210),]
# Restaurant icon
food_icon=makeIcon(
  iconUrl="food.png"
)

# Locker information
# Lockers at Subway and train stations 
trk.locker <- read.delim("台鐵置物櫃.csv",header=TRUE,sep=",")
# Taipei Subway information
mrt.locker <- read.csv("北捷置物櫃.csv",header=TRUE,sep=",")
html <- read_html("https://web.metro.taipei/c/lockerinfo2018.asp")
mrt.locker.data <- html_text(html_nodes(html, "tr td"))
mrt.locker.data <- mrt.locker.data[(which(substr(mrt.locker.data,9,10)=="公分"))+2]
mrt.locker.data <- as.numeric(mrt.locker.data[-21])
mrt.locker <- cbind(mrt.locker,可用格數=mrt.locker.data)
mrt.locker=data.frame(lapply(mrt.locker,as.character),stringsAsFactors=FALSE)

money <- ""
size <- ""
n <- ""
usen <- ""
a <- which(mrt.locker$站別=="")
money[a-1] <- mrt.locker$收費[a]
size[a-1] <- mrt.locker$尺寸[a]
n[a-1] <- mrt.locker$數量[a] 
usen[a-1] <- mrt.locker$可用格數[a]
money <- c(money,"")
size <- c(size,"")
n <- c(n,"")
usen <- c(usen,"")
mrt.locker1 <- cbind(mrt.locker,money,size,n,usen)
money1 <- NA
size1 <- NA
n1 <- NA
usen1 <- NA
for(i in 1:50){
  if(i==19){
    money1[19] <- mrt.locker$收費[i+2]
    size1[19] <- mrt.locker$尺寸[i+2]
    n1[19] <- mrt.locker$數量[i+2] 
    usen1[19] <- mrt.locker$可用格數[i+2]
  }else{
    money1[i] <- ""
    size1[i] <- ""
    n1[i] <- ""
    usen1[i] <- ""
  }
}
mrt.locker2 <- cbind(mrt.locker1,money1,size1,n1,usen1)
mrt.locker2 <- mrt.locker2[!(mrt.locker2$站別==""),]
mrt.locker2[is.na(mrt.locker2)] <- ""
# Locker icon
locker_spot_icon=makeIcon(
  iconUrl="locker_icon.png"
)