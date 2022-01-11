server=function(input,output){
  proxy=leafletProxy("map_id")
  output$map_id=renderLeaflet(map)
  observe({
    click=input$map_id_shape_click
    if(is.null(click$id))
      return()
    proxy%>%clearGroup(group=c("selected_city","selected_town_district"))%>%
            clearMarkers()%>%
            addLabelOnlyMarkers(lat=as.numeric(city[,3]),lng=as.numeric(city[,4]),label=city[,1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='grey','font-size'='10px')))
    selected=NULL
    selected=city_boundary[city_boundary$COUNTYENG==click$id,]
    proxy%>%addPolygons(data=selected,color="blue",weight=3,fillOpacity=0,group="selected_city")
    if(click$id=="Taichung City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Taichung,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="台中市"),3]),lng=as.numeric(city[which(city[,1]=="台中市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="B"),5]),lng=as.numeric(town[which(town[,4]=="B"),6]),label=town[which(town[,4]=="B"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="台中市"),3]),lng=as.numeric(city[which(city[,1]!="台中市"),4]),label=city[which(city[,1]!="台中市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Taoyuan City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Taoyuan,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="桃園市"),3]),lng=as.numeric(city[which(city[,1]=="桃園市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="H"),5]),lng=as.numeric(town[which(town[,4]=="H"),6]),label=town[which(town[,4]=="H"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="桃園市"),3]),lng=as.numeric(city[which(city[,1]!="桃園市"),4]),label=city[which(city[,1]!="桃園市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Taipei City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Taipei,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="台北市"),3]),lng=as.numeric(city[which(city[,1]=="台北市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="A"),5]),lng=as.numeric(town[which(town[,4]=="A"),6]),label=town[which(town[,4]=="A"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="台北市"),3]),lng=as.numeric(city[which(city[,1]!="台北市"),4]),label=city[which(city[,1]!="台北市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }
    else if(click$id=="Tainan City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Tainan,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="台南市"),3]),lng=as.numeric(city[which(city[,1]=="台南市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="D"),5]),lng=as.numeric(town[which(town[,4]=="D"),6]),label=town[which(town[,4]=="D"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="台南市"),3]),lng=as.numeric(city[which(city[,1]!="台南市"),4]),label=city[which(city[,1]!="台南市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Kaohsiung City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Kaohsiung,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="高雄市"),3]),lng=as.numeric(city[which(city[,1]=="高雄市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="E"),5]),lng=as.numeric(town[which(town[,4]=="E"),6]),label=town[which(town[,4]=="E"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="高雄市"),3]),lng=as.numeric(city[which(city[,1]!="高雄市"),4]),label=city[which(city[,1]!="高雄市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="New Taipei City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_New_Taipei,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="新北市"),3]),lng=as.numeric(city[which(city[,1]=="新北市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="F"),5]),lng=as.numeric(town[which(town[,4]=="F"),6]),label=town[which(town[,4]=="F"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="新北市"),3]),lng=as.numeric(city[which(city[,1]!="新北市"),4]),label=city[which(city[,1]!="新北市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Taitung County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Taitung,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="台東縣"),3]),lng=as.numeric(city[which(city[,1]=="台東縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="V"),5]),lng=as.numeric(town[which(town[,4]=="V"),6]),label=c("成攻鎮","綠島鄉","蘭嶼鄉","鹿野鄉","池上鄉","延平鄉","金峰鄉","台東市","卑南鄉","東河鄉","長濱鄉","太麻里鄉","達仁鄉","大武鄉","關山鎮","海端鄉"),labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='grey','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="台東縣"),3]),lng=as.numeric(city[which(city[,1]!="台東縣"),4]),label=city[which(city[,1]!="台東縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Keelung City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Keelung,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="基隆市"),3]),lng=as.numeric(city[which(city[,1]=="基隆市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="C"),5]),lng=as.numeric(town[which(town[,4]=="C"),6]),label=town[which(town[,4]=="C"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='grey','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="基隆市"),3]),lng=as.numeric(city[which(city[,1]!="基隆市"),4]),label=city[which(city[,1]!="基隆市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='vlack','font-size'='10px')))
    }else if(click$id=="Yilan County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Yilan,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="宜蘭縣"),3]),lng=as.numeric(city[which(city[,1]=="宜蘭縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="G"),5]),lng=as.numeric(town[which(town[,4]=="G"),6]),label=town[which(town[,4]=="G"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="宜蘭縣"),3]),lng=as.numeric(city[which(city[,1]!="宜蘭縣"),4]),label=city[which(city[,1]!="宜蘭縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Hsinchu County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Hsinchu,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="新竹縣"),3]),lng=as.numeric(city[which(city[,1]=="新竹縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="J"),5]),lng=as.numeric(town[which(town[,4]=="J"),6]),label=town[which(town[,4]=="J"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="新竹縣"),3]),lng=as.numeric(city[which(city[,1]!="新竹縣"),4]),label=city[which(city[,1]!="新竹縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Changhua County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Changhua,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="彰化縣"),3]),lng=as.numeric(city[which(city[,1]=="彰化縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="N"),5]),lng=as.numeric(town[which(town[,4]=="N"),6]),label=town[which(town[,4]=="N"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="彰化縣"),3]),lng=as.numeric(city[which(city[,1]!="彰化縣"),4]),label=city[which(city[,1]!="彰化縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Pingtung County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Pingtung,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="屏東縣"),3]),lng=as.numeric(city[which(city[,1]=="屏東縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="T"),5]),lng=as.numeric(town[which(town[,4]=="T"),6]),label=town[which(town[,4]=="T"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="屏東縣"),3]),lng=as.numeric(city[which(city[,1]!="屏東縣"),4]),label=city[which(city[,1]!="屏東縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Miaoli County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Miaoli,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="苗栗縣"),3]),lng=as.numeric(city[which(city[,1]=="苗栗縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="K"),5]),lng=as.numeric(town[which(town[,4]=="K"),6]),label=town[which(town[,4]=="K"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="苗栗縣"),3]),lng=as.numeric(city[which(city[,1]!="苗栗縣"),4]),label=city[which(city[,1]!="苗栗縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Hualien County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Hualien,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="花蓮縣"),3]),lng=as.numeric(city[which(city[,1]=="花蓮縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="U"),5]),lng=as.numeric(town[which(town[,4]=="U"),6]),label=town[which(town[,4]=="U"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="花蓮縣"),3]),lng=as.numeric(city[which(city[,1]!="花蓮縣"),4]),label=city[which(city[,1]!="花蓮縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Chiayi City"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Chiayi,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="嘉義市"),3]),lng=as.numeric(city[which(city[,1]=="嘉義市"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="I"),5]),lng=as.numeric(town[which(town[,4]=="I"),6]),label=town[which(town[,4]=="I"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="嘉義市"),3]),lng=as.numeric(city[which(city[,1]!="嘉義市"),4]),label=city[which(city[,1]!="嘉義市"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Chiayi County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Chiayi_County,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="嘉義縣"),3]),lng=as.numeric(city[which(city[,1]=="嘉義縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="Q"),5]),lng=as.numeric(town[which(town[,4]=="Q"),6]),label=town[which(town[,4]=="Q"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="嘉義縣"),3]),lng=as.numeric(city[which(city[,1]!="嘉義縣"),4]),label=city[which(city[,1]!="嘉義縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Yunlin County"){
      proxy%>%clearMarkers()%>%addPolygons(data=poly_Yunlin,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="雲林縣"),3]),lng=as.numeric(city[which(city[,1]=="雲林縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="P"),5]),lng=as.numeric(town[which(town[,4]=="P"),6]),label=town[which(town[,4]=="P"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="雲林縣"),3]),lng=as.numeric(city[which(city[,1]!="雲林縣"),4]),label=city[which(city[,1]!="雲林縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Nantou County"){
      proxy%>%clearMarkers()%>%addPolygons(data=Poly_Nantou,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="南投縣"),3]),lng=as.numeric(city[which(city[,1]=="南投縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="M"),5]),lng=as.numeric(town[which(town[,4]=="M"),6]),label=town[which(town[,4]=="M"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="南投縣"),3]),lng=as.numeric(city[which(city[,1]!="南投縣"),4]),label=city[which(city[,1]!="南投縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Penghu County"){
      proxy%>%clearMarkers()%>%addPolygons(data=Poly_Penghu,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="澎湖縣"),3]),lng=as.numeric(city[which(city[,1]=="澎湖縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="X"),5]),lng=as.numeric(town[which(town[,4]=="X"),6]),label=town[which(town[,4]=="X"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=   as.numeric(city[which(city[,1]!="澎湖縣"),3]),lng=as.numeric(city[which(city[,1]!="澎湖縣"),4]),label=city[which(city[,1]!="澎湖縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Kinmen County"){
      proxy%>%clearMarkers()%>%addPolygons(data=Poly_Kinmen,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="金門縣"),3]),lng=as.numeric(city[which(city[,1]=="金門縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="W"),5]),lng=as.numeric(town[which(town[,4]=="W"),6]),label=town[which(town[,4]=="W"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="金門縣"),3]),lng=as.numeric(city[which(city[,1]!="金門縣"),4]),label=city[which(city[,1]!="金門縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else if(click$id=="Lienchiang County"){
      proxy%>%clearMarkers()%>%addPolygons(data=Poly_Lienchiang,color="blue",weight=1,fillOpacity=0,group="selected_town_district")%>%
        setView(lat=as.numeric(city[which(city[,1]=="連江縣"),3]),lng=as.numeric(city[which(city[,1]=="連江縣"),4]),zoom=11)%>%
        addLabelOnlyMarkers(lat=as.numeric(town[which(town[,4]=="Z"),5]),lng=as.numeric(town[which(town[,4]=="Z"),6]),label=town[which(town[,4]=="Z"),2],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
        addLabelOnlyMarkers(lat=as.numeric(city[which(city[,1]!="連江縣"),3]),lng=as.numeric(city[which(city[,1]!="連江縣"),4]),label=city[which(city[,1]!="連江縣"),1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))
    }else return()
  })
  observe({
    input$reset
    proxy%>%setView(lng=120.979791,lat=23.613986,zoom=8)%>%clearGroup(group=c("selected_city","selected_town_district"))%>%clearMarkers()%>%addLabelOnlyMarkers(lat=as.numeric(city[,3]),lng=as.numeric(city[,4]),label=city[,1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='grey','font-size'='10px')))
  })
  observe({
    input$service
    if(input$service=="WIFI"){
      proxy%>%clearMarkerClusters()%>%
      clearGroup(group="selected_town_district")%>%  
      setView(lng=120.979791,lat=23.613986,zoom=8)%>%  
      addMarkers(lat=wifi_spot$lat,lng=wifi_spot$lng,icon=wifi_spot_icon,clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE),popup=paste("<h4>Wi-Fi</h4>","Spot Name:",wifi_spot$熱點名稱,"<br>","Address:",wifi_spot$地址))
    }else if(input$service=="Charging station"){
      proxy%>%clearMarkerClusters()%>%
      setView(lng=120.979791,lat=23.613986,zoom=8)%>%   
      addMarkers(lat=charge_spot$lat,lng=charge_spot$lng,icon=charge_spot_icon,clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE),popup=paste("<h4>Charging station</h4>",
                                                                                                                                                        "Name:", charge_spot$充電站名稱, "<br>",
                                                                                                                                                        "Address:", charge_spot$地址, "<br>"))}else if(input$service=="Locker"){
      proxy%>%clearMarkerClusters()%>%
      setView(lng=120.979791,lat=23.613986,zoom=8)%>%                                                                                                                                                      
      addMarkers(lat=trk.locker$lat,lng=trk.locker$lng,icon=locker_spot_icon,clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE),popup=paste("<h4>","Locker","</h4>","Location：",trk.locker$地點,"<br>","<table border=1><tr><th>Charge</td><td>Size</th></tr>","<tr><td>",trk.locker$收費1,"</td><td>",trk.locker$尺寸1,"</td></tr>","<tr><td>",trk.locker$收費2,"</td><td>",trk.locker$尺寸2,"</td></tr>","<tr><td>",trk.locker$收費3,"</td><td>",trk.locker$尺寸3,"</td></tr>","<tr><td>",trk.locker$收費4,"</td><td>",trk.locker$尺寸4,"</td></tr>","<tr><td>",trk.locker$收費5,"</td><td>",trk.locker$尺寸5,"</td></tr></table>"))%>%
      addMarkers(lat=as.numeric(mrt.locker2[,9])
                   ,lng=as.numeric(mrt.locker2[,8])
                   ,icon=locker_spot_icon
                   ,popup = paste("<h4>Locker</h4>",
                                  "Station:",mrt.locker2$站別,"<br>",
                                  "Location:",mrt.locker2$地點,"<br>",
                                  "<table border=1><tr><th>Charge</th><th>Size</th><th>Number</th><th>Available</th></tr>",
                                  "<tr><td>",mrt.locker2$收費,"</td><td>",mrt.locker2$尺寸,"</td><td>",mrt.locker2$數量,"</td><td>",mrt.locker2$可用格數,"</td></tr>",
                                  "<tr><td>",mrt.locker2$money,"</td><td>",mrt.locker2$size,"</td><td>",mrt.locker2$n,"</td><td>",mrt.locker2$usen,"</td></tr>",
                                  "<tr><td>",mrt.locker2$money1,"</td><td>",mrt.locker2$size1,"</td><td>",mrt.locker2$n1,"</td><td>",mrt.locker2$usen1,"</td></tr>",
                                  "</td></tr></table>")
                   ,clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE))
    }else if(input$service=="None"){
      proxy%>%clearMarkerClusters()%>%
      setView(lng=120.979791,lat=23.613986,zoom=8)
    }else if(input$service=="Spot"){
      proxy%>%clearMarkerClusters()%>%
        setView(lng=120.979791,lat=23.613986,zoom=8)%>% 
        addMarkers(lat=as.numeric(spot.s$lat)
                   ,lng=as.numeric(spot.s$lng)
                   ,icon=tour_spot_icon
                   ,popup = paste("<h4>Spot</h4>",
                                  "Name:",spot.s$Name,"<br>",
                                  "Address:",spot.s$Add,"<br>",
                                  "Phone:",spot.s$Tel,"<br>",
                                  "Charge:",spot.s$Ticketinfo,"<br>",
                                  "Opening time:",spot.s$Opentime,"<br>",
                                  "Spot description:",spot.s$Description,"<br>",
                                  "Spot classification",spot.s$Class,"<br>")
                   ,clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE))
         }else if(input$service=="Activity"){
           proxy%>%clearMarkerClusters()%>%
             setView(lng=120.979791,lat=23.613986,zoom=8)%>% 
             addMarkers(lat=as.numeric(activity.s$lat)
                        ,lng=as.numeric(activity.s$lng)
                        ,icon=event_spot_icon
                        ,popup = paste("<h4>Activity</h4>",
                                       "Name:",activity.s$Name,"<br>",
                                       "Location:",activity.s$Add,"<br>",
                                       "Attendees:",activity.s$Participation,"<br>",
                                       "Starting time",activity.s$Start,"<br>", 
                                       "Host:",activity.s$Org,"<br>",
                                       "Activity description:",activity.s$Description,"<br>",
                                       "Website:","<a>",activity.s$Website,"</a>","<br>"),
                        clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE))
         }else if(input$service=="Restaurant"){
           proxy%>%clearMarkerClusters()%>%
             setView(lng=120.979791,lat=23.613986,zoom=8)%>% 
             addMarkers(lat=fooduse$lat
                        ,lng=fooduse$lng
                        ,icon=food_icon
                        ,popup = paste("<h4>Food</h4>",
                                       "Store name:",fooduse$Name,"<br>",
                                       "Address:",fooduse$Add,"<br>",
                                       "Phone:",fooduse$Tel,"<br>",
                                       "Opening time:",fooduse$Opentime,"<br>",
                                       "Description:",fooduse$Description,"<br>")
                        ,clusterOptions=markerClusterOptions(showCoverageOnHover=FALSE))
         }
           else return()
  })
}

