# County and city boundaries information
city_boundary=readOGR("GEOfile",layer="COUNTY_MOI_1070516")
# Township area boundaries information
town_boundary=readOGR("GEOfile_detail",layer="TOWN_MOI_1071226")
# Normal County and city boundaries information
city=read.csv("city.csv",header=TRUE)
# Normal Township area boundaries information
town=read.csv("town.csv",header=TRUE)

# Draw base Map
map=leaflet(options=leafletOptions(minZoom=8))%>%
  addProviderTiles(providers$Hydda.Base)%>%
  addPolygons(data=city_boundary,weight=1,layerId=as.character(city_boundary$COUNTYENG),color="blue",fillOpacity=0)%>%
  addLabelOnlyMarkers(lat=as.numeric(city[,3]),lng=as.numeric(city[,4]),label=city[,1],labelOptions=labelOptions(noHide=TRUE,direction='top',textOnly=TRUE,style=list('color'='black','font-size'='10px')))%>%
  setView(lng=120.979791,lat=23.613986,zoom=8)%>%
  setMaxBounds(lng1=115.074034,lat1=21.320993,lng2=126.885548,lat2=26.626979)
  
 

# POLYGON

# 台中市
poly_Taichung=town_boundary[town_boundary$COUNTYID=="B",]
# 桃園市
poly_Taoyuan=town_boundary[town_boundary$COUNTYID=="H",]
# 台北市
poly_Taipei=town_boundary[town_boundary$COUNTYID=="A",]
# 台南市
poly_Tainan=town_boundary[town_boundary$COUNTYID=="D",]
# 高雄市
poly_Kaohsiung=town_boundary[town_boundary$COUNTYID=="E",]
# 新北市
poly_New_Taipei=town_boundary[town_boundary$COUNTYID=="F",]
# 臺東縣
poly_Taitung=town_boundary[town_boundary$COUNTYID=="V",]
# 基隆市
poly_Keelung=town_boundary[town_boundary$COUNTYID=="C",]
# 宜蘭縣
poly_Yilan=town_boundary[town_boundary$COUNTYID=="G",]
# 新竹縣
poly_Hsinchu=town_boundary[town_boundary$COUNTYID=="J",]
# 彰化縣
poly_Changhua=town_boundary[town_boundary$COUNTYID=="N",]
# 屏東縣
poly_Pingtung=town_boundary[town_boundary$COUNTYID=="T",]
# 苗栗縣
poly_Miaoli=town_boundary[town_boundary$COUNTYID=="K",]
# 花蓮縣
poly_Hualien=town_boundary[town_boundary$COUNTYID=="U",]
# 嘉義市
poly_Chiayi=town_boundary[town_boundary$COUNTYID=="I",]
# 嘉義縣
poly_Chiayi_County=town_boundary[town_boundary$COUNTYID=="Q",]
# 雲林縣
poly_Yunlin=town_boundary[town_boundary$COUNTYID=="P",]
# 南投縣
Poly_Nantou=town_boundary[town_boundary$COUNTYID=="M",]
# 澎湖縣
Poly_Penghu=town_boundary[town_boundary$COUNTYID=="X",]
# 金門縣
Poly_Kinmen=town_boundary[town_boundary$COUNTYID=="W",]
# 連江縣
Poly_Lienchiang=town_boundary[town_boundary$COUNTYID=="Z",]
