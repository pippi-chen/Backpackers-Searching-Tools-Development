#UI
ui=bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map_id",width = "100%", height = "100%"),
    absolutePanel(top=15,right=20,width="15%",height="100%",
                 uiOutput("test"),actionButton("reset","reset map"),tags$br(),
                 tags$br(),
                 radioButtons("service","Please select:",choices=c("None","WIFI","Charging station","Locker","Spot","Activity","Restaurant")
                 )
))

