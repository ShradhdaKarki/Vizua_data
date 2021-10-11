library(shiny)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(tibble)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(maps)
library(tidyverse)
library(viridis)
library(tmap)
library(leaflet)
library(ggplot2)
library(tidytext)
library(tidylo)
library(urbnmapr)
library(ggmap)
library(sqldf)


beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')


  getdata<-function(top){
    beer_awards$STATE<-toupper(beer_awards$state)
  beer_awards$state<-NULL
  
  medal_Bronze <- sqldf("select STATE,count() as BRONZE
                        from beer_awards
                        where medal = 'Bronze'
                        group by STATE")
  
  
  medal_Silver <- sqldf("select STATE,count() as SILVER
                        from beer_awards
                        where medal = 'Silver'
                        group by STATE")
  
  medal_Gold <- sqldf("select STATE,count() as GOLD
                        from beer_awards
                        where medal = 'Gold'
                        group by STATE")
  
  
  medal_total<-merge(medal_Bronze,medal_Silver,all=TRUE)
  medal_total<-merge(medal_total,medal_Gold,all=TRUE)
  medal_total[is.na(medal_total)]<-0
  
  ### counting most occurring beers in the state using tidylo library
  beer_type<-beer_awards%>%
    unnest_tokens(beer,category,token="ngrams",n=2)
  
  if(top=="1"){
    new_tab<-beer_type%>%
    count(STATE,beer)%>%group_by(STATE)%>%top_n(1)
  }else if(top=="2"){
    new_tab<-beer_type%>%
      count(STATE,beer)%>%group_by(STATE)%>%top_n(2)
  }else if(top=="3"){
    new_tab<-beer_type%>%
      count(STATE,beer)%>%group_by(STATE)%>%top_n(3)
  } else {
    new_tab<-beer_type%>%
      count(STATE,beer)%>%group_by(STATE)%>%top_n(1)
  }
  
  
  
  ### counting most occurring beers in the state using tidylo library
  beer_type<-beer_awards%>%
    unnest_tokens(beer,category,token="ngrams",n=2)
  
  
  
  
  
  
  new_beerlabel<-new_tab%>%
    group_by(STATE)%>%
    summarise(beer=paste(beer,collapse="<br/>"))
  
  # count of total medals state wise
  total_medal<-beer_awards%>%
    count(STATE)
  
  
  
  # adding map elements
  Us_map<-get_urbn_map("states",sf=TRUE)
  
  new_map<-inner_join(Us_map,new_beerlabel,by=c('state_abbv'='STATE'))
  new_map<-inner_join(new_map,total_medal,by=c('state_abbv'='STATE'))
  
  
  addresses<-unique(new_map$address)
  new_map$geometry<-NULL
  
  new_map<-inner_join(new_map,medal_total,by=c('state_abbv'='STATE'))
  
  states <- geojsonio::geojson_read("gz_2010_us_040_00_500k.json", what = "sp")
  colnames(new_map)[3]="NAME"
  
  s<-merge(x=states,y=new_map,by='NAME',all.x=TRUE)
  
  
  
  
  #Create a final spatialPolygonsDataframe to create map
  
  require(sp)
  final_table<-merge(states,s,by="NAME")
  return (final_table)
  
}




top<- list("Top 1"="1",
           "Top 2"="2",
           "Top 3"="3")

medal<-list("GOLD","SILVER","BRONZE")


ui <- fluidPage(theme=shinytheme("cyborg"),
                # titlePanel("US and Their Top Beers"), # Application title
                # 
                # # task1: add in the sidebarLayout with sidebarPanel and mainPanel
                 sidebarLayout(
                  sidebarPanel(
                    h3("Most Popular Beers"),
                    selectInput("Selection","Beers List",top),
                    actionButton("RunButton","Run"),
                    hr(),hr(),hr(),
                    h3("Total Number of Medals"),
                    selectInput("Selection2","Total Medals",medal),
                    actionButton("Run2","Run")
                                                ),
                  mainPanel(
                    tabsetPanel(type="tabs",
                          tabPanel("Top Beers",leafletOutput("map","100%",1000)),
                          tabPanel("Medal Count",leafletOutput("map2","100%",1000))
                                        
                   )
                  
                  )
                 )
                
)


server <- function(input, output) {
  
  freq<-eventReactive(input$RunButton,withProgress({
    setProgress(message = "Processing Corpus...")
    getdata(input$Selection) # ... = replace with the two inputs from Task 2
  }))
  
  
 
 
  
  output$map<-renderLeaflet({
       f<-freq()
  
    bins<-c(0,10,20,50,100,500,1000)    #Create a color palette with Bins
    pal<-colorBin("BuPu" ,domain=f,bins =bins)
    
    
    labels <- paste(                   #Create a label with state name and their number one famous beer
      f@data$NAME,"<br/>", 
      f@data$beer 
    ) %>%
      lapply(htmltools::HTML)
    
    
    
    m<-leaflet(f)%>%
      setView(-96, 37.8, 4)%>%
      addProviderTiles(providers$Stamen.Toner)%>%
      addPolygons( data=f,
                   weight=1,
                   color="#660000",
                   smoothFactor=1,
                   dashArray=3,
                   fillOpacity=0.7)
    
    
    
    m <- m %>% addPolygons(
      fillColor = ~pal(n),
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#660000",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    
    m %>% addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Total medal and their famous beer",
                    position = "bottomright")
    m
    })
  
  
  
  
  freq2<-eventReactive(input$Run2,withProgress({
    setProgress(message = "Processing Corpus...")
    getdata(1) # ... = replace with the two inputs from Task 2
  }))
  output$map2<-renderLeaflet({
    f<-freq2()
    value<-input$Selection2
    bins<-c(0,10,20,50,100,500,1000)    #Create a color palette with Bins
    pal<-colorBin("BuPu" ,domain=f,bins =bins)
    
    
   if(input$Selection2=="GOLD") {labels <- paste(                   #Create a label with state name and their number one famous beer
      f@data$NAME,"<br/>", 
      f@data$GOLD
    ) %>% lapply(htmltools::HTML)}
    else if (input$Selection2=="SILVER") 
        {labels <- paste(                   #Create a label with state name and their number one famous beer
        f@data$NAME,"<br/>", 
        f@data$SILVER
      ) %>%lapply(htmltools::HTML)}
    else{labels <- paste(                   #Create a label with state name and their number one famous beer
          f@data$NAME,"<br/>", 
          f@data$BRONZE
        ) %>%
          lapply(htmltools::HTML)}
    
    
    
    m<-leaflet(f)%>%
      setView(-96, 37.8, 4)%>%
      addProviderTiles(providers$Stamen.Toner)%>%
      addPolygons( data=f,
                   weight=1,
                   color="#660000",
                   smoothFactor=1,
                   dashArray=3,
                   fillOpacity=0.7)
    
    
    
    m <- m %>% addPolygons(
      fillColor = ~pal(n),
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#660000",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    
    m %>% addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Total medal and their famous beer",
                    position = "bottomright")
    m
  })
  
}

shinyApp(ui = ui, server = server)
