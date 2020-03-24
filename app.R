library(shiny)
library(leaflet)
library("leaflet.extras")
library("crosstalk")
library("googlesheets")
library(openair)
library(zoo)
library(dplyr)
library(DT)
library(lubridate)
library(plotly)
library(googleVis)
library(openair)
#library(readr,lib.loc="/home/pveronesi/R/x86_64-pc-linux-gnu-library/3.2/")
#library(gdalUtils,lib.loc="/home/pveronesi/R/x86_64-pc-linux-gnu-library/3.2/")
library (RCurl)
#gdal_setInstallation()
library(rgdal)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(reshape2)
# Define UI for data download app ----
myCsv <- getURL("https://docs.google.com/spreadsheets/d/12WlQnFjikE5ZFSep4n2MKglMx5KLgRONW4QHu64H5DE/gviz/tq?tqx=out:csv",ssl.verifypeer = FALSE)
death<-read.csv(textConnection(myCsv),check.names = TRUE)
dr<-round(max(death$Total.Deaths,na.rm=TRUE)/max(death$Total.Cases,na.rm=TRUE)*100,1)
dre<-round(max(death$Total.Deaths,na.rm=TRUE)/(max(death$Total.Recovered,na.rm=TRUE)+max(death$Total.Deaths,na.rm=TRUE))*100,1)
myCsv <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",ssl.verifypeer = FALSE)
Lod<-read.csv(textConnection(myCsv),check.names = FALSE)
col<-ncol(Lod)
world<-reshape(Lod, 
              direction = "long",
              varying = list(names(Lod)[5:col]),
              v.names = "Value",
              idvar = c("Country/Region","Province/State"),
              timevar = "date"
              )
world$cou<-paste0(world$`Province/State`," ",world$`Country/Region`)
world$date2<-as.Date(world$date,origin="21/01/2020",format="%d/%m/%y")
myCsv <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
Italy<-read.csv(textConnection(myCsv),check.names = FALSE)
Italy$data2<-as.POSIXct(Italy$data)
province<-unique(Italy$denominazione_provincia)
country<-unique(world$`cou`)
ui=dashboardPage(
  dashboardHeader(title = 'Dati Covid-19',
                  tags$li(a(href = 'http://www.arpae.it',
                            icon("power-off"),
                            title = "Torna ad Arpae"),
                          class = "dropdown")),
  dashboardSidebar(width=300,
                   useShinyjs(),
              
                   dateRangeInput("daterange","Scegli la data",start="01/22/20",end=Sys.Date(),format="mm/dd/yy",min="01/22/20",max=Sys.Date()+1,language="it"),
                   selectInput("country","Scegli la nazione",choices = country,selected=' Italy'),
                   checkboxInput("loga","Scala logaritmica",value=FALSE),
                   selectInput("provincia","Scegli la provincia",choices = province,selected='Bologna')
                  
                  
                   
  ),
  
  # Main panel for displaying outputs ----
  dashboardBody(
             fluidRow(
                box(plotlyOutput("grafico",height = 280),height = 300),
                box(plotlyOutput("grafico2",height = 280),height=300),
                
             
                box(leafletOutput("mappetta",height = 380),height=400),
                infoBox("% morti su totale (mondo)", dr, icon = icon("credit-card"),color='purple'),
                infoBox("% morti su esiti (mondo)", dre, icon = icon("credit-card"),color='red'),
                infoBox("Dati italiani aggiornati al ",max(Italy$data2)),
                infoBox("Dati mondo aggiornati al",max(world$date2)))
                
                
                
                
                
  ))
  
  


  



# Define server logic to display and download selected file ----


server <- function(input, output,session) {
 
  
output$grafico<-renderPlotly({
  worldplot<-world[world$date2>=as.Date(input$daterange[1],format="%m/%d/%y"),]
  worldplot<-worldplot[worldplot$date2<=as.Date(input$daterange[2],format="%m/%d/%y"),]
  if (input$loga==FALSE){
 p<- plot_ly(worldplot[worldplot$`cou`==input$country,],x=~date2)%>% add_bars(y=~Value)%>%
   layout(
     xaxis=list(title = "Date",autotick=F,tickmode="array",nticks=round(0.5*(as.Date(input$daterange[2])-as.Date(input$daterange[1])))),
     title = paste0('Casi di Covid-19 in ',input$country))
 }
  if (input$loga==TRUE){
    p<- plot_ly(worldplot[worldplot$`cou`==input$country,],x=~date2)%>% add_bars(y=~Value)%>%
      layout(
             yaxis = list(type = "log"),
             xaxis=list(title = "Date",autotick=F,tickmode="array",nticks=round(0.5*(as.Date(input$daterange[2])-as.Date(input$daterange[1])))),
             title = paste0('Casi di Covid-19 in ',input$country,'- scala logaritmica'))
    
 
  }
  p
})
output$grafico2<-renderPlotly({
  italyplot<-Italy[Italy$data2>=as.POSIXct(input$daterange[1],format="%m/%d/%y","GMT+1"),]
  italyplot$data2<-as.Date(as.character(as.POSIXct(italyplot$data,"GMT+1")))
  italyplot<-italyplot[italyplot$data2<=as.Date(input$daterange[2]),]
  tdates<-1:length(italyplot[italyplot$denominazione_provincia==input$provincia,]$data)
  m1<-lm(italyplot[italyplot$denominazione_provincia==input$provincia,]$totale_casi~tdates+I(tdates^2))
  if (input$loga==FALSE){
    p<- plot_ly(italyplot[italyplot$denominazione_provincia==input$provincia,],x=~data2)%>% add_bars(y=~totale_casi)%>%
      add_lines(x=~data2,y=predict(m1))%>%
      layout(
       xaxis=list(title = "Date",nticks=round(0.5*(as.Date(input$daterange[2])-as.Date(input$daterange[1])))),
        title = paste0('Casi di Covid-19 a ',input$provincia))
  }
  if (input$loga==TRUE){
    p<- plot_ly(italyplot[italyplot$denominazione_provincia==input$provincia,],x=~data2)%>% add_bars(y=~totale_casi)%>%
      layout(
        xaxis=list(title = "Date",nticks=round(0.5*(as.Date(input$daterange[2])-as.Date(input$daterange[1])))),
        yaxis = list(type = "log"),
        title = paste0('Casi di Covid-19 a ',input$provincia,"- scala logaritmica"))
    
    
  }
  p
})

output$mappetta<-renderLeaflet({
  italyplot<-Italy[Italy$data2<as.POSIXct(input$daterange[2],format="%m/%d/%y"),]
  leaflet("mappa")%>%addTiles(options=tileOptions(opacity=0.3))%>%
    addCircleMarkers(data=italyplot,lng = italyplot$long, lat = italyplot$lat,
                     popup = paste('Provincia:',italyplot$sigla_provincia,'<BR>',
                    'Casi:',italyplot$totale_casi,'<BR>',
                    'Data:',italyplot$data),radius=(italyplot$totale_casi/200))%>%
    flyTo(lng=12,lat=44,zoom=7)
  
})



}
# Create Shiny app ----
shinyApp(ui, server)
