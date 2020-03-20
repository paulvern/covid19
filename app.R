#load necessary libraries
library(shiny)
library(leaflet)
library("leaflet.extras")
library(dplyr)
library(plotly)
library (RCurl)
library(rgdal)
library(shinyjs)
library(shinydashboard)
library(reshape2)
#download data of total cases imported from https://www.worldometers.info/coronavirus/#countrie - google sheets is useful to 
myCsv <- getURL("https://docs.google.com/spreadsheets/d/12WlQnFjikE5ZFSep4n2MKglMx5KLgRONW4QHu64H5DE/gviz/tq?tqx=out:csv",ssl.verifypeer = FALSE)
death<-read.csv(textConnection(myCsv),check.names = TRUE)
#calculate death rate and death rate on recoverd+total deaths
dr<-round(max(death$Total.Deaths,na.rm=TRUE)/max(death$Total.Cases,na.rm=TRUE)*100,1)
dre<-round(max(death$Total.Deaths,na.rm=TRUE)/(max(death$Total.Recovered,na.rm=TRUE)+max(death$Total.Deaths,na.rm=TRUE))*100,1)
#load world covid data
myCsv <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",ssl.verifypeer = FALSE)
Lod<-read.csv(textConnection(myCsv),check.names = FALSE)
col<-ncol(Lod)
#reshape the world data from wide to long format
world<-reshape(Lod, 
              direction = "long",
              varying = list(names(Lod)[5:col]),
              v.names = "Value",
              idvar = c("Country/Region","Province/State"),
              timevar = "date"
              )
world$cou<-paste0(world$`Province/State`," ",world$`Country/Region`)
world$date2<-as.Date(world$date,origin="21/01/2020",format="%d/%m/%y")
#load italian data
myCsv <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
Italy<-read.csv(textConnection(myCsv),check.names = FALSE)
Italy$data2<-as.POSIXct(Italy$data)
province<-unique(Italy$denominazione_provincia)
country<-unique(world$`cou`)
#create the dashboard
ui=dashboardPage(
  dashboardHeader(title = 'Dati Covid-19',
                  tags$li(a(href = 'http://www.salute.gov.it/portale/home.html',
                            icon("power-off"),
                            title = "Torna al sito del ministero"),
                          class = "dropdown")),
  dashboardSidebar(width=300,
                   useShinyjs(),
              
                   dateRangeInput("daterange","Scegli la data",start="01/22/20",end=Sys.Date()-1,format="mm/dd/yy",min="01/22/20",max=Sys.Date(),language="it"),
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
                infoBox("% morti su esiti (mondo)", dre, icon = icon("credit-card"),color='red'))
  ))
  
# Define server logic 

server <- function(input, output,session) {
 
output$grafico<-renderPlotly({
  worldplot<-world[world$date2>as.Date(input$daterange[1],format="%m/%d/%y"),]
  worldplot<-worldplot[worldplot$date2<as.Date(input$daterange[2],format="%m/%d/%y"),]
  if (input$loga==FALSE){
 p<- plot_ly(worldplot[worldplot$`cou`==input$country,],x=~date2)%>% add_bars(y=~Value)%>%
   layout(
     xaxis=list(title = "Date"),
     title = paste0('Casi di Covid-19 in ',input$country))
 }
  if (input$loga==TRUE){
    p<- plot_ly(worldplot[worldplot$`cou`==input$country,],x=~date2)%>% add_bars(y=~Value)%>%
      layout(
             yaxis = list(type = "log"),
             xaxis=list(title = "Date"),
             title = paste0('Casi di Covid-19 in ',input$country,'- scala logaritmica'))
  }
  p
})

output$grafico2<-renderPlotly({
  italyplot<-Italy[Italy$data2>as.POSIXct(input$daterange[1],format="%m/%d/%y"),]
  italyplot$data<-as.POSIXct(italyplot$data)
  italyplot<-italyplot[italyplot$data<as.POSIXct(input$daterange[2],format="%m/%d/%y"),]
  if (input$loga==FALSE){
    p<- plot_ly(italyplot[italyplot$denominazione_provincia==input$provincia,],x=~data)%>% add_bars(y=~totale_casi)%>%
      layout(
       xaxis=list(title = "Date"),
        title = paste0('Casi di Covid-19 a ',input$provincia))
  }
  if (input$loga==TRUE){
    p<- plot_ly(italyplot[italyplot$denominazione_provincia==input$provincia,],x=~data)%>% add_bars(y=~totale_casi)%>%
      layout(
        xaxis=list(title = "Date"),
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
