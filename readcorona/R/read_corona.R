
library(httr)
library(jsonlite)


corona_timespan<-function(country,timespan){
  mydata<-GET(paste0("https://corona.ndo.dev/api/timespan?country=",country,"&time=%22",timespan,"%22"),ssl.verifypeer = FALSE)
datar<-content(mydata,"text")
datano<-fromJSON(datar,flatten=TRUE)
return(datano)
}

corona_daily<-function(country,rating,source){
  mydata<-GET(paste0("https://corona.ndo.dev/api/daily?country=",country,"&rating",rating,"&source=",source),ssl.verifypeer = FALSE)
  datar<-content(mydata,"text")
  datano<-fromJSON(datar,flatten=TRUE)
  return(datano)
}

corona_countries<-function(country){
  mydata<-GET(paste0("https://corona.ndo.dev/api/countries?country=",country),ssl.verifypeer = FALSE)
  datar<-content(mydata,"text")
  datano<-fromJSON(datar,flatten=TRUE)
  return(datano)
}

corona_datasources<-function(){
  mydata<-GET(paste0("https://corona.ndo.dev/api/datasources"),ssl.verifypeer = FALSE)
  datar<-content(mydata,"text")
  datano<-fromJSON(datar,flatten=TRUE)
  return(datano)
}

corona_meta<-function(){
  mydata<-GET(paste0("https://corona.ndo.dev/meta"),ssl.verifypeer = FALSE)
  datar<-content(mydata,"text")
  datano<-fromJSON(datar,flatten=TRUE)
  return(datano)
}
