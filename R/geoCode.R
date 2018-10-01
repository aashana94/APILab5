library(httr)
library(jsonlite)

latlon <- function(address, apiKey){

  param <- gsub("[\\@\\#\\%\\&\\!\\(\\)\\_\\*\\+\\;\\^]", "", address)
  params <- trimws(gsub("\\s+","+", param))

  base <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  
  urlAPI <- paste0(base,params,"&key=",apiKey)
  callAPI <- GET(urlAPI)
  content(callAPI)
  resultJSON <- fromJSON(txt = callAPI$url)
  X <- resultJSON$results$geometry$location$lng
  Y <- resultJSON$results$geometry$location$lat

  return(c(X,Y))
}


reverselatlon<- function(lat, lon, apiKey){

  params <- paste0(lat,",",lon)
  base <- "https://maps.googleapis.com/maps/api/geocode/json?latlng="
  
  urlAPI <- paste0(base,params,"&key=",apiKey)
  callAPI <- GET(urlAPI)
  content(callAPI)
  resultJSON <- fromJSON(txt = callAPI$url)
  address <- resultJSON$results$formatted_address
  
  return(address)
}
