library(httr)
library(jsonlite)

latlon <- function(x){

  param <- gsub("[\\@\\#\\%\\&\\!\\(\\)\\_\\*\\+\\;\\^]", "", x)
  print(param)
  params <- trimws(gsub("\\s+","+", param))
  print(params)
  base <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  geoKey <-"&key=AIzaSyAm1Nenb9QOGTKES6uXOpSwQ-dDdOuXha8"

  urlAPI <- paste0(base,params,geoKey)
  callAPI <- GET(urlAPI)
  content(callAPI)
  resultJSON <- fromJSON(txt = callAPI$url)
  X <- resultJSON$results$geometry$location$lng
  Y <- resultJSON$results$geometry$location$lat

  return(c(X,Y))
}
latlon("1600 Amphitheatre ,@ # . Parkway, Mountain View, CA")
