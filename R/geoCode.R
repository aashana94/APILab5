#' Making a linear regression function using S3 class
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @param address as vector
#' @param apikey as character
#'
#' @return vector
#' @export
#' @examples latlon(address = "New Delhi, India", apiKey = "randomenumbersandletters")
latlon <- function(address, apiKey){

  param <- gsub("[\\@\\#\\%\\&\\!\\(\\)\\_\\*\\+\\;\\^]", "", address)
  params <- trimws(gsub("\\s+","+", param))

  base <- "https://maps.googleapis.com/maps/api/geocode/json?address="

  urlAPI <- paste0(base,params,"&key=",apiKey)
  callAPI <- httr::GET(urlAPI)
  httr::content(callAPI)
  resultJSON <- jsonlite::fromJSON(txt = callAPI$url)
  X <- resultJSON$results$geometry$location$lng
  Y <- resultJSON$results$geometry$location$lat

  return(c(X,Y))
}
latlon(address = "New Delhi, India", apiKey = "randomenumbersandletters")

reverselatlon<- function(lat, lon, apiKey){

  params <- paste0(lat,",",lon)
  base <- "https://maps.googleapis.com/maps/api/geocode/json?latlng="

  urlAPI <- paste0(base,params,"&key=",apiKey)
  callAPI <- httr::GET(urlAPI)
  httr::content(callAPI)
  resultJSON <- jsonlite::fromJSON(txt = callAPI$url)
  address <- resultJSON$results$formatted_address

  return(address)
}
