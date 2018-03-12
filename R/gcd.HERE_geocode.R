#' Get geocoding information using HERE.com Geocoder API
#' 
#' The function \code{"gcd.HERE_geocode"} is designed to take an address
#' string, and user app credentials. The function queries the HERE.com Geocoder
#' API
#' \code{(https://developer.here.com/documentation/geocoder/topics/what-is.html)}
#' and returns the drive time from the JSON output code. This geocoding
#' information from the JSON is originally returned as a dataframe.
#' 
#' @param address the latitude coordinate for the origin location
#' @param app_id the longitude coordinate for the origin location
#' @param app_code the latitude coordinate for the destination location
#' 
#' @return location information for the entered address string:
#'   OrigAddr, Latitude, Longitude, Label, Country, State, County, City,
#'   District, Street, HouseNumber, PostalCode
#' 
#' @examples 
#' 
#' @export
#' @import jsonlite
#' @import RCurl
#' @import leaflet

gcd.HERE_geocode <- function(address, app_id, app_code) {
  
  base <- "https://geocoder.api.here.com/6.2/geocode.json?"
  addr <- paste0("searchtext=", curlEscape(address))
  id <- paste0("&app_id=", curlEscape(app_id))
  code <- paste0("&app_code=", curlEscape(app_code))
  
  request_url <- paste0(base, addr, id, code)
  message(request_url)
  
  json <- fromJSON(request_url, flatten = FALSE)
  
  results <- data.frame(c(
    address
    , json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
    , json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
    , json$Response$View$Result[[1]]$Location$Address[1:9] #All Address
    ))
  
  colnames(results) <-  c("OrigAddr"
    , "Latitude"
    , "Longitude"
    , "Label"
    , "Country"
    ," State"
    , "County"
    , "City"
    , "District"
    , "Street"
    , "HouseNumber"
    , "PostalCode"
    ) 
  
  return(results)
  
}