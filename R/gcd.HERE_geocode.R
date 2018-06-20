#' Get geocoding information using HERE.com Geocoder API
#' 
#' The function \code{"gcd.HERE_geocode"} is designed to take an address
#' string, and user app credentials. The function queries the HERE.com Geocoder
#' API
#' \code{(https://developer.here.com/documentation/geocoder/topics/what-is.html)}
#' and returns the drive time from the JSON output code. This geocoding
#' information from the JSON is originally returned as a dataframe.
#' @param address the latitude coordinate for the origin location
#' @param app_id the longitude coordinate for the origin location
#' @param app_code the latitude coordinate for the destination location
#' @param dev whether to use development or production site
#' 
#' @return location information for the entered address string:
#'   OrigAddr, Latitude, Longitude, Label, Country, State, County, City,
#'   District, Street, HouseNumber, PostalCode
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape

gcd.HERE_geocode <- function(address, app_id, app_code, dev = FALSE) {
  
  if (dev == TRUE) {
    base <- "https://geocoder.cit.api.here.com/6.2/geocode.json?"
  } else if (dev == FALSE) {
    base <- "https://geocoder.api.here.com/6.2/geocode.json?"
  } else {
    stop("Error: argument 'dev' must be given value of either TRUE or FALSE.\n")
  }
  
  addr <- paste0("searchtext=", curlEscape(address))
  id <- paste0("&app_id=", curlEscape(app_id))
  code <- paste0("&app_code=", curlEscape(app_code))
  gen <- "&gen=8"
  request_url <- paste0(base, addr, id, code, gen)
  
  message(request_url)
  
  json <- fromJSON(request_url, flatten = FALSE)
  
  results <- data.frame(c(
    address
    , json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
    , json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
    , json$Response$View$Result[[1]]$Location$Address[[1]]$Label
    , json$Response$View$Result[[1]]$Location$Address[[1]]$Country
    , json$Response$View$Result[[1]]$Location$Address[[1]]$State
    , json$Response$View$Result[[1]]$Location$Address[[1]]$County
    , json$Response$View$Result[[1]]$Location$Address[[1]]$City
    , json$Response$View$Result[[1]]$Location$Address[[1]]$Street
    , json$Response$View$Result[[1]]$Location$Address[[1]]$HouseNumber
    , json$Response$View$Result[[1]]$Location$Address[[1]]$PostalCode
    ))
  
  colnames(results) <-  c("OrigAddr"
    , "Latitude"
    , "Longitude"
    , "Label"
    , "Country"
    ," State"
    , "County"
    , "City"
    , "Street"
    , "HouseNumber"
    , "PostalCode"
    ) 
  
  return(results)
  
}