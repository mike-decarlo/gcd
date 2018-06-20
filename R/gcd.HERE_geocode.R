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
  
  r <- address
  
  if (!is.null(json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$Label)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$Label)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$Country)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$Country)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$State)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$State)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$County)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$County)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$City)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$City)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$Street)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$Street)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$HouseNumber)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$HouseNumber)
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$PostalCode)) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$PostalCode)
  } else {
    r <- c(r, NA)
  }
  
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