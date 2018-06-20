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
#' @param verbose Logical argument determining if messages are displayed.
#' 
#' @return location information for the entered address string:
#'   OrigAddr, Latitude, Longitude, Label, Country, State, County, City,
#'   District, Street, HouseNumber, PostalCode
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape

gcd.HERE_geocode <- function(address, app_id, app_code, dev = FALSE, verbose = FALSE) {
  
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
  
  if (verbose == TRUE) {
    message(request_url)
  }
  
  json <- fromJSON(request_url, flatten = FALSE)
  
  r <- address
  
  if (!is.null(json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$Label[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$Label[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$Country[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$Country[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$State[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$State[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$County[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$County[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$City[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$City[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$Street[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$Street[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$HouseNumber[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$HouseNumber[[1]])
  } else {
    r <- c(r, NA)
  }
  
  if (!is.null(json$Response$View$Result[[1]]$Location$Address$PostalCode[[1]])) {
    r <- c(r, json$Response$View$Result[[1]]$Location$Address$PostalCode[[1]])
  } else {
    r <- c(r, NA)
  }
  
  results <- data.frame(t(r))
  
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