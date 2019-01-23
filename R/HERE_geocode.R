#' Get geocoding information using HERE.com Geocoder API
#'
#' \code{HERE_geocode} is designed to take an address
#' string, and user app credentials. The function queries the HERE.com Geocoder
#' API
#' \code{
#'   (https://developer.here.com/documentation/geocoder/topics/what-is.html)
#'   }
#'   and returns the drive time from the JSON output code. This geocoding
#'   information from the JSON is originally returned as a dataframe.
#' @param address a string; the address to be queried for geocoded information.
#' @param app_id a string; the user's App ID for the HERE.com JavaScript/REST
#'   (requires registration)
#' @param app_code a string; the user's App Code for the HERE.com
#'   JavaScript/REST (requires registration)
#' @param dev a boolean; \code{TRUE} to use the development app, \code{FALSE}
#'   to use the production app
#' @param verbose a boolean; \code{TRUE} to display json webaddress,
#'   \code{FALSE} to not display json webaddress
#' @return location information for the entered address string:
#'   OrigAddr, Latitude, Longitude, Label, Country, State, County, City,
#'   District, Street, HouseNumber, PostalCode
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape
#' @export
HERE_geocode <- function(address = NULL, app_id = NULL, app_code = NULL
  , dev = FALSE, verbose = FALSE) {
  if (dev == TRUE) {
    base <- "https://geocoder.cit.api.here.com/6.2/geocode.json?"
  } else if (dev == FALSE) {
    base <- "https://geocoder.api.here.com/6.2/geocode.json?"
  } else {
    stop(
      "Error: argument 'dev' must be given value of either TRUE or FALSE.\n"
      )
  }
  address <- paste0("searchtext=", curlEscape(address))
  id <- paste0("&app_id=", curlEscape(app_id))
  code <- paste0("&app_code=", curlEscape(app_code))
  gen <- "&gen=8"
  request_url <- paste0(base, address, id, code, gen)
  if (verbose == TRUE) {
    message(request_url)
  }
  json <- fromJSON(request_url, flatten = FALSE)
  if (!is.null(
    json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude[[1]]
    )) {
      return(c(
        "original_address" =  substr(
          curlUnescape(address)
          , 12
          , nchar(curlUnescape(address))
        )
        , "latitude" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude)
            , NA
            , json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
          )
        , "longitude" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude)
            , NA
            , json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
          )
        , "formatted_address" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$Label)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$Label
          )
        , "street_number" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$HouseNumber)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$HouseNumber
          )
        , "route" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$Street)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$Street
          )
        , "neighborhood" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$neighborhood)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$neighborhood
          )
        , "sublocality" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$sublocality)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$sublocality
          )
        , "city" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$City)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$City
          )
        , "district" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$District)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$District
          )
        , "county" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$County)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$County
          )
        , "state" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$State)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$State
          )
        , "country" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$Country)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$Country
          )
        , "postal_code" = ifelse(
            is.null(json$Response$View$Result[[1]]$Location$Address$PostalCode)
            , NA
            , json$Response$View$Result[[1]]$Location$Address$PostalCode
          )
        )
      )
  } else {
    stop("\nQuery returned NULL results.\n")
  }
}
