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
#' @param address an address as a stringn
#' @param app_id the user's App ID for the HERE.com JavaScript/REST (requires
#'   registration)
#' @param app_code the user's App Code for the HERE.com JavaScript/REST
#'   (requires registration)
#' @param dev whether to use development or production site
#' @param verbose Logical argument determining if messages are displayed.
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
        , "lattitude" =
          json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
        , "longitude" =
          json$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
        , "formatted_address" =
          json$Response$View$Result[[1]]$Location$Address$Label
        , "street_number" =
          json$Response$View$Result[[1]]$Location$Address$HouseNumber
        , "route" =
          json$Response$View$Result[[1]]$Location$Address$Street
        , "city" =
          json$Response$View$Result[[1]]$Location$Address$City
        , "district" =
          json$Response$View$Result[[1]]$Location$Address$District
        , "county" =
          json$Response$View$Result[[1]]$Location$Address$County
        , "state" =
          json$Response$View$Result[[1]]$Location$Address$State
        , "country" =
          json$Response$View$Result[[1]]$Location$Address$Country
        , "postal_code" =
          json$Response$View$Result[[1]]$Location$Address$PostalCode
        )
      )
  } else {
    stop("\nQuery returned NULL results.\n")
  }
}
