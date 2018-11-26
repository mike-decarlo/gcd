#' Get geocoding information using Google Geocode API
#'
#' \code{Google_geocode} is designed to take an address
#'   string, and user app key. The function queries the Google Geocode
#'   API
#' \code{
#'   (https://developers.google.com/maps/documentation/geocoding/start)
#'   }
#'   and returns the various components from the JSON output code. This geocoding
#'   information from the JSON is originally returned as a vector.
#' @param address an address as a string
#' @param key the user's Google API key
#' @param verbose Logical argument determining if messages are displayed.
#' @return location information for the entered address string:
#'   original_address, latitude, longitude, formatted_address, street_number
#'   , route, locality (city), administrative_area_3 (district)
#'   , administrative_area_2 (county), administrative_area_1 (state)
#'   , country, postal_code
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape
#' @export
Google_geocode <- function(address = NULL, key = NULL, verbose = FALSE) {
  base <- "https://maps.googleapis.com/maps/api/geocode/json?"
  address <- paste0("address=", curlEscape(address))
  key <- paste0("&key=", curlEscape(key))
  request_url <- paste0(base, address, key)
  if (verbose == TRUE) {
    message(request_url)
  }
  json <- fromJSON(request_url, flatten = FALSE)
  if (json$status == "OK") {
    return(c(
      "original_address" =  substr(
        curlUnescape(address)
        , 9
        , nchar(curlUnescape(address))
        )
      , "lattitude" = json$results$geometry$location[1]
      , "longitude" = json$results$geometry$location[2]
      , "formatted_address" = json$results$formatted_address
      , "street_number" = json$results$address_components[[1]]$long_name[1]
      , "route" = json$results$address_components[[1]]$long_name[2]
      , "city" = json$results$address_components[[1]]$long_name[3]
      , "district" = json$results$address_components[[1]]$long_name[4]
      , "county" = json$results$address_components[[1]]$long_name[5]
      , "state" = json$results$address_components[[1]]$long_name[6]
      , "country" = json$results$address_components[[1]]$long_name[7]
      , "postal_code" = json$results$address_components[[1]]$long_name[8]
      )
    )
  } else {
    stop(
      paste0(
        "\nQuery returned status "
        , json$status
        , ".\n"
        )
    )
  }
}
