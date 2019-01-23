#' Get drive time in hours from Google.com Routing API
#'
#' \code{Google_drivetime} is designed to take degree or
#' radian geocoordinates or location names, user app credentials
#' , and some other API inputs (set to defaults). The function queries the 
#' Google Directions API \code{(
#' https://developers.google.com/maps/documentation/directions/
#' intro#DirectionsRequests)} and return
#' the drive time from the JSON output code. This drive time portion of the JSON
#' is originally return in minutes. This function is designed to take an
#' argument for the user preferred time (seconds, minutes, or hours).
#' @param origin  a string or vector of two numerics; if a location string,
#'   this should be an address, if a vector of length two numerics this should
#'   be the latitude and longitude coordinates for the origin location
#' @param destination a string or vector of two numerics; if a location string,
#'   this should be an address, if a vector of length two numerics this should
#'   be the latitude and longitude coordinates for the destination location
#' @param key a string; the user's App Code for the Google.com JavaScript/REST
#'   (requires registration)
#' @param mode a string; a transportation method for the route:
#'   \enumerate{
#'   \item \code{"driving"}
#'   \item \code{"walking"}
#'   \item \code{"bicycling"}
#'   \item \code{"transit"}
#'   }
#' @param transit_modea a string; sub-type for \code{"transit"} otherwise
#'   should be NULL:
#'   \enumerate{
#'   \item \code{"bus"}: the calculated route should prefer travel by bus
#'   \item \code{"subway"}: the calculated route should prefer travel by subway
#'   \item \code{"train"}: the calculated route should prefer travel by train
#'   \item \code{"tram"}: the calculated route should prefer travel by tram and
#'   light rail
#'   \item \code{"rail"}: the calculated route should prefer travel by train,
#'   tram, light rail, and subway
#'   }
#' @param transit_routing_preference a string; specifies preferences for the
#'   \code{"transit"}, otherwise should be NULL:
#'   \enumerate{
#'   \item \code{"less_walking"}
#'   \item \code{"fewer_transfers"}
#'   }
#' @param time_format a string; the preferred time format for results:
#'   \enumerate{
#'   \item \code{"hours"}
#'   \item \code{"minutes"}
#'   \item \code{"seconds"}
#'   }
#' @param distance_format a string; the pregerred format for measuring
#'   distance:
#'   \enumerate{
#'   \item \code{"customary"}
#'   \item \code{"metric"}
#'   }
#' @param coordinate_type a string; if using geocoordinates, whether they are
#'   in degrees or radians:
#'   \enumerate{ 
#'   \item \code{"rad"}{ (radians)}
#'   \item \code{"deg"}{ (degrees)}
#'   }
#' @param verbose a boolean; \code{TRUE} to display json webaddress,
#'   \code{FALSE} to not display json webaddress
#' @return the travel time and distance as specified between two locations
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape
#' @export
Google_drivetime <- function(origin = NULL, destination = NULL, key = NULL
  , mode = "driving", transit_mode = NULL, transit_routing_preference = NULL
  , time_format = "minutes", distance_format = "customary"
  , coordinate_type = "deg", verbose = FALSE) {
  base <- "https://maps.googleapis.com/maps/api/directions/json?"
  key <- paste0("&key=", curlEscape(key))

  if (length(origin) == 2) {
    if (coordinate_type == "rad") {
      orgn_lat <- to_deg(as.numeric(origin[[1]]))
      orgn_lon <- to_deg(as.numeric(origin[[2]]))
    } else if (coordinate_type == "deg") {
      orgn_lat <- as.numeric(origin[[1]])
      orgn_lon <- as.numeric(origin[[2]])
    } else if (!is.null(coordinate_type)) {
      stop(
        paste0(
          "\nArgument 'coordinate_type' must be NULL when using a location "
          , "string or have a value of either 'deg' for coordinates in "
          , "degrees or 'rad' for coordinates in radians.\n"
        )
      )
    }
    origin <- paste0("origin=", curlEscape(paste0(orgn_lat, ",", orgn_lon)))
  } else if (length(origin) == 1) {
    origin <- paste0("origin=", curlEscape(origin))
  } else {
    stop(
      paste0(
        "\nArgument 'origin' must either have one (1) location string value "
        , "or two (2) location numeric coordinate values.\n"
      )
    )
  }

  if (length(destination) == 2) {
    if (coordinate_type == "rad") {
      dest_lat <- to_deg(as.numeric(destination[[1]]))
      dest_lon <- to_deg(as.numeric(destination[[2]]))
    } else if (coordinate_type == "deg") {
      dest_lat <- as.numeric(destination[[1]])
      dest_lon <- as.numeric(destination[[2]])
    } else if (!is.null(coordinate_type)) {
      stop(
        paste0(
          "\nArgument 'coordinate_type' must be NULL when using a location "
          , "string or have a value of either 'deg' for coordinates in "
          , "degrees or 'rad' for coordinates in radians.\n"
        )
      )
    }
    destination <- paste0(
      "&destination="
      , curlEscape(paste0(dest_lat, ",", dest_lon))
      )
  } else if (length(destination) == 1) {
    destination <- paste0("&destination=", curlEscape(destination))
  }

  if (mode == "transit") {
    if (!is.null(transit_mode)) {
      transit_mode <- paste0(
        "&transit_mode="
        , curlEscape(transit_mode)
      )
    } else {
      transit_mode <- ""
    }
    if (!is.null(transit_routing_preference)) {
      transit_routing_preference <- paste0(
        "&transit_routing_preference="
        , curlEscape(transit_routing_preference)
      )
    } else {
      transit_routing_preference <- ""
    }
  }

  mode <- paste0(
    "&mode="
    , curlEscape(mode)
  )

  request_url <- paste0(
    base
    , origin
    , destination
    , key
    , mode
    , transit_mode
    , transit_routing_preference
  )
  if (verbose == TRUE) {
    message(request_url)
  }
  json <- fromJSON(request_url, flatten = FALSE)
  # distance <- json$routes$legs[[1]]$distance$value # in meters
  # if (distance_format == "metric") {
  #   distance <- distance / 1000
  # } else if (distance_format == "customary") {
  #   distance <- distance / 1609.344
  # } else {
  #   message("\nArgument 'time_format' must have value 'hours', 'minutes', or
  #     'seconds'. Using default value of 'customary'.\n")
  #   distance <- distance / 1609.344
  # }
  time <- json$routes$legs[[1]]$duration$value # in seconds
  if (time_format == "seconds") {
    # do nothing, already in seconds
  } else if (time_format == "minutes") {
    time <- time / 60 # convert to minutes
  } else if (time_format == "hours") {
    time <- time / 3600 # convert to hours
  } else {
    message("\nArgument 'time_format' must have value 'hours', 'minutes', or
      'seconds'. Using default value of 'minutes'.\n")
    time <- time / 60
  }
  time
}
