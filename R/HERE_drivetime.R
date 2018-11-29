#' Get drive time in hours from HERE.com Routing API
#'
#' \code{HERE_drivetime} is designed to take degree or
#' radian geocoordinates, user app credentials, and some other API inputs (set
#' to defaults). The function queries the HERE.com Routing API \code{(https://
#' developer.here.com/documentation/routing/topics/what-is.html)} and return
#' the drive time from the JSON output code. This driveTime portion of the JSON
#' is originally return in seconds. This function is designed to take an
#' argument for the user preferred time (seconds, minutes, or hours) as a non-
#' integer, numeric value (a number with decimals).
#' @param origin a vector of length two (2), the latitude and longitude
#'   coordinates of the origin location
#' @param destination a vector of length two (2), latitude and longitude
#'   coordinates of the destination location
#' @param app_id the user's App ID for the HERE.com JavaScript/REST (requires
#'   registration)
#' @param app_code the user's App Code for the HERE.com JavaScript/REST
#'   (requires registration)
#' @param time_frmt the user's preferred time format: \enumerate{ \item
#'   \code{"hours"} \item \code{"minutes"} \item \code{"seconds"} }
#' @param type a method of deciding optimal travel route: \enumerate{ \item
#'   \code{"fastest"} \item \code{"shortest"} }
#' @param trnsprt a transportation method for the route: \enumerate{ \item
#'   \code{"car"} \item \code{"carHOV"} \item \code{"pedestrian"} \item
#'   \code{"truck"} \item \code{"bicycle"} }
#' @param trfc whether or not to factor in traffic: \enumerate{ \item
#'   \code{"disabled"} \item \code{"enabled"} }
#' @param coord_typ if the geocoordinates are in degrees or radians:
#'   \enumerate{ \item \code{"rad"}{ (radians)} \item \code{"deg"}{ (degrees)} }
#' @param dev whether to use development or production site
#' @param verbose Logical argument determining if messages are displayed.
#' @return the travel time as specified between two locations
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape
#' @export
HERE_drivetime <- function(origin = NULL, destination = NULL, app_id = NULL
  , app_code = NULL, time_frmt = "minutes", type = "fastest", trnsprt = "car"
  , trfc = "disabled", coord_typ = "deg", dev = FALSE, verbose = FALSE) {
  if (dev == TRUE) {
    base <- "https://route.cit.api.here.com/routing/7.2/calculateroute.json?"
  } else if (dev == FALSE) {
    base <- "https://route.api.here.com/routing/7.2/calculateroute.json?"
  } else {
    stop(
      "Error: Argument 'dev' must be given value of either TRUE or FALSE.\n"
      )
  }
  if (length(origin) != 2) {
    stop("\nArgument 'origin' must be of length 2.\n")
  } else if (length(destination) != 2) {
    stop("\nArgument 'destination' must be of length 2.\n")
  } else {
    if (coord_typ == "rad") {
      origin <- to_deg(origin)
      destination <- to_deg(destination)
    }
  }
  id <- paste0("&app_id=", curlEscape(app_id))
  code <- paste0("&app_code=", curlEscape(app_code))
  wypnt0 <- paste0(
    "waypoint0="
    , curlEscape(paste0(origin[1], ",", origin[2]))
  )
  wypnt1 <- paste0(
    "&waypoint1="
    , curlEscape(paste0(destination[1], ",", destination[2]))
  )
  trfc <- paste0("traffic:", trfc)
  mode <- paste0(
    "&mode="
    , curlEscape(paste(type, trnsprt, trfc, sep = ";")))
  request_url <- paste0(base, wypnt0, wypnt1, mode, id, code)
  if (verbose == TRUE) {
    message(request_url)
  }
  json <- fromJSON(request_url, flatten = FALSE)
  travel_time <- json$response$route$summary$travelTime # in seconds
  if (time_frmt == "seconds") {
    # do nothing, already in seconds
  } else if (time_frmt == "minutes") {
    travel_time <- travel_time / 60 # convert to minutes
  } else if (time_frmt == "hours") {
    travel_time <- travel_time / 3600 # convert to hours
  } else {
    message("Error: argument 'time_frmt' must have value 'hours', 'minutes', or
      'seconds'. Please enter a valid argument value for 'timefrmt'.")
  }
  travel_time
}
