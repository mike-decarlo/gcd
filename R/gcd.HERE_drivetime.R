#' Get drive time in hours from HERE.com Routing API
#' 
#' The function \code{"gcd.HERE_drivetime"} is designed to take degree or
#' radian geocoordinates, user app credentials, and some other API inputs (set
#' to defaults). The function queries the HERE.com Routing API \code{"(https://
#' developer.here.com/documentation/routing/topics/what-is.html)"} and return
#' the drive time from the JSON output code. This driveTime portion of the JSON
#' is originally return in seconds. This function is designed to take an
#' argument for the user preferred time (seconds, minutes, or hours) as a non-
#' integer, numeric value (a number with decimals).
#' @param orgn_lat the latitude coordinate for the origin location
#' @param orgn_lon the longitude coordinate for the origin location
#' @param dest_lat the latitude coordinate for the destination location
#' @param dest_lon the longitude coordinate for the destination location
#' @param app_id the user's App ID for the HERE.com JavaScript/REST (requires
#' registration)
#' @param app_code the user's App Code for the HERE.com JavaScript/REST
#' (requires registration)
#' @param time_frmt the user's preferred time format: \enumerate{ \item
#' \code{"hours"} \item \code{"minutes"} \item \code{"seconds"} }
#' @param type a method of deciding optimal travel route: \enumerate{ \item 
#' \code{"fastest"} \item \code{"shortest"} }
#' @param trnsprt a transportation method for the route: \enumerate{ \item
#' \code{"car"} \item \code{"carHOV"} \item \code{"pedestrian"} \item 
#' \code{"truck"} \item \code{"bicycle"} }
#' @param trfc whether or not to factor in traffic: \enumerate{ \item
#' \code{"disabled"} \item \code{"enabled"} }
#' @param coord_type if the geocoordinates are in degrees or radians:
#' \enumerate{ \item \code{"rad"}{ (radians)} \item \code{"deg"}{ (degrees)} }
#' @param dev whether to use development or production site
#' 
#' @return the travel time as specified between two locations
#' @export
#' @import jsonlite
#' @import RCurl
#' @import leaflet
#' 
#' @examples

gcd.HERE_drivetime <- function(orgn_lat, orgn_lon, dest_lat, dest_lon
  , app_id, app_code, time_frmt = "hours", type = "fastest", trnsprt = "car"
  , trfc = "disabled", coord_typ = "rad", dev = FALSE) {
  
  if (dev == TRUE) {
    base <- "https://route.cit.api.here.com/routing/7.2/calculateroute.json?"
  } else if (dev == FALSE) {
    base <- "https://route.api.here.com/routing/7.2/calculateroute.json?"
  } else {
    message("Error: argument 'dev' must be given value of either TRUE or FALSE.\n")
    break
  }
  
  if (coord_typ == "rad") {
    
    orgn_lat <- gcd.deg(orgn_lat)
    orgn_lon <- gcd.deg(orgn_lon)
    dest_lat <- gcd.deg(dest_lat)
    dest_lon <- gcd.deg(dest_lon)
    
  }

  id <- paste0("&app_id=", curlEscape(app_id))
  code <- paste0("&app_code=", curlEscape(app_code))
  wypnt0 <- paste0("waypoint0=", curlEscape(paste0(orgn_lat, ",", orgn_lon)))
  wypnt1 <- paste0("&waypoint1=", curlEscape(paste0(dest_lat, ",", dest_lon)))
  trfc <- paste0("traffic:", trfc)
  mode <- paste0(
    "&mode="
    , curlEscape(paste(type, trnsprt, trfc, sep = ";")))

  departure <- "2018-01-17T22:00:00"
  departure <- paste0("&departure=", curlEscape(departure))
    
  request_url <- paste0(base, wypnt0, wypnt1, mode, id, code, departure)
  message(request_url)
  json <- fromJSON(request_url, flatten = FALSE)
  
  travelTime <- json$response$route$summary$travelTime # in seconds
  
  if (time_frmt == "seconds") {
    
    # do nothing, already in seconds
    
  } else if (time_frmt == "minutes") {
    
    travelTime <- travelTime / 60 # convert to minutes
    
  } else if (time_frmt == "hours") {
  
    travelTime <- travelTime / 3600 # convert to hours
  
  } else {
    
    message("Error: argument 'time_frmt' must have value 'hours', 'minutes', or
      'seconds'. Please enter a valid argument value for 'timefrmt'.")
    
  }
  
  return(travelTime)
  
}