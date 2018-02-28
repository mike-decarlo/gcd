#' Calculates the distance between geocoordinates using the Haversine formula
#' 
#' The function \code{"gcd.haversine"} takes inputs of two sets of coordinates
#' in (radian values), one set fo reach location, and a boolean indicator of
#' whether or not to return the results as kilometers (\code{"km = TRUE"}) or
#' miles (\code{"km = FALSE"}). The output is the distance using the method
#' of the Haversine formula, a more robust distance between two points on the
#' surface of a sphere.
#' 
#' @param lon1 the longitude as radians of the first point
#' @param lat1 the latitude as radians of the first point
#' @param lon2 the longitude as radians of the second point
#' @param lat2 the latitude as radians of the second point
#' @param km boolean argument for whether to return results as km (TRUE) or
#'   miles (FALSE)
#' 
#' @examples
#' # Input list of degree values
#' # Longitude values range between 0 and +-180 degrees
#' deg.lon <- runif(1000, -180, 180)
#' # Latitude values range between 0 and +-90 degrees
#' deg.lat <- runif(1000, -90, 90)
#' 
#' # Convert degrees to radians
#' rad.lon <- gcd.rad(deg.lon)
#' rad.lat <- gcd.rad(deg.lat)
#' 
#' # Obtain measures of distnace
#' hav.mi <- gcd.haversine(lon1 = rad.lon[1:500], lat1 = rad.lat[1:500]
#'   , lon2 = rad.lon[501:1000], lat2 = rad.lat[501:1000], km = FALSE)
#' 
#' @export

gcd.haversine <- function(lon1, lat1, lon2, lat2, km = TRUE) {
  R <- 6371 # Earth's mean radius [km]
  delta.lon <- (lon2 - lon1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat / 2) ^ 2 + cos(lat1) * cos(lat2) * sin(delta.lon / 2) ^ 2
  c <- 2 * asin(min(1, sqrt(a)))
  d <- R * c
  if (km == FALSE) {
    # If km == FALSE, then the desired answer should be in miles.
    # Convert `d` from km to miles by multipling by `0.621371`.
    d <- d * 0.621371
  }
  return(d)
}