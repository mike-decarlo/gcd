#' Calculates the distance between geocoordinates using the Spherical Law of
#' Cosines (SLC)
#' 
#' The function \code{"gcd.slc"} takes inputs of two sets of coordinates in
#' (radian values), one set fo reach location, and a boolean indicator of
#' whether or not to return the results as kilometers (\code{"km = TRUE"}) or
#' miles (\code{"km = FALSE"}). The output is the distance using the method
#' of Spherical Law of Cosines, a basic distance between two points on the
#' surface of a sphere.
#' 
#' @param lon1 the longitude as radians of the first point
#' @param lat1 the latitude as radians of the first point
#' @param lon2 the longitude as radians of the second point
#' @param lat2 the latitude as radians of the second point
#' @param type defaults to "deg", can also be "rad"
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
#' # Obtain measures of distnace
#' sphere.mi <- gcd.slc(lon1 = deg.lon[1:500], lat1 = deg.lat[1:500]
#'   , lon2 = deg.lon[501:1000], lat2 = deg.lat[501:1000], km = FALSE)
#' 
#' @export

gcd.slc <- function(lon1, lat1, lon2, lat2, type = "deg", km = TRUE) {
  if (type == "deg") {
    lon1 <- gcd.rad(lon1)
    lon2 <- gcd.rad(lon2)
    lat1 <- gcd.rad(lat1)
    lat2 <- gcd.rad(lat2)
  } else if ( type == "rad") {
    lon1 <- lon1
    lon2 <- lon2
    lat1 <- lat1
    lat2 <- lat2
  } else {
    stop("Error: argument 'type' must have value of 'deg' or 'rad'.\n")
  }
  
  R <- 6371 # Earth's mean radius [km]
  d <- acos(sin(lat1) * sin(lat2) + cos(lat1)
            * cos(lat2) * cos(lon2 - lon1)) * R
  if (km == FALSE) {
    # If km == FALSE, then the desired answer should be in miles.
    # Convert `d` from km to miles by multipling by `0.621371`.
    d <- d * 0.621371
  }
  return(d)
}
