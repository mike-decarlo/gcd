#' Calculates the distance between geocoordinates using the Spherical Law of
#' Cosines, Haversine formula, and Vincenty inverse formula for ellipsoids.
#' 
#' The function \code{"gcd"} takes inputs of two sets of coordinates
#' in (degree values), one set fo reach location, and a boolean indicator of
#' whether or not to return the results as kilometers (\code{"km = TRUE"}) or
#' miles (\code{"km = FALSE"}). The output is three distances using the methods
#' of Spherical Law of Cosines, Haversine formula, and Vincenty inverse formula
#' for ellipsoids.
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
#' # Obtain measures of distnace
#' gcd.mi <- gcd(lon1 = deg.lon[1:500], lat1 = deg.lat[1:500]
#'   , lon2 = deg.lon[501:1000], lat2 = deg.lat[501:1000], km = FALSE)
#' 
#' @export

gcd <- function(lon1, lat1, lon2, lat2, km = TRUE) {
  lon1 <- gcd.rad(lon1)
  lon2 <- gcd.rad(lon2)
  lat1 <- gcd.rad(lat1)
  lat2 <- gcd.rad(lat2)
  
  return(list(
    sphere = gcd.slc(lon1, lat1, lon2, lat2, km = km)
    , haversine = gcd.haversine(lon1, lat1, lon2, lat2, km = km)
    , vincenty = gcd.vincenty(lon1, lat1, lon2, lat2, km = km)
  ))
}