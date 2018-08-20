#' Calculates distance between geocoordinates: Vincenty inverse formula for
#'   ellipsoids
#' 
#' \code{dist_vincenty} takes inputs of two sets of coordinates
#' in (radian values), one set fo reach location, and a boolean indicator of
#' whether or not to return the results as kilometers (\code{km = TRUE}) or
#' miles (\code{km = FALSE}). The output is the distance using the method
#' of the Vincenty inverse formula for ellipsoinds, a more robust distance
#' between two points on the surface of an ellipsoid (Earth).
#' @param lat1 the latitude as radians of the first point
#' @param lon1 the longitude as radians of the first point
#' @param lat2 the latitude as radians of the second point
#' @param lon2 the longitude as radians of the second point
#' @param type defaults to "deg", can also be "rad"
#' @param km boolean argument for whether to return results as km (TRUE) or
#'   miles (FALSE)
#' @examples
#' # Input list of degree values
#' # Longitude values range between 0 and +-180 degrees
#' deg.lon <- runif(1000, -180, 180)
#' # Latitude values range between 0 and +-90 degrees
#' deg.lat <- runif(1000, -90, 90)
#'
#' # Obtain measures of distnace
#' vin.mi <- gcd.vincenty(lon1 = deg.lon[1:500], lat1 = deg.lat[1:500]
#'   , lon2 = deg.lon[501:1000], lat2 = deg.lat[501:1000], km = FALSE)
#' @export
dist_vincenty <- function(lat1, lon1, lat2, lon2, type = "deg", km = TRUE) {
  for (i in c("lat1", "lon1", "lat2", "lon2")) {
    if (is.numeric(get(i)) == FALSE) {
      stop(paste0("Argument ", i, " must be numeric.\n"))
    }
  }
  if (type == "deg") {
    lon1 <- to_rad(lon1)
    lon2 <- to_rad(lon2)
    lat1 <- to_rad(lat1)
    lat2 <- to_rad(lat2)
  } else if ( type == "rad") {
    lon1 <- lon1
    lon2 <- lon2
    lat1 <- lat1
    lat2 <- lat2
  } else {
    stop("Error: argument 'type' must have value of 'deg' or 'rad'.\n")
  }
  a <- 6378137         # length of major axis of ellipsoid (radius at equator)
  b <- 6356752.314245  # length of major axis of ellipsoid (radius at poles)
  f <- 1/298.257223563 # flattening of ellipsoid
  l <- lon2 - lon1 # difference in longitude
  u1 <- atan( (1 - f) * tan(lat1)) # reduced latitude
  u2 <- atan( (1 - f) * tan(lat2)) # reduced latitude
  sin_u1 <- sin(u1)
  cos_u1 <- cos(u1)
  sin_u2 <- sin(u2)
  cos_u2 <- cos(u2)
  cos_sq_alpha <- NULL
  sin_sigma <- NULL
  cos_sigma <- NULL
  cos2_sigma_m <- NULL
  sigma <- NULL
  lambda <- l
  lambda_p <- 0
  iter_limit <- 100
  while (abs(lambda - lambda_p) > 1e-12 & iter_limit > 0) {
    sin_lambda <- sin(lambda)
    cos_lambda <- cos(lambda)
    sin_sigma <- sqrt( (cos_u2 * sin_lambda) ^ 2
                       + (cos_u1 * sin_u2 - sin_u1 * cos_u2 * cos_lambda) ^ 2)
    if (sin_sigma == 0) return(0) # Co-incident points
    cos_sigma <- sin_u1 * sin_u2 + cos_u1 * cos_u2 * cos_lambda
    sigma <- atan2(sin_sigma, cos_sigma)
    sin_alpha <- cos_u1 * cos_u2 * sin_lambda / sin_sigma
    cos_sq_alpha <- 1 - sin_alpha ^ 2
    cos2_sigma_m <- cos_sigma - (2 * cos_u1 * cos_u2) / cos_sq_alpha
    if (is.na(cos2_sigma_m)) cos2_sigma_m <- 0 # Eqtrl line: cos_sq_alpha = 0
    c <- f / 16 * cos_sq_alpha * (4 + f * (4 - 3 * cos_sq_alpha))
    lambda_p <- lambda
    lambda <- l + (1 - c) * f * sin_alpha * (sigma + c * sin_sigma
      * (cos2_sigma_m + c * cos_sigma * (-1 + 2 * cos2_sigma_m ^ 2)))
    iter_limit <- iter_limit - 1
  }
  if (iter_limit == 0) return(NA) # formula failed to converge
  u_sq <- cos_sq_alpha * (a ^ 2 - b ^ 2) / (b ^ 2)
  a_ <- 1 + u_sq / 16384 * (4096 + u_sq * (-768 * u_sq * (320 - 175 * u_sq)))
  b_ <- u_sq / 1024 * (256 + u_sq * (-128 + u_sq * (74 - 47 * u_sq)))
  delta_sigma <- b_ * sin_sigma * (cos2_sigma_m + b_ / 4 * (cos_sigma
    * (-1 + 2 * cos2_sigma_m ^ 2) - b_ / 6 * cos2_sigma_m
    * (-3 + 4 * sin_sigma ^ 2) * (-3 + 4 * cos2_sigma_m ^ 2)))
  s <- b * a_ * (sigma - delta_sigma) / 1000
  if (km == FALSE) {
    # If km == FALSE, then the desired answer should be in miles.
    # Convert `d` from km to miles by multipling by `0.621371`.
    s <- s * 0.621371
  }
  return(s)
}
