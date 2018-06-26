#' Converts radians to degrees
#' 
#' The function \code{"gcd.deg"} takes an input of a radian value and will
#' convert it to a degree value by the formula:
#' \code{"degree = radian * 180 / pi"}
#' This utility conversion is useful when dealing with geocoding coordinates,
#' as it might be desirable to obtain the original coordinate values in
#' degrees.
#' @param rad a value in radians
#' 
#' @export
#' 
#' @examples
#' # Input list of degree values
#' # Longitude values range between 0 and +-180 degrees
#' rad <- runif(1000, -180, 180) * pi / 180
#' deg <- gcd.deg(rad)

gcd.deg <- function(rad) {
    options(warn = -1)
  if (abs(rad) <= 2 * pi) {
    options(warn = 0)
    return(rad * 180 / pi)
  } else {
    stop("Radian value must not exceed 2π.\n")
  }
}