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

gcd.deg <- function(rad) return(rad * 180 / pi)