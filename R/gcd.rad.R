#' Converts degrees to radians
#' 
#' The function \code{"gcd.rad"} takes an input of a degree value and will
#' convert it to a radian value by the formula:
#' \code{"radian = degree * pi / 180"}
#' Typically, this conversion would be in preparation of geocoordinate
#' calculations in which the desired form of the coordinates is radians.
#' @param deg a value in degrees
#' 
#' @export
#' 
#' @examples
#' # Input list of degree values
#' # Longitude values range between 0 and +-180 degrees
#' deg <- runif(1000, -180, 180)
#' rad <- gcd.rad(deg)

gcd.rad <- function(deg) {
    options(warn = -1)
  if (abs(deg) <= 180) {
    options(warn = 0)
    return(deg * pi / 180)
  } else {
      stop("Degree value must be between 180 and -180.\n")
  }
}