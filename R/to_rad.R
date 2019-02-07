#' Converts degrees to radians
#'
#' \code{to_rad} takes an input of a degree value and will
#' convert it to a radian value by the formula:
#' \code{radian = degree * pi / 180}
#' Typically, this conversion would be in preparation of geocoordinate
#' calculations in which the desired form of the coordinates is radians.
#' @param deg a value in degrees
#' @examples
#' # Input list of degree values
#' # Longitude values range between -180 and +180
#' deg <- runif(1000, -180, 180)
#' rad <- to_rad(deg)
#' @export
to_rad <- function(deg) {
    options(warn = -1)
  if (abs(deg) <= 180) {
    options(warn = 0)
    return(deg * pi / 180)
  } else {
    stop("Degree value must be between 180 and -180.\n")
  }
}
