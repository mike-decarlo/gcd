gcd.circumfrance <- function(lat, lon, radius, unit = "mi", npoints = 100) {
  
  # df.out <- data.frame(matrix(nrow = npoints, ncol = 2))
  
  if (unit == "mi") {
    
    radius <- radius / 0.62137 # conver to km for haversine
    
  }
  
  R <- 6371 # Earth's mean radius [km]
  
  
  # colnames(df.out) <- c("Latitude", "Longitude")
  # return(df.out)
  
}