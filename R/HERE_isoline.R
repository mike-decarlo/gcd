#' Get polygon line drawing output from HERE.com
#'
#' \code{HERE_isoline} is designed to take
#' @param app_id a string; the user's App ID for the HERE.com JavaScript/REST
#'   (requires registration)
#' @param app_code a string; the user's App Code for the HERE.com
#'   JavaScript/REST (requires registration)
#' @param origin a vector of length two (2); the latitude and longitude
#'   coordinates of the origin location
#' @param coord_type a string; if the geocoordinates are in degrees or radians:
#'   \enumerate{
#'   \item \code{"rad"}{ (radians)}
#'   \item \code{"deg"}{ (degrees)}
#'   }
#' @param mode_type a string; a method of deciding optimal travel route:
#'   \enumerate{
#'   \item \code{"fastest"}
#'   \item \code{"shortest"}
#'   }
#' @param mode_tran a string; a transportation method for the route:
#'   \enumerate{
#'   \item \code{"car"}
#'   \item \code{"carHOV"}
#'   \item \code{"pedestrian"}
#'   \item \code{"truck"}
#'   \item \code{"bicycle"}
#'   }
#' @param mode_traf a string; whether or not to factor in traffic:
#'   \enumerate{
#'   \item \code{"disabled"}
#'   \item \code{"enabled"}
#'   }
#' @param range_type a string; what range constraint the user desires to use:
#'   \enumerate{
#'   \item \code{"distance"}
#'   \item \code{"time"}
#'   }
#' @param range_unit a string; the units the provided range are given in:
#'   \enumerate{
#'   \item \code{"mi"}
#'   \item \code{"ft"}
#'   \item \code{"km"}
#'   \item \code{"m"}
#'   \item \code{"hrs"}
#'   \item \code{"min"}
#'   \item \code{"sec"}
#'   }
#' @param range_val a numeric; the value of the range constraint
#' @param dev a boolean; whether to use development or production site
#' @param verbose a boolean; \code{TRUE} to display json webaddress,
#'   \code{FALSE} to not display json webaddress
#' @return the polygon data.frame
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl curlEscape
#' @export
HERE_isoline <- function(
  app_id = NULL, app_code = NULL, origin = NULL, coord_type = "deg"
  , mode_type = "fastest", mode_tran = "car", mode_traf = "disabled"
  , range_type = "distance", range_unit = "mi", range_val = NULL
  , dev = FALSE, verbose = FALSE
) {
  if (dev == TRUE) {
    base <- paste0(
      "https://isoline.route.cit.api.here.com/"
      , "routing/7.2/calculateisoline.json?"
    )
  } else if (dev == FALSE) {
    base <- paste0(
      "https://isoline.route.api.here.com/"
      , "routing/7.2/calculateisoline.json?"
    )
  } else {
    stop(
      "Error: Argument 'dev' must be given value of either TRUE or FALSE.\n"
    )
  }
  if (length(origin) != 2) {
    stop("\nArgument 'origin' must be of length 2.\n")
  } else {
    if (coord_type == "rad") {
      origin <- gcd::to_deg(origin)
    }
  }
  if (range_type == "distance") {
    if (range_unit == "mi") {
      range_val_n <- round(range_val * 1609.34, digits = 0)
    } else if(range_unit == "ft") {
      range_val_n <- round(range_val * 0.3048, digits = 0)
    } else if (range_unit == "km") {
      range_val_n <- round(range_val * 1000, digits = 0)
    } else if (range_unit == "m") {
      range_val_n <- round(range_val, digits = 0)
    } else {
      stop(
        paste0(
          "\nDistance range unit must be 'mi' (miles), 'ft' (feet)"
          , ", 'km' (kilometers), or 'm' (meters).\n"
        )
      )
    }
  } else if (range_type == "time") {
    if (range_unit == "hrs") {
      range_val_n <- round(range_val * 3600, digits = 0)
    } else if (range_val == "min") {
      range_val_n <- round(range_val * 60, digits = 0)
    } else if (range_val == "sec") {
      range_val_n <- round(range_val, digits = 0)
    } else {
      stop(
        paste0(
          "\nTime range unit must be in 'hrs' (hours), 'min' (minutes)"
          , ", or 'sec' (seconds).\n"
        )
      )
    }
  }
  id <- paste0("&app_id=", RCurl::curlEscape(app_id))
  code <- paste0("&app_code=", RCurl::curlEscape(app_code))
  mode <- paste0("&mode=", mode_type, ";", mode_tran, ";traffic:", mode_traf)
  range_t <- paste0("&rangetype=", range_type)
  start <- paste0("&start=geo!", origin[1], ",", origin[2])
  range <- paste0("&range=", range_val_n)
  request_url <- paste0(base, id, code, mode, range_t, start, range)
  if (verbose == TRUE) {
    message(request_url)
  }
  json <- jsonlite::fromJSON(request_url, flatten = FALSE)
  iso_ll <- data.frame(
    lat_lon = matrix(
      unlist(
        json$response$isoline$component[[1]]$shape
      )
    , byrow = T
    )
  )
  iso_ll$lat_lon <- as.character(iso_ll$lat_lon)
  iso <- data.frame(
    do.call(
      rbind
      , strsplit(x = iso_ll$lat_lon, split = ",")
    )
  )
  names(iso) <- c("lat", "lon")
  rm(iso_ll)
  uid <- paste(
    paste(origin[[1]], origin[[2]], sep = ",")
    , range_val, sep = ";"
  )
  mode_type <- rep(mode_type, nrow(iso))
  mode_tran <- rep(mode_tran, nrow(iso))
  mode_traf <- rep(mode_traf, nrow(iso))
  origin_lat <- rep(origin[[1]], nrow(iso))
  origin_lon <- rep(origin[[2]], nrow(iso))
  range_type <- rep(range_type, nrow(iso))
  range <- rep(range_val, nrow(iso))
  polygon_order <- seq(1, nrow(iso))
  iso_df <- cbind(
    "uid" = iso$uid
    , "origin_lat" = origin_lat
    , "origin_lon" = origin_lon
    , "polygon_order" = polygon_order
    , "polygon_lat" = as.numeric(as.character(iso$lat))
    , "polygon_lon" = as.numeric(as.character(iso$lon))
    , "range_type" = range_type
    , "range" = range_val
    , "mode_type" = mode_type
    , "mode_tran" = mode_tran
    , "mode_traf" = mode_traf
  )
  iso_df
}
