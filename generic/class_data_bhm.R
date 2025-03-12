#' S4 constructor for weather station hourly observations
setClass("data_bhm",
  contains = c("data.frame")
)


setValidity("data_bhm", function(object) {
  stopifnot(
    "object is not a dataframe" =
      methods::is(object, "data.frame"),
    "temp must be numeric" =
      is.numeric(object$temp),
    "lat must be numeric" =
      is.numeric(object$lat),
    "lon must be numeric" =
      is.numeric(object$lon),
    "time must be a POSIXct" =
      lubridate::is.POSIXct(object$time),
    "build_h must be numeric" =
      is.numeric(object$build_h),
    "build_d must be numeric" =
      is.numeric(object$build_d),
    "dem must be numeric" =
      is.numeric(object$dem),
    "time must be in UTC" =
      attr(object$time, "tzone") == "UTC",
    "network must be a character" =
      is.character(object$network)
  )
  return(TRUE)
})


#' Build a data_bhm from a data.frame, data.table, sf or sftime
#' @param x a data.frame, data.table, sf or sftime
#' @param temp the column name for the temperature in degree celcius
#' @param lat the column name for the latitude
#' @param lon the column name for the longitude
#' @param time the column name for the time in UTC
#' @param network the name of the network
#' @return a data_bhm object
#' @importFrom methods new
#' @importFrom dplyr rename
#' @author Eva Marques
data_bhm <- function(x,
                     temp = "temp",
                     lat = "lat",
                     lon = "lon",
                     time = "time",
                     build_h,
                     build_d,
                     dem,
                     network) {
  stopifnot(
    "x is not a data.frame, data.table, sf or sftime." =
      class(x)[1] %in% c("data.frame", "data.table", "sf", "sftime"),
    "time, temp, lat, lon, build_d, build_h, dem are not all characters." =
      is.character(time) &
        is.character(temp) &
        is.character(lat) &
        is.character(lon) &
        is.character(build_h) &
        is.character(build_d) & #
        is.character(dem),
    "temp, lat, lon, time, build_h, build_d, dem columns missing or mispelled." =
      c(temp, lat, lon, time, build_h, build_d, dem) %in% colnames(x)
  )
  x <- as.data.frame(x)
  y <- x |>
    dplyr::rename("temp" = temp) |>
    dplyr::rename("lat" = lat) |>
    dplyr::rename("lon" = lon) |>
    dplyr::rename("time" = time) |>
    dplyr::rename("build_h" = build_h) |>
    dplyr::rename("build_d" = build_d) |>
    dplyr::rename("dem" = dem)
  y$time <- as.POSIXct(y$time, tz = "UTC")
  y$network <- network
  y <- y[, c("temp", "lat", "lon", "time", "build_h", "build_d", "dem", "network")] |>
    methods::new(Class = "data_bhm")
  return(y)
}
