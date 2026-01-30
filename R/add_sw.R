#' Add shortwave radiations to a dataframe
#' @description Add shortwave radiations to a dataframe
#' @param x dataframe
#' @param rad dataframe
#' @return x with an additional column sw
#' @author Eva Marques
#' @export
add_sw <- function(x, rad) {
  GLO <- NULL
  x <- merge(x,
    rad[, c("DATE", "GLO")],
    all.y = FALSE,
    by.x = "time",
    by.y = "DATE"
  ) |>
    dplyr::rename(sw = GLO)
  # change shortwave radiations in W.m-2
  x$sw <- x$sw * 10000 / 3600
  x$day_night <- ifelse(x$sw == 0, "night", "day")
  return(x)
}
