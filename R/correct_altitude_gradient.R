#' Correct 2m-temperature with altitude gradient
#' @description Correct 2m-temperature with altitude gradient
#' @author Eva Marques
#' @export
grad_alt <- function(z, temp) {
  temp <- as.numeric(temp)
  z <- as.numeric(z)
  delta <- -0.6 * z / 100
  temp_sea <- temp - delta
  return(list(delta = delta, temp_sea = temp_sea))
}
