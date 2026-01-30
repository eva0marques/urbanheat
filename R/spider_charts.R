#' Create spider charts to compare models efficiency within the day
#' @description Create spider charts to compare models efficiency within the day
#' @import fmsb
#' @importFrom lubridate hour
#' @importFrom dplyr vars summarise_at
#' @author Eva Marques
#' @export
spider_rmse <- function(scores, quantile) {
  time <- rmse_car <- rmse_cws <- rmse_joint <- NULL
  aa <- scores |>
    dplyr::group_by(lubridate::hour(time)) |>
    dplyr::summarise_at(
      dplyr::vars(rmse_joint), function(x) quantile(x, quantile)
    )

  bb <- scores |>
    dplyr::group_by(lubridate::hour(time)) |>
    dplyr::summarise_at(
      dplyr::vars(rmse_car), function(x) quantile(x, quantile)
    )

  cc <- scores |>
    dplyr::group_by(lubridate::hour(time)) |>
    dplyr::summarise_at(
      dplyr::vars(rmse_cws), function(x) quantile(x, quantile)
    )

  rmse <- cbind(aa, bb[2], cc[2], rep(0, 24))
  colnames(rmse) <- c("hour", "joint", "car", "cws", "zero")
  rmse <- as.data.frame(t(rmse[, c("joint", "car", "cws", "zero")]))
  colnames(rmse) <- seq(0, 23, 1)
  rmse <- rbind(rep(2, 24), rep(0, 24), rmse)
  rmse <- rmse[, rev(seq_len(ncol(rmse)))]
  pal <- load_palette("model")
  colors_border <- c(pal[["joint"]], pal[["car"]], pal[["cws"]], "blue")
  p <- fmsb::radarchart(rmse,
    axistype = 1,
    pcol = colors_border, plwd = 4, plty = 1,
    axislabcol = "black", caxislabels = seq(0, 2, 0.5)
  )
  legend(
    x = 1, y = 1, legend = rownames(rmse[-c(1, 2), ]), pch = 20,
    col = colors_border, text.col = "black", cex = 1.2, pt.cex = 3
  )
}
