require(fmsb)

spider_rmse <- function(scores, quantile) {
  A <- scores |>
    dplyr::group_by(hour(time)) |>
    dplyr::summarise_at(vars(rmse_joint), function(x) quantile(x, quantile))

  B <- scores |>
    dplyr::group_by(hour(time)) |>
    dplyr::summarise_at(vars(rmse_car), function(x) quantile(x, quantile))

  C <- scores |>
    dplyr::group_by(hour(time)) |>
    dplyr::summarise_at(vars(rmse_cws), function(x) quantile(x, quantile))

  rmse <- cbind(A, B[2], C[2], rep(0, 24))
  colnames(rmse) <- c("hour", "joint", "car", "cws", "zero")
  rmse <- as.data.frame(t(rmse[, c("joint", "car", "cws", "zero")]))
  colnames(rmse) <- seq(0, 23, 1)
  rmse <- rbind(rep(2, 24), rep(0, 24), rmse)
  rmse <- rmse[, order(ncol(rmse):1)]
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
