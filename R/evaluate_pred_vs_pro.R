evaluate_pred <- function(pred, pro, info, borders) {
  ts <- unique(pred$time)
  stopifnot("not the same timestamp" = ts == as.POSIXct(info$time, tz = "UTC"))
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pro <- pro[which(between(pro$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  pred <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)

  # keep only pro stations within borders area
  sf::sf_use_s2(FALSE)
  borders_block <- sf::st_combine(borders)
  keep <- sf::st_intersects(borders_block, pro)[[1]]
  pro <- pro[keep, ]

  nn_idx <- sf::st_nearest_feature(pro, pred)

  # prediction mean (= mode = median)
  pro$pred_mean_car <- pred[nn_idx, c("pred_mean_car")]$pred_mean_car
  pro$pred_mean_cws <- pred[nn_idx, c("pred_mean_cws")]$pred_mean_cws
  pro$pred_mean_joint <- pred[nn_idx, c("pred_mean_joint")]$pred_mean_joint

  # residuals for each pro station
  if (info$y_var == "temp") {
    pro$res_joint <- pro$pred_mean_joint - pro$temp
    pro$res_car <- pro$pred_mean_car - pro$temp
    pro$res_cws <- pro$pred_mean_cws - pro$temp
    # rsq score
    info$rsq_car <- cor(pro$temp, pro$pred_mean_car)**2
    info$rsq_cws <- cor(pro$temp, pro$pred_mean_cws)**2
    info$rsq_joint <- cor(pro$temp, pro$pred_mean_joint)**2
  } else if (info$y_var == "temp_sea") {
    # compute altitude gradient correction for pro$temp
    pro$grad_z <- apply(
      pro[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$delta
    )
    pro$temp_sea <- apply(
      pro[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$temp_sea
    )
    # residuals
    pro$res_joint <- pro$pred_mean_joint - pro$temp_sea
    pro$res_car <- pro$pred_mean_car - pro$temp_sea
    pro$res_cws <- pro$pred_mean_cws - pro$temp_sea

    # rsq score
    info$rsq_car <- cor(pro$temp_sea, pro$pred_mean_car)**2
    info$rsq_cws <- cor(pro$temp_sea, pro$pred_mean_cws)**2
    info$rsq_joint <- cor(pro$temp_sea, pro$pred_mean_joint)**2
  } else {
    stop("y_var not recognized.")
  }

  # rmse
  info$rmse_car <- sqrt(sum((pro$res_car)**2, na.rm = TRUE) /
                 length(which(!is.na(pro$res_car))))
  info$rmse_cws <- sqrt(sum((pro$res_cws)**2, na.rm = TRUE) /
                       length(which(!is.na(pro$res_cws))))
  info$rmse_joint <- sqrt(sum((pro$res_joint)**2, na.rm = TRUE) /
                       length(which(!is.na(pro$res_joint))))

  # mae
  info$mae_car <- mean(abs(pro$res_car), na.rm = TRUE)
  info$mae_cws <- mean(abs(pro$res_cws), na.rm = TRUE)
  info$mae_joint <- mean(abs(pro$res_joint), na.rm = TRUE)

  # median residuals
  info$med_res_car <- median(pro$res_car, na.rm = TRUE)
  info$med_res_cws <- median(pro$res_cws, na.rm = TRUE)
  info$med_res_joint <- median(pro$res_joint , na.rm = TRUE)

  return(list("pro" = pro, "scores" = info))
}
