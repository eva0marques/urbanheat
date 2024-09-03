evaluate_pred <- function(pred, pro) {
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pro <- pro[which(between(pro$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  pred <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  nn_idx <- sf::st_nearest_feature(pro, pred)
  pro$res_joint <- pred[nn_idx, c("pred_mean_joint")]$pred_mean_joint - pro$temp
  pro$res_car <- pred[nn_idx, c("pred_mean_car")]$pred_mean_car - pro$temp
  pro$res_cws <- pred[nn_idx, c("pred_mean_cws")]$pred_mean_cws - pro$temp
  # -- compute r2
  pred_joint <- pred[nn_idx, c("pred_mean_joint")]$pred_mean_joint
  dev_avg <- pro$temp - mean(pro$temp, na.rm = T)
  #r2_car <- 1 - (sum((pro$res_car)**2, na.rm = T) / sum((dev_avg)^2))
  r2_car <- cor(pro$temp, pred[nn_idx, c("pred_mean_car")]$pred_mean_car)**2
  rmse_car <- sqrt(sum((pro$res_car)**2, na.rm = T) /
                 length(which(!is.na(pro$res_car))))
  #r2_cws <- 1 - (sum((pro$res_cws)**2, na.rm = T) / sum((dev_avg)^2))
  r2_cws <- cor(pro$temp, pred[nn_idx, c("pred_mean_cws")]$pred_mean_cws)**2
  rmse_cws <- sqrt(sum((pro$res_cws)**2, na.rm = T) /
                       length(which(!is.na(pro$res_cws))))
  #r2_joint <- 1 - (sum((pro$res_joint)**2, na.rm = T) / sum((dev_avg)^2))
  r2_joint <- cor(pro$temp, pred[nn_idx,
                                 c("pred_mean_joint")]$pred_mean_joint)**2
  rmse_joint <- sqrt(sum((pro$res_joint)**2, na.rm = T) /
                       length(which(!is.na(pro$res_joint))))
  return(list("pro" = pro,
              "r2_car" = r2_car,
              "rmse_car" = rmse_car,
              "r2_cws" = r2_cws,
              "rmse_cws" = rmse_cws,
              "r2_joint" = r2_joint,
              "rmse_joint" = rmse_joint))
}
