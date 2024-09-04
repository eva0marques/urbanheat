plot_pred_mean <- function(pred) {
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) |>
    sf::st_transform(crs = 2154)
  tn <- floor(min(test$pred$pred_mean_joint))
  tx <- ceiling(max(test$pred$pred_mean_joint))
  pal <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
  p_pred <- ggplot() +
    geom_tile(
      data = pred_plot, aes(x = lon, y = lat, fill = pred_mean_joint),
      width = 0.0007, height = 0.0007
    ) +
    #geom_sf(
    #  data = pred_plot,
    #  aes(geometry = geometry, color = pred_mean_joint),
    #  size = 1,
    #  shape = 15
    #) +
    #scale_color_gradientn(
    #  colours = pal,
    #  na.value = NA,
    #  breaks = seq(tn, tx, 2),
    #  limits = c(tn, tx)
    #) +
    coord_sf(crs = 4326) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    ggtitle("Prediction mean") +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p_pred)
}

plot_marginals_joint <- function(mod) {
  marg <- extract_marginals(mod)
  linetype <- c("simul" = "solid",
                "prior" = "dotted",
                "post" = "dashed")
  color <- c('car'='#FF0800' , 'cws'='#00BFFF', 'temp.reg'='darkgreen')
  gg <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
    # stat_function(
    #   fun = dnorm, n = 101,
    #   args = list(
    #     mean = mu_car,
    #     sd = sqrt(1 / prec_int_car)
    #   ),
    #   aes(color = "car", linetype = "prior")) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = marg$int_car_mean,
        sd = marg$int_car_sd
      ),
      aes(color = "car", linetype = "post")) +
    # stat_function(
    #   fun = dnorm, n = 101,
    #   args = list(
    #     mean = mu_cws,
    #     sd = sqrt(1 / prec_int_cws)
    #   ),
    #   aes(color = "cws", linetype = "prior")) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = marg$int_cws_mean,
        sd = marg$int_cws_sd
      ),
      aes(color = "cws", linetype = "post")) +
    ggtitle("car and cws intercepts") +
    scale_color_manual("", values = color) +
    scale_linetype_manual("", values = linetype)
  return(gg)
}


plot_pred_mean_car <- function(pred) {
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = 2154)
  tn <- floor(min(test$pred$pred_mean_car))
  tx <- ceiling(max(test$pred$pred_mean_car))
  pal <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
  p_pred <- ggplot() +
    geom_sf(
      data = pred_plot,
      aes(geometry = geometry, color = pred_mean_car),
      size = 1,
      shape = 15
    ) +
    scale_color_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    ggtitle("Prediction mean") +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p_pred)
}

plot_pred_mean_cws <- function(pred) {
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = 2154)
  tn <- floor(min(test$pred$pred_mean_cws))
  tx <- ceiling(max(test$pred$pred_mean_cws))
  pal <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
  p_pred <- ggplot() +
    geom_sf(
      data = pred_plot,
      aes(geometry = geometry, color = pred_mean_cws),
      size = 1,
      shape = 15
    ) +
    scale_color_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    ggtitle("Prediction mean") +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p_pred)
}


plot_marginals_joint <- function(mod) {
  marg <- extract_marginals(mod)
  linetype <- c("simul"='solid',
                "prior"='dotted',
                'post'='dashed')
  color <- c('car'='#FF0800' , 'cws'='#00BFFF', 'temp.reg'='darkgreen')
  gg <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
    # stat_function(
    #   fun = dnorm, n = 101,
    #   args = list(
    #     mean = mu_car,
    #     sd = sqrt(1 / prec_int_car)
    #   ),
    #   aes(color = "car", linetype = "prior")) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = marg$int_car_mean,
        sd = marg$int_car_sd
      ),
      aes(color = "car", linetype = "post")) +
    # stat_function(
    #   fun = dnorm, n = 101,
    #   args = list(
    #     mean = mu_cws,
    #     sd = sqrt(1 / prec_int_cws)
    #   ),
    #   aes(color = "cws", linetype = "prior")) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = marg$int_cws_mean,
        sd = marg$int_cws_sd
      ),
      aes(color = "cws", linetype = "post")) +
    ggtitle("car and cws intercepts") +
    scale_color_manual("", values = color) +
    scale_linetype_manual("", values = linetype)
  return(gg)
}

