map_obs <- function(car, cws, ts, borders) {
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  car_plot <- car[which(between(car$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  cws_plot <- cws[which(between(cws$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  obs_plot <- rbind(car_plot, cws_plot)
  tn <- floor(min(obs_plot$temp, na.rm = TRUE))
  tx <- ceiling(max(obs_plot$temp, na.rm = TRUE))
  pal <- load_palette("uhi")
  shape <- c("car" = 23, "cws" = 22)
  p <- ggplot() +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    geom_point(
      data = obs_plot, aes(x = lon, y = lat, fill = temp, shape = network),
      size = 3
    ) +
    coord_sf(crs = 4326) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    scale_shape_manual("", values = shape) +
    labs(fill = "T (°C)") +
    guides(fill = guide_colourbar(barwidth = 20, barheight = 1.5)) +
    ggspatial::annotation_scale(
      location = "tr", text_cex = 1.5,
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      height = unit(0.30, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}

map_pred_mean <- function(pred, pro, borders) {
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  tn <- floor(min(c(pred$pred_mean_joint, pro$temp)))
  tx <- ceiling(max(c(pred$pred_mean_joint, pro$temp)))
  pal <- load_palette("uhi")
  shape <- c("mustardijon" = 21)
  p <- ggplot() +
    geom_tile(
      data = pred_plot, aes(x = lon, y = lat, fill = pred_mean_joint),
      width = 0.0007, height = 0.0007
    ) +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    geom_point(
      data = pro, aes(x = lon, y = lat, fill = temp, shape = network),
      size = 3
    ) +
    coord_sf(crs = 4326) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 2),
      limits = c(tn, tx)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    scale_shape_manual("", values = shape, labels = "MUSTARDijon network") +
    labs(fill = "T (°C)") +
    guides(fill = guide_colourbar(barwidth = 20, barheight = 1.5)) +
    ggspatial::annotation_scale(
      location = "tr", text_cex = 1.5,
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      height = unit(0.30, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}


map_pred_sd <- function(pred, borders) {
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  sdn <- floor(min(pred$pred_sd_joint) * 10) / 10
  sdx <- ceiling(max(pred$pred_sd_joint) * 10) / 10
  pal <- load_palette("reds")

  p <- ggplot() +
    geom_tile(
      data = pred_plot, aes(x = lon, y = lat, fill = pred_sd_joint),
      width = 0.0007, height = 0.0007
    ) +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    coord_sf(crs = 4326) +
    scale_fill_stepsn(
      colours = pal,
      limits = c(sdn, sdx),
      breaks = seq(sdn, sdx, .1),
      labels = seq(sdn, sdx, .1)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    labs(fill = latex2exp::TeX("$\\sigma$")) +
    guides(fill = guide_colourbar(barwidth = 20, barheight = 1.5)) +
    ggspatial::annotation_scale(
      location = "tr", text_cex = 1.5,
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      height = unit(0.30, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}



