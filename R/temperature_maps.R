map_obs <- function(car, cws, ts, borders, y_var = "temp_sea") {
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  car_plot <- car[which(dplyr::between(car$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  cws_plot <- cws[which(dplyr::between(cws$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  obs_plot <- rbind(car_plot, cws_plot)

  # sf::sf_use_s2(FALSE)
  # borders_block <- sf::st_combine(borders)
  # keep <- sf::st_intersects(borders_block, obs_plot)[[1]]
  # obs_plot <- obs_plot[keep, ]

  if (y_var == "temp_sea") {
    # compute altitude gradient correction for pro$temp
    obs_plot$grad_z <- apply(
      obs_plot[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$delta
    )
    obs_plot$temp_sea <- apply(
      obs_plot[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$temp_sea
    )
    tn <- floor(min(obs_plot$temp_sea, na.rm = TRUE))
    tx <- ceiling(max(obs_plot$temp_sea, na.rm = TRUE))
    obs_plot <- obs_plot[which(!(is.na(obs_plot$temp_sea))), ]

  } else if (y_var == "temp") {
    tn <- floor(min(obs_plot$temp, na.rm = TRUE))
    tx <- ceiling(max(obs_plot$temp, na.rm = TRUE))
    obs_plot <- obs_plot[which(!is.na(obs_plot$temp)), ]
  } else {
    stop("y_var not recognized.")
  }
  pal <- load_palette("uhi")
  shape <- c("car" = 23, "cws" = 22)
  p <- ggplot() +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    geom_point(
      data = obs_plot, aes(x = lon,
                           y = lat,
                           fill = .data[[y_var]],
                           shape = network),
      size = 3
    ) +
    coord_sf(crs = 4326) +
    scale_fill_gradientn(
     colours = pal,
     na.value = NA,
     breaks = seq(tn, tx, 1),
     limits = c(tn, tx)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    scale_shape_manual("", values = shape) +
    labs(fill = "T (°C)") +
    guides(fill = guide_colourbar(barwidth = 23, barheight = 1.5)) +
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

map_pred_mean <- function(pred,
                          pro,
                          borders,
                          y_var = "temp_sea",
                          model = "joint") {
  pred_mean_model <- paste0("pred_mean_", model)
  stopifnot("model is not one of car, cws, joint" =
              model %in% c("car", "cws", "joint"))
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  pro <- pro |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  if (y_var == "temp_sea") {
    tn <- floor(min(c(pred[, pred_mean_model], pro$temp_sea)))
    tx <- ceiling(max(c(pred[, pred_mean_model], pro$temp_sea)))
  } else if (y_var == "temp") {
    tn <- floor(min(c(pred[, pred_mean_model], pro$temp)))
    tx <- ceiling(max(c(pred[, pred_mean_model], pro$temp)))
  } else {
    stop("y_var not recognized.")
  }
  pal <- load_palette("uhi")
  shape <- c("mustardijon" = 21)
  p <- ggplot() +
    geom_tile(
      data = pred_plot, aes(x = lon,
                            y = lat,
                            fill = .data[[pred_mean_model]]),
      width = 0.0007, height = 0.0007
    ) +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    geom_point(
      data = pro, aes(x = lon,
                      y = lat,
                      fill = .data[[y_var]],
                      shape = network),
      size = 3
    ) +
    coord_sf(crs = 4326) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 1),
      limits = c(tn, tx)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    scale_shape_manual("", values = shape, labels = "MUSTARDijon network") +
    labs(fill = "T (°C)") +
    guides(fill = guide_colourbar(barwidth = 23, barheight = 1.5, order = 1)
    ) +
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


map_pred_sd <- function(pred, borders, model = "joint") {
  pred_sd_model <- paste0("pred_sd_", model)
  ts <- unique(pred$time)
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  sdn <- floor(min(pred[, pred_sd_model]) * 10) / 10
  sdx <- ceiling(max(pred[, pred_sd_model]) * 10) / 10
  pal <- load_palette("reds")

  p <- ggplot() +
    geom_tile(
      data = pred_plot, aes(x = lon,
                            y = lat,
                            fill = .data[[pred_sd_model]]),
      width = 0.0007, height = 0.0007
    ) +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    coord_sf(crs = 4326) +
    scale_fill_stepsn(
      colours = pal,
      #limits = c(sdn, sdx),
      #breaks = seq(0, sdx, .1),
      #labels = seq(0, sdx, .1)
      limits = c(0, 1.6),
      breaks = seq(0, 1.6, .2),
      labels = seq(0, 1.6, .2)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    labs(fill = latex2exp::TeX("$\\sigma$")) +
    guides(fill = guide_colourbar(barwidth = 23, barheight = 1.5)) +
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

