library(ggnewscale)
map_obs_outscale <- function(car, cws, ts, borders, y_var = "temp_sea") {
  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  car_plot <- car[which(dplyr::between(car$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  cws_plot <- cws[which(dplyr::between(cws$time, ts, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  obs_plot <- rbind(car_plot, cws_plot)

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
  obs_plot$hot <- ifelse(obs_plot$temp_sea <= 26, NA, ">26")
  p <- ggplot() +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    geom_point(
      data = obs_plot, aes(
        x = lon,
        y = lat,
        fill = .data[[y_var]],
        shape = network
      ),
      size = 3
    ) +
    scale_fill_gradientn(
      colours = pal,
      na.value = NA,
      breaks = seq(tn, tx, 1),
      limits = c(tn, min(tx, 26)),
      guide = guide_colorbar(barwidth = 17, barheight = 1, order = 1)
    ) +
    labs(fill = "T2M (°C)") +
    new_scale_fill() +
    geom_point(
      data = obs_plot[which(obs_plot$temp_sea > 26), ], aes(
      #data = obs_plot, aes(
        x = lon,
        y = lat,
        fill = .data[[y_var]],
        shape = network
      ),
      size = 3
    ) +
    scale_fill_gradientn(
      name = NULL,
      colours = "darkviolet",
      na.value = NA,
      breaks = c(26, tx),
      limits = c(26, tx),
      labels = c("-", as.character(tx)),
      guide = guide_colorbar(barwidth = 2, barheight = 1, order = 2)
    ) +
    coord_sf(crs = 4326) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    #guides(fill = guide_colourbar(barwidth = 23, barheight = 1.5)) +
    scale_shape_manual("", values = shape) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      #legend.box = "vertical",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 22),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}
