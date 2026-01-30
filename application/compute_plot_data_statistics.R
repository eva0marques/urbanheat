in_path <- paste0(getwd(), "/application/input/")
out_path <- paste0(getwd(), "/application/car_cws_statistics/")

# open data
car <- data.table::fread(paste0(in_path, "car_processed_for_bhm.csv")) |>
  data_bhm(
    temp = "temp",
    lat = "lat",
    lon = "lon",
    time = "time",
    build_h = "H_MEAN",
    build_d = "BUILD_DENS",
    dem = "alt",
    network = "car"
  )
cws_before_qc <- data.table::fread(
  paste0(
    in_path,
    "cws_processed_for_bhm.csv"
  )
)
cws <- cws_before_qc[which(cws_before_qc$m4), ] |>
  data_bhm(
    temp = "temp",
    lat = "lat",
    lon = "lon",
    time = "time",
    build_h = "H_MEAN",
    build_d = "BUILD_DENS",
    dem = "alt.y",
    network = "cws"
  )

nrow(car)
car$time_str <- format(car$time, "%Y%m%d%H")
car$day <- weekdays(car$time)
car$hour <- hour(car$time)

cws$time_str <- format(cws$time, "%Y%m%d%H")
cws$day <- weekdays(cws$time)
cws$hour <- hour(cws$time)

car_stats <- car |>
  dplyr::count(time_str, name = "freq")
car_stats <- merge(
  unique(car[, c("day", "hour", "time_str")]),
  car_stats,
  by = "time_str"
)

p0 <- ggplot(car_stats) +
  geom_boxplot(aes(x = hour, y = freq, group = hour)) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 18),
    axis.text.y = ggplot2::element_text(
      size = 18,
      angle = 90,
      hjust = .5
    ),
    plot.caption = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 22),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

dict_week <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)
car_stats$day <- factor(car_stats$day, levels = dict_week)
p1 <- ggplot(car_stats) +
  geom_boxplot(aes(x = day, y = freq, group = day)) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 18),
    axis.text.y = ggplot2::element_text(
      size = 18,
      angle = 90,
      hjust = .5
    ),
    plot.caption = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 22),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )


cws_stats <- cws |>
  dplyr::count(time_str, name = "freq")
cws_stats <- merge(
  unique(cws[, c("day", "hour", "time_str")]),
  cws_stats,
  by = "time_str"
)

p2 <- ggplot(cws_stats) +
  geom_boxplot(aes(x = hour, y = freq, group = hour)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 18),
    axis.text.y = ggplot2::element_text(
      size = 18,
      angle = 90,
      hjust = .5
    ),
    plot.caption = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 22),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

dict_week <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)
cws_stats$day <- factor(cws_stats$day, levels = dict_week)
p3 <- ggplot(cws_stats) +
  geom_boxplot(aes(x = day, y = freq, group = day)) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 18),
    axis.text.y = ggplot2::element_text(
      size = 18,
      angle = 90,
      hjust = .5
    ),
    plot.caption = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 22),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

p02 <- ggpubr::ggarrange(
  plotlist = list(p0, p2),
  align = "v",
  nrow = 2,
  ncol = 1,
  common.legend = TRUE
)
p13 <- ggpubr::ggarrange(
  plotlist = list(p1, p3),
  align = "v",
  nrow = 2,
  ncol = 1,
  common.legend = TRUE
)

p <- ggpubr::ggarrange(plotlist = list(p02, p13), nrow = 1, ncol = 2)
ggsave(
  plot = p,
  paste0(out_path, "boxplot_data_freq.pdf"),
  width = 17,
  height = 12
)


x <- list()
for (i in 0:23) {
  x <- rbind(
    x,
    c(
      "hour" = i,
      summary(car_stats[which(car_stats$hour == i), ]$freq)
    ),
    c(
      "hour" = i,
      summary(cws_stats[which(cws_stats$hour == i), ]$freq)
    )
  )
}
colnames(x) <- c(
  "hour",
  "nmin",
  "nfirstq",
  "nmedian",
  "nmean",
  "nthirdq",
  "nmax"
)
x <- as.data.frame(x)
x$source <- rep(c("car", "cws"), 24)

x[which(x$source == "car"), ]

cws_sites <- unique(cws[, c("lon", "lat", "hour")])

p_map <- ggplot(car) +
  stat_density_2d(
    mapping = aes(
      x = lon,
      y = lat,
      fill = stat(density)
    ),
    geom = "tile",
    contour = FALSE,
    alpha = 0.8
  ) +
  facet_wrap(~hour, nrow = 4) +
  coord_equal() +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_test()

ggsave(
  plot = p_map,
  paste0(out_path, "map_car_density_per_hour.pdf"),
  width = 15,
  height = 10
)


roads <- terra::vect(paste0(in_path, "roads_cote_d_or.shp"))
unique(roads$nature)
myroads <- roads[
  which(roads$nature %in% c(
    "Type autoroutier",
    "Route à 1 chaussée",
    "Route à 2 chaussées"
  )),
]
p <- ggplot2::ggplot(car) +
  stat_density_2d(
    mapping = aes(
      x = lon,
      y = lat,
      fill = stat(density)
    ),
    geom = "tile",
    contour = FALSE,
    alpha = 0.8
  ) +
  facet_wrap(~hour, nrow = 3) +
  scale_fill_gradient(low = "white", high = "darkred") +
  tidyterra::geom_spatvector(
    data = myroads,
    ggplot2::aes(),
    alpha = .1,
    stroke = 0.005
  ) +
  scale_linewidth_discrete(
    breaks = c(
      "Type autoroutier" = .1,
      "Route à 1 chaussée" = .05,
      "Route à 2 chaussées" = .01
    )
  ) +
  ggspatial::annotation_scale(
    location = "bl", pad_x = ggplot2::unit(1, "cm"),
    pad_y = ggplot2::unit(1, "cm"),
    height = ggplot2::unit(0.30, "cm"),
    text_cex = 1
  ) +
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = ggplot2::unit(0.2, "cm"),
    pad_y = ggplot2::unit(0.2, "cm")
  ) +
  coord_sf(crs = 4326) +
  ggplot2::scale_x_continuous(
    breaks = seq(4.95, 5.15, by = .1),
    limits = c(4.95, 5.15)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(47.2, 47.4, by = .05),
    limits = c(47.2, 47.4)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 18),
    axis.text.y = ggplot2::element_text(
      size = 18,
      angle = 90,
      hjust = .5
    ),
    plot.caption = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 18),
    legend.title = ggplot2::element_text(size = 18),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

ggsave(
  plot = p,
  paste0(out_path, "map_car_density_per_hour.pdf"),
  width = 30,
  height = 17
)
ggsave(
  plot = p,
  paste0(out_path, "map_car_density_per_hour.png"),
  width = 30,
  height = 17
)



p <- ggplot2::ggplot(cws) +
  stat_density_2d(
    mapping = aes(
      x = lon,
      y = lat,
      fill = stat(density)
    ),
    geom = "tile",
    contour = FALSE,
    alpha = 0.8
  ) +
  facet_wrap(~hour, nrow = 3) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  tidyterra::geom_spatvector(
    data = myroads,
    ggplot2::aes(),
    alpha = .1,
    stroke = 0.005
  ) +
  scale_linewidth_discrete(
    breaks = c(
      "Type autoroutier" = .1,
      "Route à 1 chaussée" = .05,
      "Route à 2 chaussées" = .01
    )
  ) +
  ggspatial::annotation_scale(
    location = "bl", pad_x = ggplot2::unit(1, "cm"),
    pad_y = ggplot2::unit(1, "cm"),
    height = ggplot2::unit(0.30, "cm"),
    text_cex = 1
  ) +
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = ggplot2::unit(0.2, "cm"),
    pad_y = ggplot2::unit(0.2, "cm")
  ) +
  coord_sf(crs = 4326) +
  ggplot2::scale_x_continuous(
    breaks = seq(4.95, 5.15, by = .1),
    limits = c(4.95, 5.15)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(47.2, 47.4, by = .05),
    limits = c(47.2, 47.4)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 18),
    axis.text.y = ggplot2::element_text(
      size = 18,
      angle = 90,
      hjust = .5
    ),
    plot.caption = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 18),
    legend.title = ggplot2::element_text(size = 18),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

ggsave(
  plot = p,
  paste0(out_path, "map_cws_density_per_hour.pdf"),
  width = 30,
  height = 17
)
ggsave(
  plot = p,
  paste0(out_path, "map_cws_density_per_hour.png"),
  width = 30,
  height = 17
)

pred <- data.table::fread(paste0(
  in_path,
  "prediction_grid_mapuce_dem.csv"
)) |>
  format_pred("lat", "lon", "H_MEAN", "BUILD_DENS", "dem")




save_plots_paper(
  ts_a,
  ts_b,
  out_path = paste0(
    getwd(),
    "/application/car_cws_statistics/"
  ),
  car,
  cws,
  pred,
  rad,
  borders
)
plot_eval <- plot_eval(out_path, borders)


x <- tiles_rmse(scores)
ggplot2::ggsave(x,
  filename = paste0(out_path, "/tiles_rmse.pdf"),
  dpi = 350,
  height = 7,
  width = 16,
  bg = "white"
)

x <- tiles_median_residuals(scores)
ggplot2::ggsave(x,
  filename = paste0(out_path, "/tiles_median_residual.pdf"),
  dpi = 350,
  height = 7,
  width = 16,
  bg = "white"
)
