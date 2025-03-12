# SESSION SETUP
library(data.table)
library(INLA)
library(ggplot2)
# make sure you are on the project directory, something like ./urbanheat/
getwd()
# Source all functions
for (f in list.files("./R", full.names = TRUE)) {
  source(f)
}

# OPEN DATA
# model output and graph path 
out_path <- "./output/"

# Observations
car <- data.table::fread("./input/car_processed_for_bhm.csv") |>
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
cws_before_qc <- data.table::fread("./input/cws_processed_for_bhm.csv")
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
pro <- data.table::fread("./input/mustardijon_2018010100_2018123123.csv") |>
  data_bhm(
    temp = "TEMP",
    lat = "LATITUDE",
    lon = "LONGITUDE",
    time = "DATE",
    build_h = "H_MEAN",
    build_d = "BUILD_DENS",
    dem = "ALTITUDE",
    network = "mustardijon"
  )
mustard <- read.csv("./input/mustard_metadata_stations_mustardijon_clc.csv")
mustard$lcz_100m <- as.character(mustard$LCZ_100.m)
mustard$lcz_300m <- as.character(mustard$LCZ_300.m)
mustard[mustard == "101"] <- "A"
mustard[mustard == "102"] <- "B"
mustard[mustard == "103"] <- "C"
mustard[mustard == "104"] <- "D"
mustard[mustard == "105"] <- "E"
mustard[mustard == "107"] <- "G"
mustard[mustard == "9999"] <- "9"
pro <- merge(pro,
  mustard[, c("X", "Y", "lcz_100m", "lcz_300m")],
  by.x = c("lon", "lat"),
  by.y = c("X", "Y"),
  all.y = FALSE
)
# Dijon borders
borders <- sf::st_read("./input/Dijon.shp") |>
  sf::st_transform(crs = "epsg:4326")
# Prediction grid
format_pred <- function(x, lat, lon, build_h, build_d, dem) {
  x <- as.data.frame(x)
  y <- x |>
    dplyr::rename("lat" = lat) |>
    dplyr::rename("lon" = lon) |>
    dplyr::rename("build_h" = build_h) |>
    dplyr::rename("build_d" = build_d) |>
    dplyr::rename("dem" = dem)
  y <- y[, c("lat", "lon", "build_h", "build_d", "dem")]
}
pred <- data.table::fread("./input/prediction_grid_mapuce_dem.csv") |>
  format_pred("lat", "lon", "H_MEAN", "BUILD_DENS", "dem")
# Radome station
rad <- read.csv("./input/radome_2018010100_2019010100_dijonlongevic.csv")
rad$DATE <- as.POSIXct(rad$DATE, tz = "UTC")

# PAPER CASESTUDY OUTPUTS
scores <- read.csv(paste0(out_path, "scores_201808_dijon.csv"))
scores$time <- as.POSIXct(scores$time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
pro_scores <- read.csv(
  paste0(out_path, "mustard_evaluation_201808_dijon.csv")
)

# PRINT OVERALL SCORES
overall_scores(pro_scores)

# PLOTS FOR 2 TIMESTAMPS
ts_a <- as.POSIXct("2018-08-13 23:00:00",
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
plots_ts_a <- plot_ts(ts_a, out_path, car, cws, pred, rad, borders)
plots_ts_a
ts_b <- as.POSIXct("2018-08-02 05:00:00",
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
plots_ts_b <- plot_ts(ts_b, out_path, car, cws, pred, rad, borders)
plots_ts_b
plots_eval <- plot_eval(out_path)
plots_eval
# save all plots for the paper
save_plots_paper(
  ts_a,
  ts_b,
  out_path,
  car,
  cws,
  pred,
  rad,
  borders
)

# Manual plot adjustments
# Improve and save p5 for extreme hot values (paper)
ts <- ts_a
pred_ts <- readRDS(
  file = paste0(
    out_path,
    "pred_",
    format(ts, "%Y%m%d%H"),
    ".rds"
  )
)
scores <- read.csv(paste0(out_path, "scores_201808_dijon.csv"))
scores$time <- as.POSIXct(scores$time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
pro_scores <- read.csv(
  paste0(out_path, "mustard_evaluation_201808_dijon.csv")
)
pro_scores$time <- pro_scores$time |>
  sapply(FUN = function(x) {
    ifelse(nchar(x) == 10, paste(x, "00:00:00"), x)
  }) |>
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
pro_scores <- add_sw(pro_scores, rad)
scores$day_night <- ifelse(scores$sw == 0, "night", "day")
pro_scores_ts <- pro_scores[which(pro_scores$time == ts), ]
scores_ts <- scores[which(scores$time == ts), ]
sw <- rad[which(rad$DATE == ts), "GLO"]
temp_reg <- rad[which(rad$DATE == ts), "T"]

p_5a <- map_obs_outscale(car, cws, ts = unique(pred_ts$time), borders) +
  ggspatial::annotation_scale(
    location = "tr", text_cex = 1.5,
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    height = unit(0.30, "cm")
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")
  )
ggsave(
  plot = p_5a,
  paste0(
    out_path,
    "map_temp_sea_obs_",
    format(ts, "%Y%m%d%H"),
    ".pdf"
  ),
  width = 8,
  height = 8,
  dpi = 350,
  bg = "white"
)

ts <- ts_b
pred_ts <- readRDS(
  file = paste0(
    out_path,
    "pred_",
    format(ts, "%Y%m%d%H"),
    ".rds"
  )
)
scores <- read.csv(paste0(out_path, "scores_201808_dijon.csv"))
scores$time <- as.POSIXct(scores$time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
pro_scores <- read.csv(
  paste0(out_path, "mustard_evaluation_201808_dijon.csv")
)
pro_scores$time <- pro_scores$time |>
  sapply(FUN = function(x) {
    ifelse(nchar(x) == 10, paste(x, "00:00:00"), x)
  }) |>
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
pro_scores <- add_sw(pro_scores, rad)
scores$day_night <- ifelse(scores$sw == 0, "night", "day")
pro_scores_ts <- pro_scores[which(pro_scores$time == ts), ]
scores_ts <- scores[which(scores$time == ts), ]
sw <- rad[which(rad$DATE == ts), "GLO"]
temp_reg <- rad[which(rad$DATE == ts), "T"]

p_5b <- map_obs_outscale(car, cws, ts = unique(pred_ts$time), borders)
ggsave(
  plot = p_5b,
  paste0(
    out_path,
    "map_temp_sea_obs_",
    format(ts, "%Y%m%d%H"),
    ".pdf"
  ),
  width = 8,
  height = 8,
  dpi = 350,
  bg = "white"
)

p5 <- ggpubr::ggarrange(p_5a, p_5b, ncol = 2)
p5
ggsave(
  plot = p5,
  paste0(
    out_path,
    "map_temp_sea_obs_",
    format(ts_a, "%Y%m%d%H"),
    "_",
    format(ts_b, "%Y%m%d%H"),
    "_paper.pdf"
  ),
  width = 16,
  height = 7,
  dpi = 350,
  bg = "white"
)

# Improve p3
p3 <- map_pred_mean(pred_ts, pro_scores_ts, borders, model = "joint")

# Tiles obs intercepts
tiles_int_obs_mean <- function(scores) {
  scores$int_diff_car <- scores$int_car_mean - scores$mu_car
  scores$int_diff_cws <- scores$int_cws_mean - scores$mu_cws
  p_car <- mytile(scores, "int_diff_car") +
    labs(
      y = "",
      x = "UTC",
      fill = latex2exp::TeX("$\\bar{p(\\mu_{Y}|Y)} - \\bar{p(\\mu_{Y})}$ (°C)")
    ) +
    scale_fill_stepsn(
      colours = load_palette("res"),
      breaks = seq(-1.4, 1.4, .4),
      limits = c(-1.4, 1.4),
      na.value = "grey"
    ) +
    guides(fill = guide_colourbar(barwidth = 28, barheight = 1.5)) +
    annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(2)),
      label = "CAR",
      size = 9
    ) +
    theme(
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  p_cws <- mytile(scores, "int_diff_cws") +
    labs(y = "", x = "UTC", fill = latex2exp::TeX("$\\bar{p(\\mu_{Y}|Y)} - \\bar{p(\\mu_{Y})}$ (°C)")) +
    scale_fill_stepsn(
      colours = load_palette("res"),
      breaks = seq(-1.4, 1.4, .4),
      limits = c(-1.4, 1.4),
      na.value = "grey"
    ) +
    guides(fill = guide_colourbar(barwidth = 28, barheight = 1.5)) +
    annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(2)),
      label = "CWS",
      size = 9
    ) +
    theme(
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  int_mean <- ggpubr::ggarrange(
    p_car,
    p_cws,
    common.legend = T,
    ncol = 2,
    align = "hv"
  )
  return(int_mean)
}
p_c <- tiles_int_obs_mean(scores)
ggsave(p_c,
  filename = paste0(out_path, "/tiles_obs_intercepts2.pdf"),
  dpi = 350,
  height = 7,
  width = 10,
  bg = "white"
)
