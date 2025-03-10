run_case_study <- function(r_path, in_path, out_path) {
  # load all functions
  for (f in list.files(r_path, full.names = TRUE)) {
    source(f)
  }

  # create out_path if does not exist
  dir.create(out_path, showWarnings = FALSE)

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
  cws_before_qc <- data.table::fread(paste0(
    in_path,
    "cws_processed_for_bhm.csv"
  ))
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
  pro <- data.table::fread(paste0(
    in_path,
    "mustardijon_2018010100_2018123123.csv"
  )) |>
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
  mustard <- read.csv(paste0(
    in_path,
    "mustard_metadata_stations_mustardijon_clc.csv"
  ))
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

  borders <- sf::st_read(paste0(in_path, "Dijon.shp")) |>
    sf::st_transform(crs = "epsg:4326")

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

  pred <- data.table::fread(paste0(
    in_path,
    "prediction_grid_mapuce_dem.csv"
  )) |>
    format_pred("lat", "lon", "H_MEAN", "BUILD_DENS", "dem")

  rad <- read.csv(paste0(
    in_path,
    "radome_2018010100_2019010100_dijonlongevic.csv"
  ))
  rad$DATE <- as.POSIXct(rad$DATE, tz = "UTC")

  # run model on every hour of 2018/08 in Dijon (France)

  set.seed(12)
  t_start <- as.POSIXct("2018-08-25 19:00:00", tz = "UTC")
  t_end <- as.POSIXct("2018-08-31 23:00:00", tz = "UTC")
  period <- seq(t_start, t_end, by = "1 hour")
  for (p in period) {
    p_str <- strftime(p, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") |>
      as.POSIXct(tz = "UTC")
    sw <- rad[which(rad$DATE == p_str), "GLO"]
    temp_reg <- rad[which(rad$DATE == p_str), "T"]
    inference <- run_bhm(
      car = car,
      cws = cws,
      pred = pred,
      ts = p_str,
      sw = sw,
      temp_reg = temp_reg,
      borders = borders
    )
    if (is.null(inference)) {
      next
    } else {
      saveRDS(
        object = inference$pred,
        file = paste0(
          out_path,
          "pred_",
          format(p_str, "%Y%m%d%H"),
          ".rds"
        )
      )
      inference$info <- store_post_info(
        inference$mod_joint,
        inference$info
      )
      eval <- evaluate_pred(inference$pred,
        pro,
        info = inference$info,
        borders
      )
      file <- paste0(out_path, "scores_201808_dijon.csv")
      write.table(eval$scores,
        file,
        append = TRUE,
        sep = ",",
        col.names = !file.exists(file),
        row.names = FALSE,
        quote = FALSE
      )
      file <- paste0(out_path, "mustard_evaluation_201808_dijon.csv")
      pro_eval <- as.data.frame(eval$pro)
      pro_eval$geometry <- NULL
      write.table(pro_eval,
        file,
        append = TRUE,
        sep = ",",
        col.names = !file.exists(file),
        row.names = FALSE,
        quote = FALSE
      )
    }
  }
}
