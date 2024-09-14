mytile <- function(scores, fill) {
  plot <- ggplot(
    scores,
    aes(
      x = lubridate::hour(time),
      y = as.Date(time),
      fill = .data[[fill]]
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    geom_hline(
      yintercept = as.Date("2018-08-05", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    geom_hline(
      yintercept = as.Date("2018-08-12", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    geom_hline(
      yintercept = as.Date("2018-08-19", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    geom_hline(
      yintercept = as.Date("2018-08-26", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        as.Date("2018-07-31", tz = "UTC"),
        as.Date("2018-09-01", tz = "UTC")
      )
    ) +
    #scale_fill_stepsn( # for discrete color scale
    scale_fill_gradientn(
      #colours = rev(load_palette("prior")),
      colours = load_palette("res"),
      breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      na.value = "grey") +
    guides(fill = guide_colourbar(barwidth = 65, barheight = 1.5)) +
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.key.width = unit(1, "cm"),
      panel.grid.major = element_line(color = "grey", size = 0.2),
      legend.text = element_text(size = 16),
      plot.caption = element_text(size = 14),
      legend.title = element_text(size = 18),
      #legend.text.align = 0,
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2))

  return(plot)
}

tiles_fixed_effect_post <- function(scores) {
  build_d_post_tile <- mytile(scores, "build_d_mean") +
    labs(y = "", x = "UTC", fill = latex2exp::TeX("$\\bar{p(\\beta|Y)}$")) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = "building density", size = 9
    )

  build_d_post_sd_tile <- mytile(scores, "build_d_sd") +
    labs(y = "", x = "UTC", fill = "building density\nprior sd") +
    scale_fill_stepsn(colours = load_palette("reds"),
                      breaks = seq(0, 0.4, 0.05),
                      limits = c(0, 0.4))

  build_h_post_tile <- mytile(scores, "build_h_mean") +
    labs(y = "", x = "UTC", fill = latex2exp::TeX("$\\bar{p(\\beta|Y)}$")) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = "building height", size = 9
    )

  build_h_post_sd_tile <- mytile(scores, "build_h_sd") +
    labs(y = "", x = "UTC", fill = "building height\nprior sd") +
    scale_fill_stepsn(colours = load_palette("reds"),
                      breaks = seq(0, 0.4, 0.05),
                      limits = c(0, 0.4))

  dem_post_tile <- mytile(scores, "dem_mean") +
    labs(y = "", x = "UTC", fill = latex2exp::TeX("$\\bar{p(\\beta|Y)}$")) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = "dem", size = 9
    )

  dem_post_sd_tile <- mytile(scores, "dem_sd") +
    labs(y = "", x = "UTC", fill = "dem\nprior sd") +
    scale_fill_stepsn(colours = load_palette("reds"),
                      breaks = seq(0, 0.4, 0.05),
                      limits = c(0, 0.4))

  mean_tiles <- ggpubr::ggarrange(dem_post_tile,
                          build_d_post_tile,
                          build_h_post_tile,
                          common.legend = T,
                          ncol = 3,
                          align = "hv")

  return(mean_tiles)
}

to_labels <- function(breaks) {
  breaks <- as.character(breaks)
  breaks[length(breaks)] <- paste0(">", breaks[length(breaks)])
  return(breaks)
}

tiles_rmse <- function(scores) {
  p_car <- mytile(scores, "rmse_car") +
    labs(y = "", x = "UTC", fill = "RMSE (°C)") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 5, .5),
      limits = c(0, 5),
      labels = to_labels(seq(0, 5, .5))
    )
  p_cws <- mytile(scores, "rmse_cws") +
    labs(y = "", x = "UTC", fill = "RMSE (°C)") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 5, .5),
      limits = c(0, 5),
      labels = to_labels(seq(0, 5, .5))
    )
  p_joint <- mytile(scores, "rmse_joint") +
    labs(y = "", x = "UTC", fill = "RMSE (°C)") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 5, .5),
      limits = c(0, 5),
      labels = to_labels(seq(0, 5, .5))
    )
  rmse <- ggpubr::ggarrange(p_car,
                                  p_cws,
                                  p_joint,
                                  common.legend = T,
                                  ncol = 3,
                                  align = "hv")
  return(rmse)
}


tiles_int_obs_mean <- function(scores) {
  p_car_prior <- mytile(scores, "mu_car") +
    labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
    ) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$\\bar{p(\\epsilon_{car})}$"),
             size = 9
    )
  p_cws_prior <- mytile(scores, "mu_cws") +
    labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
    ) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$\\bar{p(\\epsilon_{cws})}$"),
             size = 9
    )
  p_car <- mytile(scores, "int_car_mean") +
    labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
    ) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$\\bar{p(\\epsilon_{car}|Y)}$"),
             size = 9
    )
  p_cws <- mytile(scores, "int_cws_mean") +
    labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
    ) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$\\bar{p(\\epsilon_{cws}|Y)}$"),
             size = 9
    )
  int_mean <- ggpubr::ggarrange(p_car_prior,
                                p_car,
                                p_cws_prior,
                                p_cws,
                                common.legend = T,
                                ncol = 4,
                                align = "hv"
  )
  return(int_mean)
}



tiles_prec_obs_mean <- function(scores) {
  p_car <- mytile(scores, "prec_car_mean") +
    labs(y = "", x = "UTC", fill = "Precision mean") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 6.5, .5),
      limits = c(0, 6.5),
    ) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$\\bar{p(prec_{car}|Y)}$"),
             size = 9
    )
  p_cws <- mytile(scores, "prec_cws_mean") +
    labs(y = "", x = "UTC", fill = "Precision mean") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 6.5, .5),
      limits = c(0, 6.5),
    ) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$\\bar{p(prec_{cws}|Y)}$"),
             size = 9
    )
  hyperprec_mean <- ggpubr::ggarrange(p_car,
                                p_cws,
                                common.legend = T,
                                ncol = 2,
                                align = "hv"
  )
  return(hyperprec_mean)
}


tiles_prec_obs_sd <- function(scores) {
  p_car <- mytile(scores, "prec_car_sd") +
    labs(y = "", x = "UTC", fill = "Precision sd") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, .7, .05),
      limits = c(0, .7),
    ) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$sd(p(prec_{car}|Y))$"),
             size = 9
    )
  p_cws <- mytile(scores, "prec_cws_sd") +
    labs(y = "", x = "UTC", fill = "Precision sd") +
    scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, .7, .05),
      limits = c(0, .7),
    ) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$sd(p(prec_{cws}|Y))$"),
             size = 9
    )
  pred_sd <- ggpubr::ggarrange(p_car,
                                p_cws,
                                common.legend = T,
                                ncol = 2,
                                align = "hv"
  )
  return(pred_sd)
}


mytile_log <- function(scores, fill) {
  plot <- ggplot(
    scores,
    aes(
      x = lubridate::hour(time),
      y = as.Date(time),
      fill = log10(.data[[fill]])
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    geom_hline(
      yintercept = as.Date("2018-08-05", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    geom_hline(
      yintercept = as.Date("2018-08-12", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    geom_hline(
      yintercept = as.Date("2018-08-19", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    geom_hline(
      yintercept = as.Date("2018-08-26", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        as.Date("2018-07-31", tz = "UTC"),
        as.Date("2018-09-01", tz = "UTC")
      )
    ) +
    #scale_fill_stepsn( # for discrete color scale
    scale_fill_gradientn(
      #colours = rev(load_palette("prior")),
      colours = load_palette("res"),
      breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      na.value = "grey") +
    guides(fill = guide_colourbar(barwidth = 65, barheight = 1.5)) +
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.key.width = unit(1, "cm"),
      panel.grid.major = element_line(color = "grey", size = 0.2),
      legend.text = element_text(size = 16),
      plot.caption = element_text(size = 14),
      legend.title = element_text(size = 18),
      #legend.text.align = 0,
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2))

  return(plot)
}


tiles_n_obs <- function(scores) {
  p_car <- mytile_log(scores, "n_car") +
    labs(y = "", x = "UTC", fill = "log10(n)") +
    scale_fill_stepsn(
      #colours = load_palette("reds"), breaks = seq(0, 4700, 500),
      #limits = c(0, 4700),
      colours = load_palette("reds"), breaks = seq(0, 3.8, .2),
      limits = c(0, 3.8),
    ) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$n_{car}$"),
             size = 9
    )
  p_cws <- mytile_log(scores, "n_cws") +
    labs(y = "", x = "UTC", fill = "log10(n)") +
    scale_fill_stepsn(
      #colours = load_palette("reds"), breaks = seq(0, 100, 10),
      #limits = c(0, 100),
      colours = load_palette("reds"), breaks = seq(0, 3.8, .2),
      limits = c(0, 3.8),
    ) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
    annotate("text",
             x = 12,
             y = as.Date(max(scores$time) - lubridate::days(1)),
             label = latex2exp::TeX("$n_{cws}$"),
             size = 9
    )
  n <- ggpubr::ggarrange(p_car,
                                p_cws,
                                common.legend = T,
                                ncol = 2,
                                align = "hv"
  )
  return(n)
}
