#' Create a generic tile plot for one month
#' @description Create a generic tile plot for one month
#' @author Eva Marques
#' @export
mytile <- function(scores, fill) {
  plot <- ggplot2::ggplot(
    scores,
    ggplot2::aes(
      x = lubridate::hour(time),
      y = as.Date(time),
      fill = .data[[fill]]
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-05", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-12", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-19", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-26", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    ggplot2::scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        as.Date("2018-07-31", tz = "UTC"),
        as.Date("2018-09-01", tz = "UTC")
      )
    ) +
    # ggplot2::scale_fill_stepsn( # for discrete color scale
    ggplot2::scale_fill_gradientn(
      colours = load_palette("res"),
      breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      na.value = "grey"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 45, barheight = 1.5)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 16),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 22),
      axis.title.y = ggplot2::element_text(size = 22),
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 18),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  return(plot)
}

#' Tileplot for posterior of fixed effects
#' @author Eva Marques
#' @export
tiles_fixed_effect_post <- function(scores) {
  build_d_post_tile <- mytile(scores, "build_d_mean") +
    ggplot2::labs(
      y = "",
      x = "UTC",
      fill = latex2exp::TeX("$\\bar{p(\\beta|Y)}$")
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = "building density", size = 9
    )

  build_d_post_sd_tile <- mytile(scores, "build_d_sd") +
    ggplot2::labs(y = "", x = "UTC", fill = "building density\nprior sd") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"),
      breaks = seq(0, 0.4, 0.05),
      limits = c(0, 0.4)
    )

  build_h_post_tile <- mytile(scores, "build_h_mean") +
    ggplot2::labs(
      y = "",
      x = "UTC",
      fill = latex2exp::TeX("$\\bar{p(\\beta|Y)}$")
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = "building height", size = 9
    )

  build_h_post_sd_tile <- mytile(scores, "build_h_sd") +
    ggplot2::labs(y = "", x = "UTC", fill = "building height\nprior sd") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"),
      breaks = seq(0, 0.4, 0.05),
      limits = c(0, 0.4),
      na.value = NA
    )

  dem_post_tile <- mytile(scores, "dem_mean") +
    ggplot2::labs(
      y = "",
      x = "UTC",
      fill = latex2exp::TeX("$\\bar{p(\\beta|Y)}$")
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = "dem", size = 9
    )

  dem_post_sd_tile <- mytile(scores, "dem_sd") +
    ggplot2::labs(y = "", x = "UTC", fill = "dem\nprior sd") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"),
      breaks = seq(0, 0.4, 0.05),
      limits = c(0, 0.4),
      na.value = NA
    )

  mean_tiles <- ggpubr::ggarrange(dem_post_tile,
    build_d_post_tile,
    build_h_post_tile,
    common.legend = TRUE,
    ncol = 3,
    align = "hv"
  )

  return(mean_tiles)
}

to_labels <- function(breaks) {
  breaks <- as.character(breaks)
  breaks[length(breaks)] <- paste0(">", breaks[length(breaks)])
  return(breaks)
}

tiles_rmse <- function(scores) {
  p_car <- mytile(scores, "rmse_car") +
    ggplot2::labs(y = "", x = "UTC", fill = "RMSE (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 5, .5),
      limits = c(0, 5),
      labels = to_labels(seq(0, 5, .5)),
      na.value = NA
    )
  p_cws <- mytile(scores, "rmse_cws") +
    ggplot2::labs(y = "", x = "UTC", fill = "RMSE (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 5, .5),
      limits = c(0, 5),
      labels = to_labels(seq(0, 5, .5)),
      na.value = NA
    )
  p_joint <- mytile(scores, "rmse_joint") +
    ggplot2::labs(y = "", x = "UTC", fill = "RMSE (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 5, .5),
      limits = c(0, 5),
      labels = to_labels(seq(0, 5, .5)),
      na.value = NA
    )
  rmse <- ggpubr::ggarrange(
    p_car,
    p_cws,
    p_joint,
    common.legend = TRUE,
    ncol = 3,
    align = "hv"
  )
  return(rmse)
}


tiles_median_residuals <- function(scores) {
  p_car <- mytile(scores, "med_res_car") +
    ggplot2::labs(y = "", x = "UTC", fill = "Median residual (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      labels = to_labels(seq(-3, 3, .5)),
      na.value = NA
    )
  p_cws <- mytile(scores, "med_res_cws") +
    ggplot2::labs(y = "", x = "UTC", fill = "Median residual (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      labels = to_labels(seq(-3, 3, .5)),
      na.value = NA
    )
  p_joint <- mytile(scores, "med_res_joint") +
    ggplot2::labs(y = "", x = "UTC", fill = "Median residual (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      labels = to_labels(seq(-3, 3, .5)),
      na.value = NA
    )
  res <- ggpubr::ggarrange(
    p_car,
    p_cws,
    p_joint,
    common.legend = TRUE,
    ncol = 3,
    align = "hv"
  )
  return(res)
}


tiles_int_obs_mean <- function(scores) {
  p_car_prior <- mytile(scores, "mu_car") +
    ggplot2::labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"),
      breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
      na.value = NA
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$\\bar{p(\\mu_{car})}$"),
      size = 9
    )
  p_cws_prior <- mytile(scores, "mu_cws") +
    ggplot2::labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"),
      breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
      na.value = NA
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$\\bar{p(\\mu_{cws})}$"),
      size = 9
    )
  p_car <- mytile(scores, "int_car_mean") +
    ggplot2::labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
      na.value = NA
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$\\bar{p(\\mu_{car}|Y)}$"),
      size = 9
    )
  p_cws <- mytile(scores, "int_cws_mean") +
    ggplot2::labs(y = "", x = "UTC", fill = "Marg. mean (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("res"), breaks = seq(-3.8, 3.8, .4),
      limits = c(-3.8, 3.8),
      na.value = NA
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$\\bar{p(\\mu_{cws}|Y)}$"),
      size = 9
    )
  int_mean <- ggpubr::ggarrange(p_car_prior,
    p_car,
    p_cws_prior,
    p_cws,
    common.legend = TRUE,
    ncol = 4,
    align = "hv"
  )
  return(int_mean)
}



tiles_prec_obs_mean <- function(scores) {
  p_car <- mytile(scores, "prec_car_mean") +
    ggplot2::labs(y = "", x = "UTC", fill = "Precision mean") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 6.5, .5),
      limits = c(0, 6.5),
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$\\bar{p(prec_{car}|Y)}$"),
      size = 9
    )
  p_cws <- mytile(scores, "prec_cws_mean") +
    ggplot2::labs(y = "", x = "UTC", fill = "Precision mean") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 6.5, .5),
      limits = c(0, 6.5),
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$\\bar{p(prec_{cws}|Y)}$"),
      size = 9
    )
  hyperprec_mean <- ggpubr::ggarrange(p_car,
    p_cws,
    common.legend = TRUE,
    ncol = 2,
    align = "hv"
  )
  return(hyperprec_mean)
}


tiles_prec_obs_sd <- function(scores) {
  p_car <- mytile(scores, "prec_car_sd") +
    ggplot2::labs(y = "", x = "UTC", fill = "Precision sd") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, .7, .05),
      limits = c(0, .7),
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$sd(p(prec_{car}|Y))$"),
      size = 9
    )
  p_cws <- mytile(scores, "prec_cws_sd") +
    ggplot2::labs(y = "", x = "UTC", fill = "Precision sd") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, .7, .05),
      limits = c(0, .7),
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$sd(p(prec_{cws}|Y))$"),
      size = 9
    )
  pred_sd <- ggpubr::ggarrange(p_car,
    p_cws,
    common.legend = TRUE,
    ncol = 2,
    align = "hv"
  )
  return(pred_sd)
}


mytile_log <- function(scores, fill) {
  plot <- ggplot2::ggplot(
    scores,
    ggplot2::aes(
      x = lubridate::hour(time),
      y = as.Date(time),
      fill = log10(.data[[fill]])
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-05", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-12", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-19", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = as.Date("2018-08-26", tz = "UTC"),
      linetype = "dashed", size = 0.2
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    ggplot2::scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        as.Date("2018-07-31", tz = "UTC"),
        as.Date("2018-09-01", tz = "UTC")
      )
    ) +
    # ggplot2::scale_fill_stepsn( # for discrete color scale
    ggplot2::scale_fill_gradientn(
      # colours = rev(load_palette("prior")),
      colours = load_palette("res"),
      breaks = seq(-3, 3, .5),
      limits = c(-3, 3),
      na.value = "grey"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 65, barheight = 1.5)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 16),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 22),
      axis.title.y = ggplot2::element_text(size = 22),
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 18),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  return(plot)
}


tiles_n_obs <- function(scores) {
  p_car <- mytile_log(scores, "n_car") +
    ggplot2::labs(y = "", x = "UTC", fill = "log10(n)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"), breaks = seq(0, 3.8, .2),
      limits = c(0, 3.8),
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$n_{car}$"),
      size = 9
    )
  p_cws <- mytile_log(scores, "n_cws") +
    ggplot2::labs(y = "", x = "UTC", fill = "log10(n)") +
    ggplot2::scale_fill_stepsn(
      colours = load_palette("reds"),
      breaks = seq(0, 3.8, .2),
      limits = c(0, 3.8),
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggplot2::annotate("text",
      x = 12,
      y = as.Date(max(scores$time) - lubridate::days(1)),
      label = latex2exp::TeX("$n_{cws}$"),
      size = 9
    )
  n <- ggpubr::ggarrange(p_car,
    p_cws,
    common.legend = TRUE,
    ncol = 2,
    align = "hv"
  )
  return(n)
}
