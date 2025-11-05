#' Summarizes scores calculated with professional network
#' @description Summarizes scores calculated with professional network
#' @author Eva Marques
#' @importFrom sf st_as_sf
#' @export
summarize_pro_scores <- function(pro_scores, y_var = "temp_sea") {
  site_id <- pred_mean_joint <- res_joint <- NULL
  pred_mean_car <- res_car <- lon <- lat <- NULL
  pred_mean_cws <- res_cws <- NULL
  pro_scores$y_var <- as.data.frame(pro_scores)[, y_var]
  pro_scores$site_id <- interaction(sprintf("%.6f", pro_scores$lon),
    sprintf("%.6f", pro_scores$lat),
    sep = "_"
  )
  pro_smry_loc <- pro_scores |>
    as.data.frame() |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      lon = unique(lon),
      lat = unique(lat),
      y_var_mean = mean(y_var, na.rm = TRUE),
      rsq_joint = cor(y_var, pred_mean_joint)**2,
      rmse_joint = sqrt(sum(res_joint**2) / dplyr::n()),
      res_median_joint = median(res_joint),
      res_mean_joint = mean(res_joint),
      res_90ci_inf_joint = quantile(res_joint, 0.05),
      res_90ci_sup_joint = quantile(res_joint, 0.95),
      res_95ci_inf_joint = quantile(res_joint, 0.025),
      res_95ci_sup_joint = quantile(res_joint, 0.975),
      y_var_range_joint = max(y_var) - min(y_var),
      rsq_car = cor(y_var, pred_mean_car)**2,
      rmse_car = sqrt(sum(res_car**2) / dplyr::n()),
      res_median_car = median(res_car),
      res_mean_car = mean(res_car),
      res_90ci_inf_car = quantile(res_car, 0.05),
      res_90ci_sup_car = quantile(res_car, 0.95),
      res_95ci_inf_car = quantile(res_car, 0.025),
      res_95ci_sup_car = quantile(res_car, 0.975),
      y_var_range_car = max(y_var) - min(y_var),
      rsq_cws = cor(y_var, pred_mean_cws)**2,
      rmse_cws = sqrt(sum(res_cws**2) / dplyr::n()),
      res_median_cws = median(res_cws),
      res_mean_cws = mean(res_cws),
      res_90ci_inf_cws = quantile(res_cws, 0.05),
      res_90ci_sup_cws = quantile(res_cws, 0.95),
      res_95ci_inf_cws = quantile(res_cws, 0.025),
      res_95ci_sup_cws = quantile(res_cws, 0.975),
      y_var_range_cws = max(y_var) - min(y_var)
    ) |>
    data.frame() |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  pro_smry_hour <- pro_scores |>
    as.data.frame() |>
    dplyr::group_by(time) |>
    dplyr::summarise(
      y_var_mean = mean(y_var, na.rm = TRUE),
      rsq_joint = cor(y_var, pred_mean_joint)**2,
      rmse_joint = sqrt(sum(res_joint**2) / dplyr::n()),
      res_median_joint = median(res_joint),
      res_mean_joint = mean(res_joint),
      res_90ci_inf_joint = quantile(res_joint, 0.05),
      res_90ci_sup_joint = quantile(res_joint, 0.95),
      res_95ci_inf_joint = quantile(res_joint, 0.025),
      res_95ci_sup_joint = quantile(res_joint, 0.975),
      y_var_range_joint = max(y_var) - min(y_var),
      rsq_car = cor(y_var, pred_mean_car)**2,
      rmse_car = sqrt(sum(res_car**2) / dplyr::n()),
      res_median_car = median(res_car),
      res_mean_car = mean(res_car),
      res_90ci_inf_car = quantile(res_car, 0.05),
      res_90ci_sup_car = quantile(res_car, 0.95),
      res_95ci_inf_car = quantile(res_car, 0.025),
      res_95ci_sup_car = quantile(res_car, 0.975),
      y_var_range_car = max(y_var) - min(y_var),
      rsq_cws = cor(y_var, pred_mean_cws)**2,
      rmse_cws = sqrt(sum(res_cws**2) / dplyr::n()),
      res_median_cws = median(res_cws),
      res_mean_cws = mean(res_cws),
      res_90ci_inf_cws = quantile(res_cws, 0.05),
      res_90ci_sup_cws = quantile(res_cws, 0.95),
      res_95ci_inf_cws = quantile(res_cws, 0.025),
      res_95ci_sup_cws = quantile(res_cws, 0.975),
      y_var_range_cws = max(y_var) - min(y_var)
    ) |>
    data.frame()
  return(
    list(
      "pro_smry_loc" = pro_smry_loc,
      "pro_smry_hour" = pro_smry_hour
    )
  )
}

#' Spatial residual analysis
#' @description Map median residuals per professional station
#' @author Eva Marques
#' @importFrom sf st_as_sf
#' @import ggplot2
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @export
map_median_res <- function(
  pro_scores,
  borders,
  y_var = "temp_sea",
  model = "joint"
) {
  lon <- lat <- NULL
  pro_smry_loc <- summarize_pro_scores(pro_scores, y_var)$pro_smry_loc
  res_median_model <- paste0("res_median_", model)
  pal <- load_palette("res")

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = borders, fill = NA, size = 0.05) +
    ggplot2::geom_point(
      data = pro_smry_loc, ggplot2::aes(
        x = lon,
        y = lat,
        fill = .data[[res_median_model]]
      ),
      shape = 21,
      size = 3
    ) +
    ggplot2::coord_sf(crs = 4326) +
    ggplot2::scale_fill_stepsn(
      colours = pal,
      limits = c(-1.8, 1.8),
      breaks = seq(-1.8, 1.8, .4),
      labels = seq(-1.8, 1.8, .4)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    ggplot2::scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    ggplot2::labs(fill = latex2exp::TeX("$(T2M_{pred} - T2M_{ref})_{q0.5}$")) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 40, barheight = 1.5)
    ) +
    ggspatial::annotation_scale(
      location = "tr", text_cex = 1.5,
      pad_x = ggplot2::unit(0.5, "cm"),
      pad_y = ggplot2::unit(0.5, "cm"),
      height = ggplot2::unit(0.30, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = ggplot2::unit(0.5, "cm"), pad_y = ggplot2::unit(0.5, "cm")
    ) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
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
  return(p)
}

#' Spatial residual analysis day vs night
#' @description Map median residuals per professional station
#' day vs night
#' @author Eva Marques
#' @importFrom sf st_as_sf
#' @import ggplot2
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom ggpubr ggarrange
#' @export
map_median_res_d_vs_n <- function(pro_scores, borders) {
  all <- map_median_res(pro_scores, borders = borders) +
    ggplot2::annotate("text",
      x = 4.925,
      y = 47.38,
      label = expression(bold("ALL")),
      size = 6
    )
  night <- map_median_res(pro_scores[which(pro_scores$day_night == "night"), ],
    borders = borders
  ) +
    ggplot2::annotate("text",
      x = 4.925,
      y = 47.38,
      label = expression(bold("NIGHT")),
      size = 6
    )
  day <- map_median_res(pro_scores[which(pro_scores$day_night == "day"), ],
    borders = borders
  ) +
    ggplot2::annotate("text",
      x = 4.925,
      y = 47.38,
      label = expression(bold("DAY")),
      size = 6
    )
  p <- ggpubr::ggarrange(
    all,
    NULL,
    ggpubr::ggarrange(day, night, ncol = 2, legend = "none"),
    NULL,
    nrow = 4,
    heights = c(1, 0, 1, 0),
    common.legend = TRUE,
    legend = "top"
  )
  return(p)
}

#' Plot residuals
#' @description Plot residuals
#' @import ggplot2
#' @author Eva Marques
#' @export
plot_res_vs_ref <- function(
  pro_scores,
  y_var = "temp_sea",
  model = "joint"
) {
  res_model <- paste0("res_", model)
  ggplot2::ggplot(
    data = pro_scores,
    ggplot2::aes(
      y = .data[[res_model]],
      x = .data[[y_var]]
    )
  ) +
    ggplot2::stat_density2d(ggplot2::aes(fill = ..density..),
      alpha = 1,
      geom = "tile",
      contour = FALSE,
      n = 200
    ) +
    ggplot2::scale_fill_continuous(
      low = "white",
      high = load_palette("model")[model]
    ) +
    # geom_point() +
    ggplot2::geom_abline(
      ggplot2::aes(slope = 0, intercept = 0), color = "red"
    ) +
    ggplot2::geom_abline(
      ggplot2::aes(slope = 0, intercept = -1),
      color = "black",
      linetype = "dotted"
    ) +
    ggplot2::geom_abline(
      ggplot2::aes(slope = 0, intercept = 1),
      color = "black",
      linetype = "dotted"
    ) +
    ggplot2::ylab(latex2exp::TeX("$T2M_{pred} - T2M_{ref}$ (°C)")) +
    ggplot2::xlab(latex2exp::TeX("$T2M_{ref}$ (°C)")) +
    ggplot2::coord_equal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size = 18),
      axis.title.y = ggplot2::element_text(size = 18),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))
}

#' Plot posterior mean vs real temperature
#' @description Plot posterior mean vs real temperature
#' @import ggplot2
#' @author Eva Marques
#' @export
plot_predmean_vs_ref <- function(
  pro_scores,
  y_var = "temp_sea",
  model = "joint"
) {
  pred_model <- paste0("pred_mean_", model)
  tn <- floor(min(c(
    as.data.frame(pro_scores)[, pred_model],
    as.data.frame(pro_scores)[, y_var]
  )))
  tx <- ceiling(max(c(
    as.data.frame(pro_scores)[, pred_model],
    as.data.frame(pro_scores)[, y_var]
  )))
  ggplot2::ggplot(
    data = pro_scores,
    ggplot2::aes(
      y = .data[[pred_model]],
      x = .data[[y_var]]
    )
  ) +
    ggplot2::stat_density2d(
      ggplot2::aes(fill = ..density..),
      alpha = 1,
      geom = "tile",
      contour = FALSE,
      n = 200
    ) +
    ggplot2::scale_fill_continuous(
      low = "white",
      high = load_palette("model")[model]
    ) +
    ggplot2::geom_abline(
      ggplot2::aes(slope = 1, intercept = 0), color = "red"
    ) +
    ggplot2::geom_abline(
      ggplot2::aes(slope = 1, intercept = -1),
      color = "black",
      linetype = "dotted"
    ) +
    ggplot2::geom_abline(
      ggplot2::aes(slope = 1, intercept = 1),
      color = "black",
      linetype = "dotted"
    ) +
    ggplot2::ylab(latex2exp::TeX("$T2M_{pred}$ (°C)")) +
    ggplot2::xlab(latex2exp::TeX("$T2M_{ref}$ (°C)")) +
    ggplot2::xlim(c(tn, tx)) +
    ggplot2::ylim(c(tn, tx)) +
    ggplot2::coord_equal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size = 18),
      axis.title.y = ggplot2::element_text(size = 18),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))
}

#' Plots boxplot of residuals per Local Climate Zone
#' @description Plots boxplot of residuals per Local Climate Zone
#' @import ggplot2
#' @author Eva Marques
#' @export
boxplot_res_lcz <- function(pro_scores, model = "joint") {
  lcz_300m <- day_night <- NULL
  res_model <- paste0("res_", model)
  pal <- load_palette("lcz")
  p <- ggplot2::ggplot(pro_scores) +
    ggplot2::geom_boxplot(
      ggplot2::aes(y = .data[[res_model]], x = lcz_300m, fill = lcz_300m),
      outlier.size = 0.5,
      outlier.shape = 1
    ) +
    ggplot2::scale_fill_manual(
      values = pal$col,
      breaks = pal$class,
      labels = pal$meaning
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(ifelse(day_night == "day", "DAY", "NIGHT"))
    ) +
    ggplot2::coord_cartesian(ylim = c(-5, 5)) +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::ylab(latex2exp::TeX("$T2M_{pred}-T2M_{ref}$ (°C)")) +
    ggplot2::scale_y_continuous(breaks = seq(-5, 5, 1)) +
    ggplot2::xlab("") +
    ggplot2::labs(fill = "Local Climate Zone") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size = 18),
      axis.title.y = ggplot2::element_text(size = 18),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      strip.text = ggplot2::element_text(size = 20),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    )
  return(p)
}
