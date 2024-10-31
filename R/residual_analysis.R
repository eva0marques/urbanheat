summarize_pro_scores <- function(pro_scores, y_var = "temp_sea") {
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

# Temporal residual analysis




# Spatial residual analysis
map_median_res <- function(pro_scores,
                           borders,
                           y_var = "temp_sea",
                           model = "joint") {
  pro_smry_loc <- summarize_pro_scores(pro_scores, y_var)$pro_smry_loc
  res_median_model <- paste0("res_median_", model)
  pal <- load_palette("res")

  p <- ggplot() +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    geom_point(
      data = pro_smry_loc, aes(x = lon,
                               y = lat,
                               fill = .data[[res_median_model]]),
      shape = 21,
      size = 3
    ) +
    coord_sf(crs = 4326) +
    scale_fill_stepsn(
      colours = pal,
      limits = c(-1.8, 1.8),
      breaks = seq(-1.8, 1.8, .4),
      labels = seq(-1.8, 1.8, .4)
    ) +
    scale_x_continuous(breaks = seq(4.95, 5.15, by = .1)) +
    scale_y_continuous(breaks = seq(47.2, 47.4, by = .05)) +
    labs(fill = latex2exp::TeX("$(T_{pred} - T_{ref})_{q0.5}$")) +
    guides(fill = guide_colourbar(barwidth = 40, barheight = 1.5)) +
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
      legend.position = "top",
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

map_median_res_d_vs_n <- function(pro_scores, borders) {
  all <- map_median_res(pro_scores, borders = borders) +
    annotate("text",
             x = 4.925,
             y = 47.38,
             label = expression(bold("ALL")),
             size = 6)
  night <- map_median_res(pro_scores[which(pro_scores$day_night == "night"), ],
                        borders = borders) +
    annotate("text",
             x = 4.925,
             y = 47.38,
             label = expression(bold("NIGHT")),
             size = 6)
  day <- map_median_res(pro_scores[which(pro_scores$day_night == "day"), ],
                          borders = borders) +
    annotate("text",
             x = 4.925,
             y = 47.38,
             label = expression(bold("DAY")),
             size = 6)
  p <- ggpubr::ggarrange(all,
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


plot_res_vs_ref <- function(pro_scores,
                            y_var = "temp_sea",
                            model = "joint") {
  res_model <- paste0("res_", model)
  ggplot(data = pro_scores,
         aes(y = .data[[res_model]],
             x = .data[[y_var]])) +
    stat_density2d(aes(fill = ..density..),
                   alpha = 1,
                   geom = "tile",
                   contour = FALSE,
                   n = 200) +
    scale_fill_continuous(low = "white",
                          high = load_palette("model")[model]) +
    #geom_point() +
    geom_abline(aes(slope = 0, intercept = 0), color = "red") +
    geom_abline(aes(slope = 0, intercept = -1),
                color = "black",
                linetype = "dotted") +
    geom_abline(aes(slope = 0, intercept = 1),
                color = "black",
                linetype = "dotted") +
    ylab(latex2exp::TeX("$T_{pred} - T_{ref}$ (°C)")) +
    xlab(latex2exp::TeX("$T_{ref}$ (°C)")) +
    coord_equal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.margin=margin(0,0,0,0),
      legend.box.spacing = unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = unit(1, 'cm'),
      panel.grid.major = element_line(color="grey", size = 0.2),
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2))
}


plot_predmean_vs_ref <- function(pro_scores,
                                 y_var = "temp_sea",
                                 model = "joint") {
  pred_model <- paste0("pred_mean_", model)
  tn <- floor(min(c(as.data.frame(pro_scores)[, pred_model],
                    as.data.frame(pro_scores)[, y_var])))
  tx <- ceiling(max(c(as.data.frame(pro_scores)[, pred_model],
                      as.data.frame(pro_scores)[, y_var])))
  ggplot(data = pro_scores,
         aes(y = .data[[pred_model]],
             x = .data[[y_var]])) +
    #geom_point() +
    stat_density2d(aes(fill = ..density..),
                   alpha = 1,
                   geom = "tile",
                   contour = FALSE,
                   n = 200) +
    scale_fill_continuous(low = "white",
                          high = load_palette("model")[model]) +
    geom_abline(aes(slope = 1, intercept = 0), color = "red") +
    geom_abline(aes(slope = 1, intercept = -1),
                color = "black",
                linetype = "dotted") +
    geom_abline(aes(slope = 1, intercept = 1),
                color = "black",
                linetype = "dotted") +
    ylab(latex2exp::TeX("$T_{pred}$ (°C)")) +
    xlab(latex2exp::TeX("$T_{ref}$ (°C)")) +
    xlim(c(tn, tx)) +
    ylim(c(tn, tx)) +
    coord_equal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.margin=margin(0,0,0,0),
      legend.box.spacing = unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = unit(1, 'cm'),
      panel.grid.major = element_line(color="grey", size = 0.2),
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2))
}


boxplot_res_lcz <- function(pro_scores, model = "joint") {
  res_model <- paste0("res_", model)
  pal <- load_palette("lcz")
  p <- ggplot(pro_scores) +
    geom_boxplot(aes(y = .data[[res_model]], x = lcz_300m, fill = lcz_300m),
                 outlier.size = 0.5,
                 outlier.shape = 1
    ) +
    scale_fill_manual(values = pal$col,
                      breaks = pal$class,
                      labels = pal$meaning) +
    facet_wrap(vars(ifelse(day_night == "day", "DAY", "NIGHT"))) +
    coord_cartesian(ylim = c(-5, 5)) +
    geom_hline(yintercept = 0, color = "red") +
    ylab(latex2exp::TeX("$T_{pred}-T_{ref}$ (°C)")) +
    scale_y_continuous(breaks = seq(-5, 5, 1)) +
    xlab("") +
    labs(fill = "Local Climate Zone") +
    theme(
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      strip.text = element_text(size = 20),
      panel.grid.major = element_line(color="grey", size = 0.2),
      panel.background = element_rect(fill = "white")
    )
  return(p)
}

