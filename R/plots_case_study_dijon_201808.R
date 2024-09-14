# /!\ the model run function must correspond to right out_path
plot_ts <- function(ts, out_path, car, cws, pred, rad, borders) {
  pred_ts <- readRDS(
    file = paste0(
      out_path,
      "pred_",
      format(ts, "%Y%m%d%H"),
      ".rds"
    )
  )
  pro_scores_ts <- pro_scores[which(pro_scores$time == ts), ]
  scores_ts <- scores[which(scores$time == ts), ]
  sw <- rad[which(rad$DATE == ts), "GLO"]
  temp_reg <- rad[which(rad$DATE == ts), "T"]

  # plots
  p1 <- map_pred_mean(pred_ts, pro_scores_ts, borders, model = "car")
  p2 <- map_pred_mean(pred_ts, pro_scores_ts, borders, model = "cws")
  p3 <- map_pred_mean(pred_ts, pro_scores_ts, borders, model = "joint")
  p4 <- map_pred_sd(pred_ts, borders, "joint")
  p5 <- map_obs(car, cws, ts = unique(pred_ts$time), borders)
  p6 <- density_beta_obs(scores_ts)
  p7 <- density_beta_covar(scores_ts)

  #F or hyperprec plot, need to rerun the model:
  set.seed(12)
  p_str <- strftime(ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") |>
    as.POSIXct(tz = "UTC")
  inference <- run_bhm(
    car = car,
    cws = cws,
    pred = pred,
    ts = p_str,
    sw = sw,
    temp_reg = temp_reg,
    borders = borders
  )
  p8 <- density_hyperprec(inference$mod_joint, inference$info)
  return(list(p1, p2, p3, p4, p5, p6, p7, p8))
}



plot_eval <- function(out_path) {
  scores <- read.csv(paste0(out_path, "scores_201808_dijon.csv"))
  scores$time <- as.POSIXct(scores$time,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")
  pro_scores <- read.csv(
    paste0(out_path, "mustard_evaluation_201808_dijon.csv")
    )
  pro_scores$time <- pro_scores$time |>
    sapply(FUN = function (x) {
      ifelse(nchar(x) == 10, paste(x, "00:00:00"), x)
    }) |>
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  pro_scores <- add_sw(pro_scores, rad)
  #scores <- add_sw(scores, rad)
  scores$day_night <- ifelse(scores$sw == 0, "night", "day")

  # plots
  p_a <- tiles_fixed_effect_post(scores)
  p_b <- tiles_rmse(scores)
  p_c <- tiles_int_obs_mean(scores)
  p_d <- map_median_res_d_vs_n(pro_scores, borders)
  p_e <- density_scores(pro_scores, scores)
  p_f <- tiles_prec_obs_mean(scores)
  p_g <- tiles_prec_obs_sd(scores)
  p_h <- tiles_n_obs(scores)
  p_i <- boxplot_res_lcz(pro_scores)
  p_j <- plot_predmean_vs_ref(pro_scores)
  p_k <- plot_res_vs_ref(pro_scores)

  return(list(p_a, p_b, p_c, p_d, p_e, p_f, p_g, p_h, p_i, p_j, p_k))
}


save_plots_paper <- function(ts_a,
                             ts_b,
                             out_path,
                             car,
                             cws,
                             pred,
                             rad,
                             borders) {
  # generate all plots
  plots_ts_a <- plot_ts(ts_a, out_path, car, cws, pred, rad, borders)
  plots_ts_b <- plot_ts(ts_b, out_path, car, cws, pred, rad, borders)
  plots_eval <- plot_eval(out_path)

  # specific timestamp* plots
  p3 <- ggpubr::ggarrange(plots_ts_a[[3]], plots_ts_b[[3]], ncol = 2)
  ggsave(
    plot = p3,
    paste0(out_path,
           "map_temp_sea_joint_pred_mean_pro_",
           format(ts_a, "%Y%m%d%H"),
           "_",
           format(ts_b, "%Y%m%d%H"),
           ".png"),
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
  )

  p4 <- ggpubr::ggarrange(plots_ts_a[[4]], plots_ts_b[[4]], ncol = 2)
  ggsave(
    plot = p4,
    paste0(out_path,
           "map_temp_sea_joint_pred_sd_",
           format(ts_a, "%Y%m%d%H"),
           "_",
           format(ts_b, "%Y%m%d%H"),
           ".png"),
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
  )

  p5 <- ggpubr::ggarrange(plots_ts_a[[5]], plots_ts_b[[5]], ncol = 2)
  ggsave(
    plot = p5,
    paste0(out_path,
           "map_temp_sea_obs_",
           format(ts_a, "%Y%m%d%H"),
           "_",
           format(ts_b, "%Y%m%d%H"),
           ".png"),
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
  )

  p7 <- ggpubr::ggarrange(plots_ts_a[[7]],
                         plots_ts_b[[7]],
                         ncol = 2,
                         common.legend = TRUE)
  ggsave(
    plot = p7,
    paste0(out_path,
           "marginal_density_beta_covar_",
           format(ts_a, "%Y%m%d%H"),
           "_",
           format(ts_b, "%Y%m%d%H"),
           ".png"),
    width = 14,
    height = 4,
    dpi = 300,
    bg = "white"
  )

  p6 <- ggpubr::ggarrange(plots_ts_a[[6]],
                           plots_ts_b[[6]],
                           ncol = 2,
                           common.legend = TRUE)
  ggsave(
    plot = p6,
    paste0(out_path,
           "marginal_density_beta_obs_",
           format(ts_a, "%Y%m%d%H"),
           "_",
           format(ts_b, "%Y%m%d%H"),
           ".png"),
    width = 14,
    height = 4,
    dpi = 300,
    bg = "white"
  )

  p8 <- ggpubr::ggarrange(plots_ts_a[[8]],
                      plots_ts_b[[8]],
                      ncol = 2,
                      common.legend = TRUE)
  ggsave(
    plot = p8,
    paste0(out_path,
           "marginal_density_hyperprec_obs_",
           format(ts_a, "%Y%m%d%H"),
           "_",
           format(ts_b, "%Y%m%d%H"),
           ".png"),
    width = 14,
    height = 4,
    dpi = 300,
    bg = "white"
  )

  # evaluation plots
  p_a <- plots_eval[[1]]
  ggsave(p_a,
         filename = paste0(out_path, "/tiles_fixed_effects.png"),
         dpi = 300,
         height = 7,
         width = 12,
         bg = "white")

  p_b <- plots_eval[[2]]
  ggsave(p_b,
         filename = paste0(out_path, "/tiles_rmse.png"),
         dpi = 300,
         height = 7,
         width = 16,
         bg = "white")

  p_c <- plots_eval[[3]]
  ggsave(p_c,
         filename = paste0(out_path, "/tiles_obs_intercepts.png"),
         dpi = 300,
         height = 7,
         width = 16,
         bg = "white")

  p_d <- plots_eval[[4]]
  ggsave(p_d,
         filename = paste0(out_path, "/map_median_res.png"),
         dpi = 300,
         height = 12,
         width = 16,
         bg = "white")

  p_e <- plots_eval[[5]]
  ggsave(p_e,
         filename = paste0(out_path, "/density_scores.png"),
         dpi = 300,
         height = 7,
         width = 12,
         bg = "white")

  p_fgh <- ggpubr::ggarrange(
    plots_eval[[6]],
    plots_eval[[7]],
    plots_eval[[8]],
    nrow = 3
    )
  ggsave(
    plot = p_fgh,
    paste0(out_path,
           "tiles_hyper_prec_vs_n_",
           ".png"),
    width = 10,
    height = 18,
    dpi = 300,
    bg = "white"
  )

  p_i <- plots_eval[[9]]
  ggsave(
    plot = p_i,
    paste0(out_path,
           "boxplot_residuals_lcz_300m_",
           ".png"),
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}
