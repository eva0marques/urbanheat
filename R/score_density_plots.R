#' Plots density of RMSE and residuals
#' @description Plots density of RMSE and residuals
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @author Eva Marques
#' @export
density_scores <- function(pro_scores, scores) {
  res <- model <- rmse <- NULL
  long_res <- pro_scores[, c(
    "time",
    "lon",
    "lat",
    "day_night",
    "res_car",
    "res_cws",
    "res_joint"
  )] |>
    tidyr::pivot_longer(
      cols = c("res_car", "res_cws", "res_joint"),
      names_to = "model",
      values_to = "res",
      names_transform = ~ gsub("res_", "", .x)
    ) |>
    as.data.frame()

  long_rmse <- scores[, c(
    "time",
    "day_night",
    "rmse_car",
    "rmse_cws",
    "rmse_joint"
  )] |>
    tidyr::pivot_longer(
      cols = c("rmse_car", "rmse_cws", "rmse_joint"),
      names_to = "model",
      values_to = "rmse",
      names_transform = ~ gsub("rmse_", "", .x)
    ) |>
    as.data.frame()

  n_pro <- nrow(unique(pro_scores[, c("lon", "lat")]))
  n_day <- nrow(unique(scores[which(scores$day_night == "day"), ]))
  n_night <- nrow(unique(scores[which(scores$day_night == "night"), ]))
  linetype <- c("joint" = "solid", "cws" = "dotted", "car" = "dashed")

  res_day <- ggplot2::ggplot(
    long_res[which(long_res$day_night == "day"), ],
    ggplot2::aes(x = res, group = model, color = model, linetype = model)
  ) +
    ggplot2::stat_density(size = 1, geom = "line", position = "identity") +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::scale_color_manual(values = load_palette("model")) +
    ggplot2::scale_x_continuous(breaks = seq(-3, 3, .5), limits = c(-3, 3)) +
    ggplot2::coord_cartesian(xlim = c(-3, 3)) +
    ggplot2::scale_linetype_manual("", values = linetype) +
    ggplot2::ylab(paste0("Empirical density")) +
    ggplot2::annotate("text",
      x = -2.75,
      y = .70,
      label = expression(bold("DAY")),
      size = 6
    ) +
    ggplot2::xlab(latex2exp::TeX("$T2M_{pred}-T2M_{ref}$ (°C)")) +
    ggplot2::labs(
      color = "",
      caption = bquote(
        italic(
          paste(
            "Sample of",
            ~ .(n_pro),
            " reference stations *",
            ~ .(n_day),
            " maps of Aug. 2018"
          )
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.key.width = ggplot2::unit(.5, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "white", linewidth = 0.1
      ),
      legend.position = c(0.8, 0.7),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.text.align = 0
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  res_night <- ggplot2::ggplot(
    long_res[which(long_res$day_night == "night"), ],
    ggplot2::aes(x = res, group = model, color = model, linetype = model)
  ) +
    ggplot2::stat_density(size = 1, geom = "line", position = "identity") +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::scale_color_manual(values = load_palette("model")) +
    ggplot2::scale_x_continuous(breaks = seq(-3, 3, .5), limits = c(-3, 3)) +
    ggplot2::coord_cartesian(xlim = c(-3, 3)) +
    ggplot2::scale_linetype_manual("", values = linetype) +
    ggplot2::ylab(paste0("Empirical density")) +
    ggplot2::annotate("text",
      x = -2.75,
      y = .70,
      label = expression(bold("NIGHT")),
      size = 6
    ) +
    ggplot2::xlab(latex2exp::TeX("$T2M_{pred}-T2M_{ref}$ (°C)")) +
    ggplot2::labs(
      color = "",
      caption = bquote(
        italic(
          paste(
            "Sample of",
            ~ .(n_pro),
            " reference stations *",
            ~ .(n_night),
            " maps of Aug. 2018"
          )
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.key.width = ggplot2::unit(.5, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "white", linewidth = 0.1
      ),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.8, 0.7),
      legend.text.align = 0
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  rmse_day <- ggplot2::ggplot(
    long_rmse[which(long_rmse$day_night == "day"), ],
    ggplot2::aes(x = rmse, group = model, color = model, linetype = model)
  ) +
    ggplot2::stat_density(size = 1, geom = "line", position = "identity") +
    ggplot2::geom_vline(xintercept = 1, color = "grey") +
    ggplot2::coord_cartesian(xlim = c(0, 4)) +
    ggplot2::scale_color_manual(values = load_palette("model")) +
    ggplot2::scale_x_continuous(breaks = seq(0, 10, .5), limits = c(0, 4)) +
    ggplot2::scale_linetype_manual("", values = linetype) +
    ggplot2::ylab(paste0("Empirical density")) +
    ggplot2::annotate("text",
      x = .25, y = 1.75,
      label = expression(bold("DAY")), size = 6
    ) +
    ggplot2::xlab("RMSE (°C)") +
    ggplot2::labs(
      color = "",
      caption = bquote(
        italic(
          paste(
            "Sample of",
            ~ .(n_day),
            " maps of Aug. 2018"
          )
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.key.width = ggplot2::unit(.5, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "white", linewidth = 0.1
      ),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.8, 0.7),
      legend.text.align = 0
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  rmse_night <- ggplot2::ggplot(
    long_rmse[which(long_rmse$day_night == "night"), ],
    ggplot2::aes(x = rmse, group = model, color = model, linetype = model)
  ) +
    ggplot2::stat_density(size = 1, geom = "line", position = "identity") +
    ggplot2::geom_vline(xintercept = 1, color = "grey") +
    ggplot2::coord_cartesian(xlim = c(0, 4)) +
    ggplot2::scale_color_manual(values = load_palette("model")) +
    ggplot2::scale_x_continuous(breaks = seq(0, 10, .5), limits = c(0, 4)) +
    ggplot2::scale_linetype_manual("", values = linetype) +
    ggplot2::ylab(paste0("Empirical density")) +
    ggplot2::annotate("text",
      x = .25, y = 1.75,
      label = expression(bold("NIGHT")), size = 6
    ) +
    ggplot2::xlab("RMSE (°C)") +
    ggplot2::labs(
      color = "",
      caption = bquote(
        italic(
          paste(
            "Sample of",
            ~ .(n_pro),
            " reference stations *",
            ~ .(n_night),
            " maps of Aug. 2018"
          )
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.key.width = ggplot2::unit(.5, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "white", linewidth = 0.1
      ),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.8, 0.7),
      legend.text.align = 0
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  rmse_day_ecdf <- ggplot2::ggplot(
    long_rmse[which(long_rmse$day_night == "day"), ],
    ggplot2::aes(x = rmse, group = model, color = model, linetype = model)
  ) +
    ggplot2::stat_ecdf(geom = "step", size = 1) +
    ggplot2::geom_vline(xintercept = 1, color = "grey") +
    ggplot2::coord_cartesian(xlim = c(0, 2)) +
    ggplot2::scale_color_manual(values = load_palette("model")) +
    ggplot2::scale_x_continuous(breaks = seq(0, 10, .5), limits = c(0, 10000)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual("", values = linetype) +
    ggplot2::ylab(paste0("Empirical cumulative density")) +
    ggplot2::annotate("text",
      x = .15, y = .95,
      label = expression(bold("DAY")), size = 6
    ) +
    ggplot2::xlab("RMSE (°C)") +
    ggplot2::labs(
      color = "",
      caption = bquote(
        italic(
          paste(
            "Sample of",
            ~ .(n_day),
            " maps of Aug. 2018"
          )
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.key.width = ggplot2::unit(.5, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "white", linewidth = 0.1
      ),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.8, 0.3),
      legend.text.align = 0
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  rmse_night_ecdf <- ggplot2::ggplot(
    long_rmse[which(long_rmse$day_night == "night"), ],
    ggplot2::aes(x = rmse, group = model, color = model, linetype = model)
  ) +
    ggplot2::stat_ecdf(geom = "step", size = 1) +
    ggplot2::geom_vline(xintercept = 1, color = "grey") +
    ggplot2::coord_cartesian(xlim = c(0, 2)) +
    ggplot2::scale_color_manual(values = load_palette("model")) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 10, .5), limits = c(0, 10000)) +
    ggplot2::scale_linetype_manual("", values = linetype) +
    ggplot2::ylab(paste0("Empirical cumulative density")) +
    ggplot2::annotate("text",
      x = .15, y = .95,
      label = expression(bold("NIGHT")), size = 6
    ) +
    ggplot2::xlab("RMSE (°C)") +
    ggplot2::labs(
      color = "",
      caption = bquote(
        italic(
          paste(
            "Sample of",
            ~ .(n_pro),
            " reference stations *",
            ~ .(n_night),
            " maps of Aug. 2018"
          )
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.key.width = ggplot2::unit(.5, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "white", linewidth = 0.1
      ),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.8, 0.3),
      legend.text.align = 0
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2))

  rmse_dayvsnight <- ggpubr::ggarrange(
    rmse_day,
    rmse_night,
    nrow = 2,
    vjust = 3.5,
    hjust = -2.3,
    align = "hv"
  )
  rmse_dayvsnight_ecdf <- ggpubr::ggarrange(
    rmse_day_ecdf,
    rmse_night_ecdf,
    nrow = 2,
    vjust = 3.5,
    hjust = -2.3,
    align = "hv"
  )
  res_dayvsnight <- ggpubr::ggarrange(
    res_day,
    res_night,
    nrow = 2,
    vjust = 3.5,
    hjust = -2.3,
    align = "hv"
  )
  scores_dayvsnight <- ggpubr::ggarrange(
    rmse_dayvsnight_ecdf,
    res_dayvsnight,
    ncol = 2
  )

  return(scores_dayvsnight)
}
