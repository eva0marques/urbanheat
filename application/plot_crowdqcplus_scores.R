# Problem: this script cannot be run without cws dataset with "distance to ref"
# variables

imgdir <- paste0(getwd(), "/application/plots_manuscript/")
in_path <- paste0(getwd(), "/application/input/")

interval <- function(x) {
  cut(x, breaks = c(0, 5, 10, 14, 18, 23), include.lowest = TRUE)
}
myboxplot <- function(df, fill_color = "yellowgreen") {
  plot <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = interval(lubridate::hour(time)),
      y = meas_err,
      group = interval(lubridate::hour(time))
    )
  ) +
    ggplot2::geom_boxplot(outlier.shape = 3, fill = fill_color, width = .8) +
    ggplot2::geom_hline(
      yintercept = 0, color = "red", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = 1, color = "blue", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = -1, color = "blue", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = 2, color = "blue", linetype = "dotted", linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = -2, color = "blue", linetype = "dotted", linewidth = 1
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(-20, 20, 1), limits = c(-8, 8), expand = c(0, 0)
    ) +
    ggplot2::ylab(latex2exp::TeX("$T_{cws}-T_{ref}$ (°C)")) +
    ggplot2::xlab("UTC") +
    ggthemes::theme_tufte() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey", linewidth = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.text.x = ggplot2::element_text(
        color = "black", size = 22, angle = 90, vjust = .4
      ),
      axis.text.y = ggplot2::element_text(
        color = "black", size = 22, angle = 0
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(color = "black", size = 26),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12)
    )
  return(plot)
}


# -- for evaluation, always select cws in the
# -- 300m neighborhood of reference weather stations
cws_before_qc <- data.table::fread(
  paste0(
    in_path,
    "netatmo_2018080100_2018083123_dijon_cleaned.csv"
  )
)
names(cws_before_qc) <- chartr(".", "_", names(cws_before_qc))

df_plot <- cws_before_qc[which(cws_before_qc$neigh_pro_dist < 300), ]
boxplot_before <- myboxplot(df_plot, "firebrick1") +
  annotate("text",
    x = factor("(10,14]"), y = -7,
    label = expression(bold("RAW")), size = 6
  )

df_plot <- cws_qc_crowd[
  which(cws_qc_crowd$neigh_pro_dist < 300 & cws_qc_crowd$m4 == TRUE),
]
boxplot_qc_crowd <- myboxplot(df_plot) +
  annotate("text",
    x = factor("(10,14]"), y = -7,
    label = expression(bold("CrowdQC+")), size = 6
  )

df_plot <- cws_qc_clust_crowd[
  which(
    cws_qc_clust_crowd$neigh_pro_dist < 300 & cws_qc_clust_crowd$m4 == TRUE
  ),
]
boxplot_qc_clust_crowd <- myboxplot(df_plot) +
  annotate("text",
    x = factor("(10,14]"), y = -7,
    label = expression(bold("CLUSTERING\n+ CrowdQC+")), size = 6
  )

error_boxplots <- ggarrange(
  boxplot_before,
  boxplot_qc_crowd,
  boxplot_qc_clust,
  boxplot_qc_clust_crowd,
  ncol = 4
)
ggsave(
  plot = error_boxplots,
  paste0(imgdir, "error_boxplots.png"),
  width = 15,
  height = 7,
  dpi = 300
)
