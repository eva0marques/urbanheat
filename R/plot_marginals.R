plot_obs_int <- function(info) {
  linetype <- c("simul" = "solid", "prior" = "dotted", "post" = "dashed")
  color <- c("car" = "#FF0800", "cws" = "#00BFFF")
  p <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$mu_car,
        sd = sqrt(1 / info$prec_beta)
      ),
      aes(color = "car", linetype = "prior")
    ) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$int_car_mean,
        sd = info$int_car_sd
      ),
      aes(color = "car", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$mu_cws,
        sd = sqrt(1 / info$prec_beta)
      ),
      aes(color = "cws", linetype = "prior")
    ) +
    stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$int_cws_mean,
        sd = info$int_cws_sd
      ),
      aes(color = "cws", linetype = "post")
    ) +
    ggtitle("car and cws intercepts") +
    scale_color_manual("", values = color) +
    scale_linetype_manual("", values = linetype) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
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

plot_beta_covar <- function(info) {
  linetype <- c("prior" = "dashed", "post" = "solid")
  color <- c(
    "dem" = "darkgreen",
    "building density" = "maroon2",
    "building height" = "dodgerblue4"
  )
  p <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$dem_mean,
        sd = info$dem_sd
      ),
      linewidth = 1, aes(color = "dem", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_d_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, aes(color = "building density", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_h_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, aes(color = "building height", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$mu_covar,
        sd = sqrt(1 / info$prec_covar)
      ),
      color = "black",
      linewidth = 1,
      aes(linetype = "prior")
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
    scale_color_manual("", values = color) +
    scale_linetype_manual("",
      values = linetype,
      labels = c(
        "prior" = latex2exp::TeX("uninf. prior: $\\beta_k \\sim N(0, 10^3)$"),
        "post" = latex2exp::TeX("post")
      )
    ) +
    ylab("") +
    xlab("") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.margin=margin(0,0,0,0),
      legend.box.spacing = unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = unit(1, 'cm'),
      panel.grid.major.x = element_line(color="grey", size = 0.2),
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2), color = guide_legend(nrow = 3))


    # theme(
    #   axis.text.x = element_text(size = 20, family = "sans serif"),
    #   axis.text.y = element_blank(),
    #   axis.title.x = element_blank(),
    #   axis.title.y = element_blank(),
    #   axis.ticks.y = element_blank(),
    #   legend.key.width = unit(1, "cm"),
    #   panel.grid.major.x = element_line(color = "grey", size = 0.2),
    #   legend.text = element_text(size = 20, family = "sans serif"),
    #   plot.caption = element_text(size = 20, family = "sans serif"),
    #   legend.title = element_text(size = 22, family = "sans serif"),
    #   legend.position = "bottom",
    #   legend.box = "vertical",
    #   legend.margin = margin(0, 0, 0, 0),
    #   legend.box.spacing = unit(0, "pt"),
    #   legend.text.align = 0
    # )
  return(p)
}

plot_hyperprec <- function(mod, info) {
  # precision for gaussian observations
  list_marg <- mod$marginals.hyperpar
  names(list_marg) <- c("prec_car", "prec_cws", "range_s", "sd_s")
  xmin <- min(unlist(lapply(
    X = list_marg[1:2],
    FUN = function(x) min(x[, 1])
  ))) - .25
  xmax <- max(unlist(lapply(
    X = list_marg[1:2],
    FUN = function(x) max(x[, 1])
  ))) + .25
  # x_prec <- seq(xmin, xmax, by = 0.01)
  x_prec <- seq(0, 15, by = 0.1)
  densities <- lapply(
    X = list_marg[1:2],
    FUN = inla.dmarginal,
    x = x_prec
  )
  prec_marginals <- as.data.frame(
    list(
      "x" = rep(x_prec, 2),
      "y" = unlist(densities, use.names = FALSE),
      "source" = c(
        rep("car", length(x_prec)),
        rep("cws", length(x_prec))
      ),
      "name" = c(
        rep("prec_car_post", length(x_prec)),
        rep("prec_cws_post", length(x_prec))
      ),
      "type" = rep("post", 2 * length(x_prec))
    )
  )
  prec_car_prior <- as.data.frame(
    list(
      "x" = x_prec,
      "y" = actuar::dlgamma(x_prec,
        shapelog = info$a2_car,
        ratelog = info$b2_car
      ),
      "source" = rep("car", length(x_prec)),
      "name" = rep("prec_car_prior", length(x_prec)),
      "type" = rep("prior", length(x_prec))
    )
  )
  prec_cws_prior <- as.data.frame(
    list(
      "x" = x_prec,
      "y" = actuar::dlgamma(x_prec,
        shapelog = info$a2_cws,
        ratelog = info$b2_cws
      ),
      "source" = rep("cws", length(x_prec)),
      "name" = rep("prec_cws_prior", length(x_prec)),
      "type" = rep("prior", length(x_prec))
    )
  )
  prec_marginals <- rbind(prec_marginals, prec_car_prior, prec_cws_prior)
  # plot
  linetype <- c("simul" = "solid", "prior" = "dotted", "post" = "dashed")
  color <- c("car" = "#FF0800", "cws" = "#00BFFF")
  p <- ggplot(prec_marginals) +
    geom_line(aes(
      x = x,
      y = y,
      color = source,
      linetype = type,
      group = name
    ), ) +
    scale_color_manual("", values = color) +
    scale_linetype_manual("", values = linetype) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
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
