#' Plot density for beta per observational source
#' @description Plot density for beta per observational source
#' @author Eva Marques
#' @import ggplot2
#' @export
density_beta_obs <- function(info) {
  x <- NULL
  linetype <- c("simul" = "dashed", "prior" = "dashed", "post" = "solid")
  color <- c("car" = "#FF0800", "cws" = "#00BFFF")
  p <- ggplot2::ggplot(
    data = data.frame(x = c(-3, 3)),
    ggplot2::aes(x)
  ) +
    ggplot2::stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$mu_car,
        sd = sqrt(1 / info$prec_beta_obs)
      ),
      ggplot2::aes(color = "car", linetype = "prior"),
      linewidth = 1
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$int_car_mean,
        sd = info$int_car_sd
      ),
      ggplot2::aes(color = "car", linetype = "post"),
      linewidth = 1
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$mu_cws,
        sd = sqrt(1 / info$prec_beta_obs)
      ),
      ggplot2::aes(color = "cws", linetype = "prior"),
      linewidth = 1
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 101,
      args = list(
        mean = info$int_cws_mean,
        sd = info$int_cws_sd
      ),
      ggplot2::aes(color = "cws", linetype = "post"),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual("", values = color) +
    ggplot2::scale_linetype_manual(
      name = latex2exp::TeX("$\\mu_Y$"),
      values = linetype
    ) +
    ggplot2::scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major.x = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    )
  return(p)
}

#' Plot density of beta coefficients of covariates
#' @description Plot density for beta coefficients of covariates
#' @author Eva Marques
#' @import ggplot2
#' @export
density_beta_covar <- function(info) {
  x <- NULL
  linetype <- c("prior" = "dashed", "post" = "solid")
  color <- c(
    "dem" = "darkgreen",
    "building density" = "maroon2",
    "building height" = "dodgerblue4"
  )
  p <- ggplot2::ggplot(data = data.frame(x = c(-5, 5)), ggplot2::aes(x)) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$dem_mean,
        sd = info$dem_sd
      ),
      linewidth = 1, ggplot2::aes(color = "dem", linetype = "post")
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_d_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, ggplot2::aes(color = "building density", linetype = "post")
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_h_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, ggplot2::aes(color = "building height", linetype = "post")
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$mu_covar,
        sd = sqrt(1 / info$prec_covar)
      ),
      color = "black",
      linewidth = 1,
      ggplot2::aes(linetype = "prior")
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
    ggplot2::scale_color_manual("", values = color) +
    ggplot2::scale_linetype_manual("",
      values = linetype,
      labels = c(
        "prior" = latex2exp::TeX("uninf. prior: $\\beta_k \\sim N(0, 10^3)$"),
        "post" = latex2exp::TeX("post")
      )
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major.x = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(nrow = 2),
      color = ggplot2::guide_legend(nrow = 3)
    )
  return(p)
}

#' Plot density of hyperprecision
#' @description Plot density of hyperprecision
#' @author Eva Marques
#' @importFrom INLA inla.dmarginal
#' @import ggplot2
#' @export
density_hyperprec <- function(mod, info) {
  type <- x <- y <- name <- NULL
  # precision for gaussian observations
  list_marg <- mod$marginals.hyperpar
  names(list_marg) <- c("prec_car", "prec_cws", "range_s", "sd_s")
  # xmin <- min(unlist(lapply(
  #   X = list_marg[1:2],
  #   FUN = function(x) min(x[, 1])
  # ))) - .25
  # xmax <- max(unlist(lapply(
  #   X = list_marg[1:2],
  #   FUN = function(x) max(x[, 1])
  # ))) + .25
  # x_prec <- seq(xmin, xmax, by = 0.01)
  x_prec <- seq(0, 15, by = 0.1)
  densities <- lapply(
    X = list_marg[1:2],
    FUN = INLA::inla.dmarginal,
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
  linetype <- c("simul" = "dashed", "prior" = "dashed", "post" = "solid")
  color <- c("car" = "#FF0800", "cws" = "#00BFFF")
  p <- ggplot2::ggplot(prec_marginals) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = x,
        y = y,
        color = source,
        linetype = type,
        group = name
      ),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual("", values = color) +
    ggplot2::scale_linetype_manual(
      name = latex2exp::TeX("$\\prec_Y$"),
      values = linetype
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major.x = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    )
  return(p)
}
