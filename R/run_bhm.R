run_bhm <- function(car, cws, pred, ts, sw, temp_reg, borders) {
  start_time <- Sys.time()
  coo <- sf::st_coordinates(borders)
  domain <- coo[, c("X", "Y")] |>
    INLA::inla.nonconvex.hull(concave = -.07, resolution = c(200, 200))

  # store model parameters info
  info <- list()

  info$y_var <- "temp_sea"
  info$ts_str <- format(ts, "%Y%m%d%H")
  info$time <- strftime(ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  te <- ts + lubridate::hours(1) - lubridate::seconds(1)
  car_samp <- car[which(between(car$time, ts, te)), ]
  cws_samp <- cws[which(between(cws$time, ts, te)), ]
  info$n_car <- nrow(car_samp)
  info$n_cws <- nrow(cws_samp)
  if (info$n_car != 0 && info$n_cws != 0) {
    # altitude gradient correction
    car_samp$grad_z <- apply(
      car_samp[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$delta
    )
    car_samp$temp_sea <- apply(
      car_samp[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$temp_sea
    )

    cws_samp$grad_z <- apply(
      cws_samp[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$delta
    )
    cws_samp$temp_sea <- apply(
      cws_samp[, c("dem", "temp")], 1,
      function(y) grad_alt(y["dem"], y["temp"])$temp_sea
    )

    # covariates standardization
    # -- extract mean and sd for the whole spatial domain (grid)
    means <- apply(pred[, c("dem", "build_d", "build_h")], 2, mean)
    stdevs <- apply(pred[, c("dem", "build_d", "build_h")], 2, sd)

    # -- standardize car data
    car_samp$dem <- (car_samp$dem - means[1]) / stdevs[1]
    car_samp$build_d <- (car_samp$build_d - means[2]) / stdevs[2]
    car_samp$build_h <- (car_samp$build_h - means[3]) / stdevs[3]

    # -- standardize cws data
    cws_samp$dem <- (cws_samp$dem - means[1]) / stdevs[1]
    cws_samp$build_d <- (cws_samp$build_d - means[2]) / stdevs[2]
    cws_samp$build_h <- (cws_samp$build_h - means[3]) / stdevs[3]

    # -- standardize prediction data
    pred$dem <- (pred$dem - means[1]) / stdevs[1]
    pred$build_d <- (pred$build_d - means[2]) / stdevs[2]
    pred$build_h <- (pred$build_h - means[3]) / stdevs[3]

    # -- mesh is adapted to data location
    mesh_car <- INLA::inla.mesh.2d(
      loc = cbind(car_samp$lon, car_samp$lat),
      max.edge = 0.1, # in meters: 10000
      cutoff = 0.005, #500
      offset = c(0.01, 0.02), #1000 2000
      boundary = domain
    )
    mesh_cws <- INLA::inla.mesh.2d(
      loc = cbind(cws$lon, cws$lat),
      max.edge = 0.1,
      cutoff = 0.005,
      offset = c(0.01, 0.02),
      boundary = domain
    )
    mesh_joint <- INLA::inla.mesh.2d(
      loc = rbind(
        cbind(car_samp$lon, car_samp$lat),
        cbind(cws_samp$lon, cws_samp$lat)
      ),
      max.edge = 0.1,
      cutoff = 0.005,
      offset = c(0.01, 0.02),
      boundary = domain
    )

    # plot mesh
    # plot(mesh_joint)
    # points(cbind(car_samp$lon, car_samp$lat), col = "red")
    # points(cbind(cws_samp$lon, cws_samp$lat), col = "blue")

    # spde model

    info$r0 <- 0.05
    info$p_r0 <- 0.99
    info$sd0 <- 2
    info$p_sd0 <- 0.01
    prior_range <- c(info$r0, info$p_r0) # p(range < r0) = p_r0
    prior_sigma <- c(info$sd0, info$p_sd0) # p(sigma > sd0) = p_sd0
    spde_car <- INLA::inla.spde2.pcmatern(mesh_car,
      prior.range = prior_range,
      prior.sigma = prior_sigma
    )
    spde_cws <- INLA::inla.spde2.pcmatern(mesh_cws,
      prior.range = prior_range,
      prior.sigma = prior_sigma
    )
    spde_joint <- INLA::inla.spde2.pcmatern(mesh_joint,
      prior.range = prior_range,
      prior.sigma = prior_sigma
    )

    # projection matrices A and Ap
    # (p for prediction)

    # -- data projection matrix to the mesh
    a_car <- INLA::inla.spde.make.A(
      mesh = mesh_car,
      loc = cbind(car_samp$lon, car_samp$lat)
    )
    a_cws <- INLA::inla.spde.make.A(
      mesh = mesh_cws,
      loc = cbind(cws_samp$lon, cws_samp$lat)
    )
    a_car_joint <- INLA::inla.spde.make.A(
      mesh = mesh_joint,
      loc = cbind(car_samp$lon, car_samp$lat)
    )
    a_cws_joint <- INLA::inla.spde.make.A(
      mesh = mesh_joint,
      loc = cbind(cws_samp$lon, cws_samp$lat)
    )
    a_joint <- INLA::inla.spde.make.A(
      mesh = mesh_joint,
      loc = rbind(
        cbind(car_samp$lon, car_samp$lat),
        cbind(cws_samp$lon, cws_samp$lat)
      )
    )

    # -- pred projection matrix to the mesh
    ap_car <- INLA::inla.spde.make.A(
      mesh = mesh_car,
      loc = cbind(pred$lon, pred$lat)
    )
    ap_cws <- INLA::inla.spde.make.A(
      mesh = mesh_cws,
      loc = cbind(pred$lon, pred$lat)
    )
    ap_joint <- INLA::inla.spde.make.A(
      mesh = mesh_joint,
      loc = cbind(pred$lon, pred$lat)
    )

    # -- spatial indexes
    s_car <- INLA::inla.spde.make.index("s", spde_car$n.spde)
    s_cws <- INLA::inla.spde.make.index("s", spde_cws$n.spde)
    s_joint <- INLA::inla.spde.make.index("s", spde_joint$n.spde)

    # ------ car model data wrapper ------ #
    stk_data_car <- INLA::inla.stack(
      tag = "car",
      data = list(y = car_samp[, info$y_var]),
      A = list(1, a_car),
      effects = list(
        data.frame(
          int <- rep(1, nrow(car_samp)),
          int_car = rep(1, nrow(car_samp)),
          dem = car_samp$dem,
          build_d = car_samp$build_d,
          build_h = car_samp$build_h
        ),
        s = s_car
      )
    )
    stk_pred_car <- INLA::inla.stack(
      tag = "pred",
      data = list(y = rep(NA, nrow(pred))),
      A = list(1, ap_car),
      effects = list(
        list(
          int = rep(1, nrow(pred)),
          dem = pred$dem,
          build_d = pred$build_d,
          build_h = pred$build_h
        ),
        s = s_car
      )
    )
    stk_full_car <- INLA::inla.stack(stk_data_car, stk_pred_car)

    # ------ cws model data wrapper ------ #
    stk_data_cws <- INLA::inla.stack(
      tag = "cws",
      data = list(y = cws_samp[, info$y_var]),
      A = list(1, a_cws),
      effects = list(
        data.frame(
          int = rep(1, nrow(cws_samp)),
          int_cws = rep(1, nrow(cws_samp)),
          dem = cws_samp$dem,
          build_d = cws_samp$build_d,
          build_h = cws_samp$build_h
        ),
        s = s_cws
      )
    )
    stk_pred_cws <- INLA::inla.stack(
      tag = "pred",
      data = list(y = rep(NA, nrow(pred))),
      A = list(1, ap_cws),
      effects = list(
        list(
          int = rep(1, nrow(pred)),
          dem = pred$dem,
          build_d = pred$build_d,
          build_h = pred$build_h
        ),
        s = s_cws
      )
    )
    stk_full_cws <- INLA::inla.stack(stk_data_cws, stk_pred_cws)

    # ------ joint model data wrapper ------ #
    stk_car_joint <- INLA::inla.stack(
      tag = "car",
      data = list(y = cbind(car_samp[, info$y_var], NA)),
      A = list(1, a_car_joint),
      effects = list(
        data.frame(
          int = rep(1, nrow(car_samp)),
          int_car = rep(1, nrow(car_samp)),
          dem = car_samp$dem,
          build_d = car_samp$build_d,
          build_h = car_samp$build_h
        ),
        s = s_joint
      )
    )
    stk_cws_joint <- INLA::inla.stack(
      tag = "cws",
      data = list(y = cbind(NA, cws_samp[, info$y_var])),
      A = list(1, a_cws_joint),
      effects = list(
        data.frame(
          int = rep(1, nrow(cws_samp)),
          int_cws = rep(1, nrow(cws_samp)),
          dem = cws_samp$dem,
          build_d = cws_samp$build_d,
          build_h = cws_samp$build_h
        ),
        s = s_joint
      )
    )
    stk_data_joint <- INLA::inla.stack(stk_car_joint, stk_cws_joint)
    stk_pred_joint <- INLA::inla.stack(
      tag = "pred",
      data = list(y = cbind(
        rep(NA, nrow(pred)),
        rep(NA, nrow(pred))
      )),
      A = list(1, ap_joint),
      effects = list(
        list(
          int = rep(1, nrow(pred)),
          dem = pred$dem,
          build_d = pred$build_d,
          build_h = pred$build_h
        ),
        s = s_joint
      )
    )
    stk_full_joint <- INLA::inla.stack(stk_data_joint, stk_pred_joint)

    # prior settings
    # -- mean of beta coefficients (regional temperature)
    info$mu_0 <- temp_reg
    # short wave unit change + regression from marques et al. 2022
    info$mu_car <- 0.0034 * (sw * 10000 / 3600) + 0.2466
    info$mu_cws <- ifelse(hour(ts) %in% seq(6, 18, 1), 0.5, 1.5)
    info$mu_covar <- 0
    # -- precision of beta coefficients
    info$prec_beta <- 10
    info$prec_covar <- 0.001 # small value for uninformative
    # data variance prior
    info$s2_car_prior <- 0.8
    info$s2_cws_prior <- 1.2

    # prior on process model
    control_fixed <- list(
      mean = list(int = info$mu_0,
                  int_car = info$mu_car,
                  int_cws = info$mu_cws,
                  default = info$mu_covar),
      prec = list(int = info$prec_beta,
                  int_car = info$prec_beta,
                  int_cws = info$prec_beta,
                  default = info$prec_covar))

    est_loggamma_param <- function(a, b) {
      s <- runif(n = 50000, min = a, max = b)
      prec <- 1 / s**2
      a2 <- mean(prec)**2 / var(prec)
      b2 <- a2 / mean(prec)
      return(list(a2 = a2, b2 = b2))
    }

    prec_param_car <- est_loggamma_param(info$s2_car_prior - 0.2,
                                         info$s2_car_prior + 0.2)
    prec_param_cws <- est_loggamma_param(info$s2_cws_prior - 0.2,
                                         info$s2_cws_prior + 0.2)
    info$a2_car <- prec_param_car$a2
    info$b2_car <- prec_param_car$b2
    info$a2_cws <- prec_param_cws$a2
    info$b2_cws <- prec_param_cws$b2

    # prior on likelihood
    control_family <- list(
      list(
        hyper = list(
          prec = list(
            prior = "loggamma",
            param = c(info$a2_car, info$b2_car)
          )
        )
      ),
      list(
        hyper = list(
          prec = list(
            prior = "loggamma",
            param = c(info$a2_cws, info$b2_cws)
          )
        )
      )
    )


    f_car <- y ~ 1 + int_car + dem + build_d + build_h + f(s, model = spde_car)
    mod_car <- inla(
      formula = f_car,
      data = INLA::inla.stack.data(stk_full_car),
      family = "gaussian",
      #control.fixed = control_fixed,
      control.fixed = list(
        mean.intercept = info$mu_0,
        prec.intercept = info$prec_beta,
        mean = list(int_car = info$mu_car, default = info$mu_covar),
        prec = list(int_car = info$prec_beta, default = info$prec_covar)
        ),
      control.family = control_family[[1]],
      control.compute = list(cpo = TRUE),
      control.predictor = list(
        compute = TRUE,
        A = INLA::inla.stack.A(stk_full_car)
      )
    )
    cat(info$ts_str, "mod_car done\n")
    summary(mod_car)

    f_cws <- y ~ 1 + int_cws + dem + build_d + build_h + f(s, model = spde_cws)
    mod_cws <- inla(
      formula = f_cws,
      data = INLA::inla.stack.data(stk_full_cws),
      family = "gaussian",
      control.fixed = list(
        mean.intercept = info$mu_0,
        prec.intercept = info$prec_beta,
        mean = list(int_cws = info$mu_cws, default = info$mu_covar),
        prec = list(int_cws = info$prec_beta, default = info$prec_covar)
      ),
      control.family = control_family[[2]],
      control.compute = list(cpo = TRUE),
      control.predictor = list(
        compute = TRUE,
        A = INLA::inla.stack.A(stk_full_cws)
      )
    )
    cat(info$ts_str, "mod_cws done\n")
    summary(mod_cws)

    f_joint <- y ~ -1 + int + int_car + int_cws + dem + build_d + build_h +
      f(s, model = spde_joint)
    mod_joint <- inla(
      formula = f_joint,
      data = INLA::inla.stack.data(stk_full_joint),
      family = c("gaussian", "gaussian"),
      control.fixed = control_fixed,
      control.compute = list(cpo = TRUE),
      control.predictor = list(
        compute = TRUE,
        A = INLA::inla.stack.A(stk_full_joint)
      )
    )
    cat(info$ts_str, "mod_joint done\n")
    summary(mod_joint)

    # -- IMPORTANT
    # in the data stack for prediction, int_car and int_cws are not present
    # Hence they are ignored by the model and the predicted value is
    # the y_var without the measurement error.

    #store prediction mean, sd, and quantiles
    pred$time <- ts
    index <- INLA::inla.stack.index(stk_full_car, tag = "pred")$data
    pred$pred_mean_car <- mod_car$summary.fitted.values[index, "mean"]
    pred$pred_ll_car <- mod_car$summary.fitted.values[index, "0.025quant"]
    pred$pred_ul_car <- mod_car$summary.fitted.values[index, "0.975quant"]
    pred$pred_sd_car <- mod_car$summary.fitted.values[index, "sd"]
    index <- INLA::inla.stack.index(stk_full_cws, tag = "pred")$data
    pred$pred_mean_cws <- mod_cws$summary.fitted.values[index, "mean"]
    pred$pred_ll_cws <- mod_cws$summary.fitted.values[index, "0.025quant"]
    pred$pred_ul_cws <- mod_cws$summary.fitted.values[index, "0.975quant"]
    pred$pred_sd_cws <- mod_cws$summary.fitted.values[index, "sd"]
    index <- INLA::inla.stack.index(stk_full_joint, tag = "pred")$data
    pred$pred_mean_joint <- mod_joint$summary.fitted.values[index, "mean"]
    pred$pred_ll_joint <- mod_joint$summary.fitted.values[index, "0.025quant"]
    pred$pred_ul_joint <- mod_joint$summary.fitted.values[index, "0.975quant"]
    pred$pred_sd_joint <- mod_joint$summary.fitted.values[index, "sd"]
    info <- as.data.frame(info)

    return(list("mod_car" = mod_car,
                "mod_cws" = mod_cws,
                "mod_joint" = mod_joint,
                "pred" = pred,
                "info" = info))

  } else {
    end_time <- Sys.time()
    cat(end_time - start_time)
    return(NULL)
  }
}

