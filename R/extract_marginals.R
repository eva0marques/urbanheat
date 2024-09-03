extract_marginals <- function(mod) {
  list_marg <- mod$marginals.fixed
  marginals <- data.frame(do.call(rbind, list_marg))
  marginals$fixed.effects <- rep(names(list_marg), times = sapply(list_marg, nrow))
  marg_m <- unlist(lapply(X = list_marg,
                          FUN = inla.emarginal,
                          fun = function(x) x))
  marg_mm <- unlist(lapply(X = list_marg,
                           FUN = inla.emarginal,
                           fun = function(x) x^2))
  # -- var[X] = E[X^2] − E[X]^2 = mm - m^2
  marg_sd <- sqrt(marg_mm - marg_m^2)
  marg_m_names <- unlist(lapply(names(list_marg), paste0, "_mean"))
  marg_sd_names <- unlist(lapply(names(list_marg), paste0, "_sd"))
  mod_param <- data.frame(t(c(marg_m, marg_sd)))
  colnames(mod_param) <- c(marg_m_names, marg_sd_names)
  return(mod_param)
}
