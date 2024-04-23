# Other extensions to brms functionality e.g. stanvars, NCP, etc.
#' @export
bdlvm_stanvars <- {
brms::stanvar(block = "functions", scode = "
real expgamma_lpdf(real x, real alpha, real beta) {
  return gamma_lpdf(exp(x) | alpha, beta) + x;
}
real expgamma_lccdf(real x, real alpha, real beta) {
  return gamma_lccdf(exp(x) | alpha, beta);
}
real expst_lpdf(real x, real nu, real mu, real sigma) {
  return student_t_lpdf(exp(x) | nu, mu, sigma) + x;
}
real expst_lccdf(real x, real nu, real mu, real sigma) {
  return student_t_lccdf(exp(x) | nu, mu, sigma);
}")
}

# Not fully tested, should interact w/parse step to tag params in a cleaner way
#' @export
bdlvm_model_prior <- \(formula, data, ..., rescor = FALSE) {
  formula <- bdlvm_parse(formula)
  if(inherits(formula, "mvbrmsformula"))
    formula <- formula + brms::set_rescor(rescor)
  p <- brms::get_prior(formula, data)
  # drop empty generic priors
  p <- p[!(p$class == "b" & p$coef == "") & p$resp != "", ]
  # add prior tags
  p[grepl("y", p$resp) & grepl("Int", p$class), "source"] <- "fact_incpt"
  p[grepl("y", p$resp) & grepl("sig", p$class), "source"] <- "fact_sigma"
  p[grepl("y", p$resp) & p$class == "b", "source"] <- "coef_mean"
  p[grepl("sig", p$dpar) & grepl("Int", p$class), "source"] <- "fact_dsigma"
  p[grepl("sig", p$dpar) & p$class == "b", "source"] <- "coef_sigma"
  p[grepl("x", p$resp) & grepl("miy", p$coef), "source"] <- "item_load"
  p[grepl("x", p$resp) & grepl("Int", p$class), "source"] <- "item_incpt"
  p[grepl("x", p$resp) & grepl("sig", p$class), "source"] <- "item_sigma"
  p[grepl(".+i1$", p$resp) & p$source == "item_load", "source"] <- "first_load"
  # add random parameter so sampling always works
  p[nrow(p) + 1,] <- list(
    prior = "normal(0,1)", class = "sigma", coef = "",
    group = "", resp = "nonconstant", dpar = "", nlpar = "",
    lb = "0", ub = "", source = "ignore"
  )
  p
}
