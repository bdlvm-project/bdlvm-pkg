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