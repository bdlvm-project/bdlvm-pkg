# user-facing constructor ------------------------------------------------------
#' Set up a latent variable formula for **brms**
#'
#' Set up an object that can be parsed by`[parse_bdlvm()` into a `brms` formula that constructs a latent variable and the associated manifest variables through which it's measured.
#'
#' The first RHS term must be of the form `items(prefix, num)`, where `num` is an integer and `prefix` is an unquoted name that will be used to build the measurement variables, following the pattern `prefixLVi1` up to `prefixLVi{num}`. Additional terms on the RHS are interpreted as predictors on the latent variable's location parameter, as usual.
#'
#' Subsequent formulas are interpreted as regressions on other parameters of the latent variable. See `brms` documentation for the list of supported families and their corresponding parameter names.
#'
#' @param ...
#' Unnamed arguments must be formulas, see details
#'
#' Any named argument is assumed to be a valid [brms::bf()] argument
#'
#' @return An S4 object of class `bdlvmFormula`, which supports concatenation via `+` with `brms` formulas.
#' @export
#'
#' @examples
#' # Single latent variable with three items (i.e. CFA)
#' lv(x ~ items(y, 3))
#'
#' # Distributional regression, additional predictors and concatenation
#' # with brms formula. Note that latent variable 'x' does not require
#' # special notation to be placed in bf().
#' library(brms)
#' lv(x ~ items(y, 5) + z1,
#'    sigma ~ z1,
#'    family = "lognormal") +
#'    bf(z2 ~ x + z1)
#'
lv <- function(...) {
  bdlvmFormula() + bdlvmlvterm(...)
}

# bdlvmFormula (S4) ------------------------------------------------------------
# Not exported, only for handling by the package
bdlvmFormula <- setClass(
  "bdlvmFormula",
  slots = c(lv_terms = "list", bf_terms = "list"),
  prototype = list(lv_terms = list(), bf_terms = list())
)

# bdlvmlvterm (S3) -------------------------------------------------------------
bdlvmlvterm <- function(...) {
  dots <- list(...)
  argnames <- names(dots)

  if(is.null(argnames)) {
    formulas <- dots
    args <- NULL
  } else {
    formulas <- dots[argnames == ""]
    args <- dots[argnames != ""]
  }

  is_formula(formulas)

  structure(
    list(formulas = formulas, args = args),
    class = "bdlvmlvterm"
  )
}
