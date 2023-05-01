#' Parse a latent variable formula into `brms` format
#'
#' @param x A formula created with [lv()]
#'
#' @return
#' A set of `brms` formulas corresponding with `mi()` terms added as needed.
#' 
#' @export
#'
#' @examples
#' library(bdlvm)
#' parse_bdlvm(lv(y ~ items(x, 3)))
parse_bdlvm <- function(x, ...) {
  UseMethod("parse_bdlvm")
}

#' @method parse_bdlvm bdlvmFormula
#' @export
parse_bdlvm.bdlvmFormula <- function(x, ..., eval = TRUE) {
  lv_parsed <- parse_bdlvm(x@lv_terms, transpose = TRUE)
  bf_parsed <- parse_bdlvm(x@bf_terms, lv = lv_parsed)

  lv_bform <- make_bform(lv_parsed) # could go into parse_bdlvm

  parsed <- c(lv_bform,
              unlist(bf_parsed))

  if(!eval) return(parsed)
  eval(parse(text = paste(parsed, collapse = " + ")))
}

#' @method parse_bdlvm list
#' @export
parse_bdlvm.list <- function(x, ..., transpose = FALSE) {
  parsed <- purrr::map(x, parse_bdlvm, ...)
  if(transpose)
    parsed <- purrr::list_transpose(parsed)
  parsed
}

#' @method parse_bdlvm bdlvmlvterm
#' @export
parse_bdlvm.bdlvmlvterm <- function(x, ...) {
  resp <- get_response(x)
  items <- get_items(x)
  formulas <- drop_items(x, items)

  list(formulas = formulas,
       resp = resp,
       items = items,
       args = x$args)
}

#' @method parse_bdlvm bdlvmbform
#' @export
parse_bdlvm.bdlvmbform <- function(x, lv) {

  all_lv <- sapply(lv$resp, \(z)z[1])

  if(is.null(names(x)))
    unnamed <- seq_along(x)[-1]
  else
    unnamed <- setdiff(which(names(x) == ""), 1)

  x[unnamed] <- sapply(x[unnamed], \(z) {
    z <- as.formula(z)
    add_mi(z, all_lv)
  })

  deparse(x)
}
