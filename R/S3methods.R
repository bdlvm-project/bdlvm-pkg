#' Parse a latent variable formula into `brms` format
#'
#' Constructs a set of `brms` formulas that encode latent variables using `mi()`
#'
#' @param x A formula created with [lv()]
#' @param eval Default `TRUE`, otherwise returns the deparsed `bf()` calls
#' @param ... Additional parameters to customize item naming and use of censoring/missingness. Currently supports `suffix1`, `suffix2`, `mi`, `cens`.
#'
#' @return
#' `brms` formulas representing each latent variable and their corresponding measurement items
#'
#' @export
#'
#' @examples
#' library(bdlvm)
#' bdlvm_parse(lv(y ~ items(x, 3)))
bdlvm_parse <- function(x, ...) {
  UseMethod("bdlvm_parse")
}

#' @method bdlvm_parse bdlvmFormula
#' @export
bdlvm_parse.bdlvmFormula <- function(x, ..., eval = TRUE) {
  lv_parsed <- bdlvm_parse(x@lv_terms, transpose = TRUE)
  bf_parsed <- bdlvm_parse(x@bf_terms, lv = lv_parsed)

  lv_bform <- make_bform(lv_parsed, ...)

  parsed <- c(lv_bform,
              unlist(bf_parsed))

  if(!eval) return(parsed)
  eval(parse(text = paste(parsed, collapse = " + ")))
}

#' @method bdlvm_parse list
#' @export
bdlvm_parse.list <- function(x, ..., transpose = FALSE) {
  parsed <- purrr::map(x, bdlvm_parse, ...)
  if(transpose)
    parsed <- purrr::list_transpose(parsed)
  parsed
}

#' @method bdlvm_parse bdlvmlvterm
#' @export
bdlvm_parse.bdlvmlvterm <- function(x, ...) {
  resp <- get_response(x)
  items <- get_items(x)
  formulas <- drop_items(x, items)

  list(formulas = formulas,
       resp = resp,
       items = items,
       args = x$args)
}

#' @method bdlvm_parse bdlvmbform
#' @export
bdlvm_parse.bdlvmbform <- function(x, lv) {

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
