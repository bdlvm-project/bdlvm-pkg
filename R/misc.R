renv_update <- function(){renv::purge("bdlvm");renv::hydrate()}

is_formula <- function(x) {
  UseMethod("is_formula")
}

#' @method is_formula default
#' @export
is_formula.default <- function(x) {
  if(!inherits(x, "formula"))stop("Function expected a formula")
  TRUE
}

#' @method is_formula list
#' @export
is_formula.list <- function(x) {
  invisible(all(sapply(x, is_formula)))
}
