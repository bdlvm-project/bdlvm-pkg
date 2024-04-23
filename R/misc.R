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

#' @export 
get_varnames <- function(f) {
  f <- bdlvm_parse(f)
  if(is.null(f$forms))
    f <- list(forms = f)
  unique(unlist(lapply(
    f$forms, \(x) {
    c(all.vars(x$formula),
    lapply(x$pforms, \(z) all.vars(z)[-1]))
  })))
}

#' @export
bdlvm_template <- function(f) {
  varnames <- get_varnames(f)
  tmp_data <- as.data.frame(matrix(0, ncol = length(varnames)))
  names(tmp_data) <- varnames; tmp_data$nonconstant <- 0
  tmp_data
}

fill_na <- \(x) {
  if(all(is.na(x))) return(x)
  fill <- unique(x[!is.na(x)])
  if(length(fill)!=1) stop("More than one value found")
  rep(fill, length(x))
}