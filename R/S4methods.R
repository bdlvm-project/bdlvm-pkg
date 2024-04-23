#' @include classes.R S3methods.R
NULL

# bdlvmFormula -----------------------------------------------------------------
setMethod("show",
          signature = "bdlvmFormula",
          definition = function(object){
            lv <- object@lv_terms
            bf <- object@bf_terms

            cat(
              paste(sapply(
                sapply(lv, \(x)x$formulas),
                \(z)paste0("lv(", paste(
                  if(is.list(z)) sapply(z, deparse) else deparse(z),
                  collapse = ", "), ")")),
                collapse = " + "),
              paste(sapply(bf, deparse), collapse = " + "),
              sep = "\n")
          })

setMethod("+", signature(e1 = "bdlvmFormula", e2 = "ANY"),
          function(e1, e2) {
            if(inherits(e2, "bdlvmFormula")) {
              e1@lv_terms <- c(e1@lv_terms, e2@lv_terms)
              e1@bf_terms <- c(e1@bf_terms, e2@bf_terms)
            } else if(inherits(e2, "bdlvmlvterm")) {
              e1@lv_terms <- c(e1@lv_terms, list(e2))
            } else if(inherits(e2, "bform")) {
              e1@bf_terms <- c(e1@bf_terms,
                               structure(substitute(e2),
                                         class = "bdlvmbform"))
            } else {
              stop("`+` not defined between bdlvmFormula and object of class '",
                   class(e2), "'")
            }
            e1
          })
