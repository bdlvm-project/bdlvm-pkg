# general helpers --------------------------------------------------------------
add_mi <- function(f, vars) {
  for(v in vars) {
    rhs_terms <- attr(terms(f), "term.labels")
    for(i in rhs_terms) {
      if (!v %in% get_vars(i)) next
      new <- i
      new <- sub(v, paste("mi(", v,")"), new)
      f <- update(f, as.formula(paste(". ~ . -", i, "+", new)))
    }
  }
  parse(text = deparse(f))[[1]]
}

drop_items <- function(x, y) {
  x <- x$formulas
  c(
    update(x[[1]], as.formula(paste(". ~ . -", deparse(y)))),
    x[-1]
  )
}

get_response <- function(x) {
  x <- x$formulas
  purrr::map_chr(x, lhs)
}

get_items <- function(x) {
  x <- x$formulas[[1]]
  x <- parse(text = rownames(attr(terms(x), "factors"))[2])[[1]]

  if(tryCatch(!identical(x[[1]], expression(items)[[1]]), error = \(e)TRUE))
    stop("Must specify items for the latent variable as first term of the first formula")

  x
}

get_vars <- function(x) {
  paste0("~", x) |> as.formula() |> all.vars()
}

has_lhs <- function(x) {
  identical(attr(terms(x), "response"), 1L)
}

lhs <- function(x) {
  if(!has_lhs(x))
    stop("Could not find a left-hand side in the formula")
  deparse(x[[2]])
}

rhs_contains <- function(x, y) {
  y %in% attr(terms(x), "term.labels")
}

# specific --------------------------------------------------------------
make_bform <- function(x, suffix1 = "i", suffix2 = NULL,
                       mi = FALSE, cens = FALSE) {
  all_lv <- sapply(x$resp, \(z)z[1])

  # make lv|mi() terms
  bforms <- lapply(x$formulas, \(z) {
    if(!inherits(z, "list")) z <- list(z)
    z[[1]][[2]] <- parse(text = paste(z[[1]][[2]], "| mi()"))[[1]]
    z
  })

  bforms <- lapply(bforms, \(z) {
    sapply(z, \(w) add_mi(w, all_lv))
  })

  bforms <- lapply(seq_along(all_lv), \(i) {
    paste0("brms::bf(",
           paste(sapply(bforms[[i]], \(w) deparse(w)), collapse = ", "),
           {
             args <- gsub("^expression\\(|\\)$", "",
                          paste(deparse(as.expression(x$args[[i]])),
                          collapse = ""))
             if(identical(args, "NULL"))
               NULL
             else
               paste(",", args)
           }, ")")
  })

  iforms <- lapply(seq_along(all_lv), \(z) {
    prefix <- deparse(x$items[[z]][[2]])
    if(is.null(suffix2))suffix2 <- seq_len(x$items[[z]][[3]])
    if(length(suffix2)!=x$items[[z]][[3]])stop("suffix2 size must match items")
    iname <- paste0(prefix, suffix1, suffix2)

    separator <- if(mi|cens) "| " else NULL
    addition <- if(mi&cens) " + " else NULL
    mi <- if(mi) "mi()" else NULL
    cens <- if(cens) paste0("cens(", iname, "c)") else NULL

    paste0("brms::bf(", iname, separator, cens, addition, mi,
           "~ mi(", all_lv[z],"))")
  })

  unlist(sapply(seq_along(all_lv), \(z)c(bforms[z], iforms[[z]])))
}
