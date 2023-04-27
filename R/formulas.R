# general helpers --------------------------------------------------------------
add_mi <- function(x, y) {
  for(i in y) {
    if(rhs_contains(x, i))
      x <- update(x, as.formula(paste(". ~ . -", i, "+ mi(", i, ")")))
  }
  parse(text = deparse(x))[[1]]
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
make_bform <- function(x) {

  all_lv <- sapply(x$resp, \(z)z[1])

  # make lv|mi() terms
  bforms <- lapply(x$formulas, \(z) {
    if(class(z) != "list")z <- list(z)
    z[[1]][[2]] <- parse(text = paste(z[[1]][[2]], "| mi()"))[[1]]
    z
  })

  bforms <- lapply(bforms, \(z) {
    sapply(z, \(w) add_mi(w, all_lv))
  })

  bforms <- lapply(bforms, \(z) {
    paste0("brms::bf(", paste(sapply(z, \(w) deparse(w)), collapse = ", "), ")")
  })
  # make item_x ~ mi(lv) terms
  iforms <- lapply(seq_along(all_lv), \(z) {
    prefix <- deparse(x$items[[z]][[2]])
    suffix <- seq_len(x$items[[z]][[3]])
    paste0("brms::bf(", prefix, "LVi", suffix, " ~ mi(", all_lv[z],"))")
  })

  unlist(sapply(seq_along(all_lv), \(z)c(bforms[z], iforms[[z]])))
}
