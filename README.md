
# bdlvm

<!-- badges: start -->
<!-- badges: end -->

`bdlvm` provides a crude interface for building latent variable models on top of `brms`, by taking advantage of the `mi()` missing variable indicator.

## Installation

Download-only token:

```r
devtools::install_github("bdlvm-project/bdlvm-pkg", auth_token = "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")
```

## Example

Specify a latent variable model and transform it into a `brms` formula in two simple steps:

``` r
library(bdlvm)
cfa_formula <- lv(x ~ items(y, 3))

parse_bdlvm(cfa_formula)
# x | mi() ~ 1 
# yLVi1 ~ mi(x) 
# yLVi2 ~ mi(x) 
# yLVi3 ~ mi(x) 
```

Done! The result can be used in `brms::brm()` as usual. Just make sure your `data.frame` has the corresponding manifest variables and columns consisting solely of `NA_real_` for each latent variable.

See the documentation at `?lv()` for more details.
