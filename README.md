
# bdlvm

<!-- badges: start -->
<!-- badges: end -->

`bdlvm` provides a simple interface for building latent variable models on top of `brms`. Currently, it works through the `mi()` missing variable indicator, simply providing a wrapper around its use.

## Installation

Download from this repo:

```r
devtools::install_github("bdlvm-project/bdlvm-pkg")
```

## Example

Specify a latent variable model and transform it into a `brms` formula in two simple steps:

``` r
library(bdlvm)
cfa_formula <- lv(x ~ items(y, 3))

bdlvm_parse(cfa_formula)
# x | mi() ~ 1 
# yi1 ~ mi(x) 
# yi2 ~ mi(x) 
# yi3 ~ mi(x) 
```

Done! The result can be used in `brms::brm()` as usual. Just make sure your `data.frame` has the corresponding manifest variables and columns consisting solely of `NA_real_` for each latent variable.

See the documentation at `?lv()` for more details.
