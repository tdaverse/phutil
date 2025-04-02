
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phutil

<!-- badges: start -->

[![R-CMD-check](https://github.com/tdaverse/phutil/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tdaverse/phutil/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tdaverse/phutil/graph/badge.svg)](https://app.codecov.io/gh/tdaverse/phutil)
<!-- badges: end -->

The goal of the **phutil** package is to provide utility functions for
persistence data analysis. In particular, the package defines a data
structure for hosting persistence data. The package is part of the
[*tdaverse*](https://github.com/tdaverse) suite of packages, which aims
to provide a consistent interface for topological data analysis in R
that nicely integrates with the
[*tidymodels*](https://www.tidymodels.org) ecosystem.

## Installation

You can install the development version of **phutil** from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tdaverse/phutil")
```

## Example

The package currently provides a new data structure for hosting
persistence data. The data set `noisy_circle_points` is a simulated data
set consisting of 100 points sampled from a circle with additive
Gaussian noise using a standard deviation of $0.05$:

``` r
library(phutil)
head(noisy_circle_points)
#>               x          y
#> [1,]  0.6651721  0.6363172
#> [2,] -0.7481066 -0.6901262
#> [3,] -0.8288544 -0.5519689
#> [4,] -0.7650181 -0.7436499
#> [5,]  0.6337296 -0.7607464
#> [6,] -0.6077663 -0.7036492
```

The data set `noisy_circle_ripserr` is a persistence diagram computed
using the [**ripserr**](https://tdaverse.github.io/ripserr/) package
with the
[`ripserr::vietoris_rips()`](https://tdaverse.github.io/ripserr/reference/vietoris_rips.html)
and stored as an object of class ‘PHom’, which is a light wrapper around
a data frame with 3 variables:

``` r
head(noisy_circle_ripserr)
#>   dimension birth       death
#> 1         0     0 0.008162723
#> 2         0     0 0.008548402
#> 3         0     0 0.009238432
#> 4         0     0 0.014004707
#> 5         0     0 0.020555701
#> 6         0     0 0.024621462
```

The data set `noisy_circle_tda_rips` is a persistence diagram computed
using the **TDA** package with the
[`TDA::ripsDiag()`](https://www.rdocumentation.org/packages/TDA/versions/1.9.1/topics/ripsDiag)
and stored as an object of class ‘diagram’, which is a matrix with 3
columns:

``` r
head(noisy_circle_tda_rips)
#>      dimension Birth     Death
#> [1,]         0     0 1.6322000
#> [2,]         0     0 0.3677392
#> [3,]         0     0 0.3325817
#> [4,]         0     0 0.3226673
#> [5,]         0     0 0.2096916
#> [6,]         0     0 0.1953531
```

The data structure adopted by the *tdaverse* suite of packages and
provided by this package is designed to be a common interface for
persistence data. This allows for a seamless integration of persistence
data across different packages and ensures that the data is always in a
consistent format.

Specifically, the ‘persistence’ class is a list with the following
components:

- `pairs`: A list of 2-column matrices containing birth-death pairs. The
  -*th* element of the list corresponds to the -*th* homology dimension.
  If there is no pairs for a given dimension but there are pairs in
  higher dimensions, the corresponding element(s) is/are filled with a
  numeric matrix.

- `metadata`: A list of 3 elements containing information about how the
  data was computed:

  - `data`: The name of the object containing the original data on which
    the persistence data was computed.
  - `engine`: The name of the package that computed the persistence
    data.
  - `simplicial_complex`: The type of simplicial complex used in the
    computation.
  - `parameters`: A list of parameters used in the computation.
  - `call`: The exact call that generated the persistence data.

The `as_persistence()` function is used to convert persistence data from
different packages into the ‘persistence’ class. For example, the
persistence data computed using the **ripserr** package can be converted
into the ‘persistence’ class as follows:

``` r
as_persistence(noisy_circle_ripserr)
#> 
#> ── Persistence Data ────────────────────────────────────────────────────────────
#> ℹ There are 99 pairs in dimension 0.
#> ℹ There are  2 pairs in dimension 1.
#> 
#> ── Metadata ────────────────────────────────────────────────────────────────────
#> ℹ The data used to compute persistence is not available.
#> ℹ Persistence has been computed using the ripserr package.
#> ℹ The simplicial complex used in the computation is rips.
#> ℹ The function called to compute persistence is not available.
#> ℹ The parameters used for the computation are not available.
```

Similarly, the persistence data computed using the **TDA** package can
be converted into the ‘persistence’ class as follows:

``` r
as_persistence(noisy_circle_tda_rips)
#> 
#> ── Persistence Data ────────────────────────────────────────────────────────────
#> ℹ There are 100 pairs in dimension 0.
#> ℹ There are   2 pairs in dimension 1.
#> 
#> ── Metadata ────────────────────────────────────────────────────────────────────
#> ℹ The data used to compute persistence is stored in object noisy_circle_points.
#> ℹ Persistence has been computed using the TDA package.
#> ℹ The simplicial complex used in the computation is rips.
#> ℹ The function called was `ripsDiag()`.
#> ℹ The parameters used for the computation are:
#>     → maxdimension: 1
#>     → maxscale    : 1.6322
```
