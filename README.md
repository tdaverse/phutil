
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phutil

<!-- badges: start -->

[![R-CMD-check](https://github.com/tdaverse/phutil/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tdaverse/phutil/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tdaverse/phutil/graph/badge.svg)](https://app.codecov.io/gh/tdaverse/phutil)
<!-- badges: end -->

The goal of the {phutil} package is to provide utility functions for
persistence data analysis. In particular, the package defines a data
structure for hosting persistence data. The package is part of the
[*TDAverse*](https://github.com/tdaverse) suite of packages, which aims
to provide a consistent interface for topological data analysis in R
that nicely integrates with the
[*tidymodels*](https://www.tidymodels.org) ecosystem.

## Installation

You can install the development version of {phutil} from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tdaverse/phutil")
```

## The `persistence` class

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
and stored as an object of class `PHom`, which is a light wrapper around
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
and stored as a list of length 1 containing the object `diagram` of
class `diagram`, which is a matrix with 3 columns:

``` r
head(noisy_circle_tda_rips$diagram)
#>      dimension Birth     Death
#> [1,]         0     0 1.6322000
#> [2,]         0     0 0.3677392
#> [3,]         0     0 0.3325817
#> [4,]         0     0 0.3226673
#> [5,]         0     0 0.2096916
#> [6,]         0     0 0.1953531
```

The data structure adopted by the *TDAverse* suite of packages and
provided by this package is designed to be a common interface for
persistence data. This allows for a seamless integration of persistence
data across different packages and ensures that the data is always in a
consistent format.

Specifically, the `persistence` class is a list with the following
components:

- `pairs`: A list of 2-column matrices containing birth-death pairs. The
  -*th* element of the list corresponds to the -*th* homology dimension.
  If there is no pairs for a given dimension but there are pairs in
  higher dimensions, the corresponding element(s) is/are filled with a
  numeric matrix.

- `metadata`: A list of length 6 containing information about how the
  data was computed:

  - `orderered_pairs`: A boolean indicating whether the pairs in the
    `pairs` list are ordered (i.e. the first column is strictly less
    than the second column).
  - `data`: The name of the object containing the original data on which
    the persistence data was computed.
  - `engine`: The name of the package and the function of this package
    that computed the persistence data in the form
    `"package_name::package_function"`.
  - `filtration`: The filtration used in the computation in a
    human-readable format (i.e. full names, capitals where need, etc.).
  - `parameters`: A list of parameters used in the computation.
  - `call`: The exact call that generated the persistence data.

The `as_persistence()` function is used to convert persistence data from
different packages into the `persistence` class. For example, the
persistence data computed using the **ripserr** package can be converted
into the `persistence` class as follows:

``` r
as_persistence(noisy_circle_ripserr)
#> 
#> ── Persistence Data ────────────────────────────────────────────────────────────
#> ℹ There are 99 and 2 pairs in dimensions 0 and 1 respectively.
#> ℹ Computed from a Vietoris-Rips/Cubical filtration using `ripserr::<vietoris_rips/cubical>()`.
#> ! With unknown parameters.
```

Similarly, the persistence data computed using the **TDA** package can
be converted into the `persistence` class as follows:

``` r
as_persistence(noisy_circle_tda_rips)
#> 
#> ── Persistence Data ────────────────────────────────────────────────────────────
#> ℹ There are 100 and 2 pairs in dimensions 0 and 1 respectively.
#> ℹ Computed from a Vietoris-Rips filtration using `TDA::ripsDiag()`.
#> ℹ With the following parameters: maxdimension = 1 and maxscale = 1.6322.
```

## Distances between persistence diagrams

The package also provides a function to compute the bottleneck distance
between two persistence diagrams. The function `bottleneck_distance()`
takes two persistence diagrams as input and computes the bottleneck
distance between them:

``` r
library(phutil)
diag1 <- persistence_sample[[1]]
diag2 <- persistence_sample[[2]]
bottleneck_distance(diag1, diag2)
#> [1] 0.06197453
```

One can also compute the Wasserstein distance between two persistence
diagrams using the `wasserstein_distance()` function:

``` r
wasserstein_distance(diag1, diag2)
#> [1] 1.53403
```

It is also possible to compute these distances between all pairs of
persistence diagrams in a list using
e.g. `wasserstein_pairwise_distances()`:

``` r
wasserstein_pairwise_distances(persistence_sample[1:5])
#>          1        2        3        4
#> 2 1.534030                           
#> 3 2.301812 1.852241                  
#> 4 2.354967 2.362616 1.922799         
#> 5 2.374818 2.707366 1.985518 3.154120
```

It returns an object of class `dist`. It is parallelized using
[OpenMP](https://www.openmp.org/) and relies under the hood on the
[Hera](https://github.com/anigmetov/hera) C++ library which is
BSD-licensed. The code interfacing R and C++ is generated by the
header-only [{cpp11}](https://cpp11.r-lib.org) package which is
MIT-licensed.

## Contributions

### Code of Conduct

Please note that the {phutil} package is released with a [Contributor
Code of
Conduct](https://tdaverse.github.io/phutil/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

### Acknowledgments

This project was funded by [an ISC grant from the R
Consortium](https://r-consortium.org/all-projects/2024-group-1.html#modular-interoperable-and-extensible-topological-data-analysis-in-r)
and done in coordination with Jason Cory Brunson and with guidance from
Bertrand Michel and Paul Rosen. It builds upon conversations with
Mathieu Carrière and Vincent Rouvreau who are among the authors of the
[GUDHI](https://gudhi.inria.fr) library. Package development also
benefited from the support of colleagues at the [Department of
Mathematics Jean Leray](https://www.math.sciences.univ-nantes.fr) and
the use of equipment at [Nantes
University](https://english.univ-nantes.fr).
