#' Toy Data: Noisy circle
#'
#' A simulated data set consisting of 100 points sampled from a circle with
#' additive Gaussian noise using a standard deviation of 0.05.
#'
#' The point cloud stored in `noisy_circle_points` has been generated using the
#' [**tdaunif**](https://tdaverse.github.io/tdaunif/) package using the
#' [`tdaunif::sample_circle()`](https://tdaverse.github.io/tdaunif/reference/circles.html)
#' function. Specifically, the following parameters were used: `n = 100`, `sd =
#' 0.05` and a seed of 1234.
#'
#' The persistence diagram stored in `noisy_circle_persistence` has been
#' computed using the [**ripserr**](https://tdaverse.github.io/ripserr/) package
#' with the
#' [`ripserr::vietoris_rips()`](https://tdaverse.github.io/ripserr/reference/vietoris_rips.html)
#' function. Specifically, the following parameters were used: `max_dim = 1L`.
#'
#' @format
#' ## `noisy_circle_points`
#' A matrix with 100 rows and 2 columns listing the coordinates of the points.
#'
#' ## `noisy_circle_persistence`
#' An object of class 'PHom' as returned by the
#' [`ripserr::vietoris_rips()`](https://tdaverse.github.io/ripserr/reference/vietoris_rips.html)
#' function, which is a matrix with 3 columns:
#'
#' - `dimension`: the dimension/degree of the feature,
#' - `birth`: the birth value of the feature,
#' - `death`: the death value of the feature.
#'
#' @source <https://tdaverse.github.io/tdaunif/reference/circles.html>,
#'   <https://tdaverse.github.io/ripserr/reference/vietoris_rips.html>
#' @name noisy_circle

#' @rdname noisy_circle
"noisy_circle_points"

#' @rdname noisy_circle
"noisy_circle_persistence"
