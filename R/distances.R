#' Distances between two persistence diagrams
#'
#' This collection of functions computes the distance between two persistence
#' diagrams of the same homology dimension. The diagrams must be represented as
#' 2-column matrices. The first column of the matrix contains the birth times
#' and the second column contains the death times of the points.
#'
#' @param x A matrix of shape \eqn{n \times 2} specifying the first persistence
#'   diagram.
#' @param y A matrix of shape \eqn{m \times 2} specifying the second
#'   persistence diagram.
#' @param delta A numeric value specifying the relative error. Defaults to
#'   `0.01`. For the Bottleneck distance, it can be set to `0.0` in which case
#'   the exact Bottleneck distance is computed, while an approximate Bottleneck
#'   distance is computed if `delta > 0.0`.
#' @param wasserstein_power A numeric value specifying the power of the
#'   Wasserstein distance. Defaults to `1.0`.
#'
#' @returns A numeric value storing the Bottleneck distance between the two
#'   persistence diagrams.
#'
#' @examples
#' bottleneck_distance(
#'   persistence_sample[[1]]$pairs[[1]],
#'   persistence_sample[[2]]$pairs[[1]]
#' )
#'
#' wasserstein_distance(
#'   persistence_sample[[1]]$pairs[[1]],
#'   persistence_sample[[2]]$pairs[[1]]
#' )
#'
#' @name distances
NULL

#' @rdname distances
#' @export
bottleneck_distance <- function(x, y, delta = 0.01) {
  bottleneckDistance(
    x = x,
    y = y,
    delta = delta
  )
}

#' @rdname distances
#' @export
wasserstein_distance <- function(x, y, delta = 0.01, wasserstein_power = 1.0) {
  wassersteinDistance(
    x = x,
    y = y,
    delta = delta,
    wasserstein_power = wasserstein_power
  )
}

#' Pairwise distances within a set of persistence diagrams
#'
#' This collection of functions computes the pairwise distance matrix between
#' all pairs in a set of persistence diagrams of the same homology dimension.
#' The diagrams must be represented as 2-column matrices. The first column of
#' the matrix contains the birth times and the second column contains the death
#' times of the points.
#'
#' @param x A list of 2-column matrices specifying the set of persistence
#'   diagrams.
#' @param delta A numeric value specifying the relative error. Defaults to
#'   `0.01`. For the Bottleneck distance, it can be set to `0.0` in which case
#'   the exact Bottleneck distance is computed, while an approximate Bottleneck
#'   distance is computed if `delta > 0.0`.
#' @param wasserstein_power A numeric value specifying the power of the
#'   Wasserstein distance. Defaults to `1.0`.
#' @param ncores An integer value specifying the number of cores to use for
#'   parallel computation. Defaults to `1L`.
#'
#' @returns An object of class 'dist' containing the pairwise distance matrix
#'   between the persistence diagrams.
#'
#' @examples
#' # Extract the list of 2-column matrices for dimension 0 in the sample
#' x <- lapply(persistence_sample[1:10], function(x) x$pairs[[1]])
#'
#' # Compute the pairwise Bottleneck distances
#' Db <- bottleneck_pairwise_distances(x, delta = 0.01)
#'
#' # Compute the pairwise Wasserstein distances
#' Dw <- wasserstein_pairwise_distances(x, delta = 0.01, wasserstein_power = 1.0)
#'
#' @name pairwise-distances
NULL

#' @rdname pairwise-distances
#' @export
bottleneck_pairwise_distances <- function(x,
                                          delta = 0.01,
                                          ncores = 1L) {
  D <- bottleneckPairwiseDistances(
    x = x,
    delta = delta,
    ncores = ncores
  )
  attr(D, "Size") <- length(x)
  attr(D, "Labels") <- seq_along(x)
  attr(D, "Diag") <- FALSE
  attr(D, "Upper") <- FALSE
  attr(D, "method") <- "bottleneck"
  attr(D, "class") <- "dist"
  D
}

#' @rdname pairwise-distances
#' @export
wasserstein_pairwise_distances <- function(x,
                                           delta = 0.01,
                                           wasserstein_power = 1.0,
                                           ncores = 1L) {
  D <- wassersteinPairwiseDistances(
    x = x,
    delta = delta,
    wasserstein_power = wasserstein_power,
    ncores = ncores
  )
  attr(D, "Size") <- length(x)
  attr(D, "Labels") <- seq_along(x)
  attr(D, "Diag") <- FALSE
  attr(D, "Upper") <- FALSE
  attr(D, "method") <- "wasserstein"
  attr(D, "class") <- "dist"
  D
}
