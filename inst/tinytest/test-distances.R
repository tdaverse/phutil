x <- cbind(birth = c(1, 2), death = c(3, 1))
y <- cbind(birth = numeric(0), death = numeric(0))
expect_equal(bottleneck_distance(x, y), 1)
expect_message(
  bottleneck_distance(x, y),
  "Birth values are expected to be smaller than death values."
)
expect_equal(bottleneck_distance(y, x), 1)
expect_message(
  bottleneck_distance(y, x),
  "Birth values are expected to be smaller than death values."
)
expect_equal(wasserstein_distance(x, y), 1)
expect_message(
  wasserstein_distance(x, y),
  "Birth values are expected to be smaller than death values."
)
expect_equal(wasserstein_distance(y, x), 1)
expect_message(
  wasserstein_distance(y, x),
  "Birth values are expected to be smaller than death values."
)

x <- cbind(birth = c(1, 2), death = c(3, 4))

expect_error(
  bottleneck_distance(x, y, tol = -1.0),
  'relative error was "-1.000000", must be a number >= 0.0. Cannot proceed.'
)
expect_equal(bottleneck_distance(x, y, tol = 0.0), 1)
expect_equal(bottleneck_distance(x, y), 1)
expect_error(
  wasserstein_distance(x, y, tol = -1.0),
  'relative error was "-1.000000", must be a number > 0.0. Cannot proceed.'
)
expect_error(
  wasserstein_distance(x, y, p = 0),
  'Wasserstein_degree was "0.000000", must be a number >= 1.0. Cannot proceed.'
)
expect_error(wasserstein_distance(x, y, tol = 0.0, p = 1))
expect_message(
  wasserstein_distance(x, y, p = 7),
  'crash or stall the Wasserstein calculation'
)
expect_equal(wasserstein_distance(x, y, p = 1), 2)
expect_equal(round(wasserstein_distance(x, y, p = 2), digits = 6L), 1.414214)

expect_equal(
  bottleneck_distance(as_persistence(list(x)), as_persistence(list(y))),
  1
)
expect_equal(
  wasserstein_distance(as_persistence(list(x)), as_persistence(list(y))),
  2
)

out <- bottleneck_pairwise_distances(persistence_sample[1L:3L])
expect_equal(length(out), 3L)

out <- wasserstein_pairwise_distances(persistence_sample[1L:3L], p = 21)
expect_equal(length(out), 3L)

out <- wasserstein_pairwise_distances(persistence_sample[1L:3L])
expect_equal(length(out), 3L)

wrong_sample <- persistence_sample[1L:3L]
wrong_sample[[1L]] <- cbind(birth = c(1, 2), death = c(3, 1))

expect_inherits(bottleneck_pairwise_distances(wrong_sample), "dist")
expect_inherits(wasserstein_pairwise_distances(wrong_sample), "dist")

expect_equal(bottleneck_distance(x, cbind(1, 1)), 1)
expect_equal(wasserstein_distance(x, cbind(1, 1)), 2)

mod_sample <- persistence_sample[1L:3L]
mod_sample[[1L]] <- cbind(1, 1)
expect_equal(length(bottleneck_pairwise_distances(mod_sample)), 3L)
expect_equal(length(wasserstein_pairwise_distances(mod_sample)), 3L)

expect_equal(kantorovich_distance(x, y), wasserstein_distance(x, y))
expect_equal(
  kantorovich_pairwise_distances(mod_sample),
  wasserstein_pairwise_distances(mod_sample)
)

# alternative internal norms
X <- rbind(
  c(1, 3),
  c(3, 5)
)
Y <- rbind(
  c(3, 4)
)
Z <- matrix(NA_real_, nrow = 0L, ncol = 2L)
# Manhattan
expect_equal(wasserstein_distance(X, Y, p = 1, q = 1), 3, tol = 1e-6)
expect_equal(wasserstein_distance(X, Y, p = 2, q = 1), sqrt(5), tol = 1e-6)
# expect_equal(wasserstein_distance(X, Y, p = Inf, q = 1), 2, tol = 1e-6)
expect_error(
  wasserstein_distance(X, Y, p = Inf, q = 1),
  "q-bottleneck distances (`q < Inf`) are not yet supported."
)
# Pythagorean
expect_equal(wasserstein_distance(X, Y, p = 1, q = 2), 1+sqrt(2), tol = 1e-6)
expect_equal(wasserstein_distance(X, Y, p = 2, q = 2), sqrt(3), tol = 1e-6)
# expect_equal(wasserstein_distance(X, Y, p = Inf, q = 2), sqrt(2), tol = 1e-4)
expect_error(
  wasserstein_distance(X, Y, p = Inf, q = 2),
  "q-bottleneck distances (`q < Inf`) are not yet supported."
)
# supremum (default)
expect_equal(wasserstein_distance(X, Y, p = 1, q = Inf), 2, tol = 1e-6)
expect_equal(wasserstein_distance(X, Y, p = 2, q = Inf), sqrt(2), tol = 1e-6)
expect_equal(wasserstein_distance(X, Y, p = Inf, q = Inf), 1, tol = 1e-6)
# pairwise
XYZ <- list(X, Y, Z)
# Manhattan
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = 1, q = 1)),
  c(3, 4, 1),
  tol = 1e-6
)
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = 2, q = 1)),
  c(sqrt(5), 2*sqrt(2), 1),
  tol = 1e-6
)
# expect_equal(
#   as.vector(wasserstein_pairwise_distances(XYZ, p = Inf, q = 1)),
#   c(2, 2, 1),
#   tol = 1e-1
# )
expect_error(
  as.vector(wasserstein_pairwise_distances(XYZ, p = Inf, q = 1)),
  "q-bottleneck distances (`q < Inf`) are not yet supported."
)
# Pythagorean
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = 1, q = 2)),
  c(1+sqrt(2), 2*sqrt(2), 1/sqrt(2)),
  tol = 1e-6
)
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = 2, q = 2)),
  c(sqrt(3), 2, 1/sqrt(2)),
  tol = 1e-6
)
# expect_equal(
#   as.vector(wasserstein_pairwise_distances(XYZ, p = Inf, q = 2)),
#   c(sqrt(2), sqrt(2), 1/sqrt(2)),
#   tol = 1e-1
# )
expect_error(
  as.vector(wasserstein_pairwise_distances(XYZ, p = Inf, q = 2)),
  "q-bottleneck distances (`q < Inf`) are not yet supported."
)
# supremum
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = 1, q = Inf)),
  c(2, 2, 1/2),
  tol = 1e-6
)
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = 2, q = Inf)),
  c(sqrt(2), sqrt(2), 1/2),
  tol = 1e-6
)
expect_equal(
  as.vector(wasserstein_pairwise_distances(XYZ, p = Inf, q = Inf)),
  c(1, 1, 1/2),
  tol = 1e-6
)
