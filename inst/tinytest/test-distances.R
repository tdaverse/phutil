x <- cbind(birth = c(1, 2), death = c(3, 1))
y <- cbind(birth = numeric(0), death = numeric(0))
expect_error(bottleneck_distance(x, y))
expect_error(bottleneck_distance(y, x))
expect_error(wasserstein_distance(x, y))
expect_error(wasserstein_distance(y, x))

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

out <- wasserstein_pairwise_distances(persistence_sample[1L:3L])
expect_equal(length(out), 3L)

wrong_sample <- persistence_sample[1L:3L]
wrong_sample[[1L]] <- cbind(birth = c(1, 2), death = c(3, 1))

expect_error(
  bottleneck_pairwise_distances(wrong_sample),
  "contains pairs with death prior to birth"
)

expect_error(
  wasserstein_pairwise_distances(wrong_sample),
  "contains pairs with death prior to birth"
)
