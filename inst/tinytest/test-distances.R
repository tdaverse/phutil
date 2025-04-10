x <- cbind(birth = c(1, 2), death = c(3, 4))
y <- cbind(birth = numeric(0), death = numeric(0))

expect_equal(bottleneck_distance(x, y), 1)
expect_equal(wasserstein_distance(x, y, p = 1), 2)
expect_equal(round(wasserstein_distance(x, y, p = 2), digits = 6L), 1.414214)
