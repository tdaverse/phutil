system.time(
  D <- persistence_sample |>
    map("pairs") |>
    map(1) |>
    phutil:::bottleneckPairwiseDistances(delta = 0.01, ncores = 1L)
)

system.time(
  D <- persistence_sample |>
    map("pairs") |>
    map(1) |>
    phutil:::bottleneckPairwiseDistances(delta = 0.01, ncores = 2L)
)

system.time(
  D <- persistence_sample |>
    map("pairs") |>
    map(1) |>
    phutil:::wassersteinPairwiseDistances(
      delta = 0.01,
      wasserstein_power = 1,
      ncores = 1L
    )
)

system.time(
  D <- persistence_sample |>
    map("pairs") |>
    map(1) |>
    phutil:::wassersteinPairwiseDistances(
      delta = 0.01,
      wasserstein_power = 1,
      ncores = 2L
    )
)

x <- cbind(birth = c(1, 2), death = c(3, 4))
y <- cbind(birth = numeric(0), death = numeric(0))

bottleneck_distance(x, y)
wasserstein_distance(x, y, p = 1)
wasserstein_distance(x, y, p = 2)

xx <- cbind(dimension = rep(0, 2), birth = c(1, 2), death = c(3, 4))
yy <- cbind(dimension = 0L, birth = 1, death = 1)
TDA::bottleneck(xx, yy, dimension = 0)
TDA::wasserstein(xx, yy, p = 1, dimension = 0)
TDA::wasserstein(xx, yy, p = 2, dimension = 0)
