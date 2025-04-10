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
    phutil:::wassersteinPairwiseDistances(delta = 0.01, wasserstein_power = 1, ncores = 1L)
)

system.time(
  D <- persistence_sample |>
    map("pairs") |>
    map(1) |>
    phutil:::wassersteinPairwiseDistances(delta = 0.01, wasserstein_power = 1, ncores = 2L)
)
