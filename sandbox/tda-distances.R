# This example should show 0.5 for all distances
# It shows that TDA/Dionysus does not work as expected
library(phutil)

Z <- cbind(
  dimension = c(0, 1, 2),
  birth = c(0, 1, 2),
  death = c(5, 4, 3)
)
W <- cbind(
  dimension = c(0, 1),
  birth = c(0, 2),
  death = c(4, 3)
)

# distance from empty using phutil/Hera
wasserstein_distance(Z, W, p = 1, dimension = 2)
wasserstein_distance(Z, W, p = 2, dimension = 2)
bottleneck_distance(Z, W, dimension = 2)
# distance from empty using TDA/Dionysus
TDA::wasserstein(Z, W, p = 1, dimension = 2)
sqrt(TDA::wasserstein(Z, W, p = 2, dimension = 2))
TDA::bottleneck(Z, W, dimension = 2)
