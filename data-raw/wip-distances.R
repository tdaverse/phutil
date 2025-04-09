x <- tdaunif::sample_2sphere(n = 100, sd = 0.05) |>
  TDA::ripsDiag(
    maxdimension = 1L,
    maxscale = 1.6322
  ) |>
  getElement("diagram") |>
  as_persistence()

y <- tdaunif::sample_2sphere(n = 100, sd = 0.05) |>
  TDA::ripsDiag(
    maxdimension = 1L,
    maxscale = 1.6322
  ) |>
  getElement("diagram") |>
  as_persistence()

phutil:::bottleneckDist(
  x = x$pairs[[1]],
  y = y$pairs[[1]],
  delta = 0.0
)

phutil:::bottleneckDist(
  x = x$pairs[[1]],
  y = y$pairs[[1]],
  delta = 0.01
)

phutil:::wassersteinDist(
  x = x$pairs[[1]],
  y = y$pairs[[1]]
)

phutil:::bottleneckDist(
  x = x$pairs[[2]],
  y = y$pairs[[2]],
  delta = 0.0
)

phutil:::bottleneckDist(
  x = x$pairs[[2]],
  y = y$pairs[[2]],
  delta = 0.01
)

phutil:::wassersteinDist(
  x = x$pairs[[2]],
  y = y$pairs[[2]]
)
