withr::with_seed(1234, {
  persistence_sample <- lapply(1:100, function(i) {
    tdaunif::sample_2sphere(n = 100, sd = 0.05) |>
      TDA::ripsDiag(maxdimension = 1L, maxscale = 1.6322) |>
      getElement("diagram") |>
      as_persistence()
  })
})

usethis::use_data(persistence_sample, overwrite = TRUE)
