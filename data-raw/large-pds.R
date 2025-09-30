withr::with_seed(28415, {
  n <- 24
  trefoils <- lapply(seq(n), function(i) {
    S1 <- tdaunif::sample_trefoil(n = 120, sd = .05)
    as_persistence(TDA::ripsDiag(S1, maxdimension = 2, maxscale = 6))
  })
  arch_spirals <- lapply(seq(n), function(i) {
    S2 <- cbind(tdaunif::sample_arch_spiral(n = 120, arms = 2), 0)
    S2 <- tdaunif::add_noise(S2, sd = .05)
    as_persistence(TDA::ripsDiag(S2, maxdimension = 2, maxscale = 6))
  })
})

usethis::use_data(trefoils, overwrite = TRUE)
usethis::use_data(arch_spirals, overwrite = TRUE)
