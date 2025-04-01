# Point cloud -------------------------------------------------------------

withr::with_seed(1234, {
  noisy_circle_points <- tdaunif::sample_circle(n = 100, sd = .05)
})
colnames(noisy_circle_points) <- c("x", "y")

usethis::use_data(noisy_circle_points, overwrite = TRUE)

# Persistence -------------------------------------------------------------

noisy_circle_ripserr <- ripserr::vietoris_rips(noisy_circle_points, max_dim = 1L)

usethis::use_data(noisy_circle_ripserr, overwrite = TRUE)

noisy_circle_tda_rips <- TDA::ripsDiag(noisy_circle_points, maxdimension = 1L, maxscale = 1.6322)$diagram

usethis::use_data(noisy_circle_tda_rips, overwrite = TRUE)
