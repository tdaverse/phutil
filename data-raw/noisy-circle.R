# Point cloud -------------------------------------------------------------

withr::with_seed(1234, {
  noisy_circle_points <- tdaunif::sample_circle(n = 100, sd = .05)
})
colnames(noisy_circle_points) <- c("x", "y")

usethis::use_data(noisy_circle_points, overwrite = TRUE)

# Persistence -------------------------------------------------------------

noisy_circle_persistence <- ripserr::vietoris_rips(noisy_circle_points, max_dim = 1L)

usethis::use_data(noisy_circle_persistence, overwrite = TRUE)
