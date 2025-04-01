using("tinysnapshot")

opts <- options(cli.width = 80)

ripserr <- as_persistence(noisy_circle_ripserr)
expect_equal(sapply(ripserr$pairs, ncol), c(2L, 2L))
expect_equal(as.character(ripserr$metadata$data), "?")
expect_equal(ripserr$metadata$engine, "ripserr")
expect_equal(ripserr$metadata$simplicial_complex, "rips")
expect_equal(as.character(ripserr$metadata$call), "?")
expect_equal(length(ripserr$metadata$parameters), 0L)
expect_snapshot_print(ripserr, label = "print-ripserr-persistence")

tda_rips <- as_persistence(noisy_circle_tda_rips)
expect_equal(sapply(tda_rips$pairs, ncol), c(2L, 2L))
expect_equal(as.character(tda_rips$metadata$data), "noisy_circle_points")
expect_equal(tda_rips$metadata$engine, "TDA")
expect_equal(tda_rips$metadata$simplicial_complex, "rips")
expect_equal(as.character(tda_rips$metadata$call), c("TDA::ripsDiag", "noisy_circle_points", "1", "1.6322"))
expect_equal(length(tda_rips$metadata$parameters), 2L)
expect_equal(names(tda_rips$metadata$parameters), c("maxdimension", "maxscale"))
expect_equal(tda_rips$metadata$parameters$maxdimension, 1L)
expect_equal(tda_rips$metadata$parameters$maxscale, 1.6322)
expect_snapshot_print(as_persistence(tda_rips), label = "print-tda-rips-persistence")

expect_equal(ncol(get_pairs(tda_rips, dimension = 1)), 2L)
expect_equal(ncol(as.data.frame(tda_rips)), 3L)

options(opts)
