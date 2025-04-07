using("tinysnapshot")

opts <- options(cli.width = 80)

expect_error(as_persistence(list()))

M <- as.matrix(noisy_circle_ripserr)

mat <- as_persistence(M)
expect_equal(mat$metadata$data, "?")
expect_equal(mat$metadata$engine, "?")
expect_equal(mat$metadata$filtration, "?")
expect_equal(mat$metadata$call, "?")
expect_equal(mat$metadata$parameters, list())

params <- list(maxdimension = 1L, maxscale = 1.6322)
mat <- as_persistence(M, parameters = params)
expect_equal(mat$metadata$parameters, params)
expect_snapshot_print(mat, label = "print-mat-persistence")

wrong_df <- noisy_circle_ripserr
wrong_df$morevar <- 1L
expect_error(as_persistence(wrong_df))

wrong_df <- noisy_circle_ripserr
wrong_df$dimension <- NULL
expect_error(as_persistence(wrong_df))

wrong_df <- noisy_circle_ripserr
colnames(wrong_df) <- c("x", "y", "z")
expect_error(as_persistence(wrong_df))

ripserr <- as_persistence(noisy_circle_ripserr)
expect_equal(sapply(ripserr$pairs, ncol), c(2L, 2L))
expect_equal(as.character(ripserr$metadata$data), "?")
expect_equal(ripserr$metadata$engine, "ripserr::vietoris_rips")
expect_equal(ripserr$metadata$filtration, "Vietoris-Rips")
expect_equal(as.character(ripserr$metadata$call), "?")
expect_equal(length(ripserr$metadata$parameters), 0L)
expect_snapshot_print(ripserr, label = "print-ripserr-persistence")

tda_rips <- as_persistence(noisy_circle_tda_rips)
expect_equal(sapply(tda_rips$pairs, ncol), c(2L, 2L))
expect_equal(as.character(tda_rips$metadata$data), "noisy_circle_points")
expect_equal(tda_rips$metadata$engine, "TDA::ripsDiag")
expect_equal(tda_rips$metadata$filtration, "Vietoris-Rips")
expect_equal(as.character(tda_rips$metadata$call), c("TDA::ripsDiag", "noisy_circle_points", "1", "1.6322"))
expect_equal(length(tda_rips$metadata$parameters), 2L)
expect_equal(names(tda_rips$metadata$parameters), c("maxdimension", "maxscale"))
expect_equal(tda_rips$metadata$parameters$maxdimension, 1L)
expect_equal(tda_rips$metadata$parameters$maxscale, 1.6322)
expect_snapshot_print(as_persistence(tda_rips), label = "print-tda-rips-persistence")

expect_equal(ncol(get_pairs(tda_rips, dimension = 1)), 2L)
expect_error(get_pairs(M, dimension = 1))
expect_equal(nrow(get_pairs(tda_rips, dimension = 2)), 0L)
expect_equal(ncol(get_pairs(tda_rips, dimension = 2)), 2L)

expect_equal(ncol(as.data.frame(tda_rips)), 3L)
row_names <- c(paste("0", 1:100, sep = "-"), paste("1", 1:2, sep = "-"))
expect_equal(
  rownames(as.data.frame(tda_rips, optional = FALSE)),
  row_names
)
row_names <- paste0("row", 1:102)
expect_equal(
  rownames(as.data.frame(tda_rips, row.names = row_names)),
  row_names
)

options(opts)
