using("tinysnapshot")

opts <- options(cli.width = 80)

expect_error(as_persistence(list()))

m <- as.matrix(noisy_circle_ripserr)

mat <- as_persistence(m)
expect_equal(mat$metadata$data, "?")
expect_equal(mat$metadata$engine, "?")
expect_equal(mat$metadata$filtration, "?")
expect_equal(mat$metadata$call, "?")
expect_equal(mat$metadata$parameters, list())
expect_snapshot_print(mat, label = "print-mat-persistence")

params <- list(maxdimension = 1L, maxscale = 1.6322)
mat <- as_persistence(m, engine = "ripserr::vietoris_rips", parameters = params)
expect_equal(mat$metadata$engine, "ripserr::vietoris_rips")
expect_equal(mat$metadata$parameters, params)
expect_snapshot_print(mat, label = "print-mat-persistence-engine")

mat <- as_persistence(m, filtration = "vietoris-rips")
expect_equal(mat$metadata$filtration, "vietoris-rips")
expect_snapshot_print(mat, label = "print-mat-persistence-filtration")

more_df <- noisy_circle_ripserr
more_df$morevar <- 1L
expect_true(inherits(as_persistence(more_df), "persistence"))

wrong_df <- noisy_circle_ripserr
wrong_df$dimension <- NULL
expect_error(as_persistence(wrong_df))

wrong_df <- noisy_circle_ripserr
colnames(wrong_df) <- c("x", "y", "z")
expect_error(as_persistence(wrong_df))

ripserr <- as_persistence(noisy_circle_ripserr)
expect_equal(sapply(ripserr$pairs, ncol), c(2L, 2L))
expect_equal(ripserr$metadata$ordered_pairs, TRUE)
expect_equal(as.character(ripserr$metadata$data), "?")
expect_equal(ripserr$metadata$engine, "ripserr::<vietoris_rips/cubical>")
expect_equal(ripserr$metadata$filtration, "Vietoris-Rips/cubical")
expect_equal(as.character(ripserr$metadata$call), "?")
expect_equal(length(ripserr$metadata$parameters), 0L)
expect_snapshot_print(ripserr, label = "print-ripserr-persistence")

tda_rips <- as_persistence(getElement(noisy_circle_tda_rips, "diagram"))
expect_equal(sapply(tda_rips$pairs, ncol), c(2L, 2L))

tda_rips <- as_persistence(noisy_circle_tda_rips)
expect_equal(sapply(tda_rips$pairs, ncol), c(2L, 2L))
expect_equal(as.character(tda_rips$metadata$data), "noisy_circle_points")
expect_equal(tda_rips$metadata$engine, "TDA::ripsDiag")
expect_equal(tda_rips$metadata$filtration, "Vietoris-Rips")
expect_equal(
  as.character(tda_rips$metadata$call),
  c("TDA::ripsDiag", "noisy_circle_points", "1", "1.6322")
)
expect_equal(length(tda_rips$metadata$parameters), 2L)
expect_equal(names(tda_rips$metadata$parameters), c("maxdimension", "maxscale"))
expect_equal(tda_rips$metadata$parameters$maxdimension, 1L)
expect_equal(tda_rips$metadata$parameters$maxscale, 1.6322)
expect_snapshot_print(
  as_persistence(tda_rips),
  label = "print-tda-rips-persistence"
)

expect_equal(ncol(get_pairs(tda_rips, dimension = 1)), 2L)
expect_error(get_pairs(m, dimension = 1))
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

d <- dist(cbind(x = c(0, 3, 0), y = c(0, 0, 4)))
hc <- hclust(d, method = "complete")
hclust <- as_persistence(hc)
expect_equal(hclust$pairs[[1]][, 1], rep(0, 3L))
expect_snapshot_print(hclust, label = "print-hclust-persistence")
# negative distances
d2 <- d
d2[2] <- -d2[2]
hc2 <- hclust(d2, method = "single")
hclust2 <- as_persistence(hc2)
expect_equal(hclust2$pairs[[1]][, 1], rep(-Inf, 3L))

options(opts)
