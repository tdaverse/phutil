## check_2column_matrix

using("tinysnapshot")

opts <- options(cli.width = 80)

expect_error(phutil:::check_2column_matrix(0))
M <- as.matrix(noisy_circle_ripserr)
expect_error(phutil:::check_2column_matrix(M))
M <- M[, 2:3]
M1 <- M
M1[1, 1] <- "a"
expect_error(phutil:::check_2column_matrix(M1))
M1 <- M
M1[1, 1] <- NA
expect_error(phutil:::check_2column_matrix(M1))
M1 <- M
M1[1, 1] <- 1
M1[1, 2] <- 0
expect_false(phutil:::check_2column_matrix(M1))
expect_snapshot_print(
  phutil:::check_2column_matrix(M1),
  label = "print-check-2column-matrix-warn"
)
expect_snapshot_print(
  phutil:::check_2column_matrix(M1, warn = FALSE),
  label = "print-check-2column-matrix-nowarn"
)
M1 <- M
M1[1, 1] <- -1
expect_true(phutil:::check_2column_matrix(M1))

options(opts)
