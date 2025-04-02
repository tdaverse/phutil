## check_2d_matrix

expect_error(phutil:::check_2d_matrix(0))
M <- as.matrix(noisy_circle_ripserr)
expect_error(phutil:::check_2d_matrix(M))
M <- M[, 2:3]
M1 <- M
M1[1, 1] <- "a"
expect_error(phutil:::check_2d_matrix(M1))
M1 <- M
M1[1, 1] <- NA
expect_error(phutil:::check_2d_matrix(M1))
M1 <- M
M1[1, 1] <- 1
M1[1, 2] <- 0
expect_error(phutil:::check_2d_matrix(M1))
M1 <- M
M1[1, 1] <- -1
expect_error(phutil:::check_2d_matrix(M1))
