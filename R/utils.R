check_2d_matrix <- function(x) {
  if (!is.matrix(x)) {
    cli::cli_abort("Input must be a matrix.")
  }

  if (ncol(x) != 2L) {
    cli::cli_abort("Input must be a matrix with 2 columns.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("Input must be a matrix with numeric entries.")
  }

  if (anyNA(x)) {
    cli::cli_abort("Input must be a matrix with no missing values.")
  }

  if (any(x[, 1L] >= x[, 2L])) {
    cli::cli_abort("Birth values must be stricly less than death values.")
  }

  if (any(x[, 1L] < 0)) {
    cli::cli_abort("Birth values must be non-negative.")
  }
}

split_df_by_dimension <- function(x) {
  x <- base::split(x, x$dimension)
  names(x) <- NULL
  lapply(x, \(.x) {
    .x$dimension <- NULL
    .x <- as.matrix(.x)
    colnames(.x) <- NULL
    .x
  })
}

capitalize <- function(x) {
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl = TRUE)
}
