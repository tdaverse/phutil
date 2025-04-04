#' An `S3` class for storing persistence data
#'
#' A collection of functions to coerce persistence data into objects of class [`persistence`] (See **Value** section for more details on this class). It is currently possible to coerce
#' persistence data from the following sources:
#' - a matrix with at least 3 columns (dimension/degree, start/birth, end/death)
#' as returned by [`ripserr::vietoris_rips()`](https://tdaverse.github.io/ripserr/reference/vietoris_rips.html)
#' in the form of the 'PHom' class,
#' - a list as returned by any `*Diag()` function in the **TDA** package.
#'
#' @name persistence
#'
#' @param x An `R` object containing the persistence data to be coerced into an
#'   object of class [`persistence`]. Currently supported forms are:
#'
#' - a \eqn{\geq 3}-column matrix (or object coercible to one) with
#' dimension/degree, start/birth and end/death columns,
#' - a list whose first element is such an object,
#' - an object of class 'PHom' as returned by [`ripserr::vietoris_rips()`](https://tdaverse.github.io/ripserr/reference/vietoris_rips.html),
#' - (a list as returned by a `*Diag()` function in **TDA**
#' (e.g. [`TDA::ripsDiag()`](https://www.rdocumentation.org/packages/TDA/versions/1.9.1/topics/ripsDiag))
#' whose first element is) an object of class 'diagram'.
#'
#' @param ... Parameters passed to methods.
#' @param dimension A non-negative integer specifying the homology dimension for
#'   which to recover a matrix of persistence pairs.
#' @inheritParams base::as.data.frame

#' @returns An object of class [`persistence`] which is a list of 2 elements:
#'
#' - `pairs`: A list of 2-column matrices containing birth-death pairs. The
#' \eqn{i}-*th* element of the list corresponds to the \eqn{(i-1)}-*th* homology
#' dimension. If there is no pairs for a given dimension but there are pairs in
#' higher dimensions, the corresponding element(s) is/are filled with a
#' \eqn{0 \times 2} numeric matrix.
#'
#' - `metadata`: A list of 3 elements containing information about how the data
#' was computed:
#'
#'   - `data`: The name of the object containing the original data on which the
#'   persistence data was computed.
#'   - `engine`: The name of the package that computed the persistence data.
#'   - `filtration`: The type of simplicial complex used in the
#'   computation.
#'   - `parameters`: A list of parameters used in the computation.
#'   - `call`: The exact call that generated the persistence data.
#'
#' @export
#' @examples
#' as_persistence(noisy_circle_ripserr)
#'
#' x <- as_persistence(noisy_circle_tda_rips)
#' x
#'
#' as_persistence(x)
#'
#' get_pairs(x, dimension = 1)
#'
#' as.data.frame(x)
as_persistence <- function(x, ...) {
  UseMethod("as_persistence")
}

#' @rdname persistence
#' @export
as_persistence.list <- function(x, ...) {
  pd <- list()

  # Handle persistence data stored in `x`

  if (length(x) == 0L) {
    cli::cli_abort("The list is empty.")
  }

  lapply(x, check_2d_matrix)

  pd$pairs <- x

  # Handle metadata

  pd$metadata <- list()

  dots <- rlang::list2(...)
  if ("data" %in% names(dots)) {
    pd$metadata$data <- dots$data
    dots$data <- NULL
  } else {
    pd$metadata$data <- "?"
  }
  if ("engine" %in% names(dots)) {
    pd$metadata$engine <- dots$engine
    dots$engine <- NULL
  } else {
    pd$metadata$engine <- "?"
  }
  if ("filtration" %in% names(dots)) {
    pd$metadata$filtration <- dots$filtration
    dots$filtration <- NULL
  } else {
    pd$metadata$filtration <- "?"
  }
  if ("call" %in% names(dots)) {
    pd$metadata$call <- dots$call
    dots$call <- NULL
  } else {
    pd$metadata$call <- "?"
  }
  if ("parameters" %in% names(dots)) {
    pd$metadata$parameters <- dots$parameters
  } else {
    pd$metadata$parameters <- dots
  }

  class(pd) <- "persistence"
  pd
}

#' @rdname persistence
#' @export
as_persistence.persistence <- function(x, ...) {
  x
}

#' @rdname persistence
#' @export
as_persistence.data.frame <- function(x, ...) {
  if (ncol(x) != 3L) {
    cli::cli_abort("The data frame must have 3 columns.")
  }
  if (!all(c("dimension", "birth", "death") %in% colnames(x))) {
    cli::cli_abort("The data frame must have columns named {.var dimension}, {.var birth} and {.var death}.")
  }
  x <- split_df_by_dimension(x)
  as_persistence(x, ...)
}

#' @rdname persistence
#' @export
as_persistence.matrix <- function(x, ...) {
  x <- as.data.frame(x)
  as_persistence(x, ...)
}

#' @rdname persistence
#' @export
as_persistence.diagram <- function(x, ...) {
  info <- attributes(x)
  filt_nm <- gsub("*Diag", "", rlang::call_name(info$call))
  if (filt_nm == "rips") {
    filt_nm <- "Vietoris-Rips"
  }
  params <- list(
    data = info$call$X,
    engine = paste0(rlang::call_ns(info$call), "::", rlang::call_name(info$call)),
    filtration = filt_nm,
    call = info$call
  )
  nms <- names(info$call)
  nms <- nms[nms != ""]
  nms <- nms[nms != "X"]
  if (length(nms) > 0L) {
    for (nm in nms) {
      params[[nm]] <- info$call[[nm]]
    }
  }

  dims <- dim(x)
  x <- as.matrix(x)[1:dims[1], 1:dims[2]]
  colnames(x) <- base::tolower(colnames(x))
  as_persistence.matrix(x, rlang::splice(params))
}

#' @rdname persistence
#' @export
as_persistence.PHom <- function(x, ...) {
  # coerce to matrix and reroute to default method
  # (will need to change if 'PHom' class changes)
  as_persistence.matrix(
    as.matrix(x),
    engine = "ripserr::vietoris_rips",
    filtration = "Vietoris-Rips",
    ...
  )
}

#' @rdname persistence
#' @export
print.persistence <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @rdname persistence
#' @export
format.persistence <- function(x, ...) {
  ndim <- length(x$pairs)
  npts <- sapply(x$pairs, nrow)
  max_npts <- max(npts)
  pad_size <- max(nchar(npts))
  param_vals <- x$metadata$parameters
  param_nms <- NULL
  if (!is.null(param_vals) && length(param_vals) > 0L) {
    param_nms <- names(param_vals)
    param_nms <- paste(param_nms, "=", param_vals)
  }

  cli::cli_format_method({
    cli::cli_h1("Persistence Data")

    cli::cli_alert_info('There are {npts} {cli::qty(max_npts)}pair{?s} in {cli::qty(ndim)}dimension{?s} {seq_len(ndim)} respectively.')

    cli::cli_h1("Metadata")
    filt_nm <- capitalize(x$metadata$filtration)
    if (filt_nm == "Rips")
      filt_nm <- "Vietoris-Rips"
    cli::cli_alert_info("Computed from a {filt_nm} filtration using the {.fn {x$metadata$engine}} engine")
    if (!is.null(param_nms))
      cli::cli_alert_info("with the following parameters: {param_nms}.")
  })
}

#' @rdname persistence
#' @export
get_pairs <- function(x, dimension, ...) {
  if (!inherits(x, "persistence")) {
    cli::cli_abort("Input must be an object of class {.cls persistence}.")
  }
  if (length(x$pairs) > dimension) {
    x$pairs[[dimension + 1L]]
  } else {
    matrix(NA_real_, nrow = 0L, ncol = 2L)
  }
}

#' @rdname persistence
#' @export
as.data.frame.persistence <- function(x,
                                      row.names = NULL,
                                      optional = TRUE,
                                      ...) {
  npts <- sapply(x$pairs, nrow)
  dimension <- rep(seq_along(x$pairs) - 1L, npts)
  pairs <- Reduce(rbind, x$pairs)
  colnames(pairs) <- c("birth", "death")
  df <- data.frame(dimension, pairs)
  if (!is.null(row.names)) {
    rownames(df) <- row.names
  } else if (!optional) {
    id <- unlist(lapply(npts, seq))
    rownames(df) <- paste(dimension, id, sep = "-")
  }
  df
}
