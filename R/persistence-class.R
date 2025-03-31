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
#' @param degree Non-negative integer; the homology degree for which to recover
#'   a matrix of persistence pairs.
#' @param modulus,max_dim,threshold Possibly missing parameters of the
#'   calculation of the persistence data `x` to be included in the 'persistence'
#'   object.
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
#'   - `simplicial_complex`: The type of simplicial complex used in the
#'   computation.
#'   - `parameters`: A list of parameters used in the computation.
#'   - `call`: The exact call that generated the persistence data.
#'
#' @export
#' @examples
#' # TO DO
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

  sapply(x, check_2d_matrix)

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
  if ("simplicial_complex" %in% names(dots)) {
    pd$metadata$simplicial_complex <- dots$simplicial_complex
    dots$simplicial_complex <- NULL
  } else {
    pd$metadata$simplicial_complex <- "?"
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
as_persistence.matrix <- function(x, ...) {
  x <- split_matrix_by_dimension(x)
  as_persistence(x, ...)
}

#' @rdname persistence
#' @export
as_persistence.diagram <- function(x, ...) {
  info <- attributes(x)
  params <- list(
    data = info$call$X,
    engine = rlang::call_ns(info$call),
    simplicial_complex = gsub("*Diag", "", rlang::call_name(info$call)),
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
  as_persistence.matrix(
    as.matrix(x)[1:dims[1], 1:dims[2]],
    rlang::splice(params)
  )
}

#' @rdname persistence
#' @export
as_persistence.PHom <- function(x, ...) {
  # coerce to matrix and reroute to default method
  # (will need to change if 'PHom' class changes)
  as_persistence.matrix(
    as.matrix(x),
    engine = "ripserr",
    simplicial_complex = "rips",
    ...
  )
}

#' @rdname persistence
#' @export
print.persistence <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @rdname persistence
#' @export
format.persistence <- function(x, ...) {
  cli::cli_h1("Persistence Data")
  ndim <- length(x$pairs)
  npts <- sapply(x$pairs, nrow)
  for (i in seq_len(ndim)) {
    cli::cli_alert_info("There are {npts[i]} pairs in dimension {i - 1}.")
  }

  cli::cli_h1("Metadata")
  cli::cli_alert_info("Persistence has been computed from data stored in object {.field {x$metadata$data}}")
  cli::cli_alert_info("Persistence has been computed using the {.pkg {x$metadata$engine}} package.")
  cli::cli_alert_info("The simplicial complex used in the computation is {.field {x$metadata$simplicial_complex}}.")
  cli::cli_alert_info("The parameters used in the computation are:")
  cli::cli_bullets("{x$metadata$parameters}")
}

#' @rdname persistence
#' @export
get_pairs <- function(x, degree, ...) {
  stopifnot(inherits(x, "persistence"))
  if (length(x$pairs) > degree) {
    x$pairs[[degree + 1L]]
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
  features <- vapply(x$pairs, nrow, 0L)
  degree <- rep(seq_along(x$pairs) - 1L, features)
  pairs <- Reduce(rbind, x$pairs)
  df <- data.frame(degree, pairs)
  names(df) <- c("degree", "birth", "death")
  if (!is.null(row.names)) {
    rownames(df) <- row.names
  } else if (!optional) {
    id <- unlist(lapply(features, seq))
    rownames(df) <- paste(degree, id, sep = ".")
  }
  df
}
