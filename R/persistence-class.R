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
#' @param warn A boolean specifying whether to issue a warning if the input
#'   persistence data contained unordered pairs. Defaults to `TRUE`.
#' @param ... Parameters passed to methods.
#' @param dimension A non-negative integer specifying the homology dimension for
#'   which to recover a matrix of persistence pairs.
#' @param birth A numeric value specifying the height at which to declare all
#'   leaves were born. Defaults to `0` if all heights are non-negative and
#'   `-Inf` otherwise.
#' @inheritParams base::as.data.frame

#' @returns An object of class [`persistence`] which is a list of 2 elements:
#'
#' - `pairs`: A list of 2-column matrices containing birth-death pairs. The
#' \eqn{i}-*th* element of the list corresponds to the \eqn{(i-1)}-*th* homology
#' dimension. If there is no pairs for a given dimension but there are pairs in
#' higher dimensions, the corresponding element(s) is/are filled with a
#' \eqn{0 \times 2} numeric matrix.
#'
#' - `metadata`: A list of length 6 containing information about how the data
#' was computed:
#'
#'   - `orderered_pairs`: A boolean indicating whether the pairs in the
#'   `pairs` list are ordered (i.e. the first column is strictly less than the
#'   second column).
#'   - `data`: The name of the object containing the original data on which the
#'   persistence data was computed.
#'   - `engine`: The name of the package and the function of this package that
#'   computed the persistence data in the form `"package_name::package_function"`.
#'   - `filtration`: The filtration used in the computation in a human-readable
#'   format (i.e. full names, capitals where need, etc.).
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
#'
#' # distances between cities
#' euroclust <- hclust(eurodist, method = "ward.D")
#' as_persistence(euroclust)
#'
#' # `hclust()` can accommodate negative distances
#' d <- as.dist(rbind(c(0, 3, -4), c(3, 0, 5), c(-4, 5, 0)))
#' hc <- hclust(d, method = "single")
#' ph <- as_persistence(hc, birth = -10)
#' get_pairs(ph, 0)
as_persistence <- function(x, warn = TRUE, ...) {
  UseMethod("as_persistence")
}

#' @rdname persistence
#' @export
as_persistence.list <- function(x, warn = TRUE, ...) {
  if (length(x) == 1L) {
    if ("diagram" %in% names(x)) {
      return(as_persistence(x$diagram, ...))
    }
  }

  pd <- list()

  # Handle persistence data stored in `x`

  if (length(x) == 0L) {
    cli::cli_abort("The list is empty.")
  }

  valid_elements <- sapply(
    x,
    function(.x) check_2column_matrix(.x, warn = warn)
  )

  pd$pairs <- x

  # Handle metadata

  pd$metadata <- list()
  pd$metadata$ordered_pairs <- all(valid_elements)

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
as_persistence.persistence <- function(x, warn = TRUE, ...) {
  x
}

#' @rdname persistence
#' @export
as_persistence.data.frame <- function(x, warn = TRUE, ...) {
  if (ncol(x) != 3L) {
    cli::cli_abort("The data frame must have 3 columns.")
  }
  if (!all(c("dimension", "birth", "death") %in% colnames(x))) {
    cli::cli_abort("The data frame must have columns named {.var dimension}, {.var birth} and {.var death}.")
  }
  x <- split_df_by_dimension(x)
  as_persistence(x, warn = warn, ...)
}

#' @rdname persistence
#' @export
as_persistence.matrix <- function(x, warn = TRUE, ...) {
  x <- as.data.frame(x)
  as_persistence(x, warn = warn, ...)
}

#' @rdname persistence
#' @export
as_persistence.diagram <- function(x, warn = TRUE, ...) {
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
  as_persistence.matrix(x, warn = warn, rlang::splice(params))
}

#' @rdname persistence
#' @export
as_persistence.PHom <- function(x, ...) {
  # coerce to matrix and reroute to default method
  # (will need to change if 'PHom' class changes)
  as_persistence.matrix(
    as.matrix(x),
    engine = "ripserr::<vietoris_rips/cubical>",
    filtration = "Vietoris-Rips/cubical",
    ...
  )
}

#' @rdname persistence
#' @export
as_persistence.hclust <- function(x, warn = TRUE, birth = NULL, ...) {

  if (is.null(birth)) {
    birth <- if (min(x$height) < 0) -Inf else 0
  }

  if (birth > max(x$height)) {
    cli::cli_abort(
      "The birth value ({birth}) must be less than
      the maximum height (max(x$height))."
    )
  }

  res <- list(cbind(birth = birth, death = c(x$height, Inf)))

  params <- list(
    engine = paste0("stats::", rlang::call_name(x$call)),
    filtration = paste(x$method, "linkage", sep = "-"),
    call = x$call
  )

  as_persistence.list(res, warn = warn, rlang::splice(params))
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
  filt_nm <- capitalize(x$metadata$filtration)

  cli::cli_format_method({
    cli::cli_h1("Persistence Data")
    cli::cli_alert_info('There are {npts} {cli::qty(max_npts)}pair{?s} in {cli::qty(ndim)}dimension{?s} {seq_len(ndim) - 1L} respectively.')
    if (filt_nm == "?" && x$metadata$engine == "?") {
      cli::cli_alert_warning("Both filtration and computation engine are unknown.")
    } else if (filt_nm == "?") {
      cli::cli_alert_info("Computed using {.fn {x$metadata$engine}}.")
      cli::cli_alert_warning("Filtration is unknown.")
    } else if (x$metadata$engine == "?") {
      cli::cli_alert_info("Computed from a {filt_nm} filtration.")
      cli::cli_alert_warning("Computation engine is unknown.")
    } else {
      cli::cli_alert_info("Computed from a {filt_nm} filtration using {.fn {x$metadata$engine}}.")
    }
    if (is.null(param_nms)) {
      cli::cli_alert_warning("With unknown parameters.")
    } else {
      cli::cli_alert_info("With the following parameters: {param_nms}.")
    }
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
