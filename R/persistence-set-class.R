#' An 'S3' class object for storing sets of persistence diagrams
#'
#' An 'S3' class object for storing sets of persistence diagrams
#'
#' @param x A list of objects of class [persistence].
#'
#' @returns An object of class 'persistence_set' containing the set of
#'   persistence diagrams.
#'
#' @name persistence-set
#' @examples
#' # Create a persistence set from a list of persistence diagrams
#' as_persistence_set(persistence_sample[1:10])
NULL

#' @rdname persistence-set
#' @export
as_persistence_set <- function(x) {
  if (!is.list(x)) {
    cli::cli_abort("The input must be a list.")
  }

  if (length(x) == 0L) {
    cli::cli_abort("The list is empty.")
  }

  # Check if all elements are of class 'persistence'
  if (!all(sapply(x, inherits, "persistence"))) {
    cli::cli_abort("All elements of the list must be of class 'persistence'.")
  }

  class(x) <- c("persistence_set", class(x))
  x
}

#' @rdname persistence-set
#' @export
format.persistence_set <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("A set of persistence diagrams with {.val {length(x)}} elements.")
    cli::cli_text("Each element is a persistence diagram of class {.cls persistence}.")
    cli::cli_text("The diagrams are stored in the list as 2-column matrices.")
  })
}

#' @rdname persistence-set
#' @export
print.persistence_set <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
