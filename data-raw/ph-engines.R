# https://github.com/tdaverse/phutil/issues/6

require(readODS)
require(tidyverse)

# read in spreadsheet
read_ods("data-raw/ph-engines.ods") |>
  rename_with(\(s) str_replace_all(s, " ", "_"), everything()) |>
  mutate(filtration = str_replace(filtration, "–", "-")) |>
  mutate(data_structures = str_split(data_structures, "\n")) |>
  unnest(data_structures) |> rename(data_structure = data_structures) |>
  print() -> ph_engines

# illustrate usage: available options
engine_options <- function(
    object = NULL,
    filtration = NULL,
    data_structure = NULL
) {

  ph_options <- ph_engines
  if (is.null(object) && is.null(filtration) && is.null(data_structure)) {
    stop("No engine specifications provided.")
  }

  if (! is.null(object)) {
    object <-
      match.arg(object, unique(ph_engines$object))
    ph_options <-
      ph_options[ph_options$object == object, , drop = FALSE]
  }
  if (! is.null(filtration)) {
    filtration <-
      match.arg(filtration, unique(c(ph_engines$filtration, "Rips")))
    if (filtration == "Rips") filtration <- "Vietoris-Rips"
    ph_options <-
      ph_options[ph_options$filtration == filtration, , drop = FALSE]
  }
  if (! is.null(data_structure)) {
    data_structure <-
      match.arg(data_structure, unique(ph_engines$data_structure))
    ph_options <-
      ph_options[ph_options$data_structure == data_structure, , drop = FALSE]
  }

  ph_options <- unique(ph_options[, c("package", "function")])
  message(
    "For the following specification:\n",
    "* object:         ",
    if (is.null(object)) "<unspecified>" else object, "\n",
    "* filtration:     ",
    if (is.null(filtration)) "<unspecified>" else filtration, "\n",
    "* data_structure: ",
    if (is.null(data_structure)) "<unspecified>" else data_structure, "\n",
    "the following engines are available:\n",
    sapply(
      seq(nrow(ph_options)),
      \(r) paste0(
        "* `", ph_options$`function`[[r]], "` from {",
        ph_options$package[[r]], "}\n"
      )
    )
  )
}
engine_options(filtration = "Vietoris-Rips", data_structure = "distance matrix")
engine_options(object = "lattice")
