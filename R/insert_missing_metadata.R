#' Insert missing metadata in `_quarto.yml`
#' @inheritParams add_recommendations
#' @export
#' @importFrom assertthat assert_that has_name is.string noNA
#' @importFrom fs is_dir is_file path
#' @importFrom stats setNames
#' @importFrom yaml read_yaml write_yaml
insert_missing_metadata <- function(report_path = ".") {
  assert_that(is.string(report_path), noNA(report_path), is_dir(report_path))
  target <- path(report_path, "_quarto.yml")
  stopifnot("no `_quarto.yml` found at `report_path`" = is_file(target))
  yaml <- read_yaml(target)
  stopifnot(
    "No `flandersqmd` entry in `_quarto.yml`" = has_name(yaml, "flandersqmd")
  )
  metadata <- data.frame(
    variable = c(
      "year",
      "reportnr",
      "depotnr",
      "ordernr",
      "doi",
      "watermark",
      "coverphoto",
      "coverdescription"
    ),
    description = c(
      "Publication year of the report",
      "Report number",
      "Depot number",
      "Order number",
      "DOI of the report",
      "Watermark text",
      "URL or file path to the cover photo",
      "description of the cover photo"
    )
  )
  metadata <- metadata[!metadata$variable %in% names(yaml$flandersqmd), ]
  sprintf("%s (leave empty if you don't need it): ", metadata$description) |>
    vapply(readline, character(1)) -> metadata$answer
  metadata <- metadata[metadata$answer != "", ]
  yaml$flandersqmd <- c(
    yaml$flandersqmd,
    as.list(metadata$answer) |>
      setNames(metadata$variable)
  )
  store_yaml(yaml, target = target)
  return(target)
}
