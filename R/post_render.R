#' Post render function
#' @export
#' @importFrom quarto quarto_inspect
post_render <- function() {
  Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES") |>
    strsplit(split = "\n") |>
    unlist() -> candidates
  # fmt: skip
  if (length(candidates) == 0) {
    return(invisible(NULL))
  }
  yml <- quarto_inspect()
  # fmt: skip
  stopifnot(
    "no `flandersqmd` section found in `_quarto.yml`" =
      "flandersqmd" %in% names(yml$config),
    "no `shorttitle` item in the `flandersqmd` section found in `_quarto.yml`" =
      "shorttitle" %in% names(yml$config$flandersqmd)
  )
  cover_info(yml$config$flandersqmd)
  relevant <- candidates[grepl("\\.pdf$", candidates)]
  if (length(relevant) == 0) {
    return(invisible(NULL))
  }
  for (x in relevant) {
    dirname(x) |>
      file.path(
        paste0(gsub("-", "_", yml$config$flandersqmd$shorttitle), ".pdf")
      ) |>
      file.rename(from = x)
  }
  list.files(
    "_extensions",
    pattern = "_extension.yml",
    full.names = TRUE,
    recursive = TRUE
  ) |>
    grepv(pattern = "flandersqmd-book") |>
    read_yaml() -> resources
  c(
    resources$contributes$format$common[["format-resources"]],
    resources$contributes$format$html[["format-resources"]],
    resources$contributes$format$pdf[["format-resources"]]
  ) |>
    basename() |>
    file.remove()

  return(invisible(NULL))
}
