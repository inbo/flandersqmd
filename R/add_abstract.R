#' Add an abstract file to a Quarto report
#'
#' @description
#' This function adds an abstract file to a Quarto report.
#' @inheritParams add_recommendations
#' @return The name of the index file.
#' @export
#' @importFrom assertthat assert_that is.string has_name noNA
#' @importFrom fs is_dir is_file path
#' @importFrom utils head tail
#' @importFrom yaml read_yaml write_yaml
add_abstract <- function(report_path = ".") {
  assert_that(is.string(report_path), noNA(report_path), is_dir(report_path))
  target <- path(report_path, "_quarto.yml")
  stopifnot("no `_quarto.yml` found at `report_path`" = is_file(target))
  yaml <- read_yaml(target)
  stopifnot(
    "No `book` entry in `_quarto.yml`" = has_name(yaml, "book"),
    "No `chapters` entry under `book` in `_quarto.yml`" = has_name(
      yaml$book,
      "chapters"
    ),
    "No `lang` entry in `_quarto.yml`" = has_name(yaml, "lang")
  )
  lang <- yaml$lang
  assert_that(
    is.string(lang),
    noNA(lang),
    lang %in% c("nl-BE", "en-GB", "fr-FR"),
    msg = paste(
      "The language must be one of the following: `nl-BE`, `en-GB`, `fr-FR`"
    )
  )

  filename <- ifelse(lang != "nl-BE", "samenvatting.md", "abstract.md")
  c(
    "---",
    "toc: false",
    "---",
    "",
    sprintf("::: {lang=%s}", ifelse(lang != "nl-BE", "nl-BE", "en-GB")),
    "",
    sprintf("# %s {-}", ifelse(lang != "nl-BE", "Samenvatting", "Abstract")),
    "",
    "**TO DO**",
    "",
    ":::"
  ) |>
    writeLines(con = path(report_path, filename))
  if (filename %in% yaml$book$chapters) {
    return(filename)
  }
  yaml$book$chapters <- c(
    head(yaml$book$chapters, 1),
    filename,
    tail(yaml$book$chapters, -1)
  )
  store_yaml(yaml, target = target)
  return(filename)
}
