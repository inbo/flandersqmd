#' Add a bibliography file to a Quarto report
#'
#' @description
#' This function adds a bibliography file to a Quarto report.
#' @inheritParams add_recommendations
#' @return The name of the bibliography file.
#' @export
#' @importFrom assertthat assert_that is.string has_name noNA
#' @importFrom fs is_dir is_file file_exists path
#' @importFrom utils head tail
#' @importFrom yaml read_yaml write_yaml
add_bibliography <- function(report_path) {
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

  if (!has_name(yaml, "bibliography")) {
    yaml$bibliography <- "references.bib"
  }
  if (!file_exists(yaml$bibliography)) {
    citation() |>
      toBibtex() |>
      gsub(pattern = "Manual\\{,", replacement = "Manual{R,") |>
      writeLines(con = path(report_path, "references.bib"))
  }

  filename <- c(
    `nl-BE` = "referenties.md",
    `en-GB` = "bibliography.md",
    `fr-FR` = "bibliographie.md"
  )[lang]
  title <- c(
    `nl-BE` = "Referenties",
    `en-GB` = "Bibliography",
    `fr-FR` = "Bibliographie"
  )[lang]
  c(
    "---",
    "toc: false",
    "---",
    "",
    sprintf("# %s {-}", title),
    "",
    "::: {#refs}",
    ":::"
  ) |>
    writeLines(con = path(report_path, filename))
  if (filename %in% yaml$book$chapters) {
    return(filename)
  }
  yaml$book$chapters <- c(yaml$book$chapters, filename)
  fix_affiliation(yaml) |>
    write_yaml(
      file = target,
      handlers = c(
        "logical" = function(x) {
          attr(x, "class") <- "verbatim"
          ifelse(x, "true", "false")
        }
      )
    )
  return(filename)
}
