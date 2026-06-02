#' Add an index file to a Quarto report or website
#'
#' @description
#' This function adds an index file to a Quarto report or website.
#' @inheritParams add_recommendations
#' @return The name of the index file.
#' @export
#' @importFrom assertthat assert_that is.string has_name noNA
#' @importFrom fs is_dir is_file path
#' @importFrom yaml read_yaml write_yaml
add_index <- function(report_path = ".") {
  assert_that(is.string(report_path), noNA(report_path), is_dir(report_path))
  target <- path(report_path, "_quarto.yml")
  stopifnot("no `_quarto.yml` found at `report_path`" = is_file(target))
  yaml <- read_yaml(target)
  type <-
    c("website"[has_name(yaml, "website")], "book"[has_name(yaml, "book")])
  stopifnot(
    "No `book/website` entry in `_quarto.yml`" = length(type) == 1,
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

  title <- c(
    `nl-BE` = "Samenvatting",
    `en-GB` = "Abstract",
    `fr-FR` = "R\u00e9sum\u00e9"
  )
  bib_file <- c(
    `nl-BE` = "referenties.md",
    `en-GB` = "bibliography.md",
    `fr-FR` = "bibliographie.md"
  )

  cover <- !has_name(yaml$website, "page-footer")

  c(
    "---",
    "toc: false",
    "---",
    "",
    "{{< colophon >}}"[cover],
    ""[cover],
    sprintf("# %s {-}", title[lang]),
    "",
    "<!-- description: start -->",
    "This section provides a brief summary of the report."[cover],
    "It should give the reader an overview of the main findings and"[cover],
    "conclusions."[cover],
    "This section can serve as the homepage of the website."[!cover],
    "It could give the reader an introduction, overview or summary."[!cover],
    "<!-- description: end -->",
    "",
    "**TO DO**",
    "",
    "Dummy reference to [@R].",
    "Only remove after including other references or after removing the",
    sprintf("bibliography file %s.", bib_file[lang])
  ) |>
    writeLines(con = path(report_path, "index.md"))
  if (type == "book") {
    if (
      !has_name(yaml$book, "chapters") || !"index.md" %in% yaml$book$chapters
    ) {
      yaml$book$chapters <- c("index.md", yaml$book$chapters)
    }
  } else {
    if (
      !has_name(yaml$website, "sidebar") ||
        !has_name(yaml$website$sidebar, "contents") ||
        !"index.md" %in% yaml$website$sidebar$contents[[1]]
    ) {
      yaml$website$sidebar$contents <- c(
        list(list(text = c("Cover"[cover], "Home"[!cover]), file = "index.md")),
        yaml$website$sidebar$contents
      )
    }
  }
  append_navbar(
    yaml, text = c("Cover"[cover], "Home"[!cover]), filename = "index.md"
  ) |>
    store_yaml(target = target)
  return("index.md")
}
