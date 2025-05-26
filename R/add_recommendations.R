#' Add a recommendations section to a `flandersqmd` report
#'
#' @description
#' This function adds a recommendations section to a `flandersqmd` report.
#' The file also add a table of contents, a list of figures and a list of
#' tables to the pdf version of the report.
#' @param report_path The path to the folder containing the report.
#' @param lof A logical value indicating whether to add a list of figures.
#' Defaults to `TRUE`.
#' If `TRUE`, a list of figures is added to the pdf version of the report.
#' @param lot A logical value indicating whether to add a list of tables.
#' Defaults to `TRUE`.
#' If `TRUE`, a list of tables is added to the pdf version of the report.
#' @export
#' @importFrom assertthat assert_that is.flag is.string has_name noNA
#' @importFrom fs is_dir is_file path
#' @importFrom utils head tail
#' @importFrom yaml read_yaml write_yaml
add_recommendations <- function(
  report_path,
  lof = TRUE,
  lot = TRUE
) {
  assert_that(is.string(report_path), noNA(report_path), is_dir(report_path))
  assert_that(
    is.flag(lof),
    noNA(lof)
  )
  assert_that(
    is.flag(lot),
    noNA(lot)
  )
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

  chapter_title <- c(
    `nl-BE` = "# Aanbevelingen voor het beleid {-}",
    `en-GB` = "# Recommendations for management and / or policy {-}",
    `fr-FR` = "# Recommandations pour la gestion et / ou la politique {-}"
  )[lang]
  c(
    "---",
    "toc: false",
    "---",
    "",
    chapter_title,
    "",
    "**TO DO**",
    rep("", 5),
    "<!-- This part adds the table of content in the pdf -->",
    "<!-- Add it at the end of the last chapter of the frontmatter -->",
    "<!-- spell-check: ignore:start-->",
    "::: {.content-visible when-format=\"pdf\"}",
    "\\clearpage",
    "\\phantomsection",
    "\\addcontentsline{toc}{chapter}{\\contentsname}",
    "\\setcounter{tocdepth}{2}",
    "\\tableofcontents"
  ) -> md
  if (lot || lof) {
    c(
      md,
      "",
      paste(
        "\\clearpage <!-- remove this line when you have neither a list of",
        "figures and a list of tables -->"
      )
    ) -> md
    if (lof) {
      c(
        md,
        "",
        "<!-- remove this block when you don't want a list of figures -->",
        "\\phantomsection",
        "\\addcontentsline{toc}{chapter}{\\listfigurename}",
        "\\listoffigures",
        "\\vspace{34pt}"[lot],
        "<!-- remove this block when you don't want a list of figures -->"
      ) -> md
    }
    if (lot) {
      c(
        md,
        "",
        "<!-- remove this block when you don't want a list of tables -->",
        "\\phantomsection",
        "\\addcontentsline{toc}{chapter}{\\listtablename}",
        "\\listoftables",
        "<!-- remove this block when you don't want a list of tables -->"
      ) -> md
    }
  }
  c(
    md,
    "",
    "<!-- keep the lines below -->",
    ":::",
    "<!-- spell-check: ignore:end-->",
    "<!-- This part adds the tables of contents in the pdf -->"
  ) -> md
  c(
    `nl-BE` = "aanbevelingen.md",
    `en-GB` = "recommendations.md",
    `fr-FR` = "recommandations.md"
  )[lang] |>
    unname() -> filename
  writeLines(md, con = path(report_path, filename))
  if (!filename %in% yaml$book$chapters) {
    yaml$book$chapters <- c(
      head(yaml$book$chapters, 2),
      filename,
      tail(yaml$book$chapters, -2)
    )
  }
  yaml <- append_navbar(
    yaml,
    text = c(
      `nl-BE` = "Aanbevelingen",
      `en-GB` = "Recommendations",
      `fr-FR` = "Recommandations"
    )[lang],
    filename = filename
  )
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
