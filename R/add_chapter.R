#' Add a chapter file to a Quarto report
#'
#' @description
#' This function adds a chapter file to a Quarto report.
#' @inheritParams add_recommendations
#' @param title The title of the chapter.
#' If missing, the chapter is assumed the introduction with a default title
#' based on the language.
#' @param filename The name of the chapter file.
#' If missing, the chapter is assumed the introduction with a default filename
#' based on the language.
#' @param toc A logical value indicating whether to add a local table of
#' contents.
#' Defaults to `TRUE`.
#' @return The name of the chapter file.
#' @export
#' @importFrom assertthat assert_that is.flag is.string has_name noNA
#' @importFrom fs is_dir is_file path
#' @importFrom utils head tail
#' @importFrom yaml read_yaml
add_chapter <- function(report_path, title, filename, toc = TRUE) {
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

  if (missing(title)) {
    stopifnot(
      "please provide both `title` and `filename` or neither" = missing(
        filename
      )
    )
    c(
      `nl-BE` = "inleiding.md",
      `en-GB` = "introduction.md",
      `fr-FR` = "introduction.md"
    )[lang] |>
      unname() -> filename
    c(
      `nl-BE` = "Inleiding",
      `en-GB` = "Introduction",
      `fr-FR` = "Introduction"
    )[lang] -> title
    toc <- FALSE
  } else {
    assert_that(
      is.string(title),
      noNA(title),
      is.string(filename),
      noNA(filename),
      is.flag(toc),
      noNA(toc)
    )
  }

  c(
    "---",
    paste("toc:", ifelse(toc, "true", "false")),
    "---",
    "",
    sprintf("# %s", title),
    "",
    "**TO DO**"
  ) |>
    writeLines(con = path(report_path, filename))
  if (!filename %in% yaml$book$chapters) {
    yaml$book$chapters <- c(yaml$book$chapters, filename)
  }
  append_navbar(yaml, text = title, filename = filename) |>
    store_yaml(target = target)
  return(filename)
}


#' @importFrom yaml read_yaml write_yaml
store_yaml <- function(yaml, target) {
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
}

#' @importFrom assertthat has_name
fix_affiliation <- function(yaml) {
  if (!has_name(yaml, "flandersqmd")) {
    return(yaml)
  }
  for (i in seq_along(yaml$flandersqmd$author)) {
    if (!has_name(yaml$flandersqmd$author[[i]], "affiliation")) {
      next
    }
    yaml$flandersqmd$author[[i]]$affiliation <- unlist(
      yaml$flandersqmd$author[[i]]$affiliation
    ) |>
      as.list()
  }
  for (i in seq_along(yaml$flandersqmd$reviewer)) {
    if (!has_name(yaml$flandersqmd$reviewer[[i]], "affiliation")) {
      next
    }
    yaml$flandersqmd$reviewer[[i]]$affiliation <- unlist(
      yaml$flandersqmd$reviewer[[i]]$affiliation
    ) |>
      as.list()
  }
  return(yaml)
}
