#' Create a template for a `flandersqmd` report
#'
#' @param path The folder in which to create the folder containing the report.
#'  Defaults to the current working directory.
#'  It also creates an RStudio project file in the report folder.
#'  When ran from RStudio, the project will be opened automatically in a new
#'  session.
#' @param reportname The folder name of the report.
#' The location of the folder `reportname` depends on the content of `path`.
#' When `path` is a `checklist::checklist` project, you will find the new report
#' at `path/source/reportname`.
#' When `path` is a `checklist::checklist` package, you will find the new report
#' at `path/inst/reportname`.
#' Otherwise you will find the new report at `path/reportname`.
#' @param version The version of the `flandersqmd-book` extension to use.
#' Defaults to `"main"`, which refers to the current version.
#' @param shortname Deprecated.
#' Use `reportname` instead.
#' @family utils
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom checklist ask_yes_no menu_first read_checklist use_author
#' @importFrom fs dir_create is_dir is_file path
#' @importFrom gert git_find
#' @importFrom quarto quarto_add_extension
#' @importFrom utils citation toBibtex
create_report <- function(path = ".", reportname, version = "main", shortname) {
  if (missing(reportname) && !missing(shortname)) {
    warning(
      "`shortname` is deprecated, use `reportname` instead.",
      call. = FALSE,
      immediate. = TRUE
    )
    reportname <- shortname
  }
  assert_that(is.string(path), noNA(path), is_dir(path))
  assert_that(is.string(reportname), noNA(reportname))
  assert_that(is.string(version), noNA(version))
  assert_that(
    grepl("^[a-z0-9_]+$", reportname),
    msg = paste(
      "The report name folder may only contain lower case letters, digits and _"
    )
  )
  x <- try(read_checklist(path), silent = TRUE)
  if (inherits(x, "checklist")) {
    path <- path(x$get_path, ifelse(x$package, "inst", "source"))
    dir_create(path)
    output_dir <- path("..", "..", "output", reportname)
  } else {
    output_dir <- path("output", reportname)
  }
  path <- normalizePath(path, mustWork = TRUE)

  stopifnot(
    "The report name folder already exists." = !is_dir(path(path, reportname))
  )

  # ask required information
  lang <- c(`nl-BE` = "Dutch", `en-GB` = "English", `fr-FR` = "French")
  lang <- names(lang)[
    menu_first(lang, title = "What is the main language of the report?")
  ]
  level <- c(`2` = "INBO", `1` = "Flanders")
  selected_level <- menu_first(
    level,
    title = "Which type of corporate identity?"
  )
  readline(prompt = "Enter the title: ") |>
    gsub(pattern = "[\"|']", replacement = "") |>
    sprintf(fmt = "  title: \"%s\"") -> title
  readline(
    prompt = "Enter the optional subtitle (leave empty to omit): "
  ) |>
    gsub(pattern = "[\"|']", replacement = "") -> subtitle
  while (TRUE) {
    short <- readline(
      prompt = "Enter the filename (without extension) used for the output: "
    )
    if (grepl("^[a-z0-9-]+$", short)) {
      break
    }
    warning(
      "The filename may only contain lower case letters, digits and -",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  lof <- ask_yes_no("Add a list of figures?", default = FALSE)
  lot <- ask_yes_no("Add a list of tables?", default = FALSE)
  authors <- insert_author_reviewer(lang = lang)

  # build new yaml
  c(
    "project:",
    "  type: book",
    "  preview:",
    "    port: 4201",
    "    browser: true",
    "  render:",
    "    - \"*.md\"",
    "    - \"*.qmd\"",
    "    - \"!LICENSE.md\" #ignore LICENSE.md",
    "    - \"!README.md\" #ignore README.md",
    paste("  output-dir:", output_dir),
    "  post-render: _extensions/inbo/flandersqmd-book/filters/post_render.R",
    "",
    "execute:",
    "  echo: false",
    "  warnings: true",
    "  errors: true",
    "  message: true",
    "  freeze: false",
    "  cache: false",
    "",
    "format:",
    "  flandersqmd-book-html: default",
    "  flandersqmd-book-pdf: default",
    "",
    sprintf("lang: %s", lang),
    "",
    "flandersqmd:",
    "  entity: INBO",
    sprintf("  level: %s", names(level)[selected_level]),
    title,
    sprintf("  subtitle: \"%s\"", subtitle)[subtitle != ""],
    sprintf("  shorttitle: %s", short),
    authors,
    add_address("client"),
    add_address("cooperation"),
    "  public_report: true",
    "  colophon: true",
    "  floatbarrier: section",
    "book:",
    "  downloads: pdf",
    "  open-graph: true",
    "  body-footer: '{{< footer >}}'",
    "  navbar:",
    "    pinned: true",
    "    right:",
    "    - icon: mastodon",
    "      href: https://mastodon.online/&#64;inbo",
    "    - icon: bluesky",
    "      href: https://bsky.app/profile/inbo.be",
    "    - icon: facebook",
    "      href: https://www.facebook.com/INBOVlaanderen/"
  ) -> yaml

  dir_create(path(path, reportname))
  writeLines(yaml, path(path, reportname, "_quarto.yml"))
  writeLines(
    text = c(
      "Version: 1.0",
      "",
      "RestoreWorkspace: No",
      "SaveWorkspace: No",
      "AlwaysSaveHistory: No",
      "",
      "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes",
      "NumSpacesForTab: 2",
      "Encoding: UTF-8",
      "",
      "RnwWeave: knitr",
      "LaTeX: XeLaTeX",
      "",
      "AutoAppendNewline: Yes",
      "StripTrailingWhitespace: Yes",
      "LineEndingConversion: Posix"
    ),
    con = path(path, reportname, reportname, ext = "Rproj")
  )
  add_index(path(path, reportname))
  add_abstract(path(path, reportname))
  add_recommendations(path(path, reportname), lof = lof, lot = lot)
  add_chapter(path(path, reportname))
  add_bibliography(path(path, reportname))
  c(
    "/\\.quarto",
    "/\\.Rproj.user",
    "/*\\.eps",
    "/*\\.pdf",
    "/*\\.sty",
    "/*\\.tex",
    "output",
    "site_libs"
  ) |>
    writeLines(path(path, reportname, ".gitignore"))

  old_wd <- getwd()
  setwd(path(path, reportname))
  paste0("inbo/flandersqmd-book@", version) |>
    quarto_add_extension(no_prompt = TRUE)
  setwd(old_wd)
  if (
    !requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()
  ) {
    return(invisible(NULL))
  }
  rstudioapi::openProject(path(path, reportname), newSession = TRUE)
}

#' @importFrom checklist ask_yes_no
insert_author_reviewer <- function(lang) {
  cat("Please select the corresponding author")
  authors <- use_author(lang = lang)
  c("  author:", author2yaml(authors, corresponding = TRUE)) -> yaml
  while (isTRUE(ask_yes_no("Add another author?", default = FALSE))) {
    author <- use_author(lang = lang)
    authors[, c("given", "family", "email")] |>
      rbind(author[, c("given", "family", "email")]) |>
      anyDuplicated() -> duplo
    if (duplo > 0) {
      warning(
        paste(author$given, author$family, "is already listed as author"),
        call. = FALSE,
        immediate. = TRUE
      )
      next
    }
    c(yaml, author2yaml(author, corresponding = FALSE)) -> yaml
    authors <- rbind(authors, author)
  }
  cat("Please select the reviewer")
  duplo <- 1
  while (duplo > 0) {
    author <- use_author(lang = lang)
    authors[, c("given", "family", "email")] |>
      rbind(author[, c("given", "family", "email")]) |>
      anyDuplicated() -> duplo
    if (duplo > 0) {
      warning(
        author$given,
        " ",
        author$family,
        "is already listed as author.",
        "\nPlease select someone else.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }
  c(yaml, "  reviewer:", author2yaml(author, corresponding = FALSE))
}
