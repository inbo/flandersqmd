#' Create a template for a `flandersqmd` report
#'
#' @param path The folder in which to create the folder containing the report.
#'  Defaults to the current working directory.
#' @param shortname The name of the report project.
#' The location of the folder `shortname` depends on the content of `path`.
#' When `path` is a subfolder of a git repository, it is changed to the root
#' of that git repository.
#' When `path` is a `checklist::checklist` project, you will find the new report
#' at `path/source/shortname`.
#' When `path` is a `checklist::checklist` package, you will find the new report
#' at `path/inst/shortname`.
#' Otherwise you will find the new report at `path/shortname`.
#' @param version The version of the `flandersqmd-book` extension to use.
#' Defaults to `"main"`, which refers to the current version.
#' @family utils
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom checklist ask_yes_no menu_first read_checklist use_author
#' @importFrom fs dir_create is_dir is_file path
#' @importFrom gert git_find
#' @importFrom quarto quarto_add_extension quarto_render
#' @importFrom utils citation toBibtex
create_report <- function(path = ".", shortname, version = "main") {
  assert_that(is.string(path), noNA(path), is_dir(path))
  assert_that(is.string(shortname), noNA(shortname))
  assert_that(is.string(version), noNA(version))
  assert_that(
    grepl("^[a-z0-9_]+$", shortname),
    msg = paste(
      "The report name folder may only contain lower case letters, digits and _"
    )
  )
  root <- try(git_find(path), silent = TRUE)
  path <- ifelse(inherits(root, "try-error"), path, root)
  if (is_file(path(path, "checklist.yml"))) {
    x <- read_checklist(path)
    path <- path(path, ifelse(x$package, "inst", "source"))
    dir_create(path)
    output_dir <- "../../output"
  } else {
    output_dir <- "output"
  }

  assert_that(
    !is_dir(path(path, shortname)),
    msg = "The report name folder already exists."
  )

  # build new yaml
  lang <- c(`nl-BE` = "Dutch", `en-GB` = "English", `fr-FR` = "French")
  lang <- names(lang)[
    menu_first(lang, title = "What is the main language of the report?")
  ]
  level <- c(`2` = "entity", `1` = "Flanders")
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
    "  post-render: _extensions/inbo/flandersqmd-book/filters/rename.R",
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
    sprintf(
      "  level: %s",
      names(level)[
        menu_first(level, title = "Which type of corporate identity?")
      ]
    )
  ) -> yaml
  readline(prompt = "Enter the title: ") |>
    gsub(pattern = "[\"|']", replacement = "") |>
    sprintf(fmt = "  title: \"%s\"") -> title
  readline(
    prompt = "Enter the optional subtitle (leave empty to omit): "
  ) |>
    gsub(pattern = "[\"|']", replacement = "") -> subtitle
  while (TRUE) {
    short <- readline(prompt = "Enter the short title used for the filename: ")
    if (grepl("^[a-z0-9-]+$", short)) {
      break
    }
    cat("The short title may only contain lower case letters, digits and -")
  }
  c(
    yaml,
    title,
    sprintf("  subtitle: \"%s\"", subtitle)[subtitle != ""],
    sprintf("  shorttitle: %s", short)
  ) -> yaml
  c(
    insert_author_reviewer(yaml),
    add_address("client"),
    add_address("cooperation"),
    "  public_report: true",
    "  colophon: true",
    "  floatbarrier: section",
    "book:",
    "  downloads: pdf",
    "  open-graph: true",
    "  sidebar:",
    "    logo: media/cover.png",
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

  dir_create(path(path, shortname))
  writeLines(yaml, path(path, shortname, "_quarto.yml"))
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
      "LineEndingConversion: Posix",
      "",
      "",
      "MarkdownWrap: Sentence",
      "MarkdownReferences: Document",
      "MarkdownCanonical: Yes"
    ),
    con = path(path, shortname, shortname, ext = "Rproj")
  )
  add_index(path(path, shortname))
  add_abstract(path(path, shortname))
  add_recommendations(
    path(path, shortname),
    lof = ask_yes_no("Add a list of figures?", default = TRUE),
    lot = ask_yes_no("Add a list of tables?", default = TRUE)
  )
  add_chapter(path(path, shortname))
  add_bibliography(path(path, shortname))
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
    writeLines(path(path, shortname, ".gitignore"))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(path(path, shortname))
  paste0("inbo/flandersqmd-book@", version) |>
    quarto_add_extension(no_prompt = TRUE)
  quarto_render(
    use_freezer = FALSE,
    cache = FALSE,
    quiet = FALSE,
    as_job = FALSE
  )
  if (
    !requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()
  ) {
    return(invisible(NULL))
  }
  rstudioapi::openProject(path(path, shortname), newSession = TRUE)
}

#' @importFrom checklist ask_yes_no
insert_author_reviewer <- function(yaml) {
  cat("Please select the corresponding author")
  authors <- use_author()
  c(yaml, "  author:", author2yaml(authors, corresponding = TRUE)) -> yaml
  while (isTRUE(ask_yes_no("Add another author?", default = FALSE))) {
    author <- use_author()
    authors[, c("given", "family", "email")] |>
      rbind(author[, c("given", "family", "email")]) |>
      anyDuplicated() -> duplo
    if (duplo > 0) {
      cat(
        paste(author$given, author$family, "is already listed as author")
      )
      next
    }
    c(yaml, author2yaml(author, corresponding = FALSE)) -> yaml
    authors <- rbind(authors, author)
  }
  cat("Please select the reviewer")
  duplo <- 1
  while (duplo > 0) {
    author <- use_author()
    authors[, c("given", "family", "email")] |>
      rbind(author[, c("given", "family", "email")]) |>
      anyDuplicated() -> duplo
    if (duplo > 0) {
      cat(
        paste(author$given, author$family, "is already listed as author")
      )
    }
  }
  c(yaml, "  reviewer:", author2yaml(author, corresponding = FALSE))
}
