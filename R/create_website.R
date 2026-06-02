#' Create a template for a `flandersqmd` website
#'
#' @param path The folder in which to create the folder containing the website.
#'  Defaults to the current working directory.
#'  It also creates an RStudio project file in the website folder.
#'  When ran from RStudio, the project will be opened automatically in a new
#'  session.
#' @param websitename The folder name of the website.
#' The location of the folder `websitename` depends on the content of `path`.
#' When `path` is a `checklist::checklist` project, you will find the new
#' website at `path/source/websitename`.
#' When `path` is a `checklist::checklist` package, you will find the new
#' website at `path/inst/websitename`.
#' Otherwise you will find the new website at `path/websitename`.
#' @param version The version of the `flandersqmd-website` extension to use.
#' Defaults to `"main"`, which refers to the current version.
#' @family utils
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom checklist ask_yes_no get_branches_tags menu_first read_checklist
#' @importFrom fs dir_create is_dir path
#' @importFrom quarto quarto_add_extension
create_website <- function(path = ".", websitename, version = "main") {
  assert_that(is.string(path), noNA(path), is_dir(path))
  assert_that(is.string(websitename), noNA(websitename))
  assert_that(is.string(version), noNA(version))
  assert_that(
    grepl("^[a-z0-9_]+$", websitename),
    msg = paste(
      "The website name folder may only contain lower case letters, digits",
      "and _"
    )
  )
  available <- get_branches_tags(owner = "inbo", repo = "flandersqmd-website")
  assert_that(
    version %in% available,
    msg = paste(
      "Version not found. Available versions are:",
      paste(available, collapse = ", ")
    )
  )
  x <- try(read_checklist(path), silent = TRUE)
  if (inherits(x, "checklist")) {
    path <- path(x$get_path, ifelse(x$package, "inst", "source"))
    dir_create(path)
    output_dir <- path("..", "..", "output", websitename)
  } else {
    output_dir <- path("output", websitename)
  }
  path <- normalizePath(path, mustWork = TRUE)

  stopifnot(
    "The website name folder already exists." = !is_dir(path(path, websitename))
  )

  # ask required information
  lang <- c(`nl-BE` = "Dutch", `en-GB` = "English", `fr-FR` = "French")
  lang <- names(lang)[
    menu_first(lang, title = "What is the main language of the website?")
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
  authors <- insert_author_reviewer(lang = lang)

  website_types <- c("report", "footer colophon", "minimal")
  website_type <- website_types[
    menu_first(website_types, title = "Which website layout?")
  ]
  rm(website_types)

  # build new yaml
  c(
    "project:",
    "  type: website",
    "  preview:",
    "    port: 4201",
    "    browser: true",
    "  render:",
    "    - \"*.md\"",
    "    - \"*.qmd\"",
    "    - \"!LICENSE.md\" #ignore LICENSE.md",
    "    - \"!README.md\" #ignore README.md",
    paste("  output-dir:", output_dir),
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
    "  flandersqmd-website-html: default",
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
    "  public_website: true",
    "  colophon: true",
    "website:",
    "  page-footer:"[website_type != "report"],
    "    center: '{{< minimal >}}'"[website_type == "minimal"],
    "    left: '{{< authors >}}'"[website_type == "footer colophon"],
    "    center: '{{< inbo_info >}}'"[website_type == "footer colophon"],
    "    right: '{{< reference >}}'"[website_type == "footer colophon"],
    "  navbar:",
    "    pinned: true",
    "    right:",
    "    - icon: mastodon",
    "      href: https://mastodon.online/&#64;inbo",
    "    - icon: facebook",
    "      href: https://www.facebook.com/INBOVlaanderen/"
  ) -> yaml
  if (
    isTRUE(
      ask_yes_no(
        "Is the website code on GitHub and should a link to the repo be added?",
        default = FALSE
      )
    )
  ) {
    repo_name <- readline(prompt = "Enter the repo name: ")
    yaml <- c(
      yaml,
      "    - icon: github",
      "      menu:",
      "        - text: Source Code",
      paste0("          url: https://github.com/inbo/", repo_name),
      "        - text: Report a Bug",
      paste0("          url: https://github.com/inbo/", repo_name, "/issues")
    )
  }

  dir_create(path(path, websitename))
  writeLines(yaml, path(path, websitename, "_quarto.yml"))
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
    con = path(path, websitename, websitename, ext = "Rproj")
  )
  add_index(path(path, websitename))
  if (website_type == "report") {
    add_abstract(path(path, websitename))
    add_recommendations(path(path, websitename), lof = FALSE, lot = FALSE)
  }
  add_chapter(path(path, websitename))
  add_bibliography(path(path, websitename))
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
    writeLines(path(path, websitename, ".gitignore"))

  old_wd <- getwd()
  setwd(path(path, websitename))
  paste0("inbo/flandersqmd-website@", version) |>
    quarto_add_extension(no_prompt = TRUE)
  setwd(old_wd)
  if (
    !requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()
  ) {
    return(invisible(NULL))
  }
  rstudioapi::openProject(path(path, websitename), newSession = TRUE)
}

#' @importFrom checklist ask_yes_no ask_rightsholder_funder author2df
#' @importFrom checklist inbo_org_list use_author
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
  yaml <- c(yaml, "  reviewer:", author2yaml(author, corresponding = FALSE))

  inbo_org_list() |>
    ask_rightsholder_funder(type = "rightsholder") -> rh
  fund <- ask_rightsholder_funder(org = rh$org, type = "funder")
  org <- fund$org

  vapply(
    rh$selection,
    FUN = function(x) {
      org$get_person(x, role = "cph", lang = lang) |>
        author2df() |>
        author2yaml() |>
        list()
    },
    FUN.VALUE = vector("list", 1)
  ) |>
    unlist() |>
    gsub(pattern = "\n", replacement = "\n  ") |>
    gsub(pattern = "^", replacement = "  ") -> rh_yaml
  vapply(
    fund$selection,
    FUN = function(x) {
      org$get_person(x, role = "fnd", lang = lang) |>
        author2df() |>
        author2yaml() |>
        list()
    },
    FUN.VALUE = vector("list", 1)
  ) |>
    unlist() |>
    gsub(pattern = "\n", replacement = "\n  ") |>
    gsub(pattern = "^", replacement = "  ") -> fund_yaml
  org$get_zenodo_by_email(rh$selection) |>
    c(org$get_zenodo_by_email(fund$selection)) |>
    unique() |>
    paste(collapse = "; ") -> zenodo
  if (zenodo != "") {
    zenodo <- paste("  community:", zenodo)
  }
  c(
    yaml,
    "  rightsholder:",
    unlist(rh_yaml),
    "  funder:",
    unlist(fund_yaml),
    zenodo
  )
}
