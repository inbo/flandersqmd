#' Add an author or reviewer to the `_quarto.yml` file
#' @inheritParams add_recommendations
#' @param reviewer If `TRUE`, the person is added as a reviewer.
#' Defaults to `FALSE`.
#' If `FALSE`, the person is added as an author.
#' @return The path to the `_quarto.yml` file.
#' @export
#' @importFrom assertthat assert_that is.flag is.string has_name noNA
#' @importFrom fs is_dir is_file path
#' @importFrom yaml read_yaml
add_author <- function(report_path = ".", reviewer = FALSE) {
  assert_that(
    is.string(report_path),
    noNA(report_path),
    is_dir(report_path),
    is.flag(reviewer),
    noNA(reviewer)
  )
  target <- path(report_path, "_quarto.yml")
  stopifnot("no `_quarto.yml` found at `report_path`" = is_file(target))
  yaml <- read_yaml(target)
  stopifnot(
    "No `flandersqmd` entry in `_quarto.yml`" = has_name(yaml, "flandersqmd")
  )
  use_author() |>
    author2list() -> extra
  if (reviewer) {
    yaml$flandersqmd$reviewer <- c(yaml$flandersqmd$reviewer, list(extra))
  } else {
    yaml$flandersqmd$author <- c(yaml$flandersqmd$author, list(extra))
  }
  store_yaml(yaml, target = target)
  return(target)
}

author2list <- function(author) {
  author_list <- list(name = list(given = author$given, family = author$family))
  if (!is.na(author$email) && author$email != "") {
    author_list$email <- author$email
  }
  if (!is.na(author$orcid) && author$orcid != "") {
    author_list$orcid <- author$orcid
  }
  if (!is.na(author$affiliation) && author$affiliation != "") {
    author_list$affiliation <- list(author$affiliation)
  }
  return(author_list)
}
