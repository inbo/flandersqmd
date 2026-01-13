#' Convert a data frame with author information to YAML format
#' @param author a data frame with author information.
#' It should contain the columns:
#' - `given`: the given name of the author
#' - `family`: the family name of the author
#' - `email`: the email address of the author (optional)
#' - `orcid`: the ORCID of the author (optional)
#' - `affiliation`: the affiliation of the author (optional)
#' @param corresponding a logical value indicating whether the author is the
#' corresponding author.
#' If `TRUE`, the email address of the author must be provided.
#' @return a character vector containing the YAML representation of the author
#' @importFrom assertthat assert_that is.flag noNA
#' @export
author2yaml <- function(author, corresponding = FALSE) {
  assert_that(is.flag(corresponding), noNA(corresponding))
  c(
    "  - name:",
    sprintf("      given: \"%s\"", author$given),
    paste0("  ", append_element(author, "family")),
    append_element(author, "email"),
    append_element(author, "orcid"),
    append_element(author, "ror")
  ) -> yaml
  if (!is.na(author$affiliation) && author$affiliation != "") {
    yaml <- c(
      yaml,
      sprintf("    affiliation:\n      - \"%s\"", author$affiliation)
    )
  }
  if (!corresponding) {
    return(paste(yaml, collapse = "\n"))
  }
  assert_that(
    noNA(author$email),
    author$email != "",
    msg = "please provide an email for the corresponding author"
  )
  paste(c(yaml, "    corresponding: true"), collapse = "\n")
}

append_element <- function(author, element) {
  if (is.null(author[[element]])) {
    return(character(0))
  }
  if (is.na(author[[element]])) {
    return(character(0))
  }
  if (author[[element]] == "") {
    return(character(0))
  }
  sprintf("    %s: \"%s\"", element, author[[element]])
}
