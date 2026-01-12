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
    sprintf("      given: \"%s\"", author$given)
  ) -> yaml
  if (!is.na(author$family) && author$family != "") {
    yaml <- c(yaml, sprintf("      family: \"%s\"", author$family))
  }
  if (!is.na(author$email) && author$email != "") {
    yaml <- c(yaml, sprintf("    email: \"%s\"", author$email))
  }
  if (!is.na(author$orcid) && author$orcid != "") {
    yaml <- c(yaml, sprintf("    orcid: \"%s\"", author$orcid))
  }
  if (!is.null(author$ror) && author$ror != "") {
    yaml <- c(yaml, sprintf("    ror: \"%s\"", author$ror))
  }
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
