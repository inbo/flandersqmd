#' Add an author or reviewer to the `_quarto.yml` file
#'
#' This function is deprecated.
#' Please use `citeme::add_individual()` instead.
#' @inheritParams add_recommendations
#' @param reviewer If `TRUE`, the person is added as a reviewer.
#' Defaults to `FALSE`.
#' If `FALSE`, the person is added as an author.
#' @return The path to the `_quarto.yml` file.
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom citeme add_individual
add_author <- function(report_path = ".", reviewer = FALSE) {
  .Deprecated("citeme::add_individual")
  assert_that(is.flag(reviewer), noNA(reviewer))
  add_individual(path = report_path, role = ifelse(reviewer, "rev", "aut"))
}
