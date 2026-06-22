#' @importFrom assertthat assert_that has_name is.count is.number
format_print_order <- function(yml) {
  if (!has_name(yml, "print")) {
    return("Geen druk")
  }
  assert_that(
    has_name(yml$print, "copies"),
    is.number(yml$print$copies)
  )
  if (yml$print$copies == 0) {
    return("Geen druk")
  }
  assert_that(
    has_name(yml$print, c("motivation", "pages")),
    is.count(yml$print$copies),
    is.count(yml$print$pages)
  )
  sprintf(
    "Aantal gedrukte examplaren: %i\nMotivatie: %s\nAantal bladzijden: %i",
    yml$print$copies,
    yml$print$motivation,
    yml$print$pages
  )
}
