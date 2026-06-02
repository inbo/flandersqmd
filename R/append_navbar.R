#' @importFrom assertthat assert_that has_name
append_navbar <- function(yaml, text, filename) {
  assert_that(inherits(yaml, "list"))
  type <-
    c("website"[has_name(yaml, "website")], "book"[has_name(yaml, "book")])
  if (!has_name(yaml[[type]], "navbar")) {
    yaml[[type]]$navbar <- list(left = list(text = text, file = filename))
    return(yaml)
  }
  if (!has_name(yaml[[type]]$navbar, "left")) {
    yaml[[type]]$navbar <- c(
      yaml[[type]]$navbar,
      list(left = list(list(text = text, file = filename)))
    )
    return(yaml)
  }
  vapply(
    yaml[[type]]$navbar$left,
    FUN.VALUE = logical(1),
    FUN = function(x) {
      if (has_name(x[[1]], "file")) {
        return(x[[1]]$file == filename)
      }
      return(FALSE)
    }
  ) |>
    any() -> done
  if (done) {
    return(yaml)
  }
  yaml[[type]]$navbar$left <- c(
    yaml[[type]]$navbar$left,
    list(list(text = text, file = filename))
  )
  return(yaml)
}
