#' @importFrom assertthat assert_that has_name
append_navbar <- function(yaml, text, filename) {
  assert_that(inherits(yaml, "list"))
  if (!has_name(yaml$book, "navbar")) {
    yaml$book$navbar <- list(left = list(text = text, file = filename))
    return(yaml)
  }
  if (!has_name(yaml$book$navbar, "left")) {
    yaml$book$navbar <- c(
      yaml$book$navbar,
      list(left = list(list(text = text, file = filename)))
    )
    return(yaml)
  }
  vapply(
    yaml$book$navbar$left,
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
  yaml$book$navbar$left <- c(
    yaml$book$navbar$left,
    list(list(text = text, file = filename))
  )
  return(yaml)
}
