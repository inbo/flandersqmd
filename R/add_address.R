#' Format an address in YAML
#' @param type The type of address.
#' Defaults to `"client"`.
#' @return A character vector containing the formatted address.
#' @export
#' @importFrom assertthat assert_that is.string noNA
add_address <- function(type = "client") {
  assert_that(is.string(type), noNA(type))
  stopifnot(
    "type must only contain alphanumeric characters" = grepl(
      "^[[:alnum:]]+$",
      type
    )
  )
  address <- character(0)
  while (TRUE) {
    sprintf(
      "Add line %i of the %s name and address (leave empty to stop): ",
      length(address) + 1,
      type
    ) |>
      readline() -> extra
    if (extra == "") {
      break
    }
    address <- c(address, extra)
  }
  if (length(address) == 0) {
    return(address)
  }
  sprintf("optional url of the %s: ", type) |>
    readline() -> url
  sprintf("optional filename of the %s logo: ", type) |>
    readline() -> logo
  c(
    sprintf("  %s:", type),
    sprintf("    - %s", address),
    sprintf("  %s_url: %s", type, url)[url != ""],
    sprintf("  %s_logo: %s", type, logo)[logo != ""]
  )
}
