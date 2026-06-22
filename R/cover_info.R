cover_info <- function(yml) {
  yml$title <- paste0(yml$title, "\nSubtitel: ", yml$subtitle)
  yml$author$email[
    !is.na(yml$author$corresponding) & yml$author$corresponding
  ] |>
    paste(collapse = ", ") -> yml$corresponding
  paste(yml$author$name$given, yml$author$name$family) |>
    paste(collapse = ", ") -> yml$author
  yml$print <- format_print_order(yml)
  cover_txt <- sprintf(
    "Titel: %s\nAuteur(s): %s\nContactpersoon: %s\nAfbeelding voor cover: %s
Embargo tot: %s\n%s",
    yml$title,
    yml$author,
    yml$corresponding,
    paste0(yml$coverphoto, ""),
    paste0(yml$embargo, ""),
    yml$print
  )
  if (has_name(yml, "client_logo")) {
    cover_txt <- c(cover_txt, paste("Logo klant:", yml$client_logo))
  }
  if (has_name(yml, "cooperation_logo")) {
    cover_txt <- c(
      cover_txt,
      paste("Logo samenwerking:", yml$cooperation_logo)
    )
  }
  writeLines(cover_txt, "cover.txt")
  return(invisible(NULL))
}
