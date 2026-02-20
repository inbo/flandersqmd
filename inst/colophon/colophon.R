form2person <- function(x) {
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  stringr::str_split(x, pattern = "\n") |>
    unlist() |>
    stringr::str_split(pattern = " *; *") |>
    purrr::map_chr(
      ~ sprintf(
        "  - name:\n      family: %s\n      given: %s%s",
        .x[1],
        .x[2],
        ifelse(.x[3] != "", paste("\n    orcid:", .x[3]), "")
      )
    ) |>
    paste(collapse = "\n")
}

display_client_coop <- function(
  name,
  url,
  logo,
  type = c("client", "cooperation"),
  path = "."
) {
  requireNamespace("stringr", quietly = TRUE)
  type <- match.arg(type)
  if (is.na(name)) {
    return("")
  }
  stringr::str_split(name, "\n") |>
    unlist() |>
    sprintf(fmt = "    - %s") |>
    paste(collapse = "\n") |>
    sprintf(fmt = "  %2$s:\n%1$s", type) -> result
  if (!is.na(url)) {
    result <- paste0(result, "\n  ", type, "url: ", url)
  }
  if (!is.na(logo)) {
    requireNamespace("googledrive", quietly = TRUE)
    logo_file <- file.path(path, paste0(type, ".jpg"))
    googledrive::drive_download(file = logo, logo_file, overwrite = TRUE)
    result <- paste0(result, "\n  ", type, "logo: ", logo_file)
  }
  return(result)
}

create_quarto_yml <- function(this_colophon, path = ".") {
  if (nrow(this_colophon) > 1) {
    use_this_colophon <- tail(order(this_colophon$Tijdstempel), 1)
    warning(
      "Meerdere colofons gevonden met deze PURE ID. ",
      "We gebruiken de meest recente."
    )
    c(
      "Meerdere colofons gevonden met deze PURE ID.",
      "We gebruiken de meest recente:",
      "",
      sprintf(
        "- Ingevoerd door %s op %s.",
        this_colophon$`E-mailadres`,
        this_colophon$Tijdstempel
      ),
      "",
      "",
      this_colophon$Opmerkingen[use_this_colophon][
        !is.na(this_colophon$Opmerkingen[use_this_colophon])
      ]
    ) |>
      paste(collapse = "\n") -> this_colophon$Opmerkingen[use_this_colophon]
    this_colophon <- this_colophon[use_this_colophon, ]
  }
  system.file("colophon/template.yml", package = "flandersqmd") |>
    readLines() |>
    paste(collapse = "\n") |>
    sprintf(
      lang = c(Nederlands = "nl-BE", Engels = "en-GB", Frans = "fr-FR")[
        this_colophon$Taal
      ],
      level = 1 + (this_colophon$Stijl == "INBO"),
      title = paste0(
        "\"",
        this_colophon$Titel,
        "\"",
        ifelse(
          is.na(this_colophon$Ondertitel),
          "",
          paste0("\n  subtitle: \"", this_colophon$Ondertitel, "\"")
        )
      ),
      shorttitle = sprintf("colofon-%i", this_colophon$`PURE id`),
      author = form2person(this_colophon$Auteurs),
      reviewer = form2person(this_colophon$Reviewer),
      year = this_colophon$Jaartal,
      reportnr = this_colophon$Rapportnummer,
      coverdescription = this_colophon$`Beschrijving coverfoto`,
      public_report = ifelse(
        this_colophon$`Type rapport` == "Intern rapport",
        "false",
        "true"
      ),
      doi = ifelse(
        this_colophon$`Type rapport` == "Intern rapport",
        "",
        sprintf("\n  doi: 10.21436/inbor.%i", this_colophon$`PURE id`)
      ),
      deportnr = ifelse(
        is.na(this_colophon$Depotnummer),
        "",
        paste0("\n  depotnr: \"", this_colophon$Depotnummer, "\"")
      ),
      ordernr = ifelse(
        is.na(this_colophon$Opdrachtnummer),
        "",
        paste0("\n  ordernr: \"", this_colophon$Opdrachtnummer, "\"")
      ),
      client_coop = display_client_coop(
        name = this_colophon$Opdrachtgever,
        url = this_colophon$`Website opdrachtgever`,
        logo = this_colophon$`Opdrachtgever logo`,
        type = "client"
      ) |>
        c(
          display_client_coop(
            name = this_colophon$Samenwerking,
            url = this_colophon$`Website samenwerking`,
            logo = this_colophon$`Samenwerking logo`,
            type = "cooperation"
          )
        ) |>
        paste(collapse = "\n")
    ) |>
    writeLines(file.path(path, "_quarto.yml"))
  c(
    "---",
    "toc: false",
    "---",
    "",
    "{{< colophon >}}",
    "",
    "# Opmerkingen {-}",
    "",
    this_colophon$Opmerkingen[!is.na(this_colophon$Opmerkingen)]
  ) |>
    writeLines(file.path(path, "index.md"))
}

read_colophon <- function(
  pure_id,
  token = "1MOMUI3pzBzxJNtFRDbJrSOKYfYlRR5CAG_TMquFm-bI"
) {
  requireNamespace("googlesheets4", quietly = TRUE)
  requireNamespace("assertthat", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  assertthat::assert_that(
    assertthat::is.number(pure_id),
    assertthat::noNA(pure_id)
  )
  paste0("https://docs.google.com/spreadsheets/d/", token) |>
    googlesheets4::read_sheet() -> colophons
  stopifnot("PURE id not avialable in form" = pure_id %in% colophons$`PURE id`)
  colophons[colophons$`PURE id` == pure_id, ]
}

generate_colophon <- function(pure_id, path = ".") {
  requireNamespace("quarto", quietly = TRUE)
  requireNamespace("qpdf", quietly = TRUE)
  this_colophon <- read_colophon(pure_id = pure_id)
  create_quarto_yml(this_colophon = this_colophon, path = path)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(path)
  quarto::quarto_add_extension("inbo/flandersqmd-book@bugfix", no_prompt = TRUE)
  quarto::quarto_render(input = "index.md")
  unique(this_colophon$`PURE id`) |>
    sprintf(fmt = "colofon_%i.pdf") -> pdf_file
  n <- 2 + (nrow(this_colophon) > 1 || any(!is.na(this_colophon$Opmerkingen)))
  qpdf::pdf_subset(pdf_file, pages = seq_len(n)) -> cover_pdf
  file.remove(pdf_file)
  file.rename(cover_pdf, pdf_file)
}
