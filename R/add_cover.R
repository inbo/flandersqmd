#' Add cover to `_quarto.yml`
#' @inheritParams add_recommendations
#' @param cover_pdf The path to a PDF file.
#' The first page of this PDF file will be used as the cover of the report.
#' @export
#' @importFrom assertthat assert_that has_name is.string noNA
#' @importFrom fs file_move is_dir is_file path
#' @importFrom pdftools pdf_convert pdf_subset
#' @importFrom yaml read_yaml write_yaml
add_cover <- function(report_path = ".", cover_pdf) {
  assert_that(
    is.string(report_path),
    noNA(report_path),
    is_dir(report_path),
    is.string(cover_pdf),
    noNA(cover_pdf),
    is_file(cover_pdf)
  )
  target <- path(report_path, "_quarto.yml")
  stopifnot("no `_quarto.yml` found at `report_path`" = is_file(target))
  yaml <- read_yaml(target)
  stopifnot(
    "No `flandersqmd` entry in `_quarto.yml`" = has_name(yaml, "flandersqmd")
  )
  pdf_subset(cover_pdf, pages = 1, output = path(report_path, "cover.pdf"))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(report_path)
  pdf_convert("cover.pdf", format = "png", dpi = 300, filenames = "cover-%s.%s")
  file_move("cover-1.png", "cover.png")
  yaml$flandersqmd$cover <- "cover.pdf"
  yaml$book$sidebar$logo <- "cover.png"
  store_yaml(yaml, target = target)
  return(target)
}
