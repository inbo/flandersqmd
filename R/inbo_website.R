#' Render the report into a zip file for the INBO website
#' @param path The path to the directory where the report is located
#' @family utils
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom checklist citation_meta
#' @importFrom fs dir_ls dir_delete file_delete is_dir is_file path path_rel
#' @importFrom utils zip
inbo_website <- function(path = ".") {
  assert_that(is.string(path), noNA(path))
  path <- normalizePath(path, mustWork = FALSE)
  assert_that(is_dir(path), msg = "`path` is not an existing directory")
  stopifnot(
    "no `_quarto.yml` found in `path`" = is_file(path(path, "_quarto.yml"))
  )
  assert_that(requireNamespace("quarto", quietly = TRUE))
  system.file("generic_template/cc_by_4_0.md", package = "checklist") |>
    file.copy("LICENSE.md", overwrite = FALSE)
  cit <- citation_meta$new(path)
  if (
    !is.null(cit$get_errors) &&
      !all(grepl("\\.zenodo.json is modified", cit$get_errors))
  ) {
    return(cit)
  }
  yml <- quarto::quarto_inspect(path)
  assert_that(
    has_name(yml$config$project, "output-dir"),
    has_name(yml$config, "flandersqmd"),
    has_name(yml$config$flandersqmd, "shorttitle")
  )
  output_dir <- yml$config$project$`output-dir`

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(path)
  quarto::quarto_render(".", as_job = FALSE)

  # copy .zenodo.json to output directory
  list.files(path, pattern = ".zenodo.json", all.files = TRUE) |>
    file.copy(output_dir, overwrite = TRUE)

  # pack report into a zip archive
  dir_ls(
    output_dir,
    recurse = TRUE,
    regexp = "\\.zip",
    invert = TRUE,
    all = TRUE
  ) |>
    path_rel(output_dir) -> files

  setwd(output_dir)
  path(output_dir, tolower(yml$config$flandersqmd$shorttitle), ext = "zip") |>
    zip(files = files, flags = "-r9XqT")
  # remove output except zip archive
  dir_ls(output_dir, type = "dir") |>
    dir_delete()
  dir_ls(output_dir, type = "file", regexp = "\\.zip", invert = TRUE) |>
    file_delete()
  return(invisible(NULL))
}
