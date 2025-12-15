#' Generate child QMD files from a template
#'
#' This function generates one or more Quarto (`.qmd`) files from a
#' template, filling in variables supplied via `...`. Each output file is
#' written to a specified directory and given either a random unique name or,
#' if `freeze` is specified, a name derived from the provided variable.
#'
#' @param ... Named vectors or list of variables of equal length, passed to
#'   `knitr::knit_expand`. Each position across vectors represents
#'   one document to generate.
#' @param template Path to a Quarto template (`.qmd`) file to expand.
#' @param child_dir Directory where the generated files should be written.
#'   The directory is created if it does not exist.
#' @param freeze Optional string giving the name of a variable in `...`
#'   whose values will be used for deterministic file names.
#'   If provided, filenames will be of the form `"_qmd_<value>.qmd"`,
#'   and files will only be regenerated if the template is newer.
#'
#' @details
#' - Random names (default) are generated using 8-digit hexadecimal strings.
#' - When `freeze` is used, existing files are not regenerated unless the
#'  template has been modified since their last creation. This enables
#'  incremental regeneration for long or expensive builds.
#'  The template timestamp is checked against each corresponding file; if the
#'  file exists and is newer, it will be skipped.
#'
#' @return Invisibly returns a character vector with the paths of the generated
#'   QMD files. Files are written to disk (unless all were skipped).
#'
#' @seealso [autoqmd_prepare()]
#'
#' @export
#'
#' @importFrom knitr knit_expand
#' @importFrom assertthat is.string
#'
#' @examples
#' \dontrun{
#' my_species <- paste("Iris", levels(iris$Species))
#'
#' # Create child document for each species
#' # Use freeze to keep child files with label names
#' autoqmd_generate_children(
#'   species = my_species,
#'   label = gsub("\\s", ".", tolower(my_species)),
#'   template = "species_template.qmd",
#'   child_dir = "spec_files",
#'   freeze = "label"
#' )
#' }
autoqmd_generate_children <- function(
  ...,
  template,
  child_dir,
  freeze = NULL
) {
  # Capture input arguments
  dots <- list(...)

  # Validation
  stopifnot("You must provide at least one named argument via ..." =
              length(dots) == 0)
  lens <- vapply(dots, length, integer(1))
  stopifnot("All arguments in ... must have the same length." =
              length(dots) == 0)
  n <- lens[1]

  stopifnot("`template` must be a path to a Quarto template." =
              assertthat::is.string(template))
  stopifnot("`template` must be a path to a Quarto template." =
              file.exists(template))

  # Create directory if needed
  dir.create(child_dir, recursive = TRUE, showWarnings = FALSE)

  # Determine filenames
  if (!is.null(freeze)) {
    stopifnot("`freeze` must be a scalar character vector." =
                assertthat::is.string(freeze))
    stopifnot("`freeze` must match one of the named arguments in ..." =
                !freeze %in% names(dots))

    # Sanitize freeze names (replace spaces etc.)
    base_names <- gsub("[^a-zA-Z0-9_.-]", "_", as.character(dots[[freeze]]))
    random_names <- paste0("_qmd_", tolower(base_names), ".qmd")
  } else {
    random_names <- paste0(
      "_qmd_", sprintf("%08x", sample.int(16^7, n, replace = TRUE)), ".qmd"
    )
  }

  out_files <- file.path(child_dir, random_names)
  template_mtime <- file.info(template)$mtime

  # Generate files
  generated <- 0
  skipped <- 0

  for (i in seq_len(n)) {
    args <- lapply(dots, function(x) x[i])
    out_file <- out_files[i]

    # Skip regeneration if freeze is active and file is up to date
    if (!is.null(freeze) && file.exists(out_file)) {
      file_mtime <- file.info(out_file)$mtime
      if (!is.na(file_mtime) && file_mtime >= template_mtime) {
        skipped <- skipped + 1
        next
      }
    }

    # Expand template and write to file
    content <- do.call(knitr::knit_expand, c(list(template), args))
    writeLines(content, out_file)
    generated <- generated + 1
  }

  # Message summary
  msg <- sprintf(
    "Processed %d QMD files (%d generated, %d skipped) in '%s'.",
    n, generated, skipped, child_dir
  )
  message(msg)

  invisible(out_files)
}
