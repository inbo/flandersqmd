#' Generate dependency timestamp from files
#'
#' Collect modification times (`mtime`) of one or more files and return
#' them as a single dependency stamp comment.
#'
#' @param file Character vector of file paths.
#' @param quiet Logical; if TRUE suppress warnings about missing files.
#'
#' @return A single string: an HTML comment with dependency `mtimes`.
#'
#' @seealso [autoqmd_insert_children()]
#'
#' @export
#'
#' @importFrom assertthat is.string is.flag
#'
#' @examples
#' \dontrun{
#' autoqmd_dependency_stamp("_template.qmd")
#' autoqmd_dependency_stamp(
#'   c("_template.qmd", "helpers.R", "style.scss")
#' )
#' }

autoqmd_dependency_stamp <- function(file = NULL, quiet = FALSE) {
  # Validation
  stopifnot("`file` must be one or more file paths." =
              all(sapply(file, assertthat::is.string)))
  stopifnot("`quiet` must be a scalar logical vector." =
              assertthat::is.flag(quiet))

  # If no files are supplied, fall back to the current time
  # (ensures a deterministic stamp is still produced)
  if (is.null(file) || length(file) == 0) {
    return(sprintf(
      "<!-- DEPENDENCY-MTIME: %s -->",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }

  # Ensure input is a character vector
  file <- as.character(file)

  # Check which files exist
  exists <- file.exists(file)

  # Warn about missing files unless suppressed
  if (any(!exists) && !quiet) {
    warning(
      "Some files do not exist: ",
      paste(file[!exists], collapse = ", "),
      call. = FALSE
    )
  }

  # Retrieve file metadata (mtime will be NA for missing files)
  info <- file.info(file)
  mtimes <- info$mtime

  # Build "filename timestamp" entries for each file
  entries <- vapply(seq_along(file), function(i) {
    fn <- basename(file[i])
    mt <- mtimes[i]

    # Use NA explicitly when mtime is unavailable
    if (is.na(mt)) {
      paste0(fn, " NA")
    } else {
      paste0(fn, " ", format(mt, "%Y-%m-%d %H:%M:%S"))
    }
  }, character(1), USE.NAMES = FALSE)

  # Combine all entries into a single HTML comment
  sprintf(
    "<!-- DEPENDENCY-MTIME: %s -->",
    paste(entries, collapse = "; ")
  )
}
