#' Generate dependency timestamp from a QMD
#'
#' Parse a QMD (via `knitr::purl`), detect `source()` calls (including
#' `file.path(...)` forms), attempt to evaluate simple assignments that appear
#' before the first `source()` (so variables become available),
#' and collect `mtimes` of the file and any dependency files.
#'
#' @param file Path to a `.qmd` file.
#' @param quiet Logical; if TRUE suppresses messages about unresolved sources.
#'
#' @return A single string: an HTML comment with dependency `mtimes`, e.g.
#'   `<!-- DEPENDENCY-MTIME: template.qmd 2025-10-08 14:30:11 -->`
#'
#' @seealso [autoqmd_insert_includes()]
#'
#' @export
#'
#' @importFrom knitr purl
#' @importFrom rprojroot find_root_file
#'
#' @examples
#' \dontrun{
#' autoqmd_dependency_stamp("species_template.qmd")
#' }
autoqmd_dependency_stamp <- function(file = NULL, quiet = FALSE) { # nolint: cyclocomp_linter
  # If no valid file -> fallback to current time
  if (is.null(file) || !file.exists(file)) {
    return(sprintf("<!-- DEPENDENCY-MTIME: %s -->",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  }

  # 1) Extract R code from the file (works for QMD / knitr chunks)
  tmp_r <- tempfile(fileext = ".R")
  knitr::purl(file, output = tmp_r, documentation = 0L, quiet = TRUE)
  lines <- readLines(tmp_r, warn = FALSE)

  # 2) Find indices of lines that contain source(
  src_idx <- grep("source\\(", lines)
  if (length(src_idx) == 0) {
    # No sources found -> return file mtime only
    mt <- file.info(file)$mtime
    stamp <- paste0(basename(file), " ", format(mt, "%Y-%m-%d %H:%M:%S"))
    return(sprintf("<!-- DEPENDENCY-MTIME: %s -->", stamp))
  }

  # 3) Prepare a restricted environment for safe evaluation
  env <- new.env(parent = baseenv())
  # provide helpers that are commonly used in files
  env$file.path <- file.path
  env$here <- function(...) file.path(...) # minimal here()-like helper

  # If rprojroot is available, provide the function directly
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    env$find_root_file <- rprojroot::find_root_file
    # Some files call rprojroot::find_root_file(...) explicitly;
    # to allow that exact syntax we don't need to attach the namespace,
    # we will evaluate file expressions like rprojroot::find_root_file(...)
    # below by letting parse/eval handle the :: call (baseenv has '::'
    # available).
  }

  # 4) Attempt to evaluate simple assignments that appear BEFORE the first
  # source
  first_src <- min(src_idx)
  if (first_src > 1) {
    assign_lines <- lines[seq_len(first_src - 1)]
    assign_exprs <- try(parse(text = assign_lines), silent = TRUE)
    if (!inherits(assign_exprs, "try-error")) {
      for (e in assign_exprs) {
        # Only evaluate simple assignments (lhs <- rhs). Skip complex constructs
        if (is.call(e) && identical(e[[1]], as.name("<-"))) {
          try(eval(e, envir = env), silent = TRUE)
        }
      }
    } else {
      if (!quiet) message(paste("Could not parse assignment lines before first",
                                "source(); skipping evaluation of pre-source",
                                "assignments."))
    }
  }

  # 5) For each source() line: parse and try to evaluate the first argument
  deps <- character(0)
  for (i in src_idx) {
    ln <- lines[i]
    ex <- try(parse(text = ln)[[1]], silent = TRUE)
    if (inherits(ex, "try-error")) {
      if (!quiet) message("Could not parse line: ", ln)
      next
    }

    # The first arg of source(...) is ex[[2]] (could be a string,
    # file.path(...), or variable)
    path_expr <- try(ex[[2]], silent = TRUE)
    if (inherits(path_expr, "try-error")) {
      if (!quiet) message("No path expression in: ", ln)
      next
    }

    # Try to evaluate the path expression in the prepared env
    res <- try(eval(path_expr, envir = env), silent = TRUE)
    if (inherits(res, "try-error") || !is.character(res)) {
      # If expression uses rprojroot::find_root_file(...) it will evaluate here
      # if rprojroot available
      if (!quiet) message("Could not evaluate source() argument in line: ", ln)
      next
    }

    # res might be relative; try two candidates:
    #  - as given (res)
    #  - relative to directory of file
    candidate1 <- res[1]
    candidate2 <- file.path(dirname(normalizePath(file)), res[1])

    chosen <- NA_character_
    if (!is.na(candidate1) && nzchar(candidate1) && file.exists(candidate1)) {
      chosen <- normalizePath(candidate1)
    } else if (!is.na(candidate2) && nzchar(candidate2) &&
                 file.exists(candidate2)) {
      chosen <- normalizePath(candidate2)
    } else {
      if (!quiet) message("Detected source() but file not found: '", res[1],
                          "' (tried relative to file: '", candidate2, "')")
      next
    }

    deps <- c(deps, chosen)
  }

  # 6) Build stamp from file + deps (unique)
  files_to_track <- unique(c(normalizePath(file), deps))
  info <- file.info(files_to_track)
  mtimes <- info$mtime

  entries <- vapply(seq_along(files_to_track), function(i) {
    fn <- basename(files_to_track[i])
    mt <- mtimes[i]
    if (is.na(mt)) {
      paste0(fn, " NA")
    } else {
      paste0(fn, " ", format(mt, "%Y-%m-%d %H:%M:%S"))
    }
  }, character(1), USE.NAMES = FALSE)

  dep_stamp <- paste(entries, collapse = "; ")
  sprintf("<!-- DEPENDENCY-MTIME: %s -->", dep_stamp)
}
