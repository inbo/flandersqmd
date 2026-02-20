# Add a chapter file to a Quarto report

This function adds a chapter file to a Quarto report.

## Usage

``` r
add_chapter(report_path = ".", title, filename, toc = TRUE)
```

## Arguments

- report_path:

  The path to the folder containing the report. Defaults to the current
  working directory.

- title:

  The title of the chapter. If missing, the chapter is assumed the
  introduction with a default title based on the language.

- filename:

  The name of the chapter file. If missing, the chapter is assumed the
  introduction with a default filename based on the language.

- toc:

  A logical value indicating whether to add a local table of contents.
  Defaults to `TRUE`.

## Value

The name of the chapter file.
