# Add a recommendations section to a `flandersqmd` report

This function adds a recommendations section to a `flandersqmd` report.
The file also add a table of contents, a list of figures and a list of
tables to the pdf version of the report.

## Usage

``` r
add_recommendations(report_path = ".", lof = TRUE, lot = TRUE)
```

## Arguments

- report_path:

  The path to the folder containing the report. Defaults to the current
  working directory.

- lof:

  A logical value indicating whether to add a list of figures. Defaults
  to `TRUE`. If `TRUE`, a list of figures is added to the pdf version of
  the report.

- lot:

  A logical value indicating whether to add a list of tables. Defaults
  to `TRUE`. If `TRUE`, a list of tables is added to the pdf version of
  the report.
