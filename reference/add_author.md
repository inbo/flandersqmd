# Add an author or reviewer to the `_quarto.yml` file

This function is deprecated. Please use
[`citeme::add_individual()`](https://inbo.github.io/citeme/reference/add_individual.html)
instead.

## Usage

``` r
add_author(report_path = ".", reviewer = FALSE)
```

## Arguments

- report_path:

  The path to the folder containing the report. Defaults to the current
  working directory.

- reviewer:

  If `TRUE`, the person is added as a reviewer. Defaults to `FALSE`. If
  `FALSE`, the person is added as an author.

## Value

The path to the `_quarto.yml` file.
