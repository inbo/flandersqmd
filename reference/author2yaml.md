# Convert a data frame with author information to YAML format

Convert a data frame with author information to YAML format

## Usage

``` r
author2yaml(author, corresponding = FALSE)
```

## Arguments

- author:

  a data frame with author information. It should contain the columns:

  - `given`: the given name of the author

  - `family`: the family name of the author

  - `email`: the email address of the author (optional)

  - `orcid`: the ORCID of the author (optional)

  - `affiliation`: the affiliation of the author (optional)

- corresponding:

  a logical value indicating whether the author is the corresponding
  author. If `TRUE`, the email address of the author must be provided.

## Value

a character vector containing the YAML representation of the author
