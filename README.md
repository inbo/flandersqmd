
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GPL-3](https://img.shields.io/badge/License-GPL-3-brightgreen)](https://raw.githubusercontent.com/inbo/checklist/refs/heads/main/inst/generic_template/gplv3.md)
[![Release](https://img.shields.io/github/release/inbo/flandersqmd.svg)](https://github.com/inbo/flandersqmd/releases)
![GitHub Workflow
Status](https://github.com/inbo/flandersqmd/actions/workflows/check_on_main.yml/badge.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/flandersqmd) ![GitHub
code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/flandersqmd.svg)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/flandersqmd)
[![Codecov test
coverage](https://codecov.io/gh/inbo/flandersqmd/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/flandersqmd?branch=main)
<!-- badges: end -->

# flandersqmd: Auxiliary Function for the Flandersqmd Quarto Extensions

[Onkelinx, Thierry![ORCID
logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-8804-4216)[^1][^2][^3]
[Research Institute for Nature and Forest
(INBO)](mailto:info%40inbo.be)[^4][^5]

**keywords**: corporate identity; quarto

<!-- description: start -->

Prepare new documents using the Flandersqmd extensions.
<!-- description: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("inbo/flandersqmd")
```

A stable version is available on
[r-universe](https://inbo.r-universe.dev/):

``` r
install.packages("flandersqmd", repos = c("https://inbo.r-universe.dev", "https://cloud.r-project.org"))
```

## Example

### Working on a book style report

A book style report provides a structure to write a report in a book
style format. It is based on the `quarto` book format and uses the
`flandersqmd-book` extension. The available output formats are pdf and
html.

Though optional, we strongly recommend to create a `checklist` project
first. For more information on how to create a `checklist` project, see
the
[`checklist`](https://inbo.github.io/checklist/articles/getting_started_project.html)
package documentation.

``` r
library(checklist)
# Where to store the checklist project
path <- tempfile()
dir.create(path)
# create the checklist project
create_project(path = path, project = "flandersqmd-book")
```

Then generate a skeleton for the report using `create_report()`. The
function will guide you interactively through the process of setting up
a report. Once the setup is completed the report will automatically be
rendered in both html and pdf format. The respective output files are
located in the output folder of the report folder. The function will
also create an RStudio project file in the report folder. When you run
the function from RStudio, the project will be opened automatically in a
new session.

``` r
# Create a report skeleton
library(flandersqmd)
create_report(file.path(path, "flandersqmd-book"), shortname = "myreport")
```

When starting a new report, you often don’t have all the metadata
available yet. Run `insert_missing_metadata()` to add the required
metadata to the report when it becomes available to you. E.g. the report
number, DOI, …

``` r
insert_missing_metadata(file.path(path, "flandersqmd-book", "myreport"))
```

`flandersqmd` does not create a cover page. You have to provide a cover
page yourself. Use `add_cover()` to add a cover page to the report and
then render the report again to include the cover page. Note that you
can render a draft version of the report without cover page or missing
metadata.

``` r
add_cover(
  report_path = file.path(path, "flandersqmd-book", "myreport"),
  cover_pdf = "path/to/cover.pdf"
)
# Render the report
quarto::quarto_render(file.path(path, "flandersqmd-book", "myreport"))
```

## Workshop

This package contains the slides of a workshop. Run the code below to
render and view the slides.

``` r
system.file("workshop", package = "flandersqmd") |>
  quarto::quarto_render()
system.file("output/index.html", package = "flandersqmd") |>
  browseURL()
```

[^1]: author

[^2]: contact person

[^3]: Research Institute for Nature and Forest (INBO)

[^4]: copyright holder

[^5]: funder
