# Create a template for a `flandersqmd` report

Create a template for a `flandersqmd` report

## Usage

``` r
create_report(path = ".", reportname, version = "main", shortname)
```

## Arguments

- path:

  The folder in which to create the folder containing the report.
  Defaults to the current working directory. It also creates an RStudio
  project file in the report folder. When ran from RStudio, the project
  will be opened automatically in a new session.

- reportname:

  The folder name of the report. The location of the folder `reportname`
  depends on the content of `path`. When `path` is a
  [`checklist::checklist`](https://inbo.github.io/checklist/reference/checklist.html)
  project, you will find the new report at `path/source/reportname`.
  When `path` is a
  [`checklist::checklist`](https://inbo.github.io/checklist/reference/checklist.html)
  package, you will find the new report at `path/inst/reportname`.
  Otherwise you will find the new report at `path/reportname`.

- version:

  The version of the `flandersqmd-book` extension to use. Defaults to
  `"main"`, which refers to the current version.

- shortname:

  Deprecated. Use `reportname` instead.

## See also

Other utils:
[`inbo_website()`](https://inbo.github.io/flandersqmd/reference/inbo_website.md)
