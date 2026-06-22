test_that("single file input returns a dependency stamp", {
  skip_if_not(file.exists("_species_template.qmd"))

  res <- autoqmd_dependency_stamp("_species_template.qmd")

  expect_type(res, "character")
  expect_length(res, 1)

  # Basic structure
  expect_match(res, "^<!-- DEPENDENCY-MTIME:")
  expect_match(res, "_species_template\\.qmd")
})


test_that("multiple file input returns combined dependency stamp", {
  files <- c(
    "_species_template.qmd",
    "iris_chapter.qmd",
    "test-autoqmd_dependency_stamp.R"
  )

  skip_if_not(all(file.exists(files)))

  res <- autoqmd_dependency_stamp(files)

  expect_type(res, "character")
  expect_length(res, 1)

  # All basenames should be present
  for (f in basename(files)) {
    expect_match(res, f)
  }

  # Separator between entries
  expect_match(res, ";")
})


test_that("quiet = TRUE suppresses warnings for missing files", {
  expect_no_warning(
    autoqmd_dependency_stamp(
      c("this_file_does_not_exist.qmd"),
      quiet = TRUE
    )
  )
})


test_that("quiet = FALSE warns for missing files", {
  expect_warning(
    autoqmd_dependency_stamp(
      c("this_file_does_not_exist.qmd"),
      quiet = FALSE
    ),
    "do not exist"
  )
})


test_that("wrong input types are rejected", {

  # file must be character
  expect_error(
    autoqmd_dependency_stamp(123),
    "`file` must be one or more file paths"
  )

  # quiet must be scalar logical
  expect_error(
    autoqmd_dependency_stamp("_species_template.qmd", quiet = "yes"),
    "`quiet` must be a scalar logical"
  )

  expect_error(
    autoqmd_dependency_stamp("_species_template.qmd", quiet = c(TRUE, FALSE)),
    "`quiet` must be a scalar logical"
  )
})
