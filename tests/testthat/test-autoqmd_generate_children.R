test_that("generated child files contain read-only warning", {
  species <- paste("Iris", levels(iris$Species))
  labels  <- gsub("\\s", ".", tolower(species))

  out <- autoqmd_generate_children(
    species   = species,
    label     = labels,
    template  = "_species_template.qmd",
    child_dir = "child_qmd",
    quiet     = TRUE
  )

  content <- readLines(out[1])

  # First lines should contain the auto-generated warning
  expect_true(any(grepl("AUTO-GENERATED", content)))
  expect_true(any(grepl("DO NOT EDIT", content)))
})

test_that("autoqmd_generate_children works with the real species template", {
  # Define input data: iris species and corresponding labels
  species <- paste("Iris", levels(iris$Species))
  labels  <- gsub("\\s", ".", tolower(species))

  # Generate child QMD files from the real template
  out <- autoqmd_generate_children(
    species   = species,
    label     = labels,
    template  = "_species_template.qmd",
    child_dir = "child_qmd",
    quiet     = TRUE
  )

  # 1. One output file should be generated per species
  expect_length(out, length(species))

  # 2. All generated files must exist on disk
  expect_true(all(file.exists(out)))

  # 3. Template placeholders should be correctly substituted
  #    (i.e. no literal {{species}} remains in the output)
  content <- readLines(out[1])
  expect_true(any(grepl("Iris setosa", content)))
  expect_false(any(grepl("\\{\\{species\\}\\}", content)))
})

test_that("freeze = 'label' reuses real child documents", {
  # Define input data: iris species and deterministic labels
  species <- paste("Iris", levels(iris$Species))
  labels  <- gsub("\\s", ".", tolower(species))

  # First generation: create child documents with freezing enabled
  out1 <- autoqmd_generate_children(
    species   = species,
    label     = labels,
    template  = "_species_template.qmd",
    child_dir = "child_qmd",
    freeze    = "label",
    quiet     = TRUE
  )

  # Record modification times of generated files
  mtimes_before <- file.info(out1)$mtime

  # Ensure filesystem timestamp resolution does not mask changes
  Sys.sleep(1)

  # Second generation: should reuse existing child documents
  out2 <- autoqmd_generate_children(
    species   = species,
    label     = labels,
    template  = "_species_template.qmd",
    child_dir = "child_qmd",
    freeze    = "label",
    quiet     = TRUE
  )

  # Record modification times again
  mtimes_after <- file.info(out2)$mtime

  # 1. Output file paths should be identical
  expect_equal(out1, out2)

  # 2. Modification times should be unchanged (files were reused)
  expect_equal(mtimes_before, mtimes_after)
})

test_that("error when no variables are provided", {
  expect_error(
    autoqmd_generate_children(
      template  = "_species_template.qmd",
      child_dir = "child_qmd"
    ),
    "You must provide at least one named argument"
  )
})

test_that("error when variable lengths differ", {
  expect_error(
    autoqmd_generate_children(
      species   = c("Iris setosa", "Iris versicolor"),
      label     = "iris.setosa",
      template  = "_species_template.qmd",
      child_dir = "child_qmd"
    ),
    "All arguments in ... must have the same length"
  )
})

# Remove child documents folder
unlink("child_qmd", recursive = TRUE)
