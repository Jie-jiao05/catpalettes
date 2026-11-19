# Install once if needed:
#install.packages("testthat")

library(testthat)
source("R/cat_palettes.R")  # Run from the project's main folder.


# 1. SEARCH: Return all discrete palettes with enough colours.
test_that("search returns all eligible palettes", {
  candidates <- find_palettes(n = 15)

  catalogue <- as.data.frame(paletteer::palettes_d_names)
  eligible <- catalogue[catalogue$length >= 15, ]

  expect_setequal(
    candidates$palette_id,
    paste(eligible$package, eligible$palette, sep = "::")
  )
  expect_true(all(candidates$n_available >= 15))
})


# 2. SELECTION: Assign the supplied colours to the correct categories.
test_that("colours are assigned to the correct categories", {
  colours <- select_palette(
    colours = c("#0072B2", "#E69F00"),
    categories = c("Dog", "Cat")
  )

  expect_named(colours, c("Dog", "Cat"))
  expect_equal(
    unname(grDevices::col2rgb(colours)),
    unname(grDevices::col2rgb(c("#0072B2", "#E69F00")))
  )
})


# 3. WITHOUT PARENT: Generate valid, reproducible grouped colours.
test_that("automatic grouped colours are valid and reproducible", {
  first <- nest_palette(
    categories = c("Labrador", "Poodle", "Siamese"),
    groups = c("Dog", "Dog", "Cat")
  )

  second <- nest_palette(
    categories = c("Labrador", "Poodle", "Siamese"),
    groups = c("Dog", "Dog", "Cat")
  )

  expect_named(first, c("Labrador", "Poodle", "Siamese"))
  expect_false(anyNA(first))
  expect_no_error(grDevices::col2rgb(first))
  expect_identical(first, second)
  expect_equal(
    unname(attr(first, "groups")),
    c("Dog", "Dog", "Cat")
  )
})


# 4. WITH PARENT: Preserve the parent colour for a single child.
# Groups with multiple children should receive valid colours and a report.
test_that("nesting uses the supplied parent mapping", {
  colours <- nest_palette(
    parent = c(Dog = "#0072B2", Cat = "#E69F00"),
    categories = c("Labrador", "Poodle", "Siamese"),
    groups = c("Dog", "Dog", "Cat")
  )

  expect_named(colours, c("Labrador", "Poodle", "Siamese"))
  expect_no_error(grDevices::col2rgb(colours))
  expect_equal(
    unname(grDevices::col2rgb(colours["Siamese"])),
    unname(grDevices::col2rgb("#E69F00"))
  )

  report <- attr(colours, "report")
  expect_setequal(report$group, c("Dog", "Cat"))
  expect_false(anyNA(report$method))
})


# 5. GRAYSCALE: Convert colours correctly without rejecting similar shades.
test_that("grayscale previews display every category", {
  colours <- c(A = "#0072B2", B = "#0072B2", C = "#E69F00")

  preview <- plot_palette(
    colours,
    views = c("original", "grayscale")
  )

  expect_no_error(ggplot2::ggplot_build(preview))

  gray_rows <- preview$data[
    preview$data$view == "grayscale", , drop = FALSE
  ]

  expect_equal(nrow(gray_rows), length(colours))
  expect_setequal(as.character(gray_rows$category), names(colours))

  # True grayscale has equal red, green and blue channel values.
  rgb <- grDevices::col2rgb(gray_rows$colour)
  expect_equal(unname(rgb[1, ]), unname(rgb[2, ]))
  expect_equal(unname(rgb[2, ]), unname(rgb[3, ]))
})


# 6. COMPARISON: The preview limit must not change the search results.
test_that("preview limits do not remove search candidates", {
  candidates <- find_palettes(n = 5)
  original <- candidates

  preview <- plot_palettes(candidates, max_palettes = 2)

  expect_no_error(ggplot2::ggplot_build(preview))
  expect_equal(length(unique(preview$data$palette)), 2L)
  expect_identical(candidates, original)
})


# 7. ASSESSMENT: Report identical colours without modifying them.
test_that("assessment reports low separation without changing colours", {
  colours <- c(Dog = "#777777", Cat = "#777777")
  original <- colours

  assessment <- assess_palette(
    colours, views = "original", threshold = 10
  )

  expect_equal(assessment$min_delta_e, 0)
  expect_false(assessment$meets_threshold)
  expect_identical(colours, original)
})


# 8. EXPORT: Preserve category names, colours and groups in the CSV.
test_that("export preserves the colour mapping", {
  colours <- nest_palette(
    categories = c("Labrador", "Poodle"),
    groups = c("Dog", "Dog")
  )

  file <- tempfile(fileext = ".csv")
  export_palette(colours, file = file)
  saved <- read.csv(file)
  unlink(file)

  expect_equal(saved$category, names(colours))
  expect_equal(saved$group, c("Dog", "Dog"))
  expect_equal(
    unname(grDevices::col2rgb(saved$colour)),
    unname(grDevices::col2rgb(colours))
  )
})


# 9. VALIDATION: Reject invalid requests with clear errors.
test_that("invalid inputs are rejected", {
  expect_error(find_palettes(n = 0), "positive whole number")

  expect_error(
    nest_palette(
      categories = c("Labrador", "Poodle"),
      groups = "Dog"
    ),
    "one non-empty group"
  )

  expect_error(
    nest_palette(
      parent = c(Dog = "#0072B2"),
      categories = "Siamese",
      groups = "Cat"
    ),
    "covering every group"
  )
})
