library(Ramf)
context("Class grid table")

test_that("Grid_table", {
  expect_equal(class(example_grid)[1], "grid")
  expect_equal(class(example_grid)[2], "tbl_df")
  expect_equal(class(example_grid)[3], "tbl")
  expect_equal(class(example_grid)[4], "data.frame")
})

context("Class grid summary")

test_that("Grid_summary", {
  expect_equal(class(am_summary(example_grid))[1], "am_summary")
  expect_equal(class(am_summary(example_grid))[2], "list")
})

context("Class trouvelot table")

test_that("Trouvelot_table", {
  expect_equal(class(example_trouvelot)[1], "trouvelot")
  expect_equal(class(example_trouvelot)[2], "tbl_df")
  expect_equal(class(example_trouvelot)[3], "tbl")
  expect_equal(class(example_trouvelot)[4], "data.frame")
})

context("Class trouvelot summary")

test_that("Trouvelot_summary", {
  expect_equal(class(am_summary(example_trouvelot))[1], "am_summary")
  expect_equal(class(am_summary(example_trouvelot))[2], "list")
})

context("Class grid plot")

test_that("Grid_plot", {
  expect_equal(class(am_barplot(example_grid))[1], "gg")
  expect_equal(class(am_barplot(example_grid))[2], "ggplot")
  expect_equal(class(am_boxplot(example_grid))[1], "gg")
  expect_equal(class(am_boxplot(example_grid))[2], "ggplot")
})

context("Class trouvelot plot")

test_that("Trouvelot_plot", {
  expect_equal(class(am_barplot(example_trouvelot))[1], "gg")
  expect_equal(class(am_barplot(example_trouvelot))[2], "ggplot")
  expect_equal(class(am_boxplot(example_trouvelot))[1], "gg")
  expect_equal(class(am_boxplot(example_trouvelot))[2], "ggplot")
})
