library(Ramf)
context("Read data table")

test_that("Grid table", {
  expect_equal(class(readData("../../inst/extdata/grid.csv"))[1], "grid")
  expect_equal(class(readData("../../inst/extdata/grid.csv"))[2], "tbl_df")
  expect_equal(class(readData("../../inst/extdata/grid.csv"))[3], "tbl")
  expect_equal(class(readData("../../inst/extdata/grid.csv"))[4], "data.frame")
})

test_that("Trouvelot table", {
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv"))[1], "trouvelot")
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv"))[2], "tbl_df")
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv"))[3], "tbl")
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv"))[4], "data.frame")
})


test_that("Grid_table column names", {
  expect_equal(names(readData("../../inst/extdata/grid.csv")), c("Replicates",
																 "Samples",
																 "Total",
																 "Hyphopodia",
																 "IntrHyphae",
																 "Arbuscule",
																 "Vesicles"))
})

test_that("Trouvelot_table column names", {
  expect_equal(names(readData("../../inst/extdata/trouvelot.csv")),
			   c("scoring", "Replicates", "Samples"))
})

test_that("Grid_table column classes", {
  expect_equal(class(readData("../../inst/extdata/grid.csv")$Replicates), "character")
  expect_equal(class(readData("../../inst/extdata/grid.csv")$Samples), "character")
  expect_equal(class(readData("../../inst/extdata/grid.csv")$Total), "numeric")
  expect_equal(class(readData("../../inst/extdata/grid.csv")$Hyphopodia), "numeric")
  expect_equal(class(readData("../../inst/extdata/grid.csv")$IntrHyphae), "numeric")
  expect_equal(class(readData("../../inst/extdata/grid.csv")$Arbuscule), "numeric")
  expect_equal(class(readData("../../inst/extdata/grid.csv")$Vesicles), "numeric")
})

test_that("Trouvelot_table column classes", {
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv")$scoring), "character")
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv")$Replicates), "character")
  expect_equal(class(readData("../../inst/extdata/trouvelot.csv")$Samples), "character")
})

test_that("Grid table dimention", {
  expect_equal(dim(readData("../../inst/extdata/grid.csv"))[2], 7)
})

test_that("Trouvelot table dimention", {
  expect_equal(dim(readData("../../inst/extdata/trouvelot.csv"))[2], 3)
})


