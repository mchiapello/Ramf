library(Ramf)
context("Grid summary")

test_that("Grid_summary", {
  expect_equal(dim(table(example_grid$samples)), dim(am_summary(example_grid)[[2]][,1])[1])
  expect_equal(dim(table(example_grid$replicates, example_grid$samples))[1] * dim(table(example_grid$replicates, example_grid$samples))[2], dim(am_summary(example_grid)[[1]][, 1])[1])
})
