library(Ramf)
context("Grid summary")

test_that("Grid_summary", {
  expect_equal(dim(table(example_grid$Samples)), dim(am_summary(example_grid)[[2]][,1])[1])
  expect_equal(dim(table(example_grid$Replicates, example_grid$Samples))[1] * dim(table(example_grid$Replicates, example_grid$Samples))[2], dim(am_summary(example_grid)[[1]][, 1])[1])
})
