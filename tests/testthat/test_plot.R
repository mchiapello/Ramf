library(Ramf)
context("GRID")

test_that("coordinates", {
  expect_equal(class(am_barplot(example_grid)$coordinates)[1], "CoordCartesian")
  expect_equal(class(am_barplot(example_grid)$coordinates)[2], "Coord")
  expect_equal(class(am_barplot(example_grid)$coordinates)[3], "ggproto")
  expect_equal(class(am_boxplot(example_grid)$coordinates)[1], "CoordCartesian")
  expect_equal(class(am_boxplot(example_grid)$coordinates)[2], "Coord")
  expect_equal(class(am_boxplot(example_grid)$coordinates)[3], "ggproto")
  expect_equal(class(am_dotplot(example_grid)$coordinates)[1], "CoordCartesian")
  expect_equal(class(am_dotplot(example_grid)$coordinates)[2], "Coord")
  expect_equal(class(am_dotplot(example_grid)$coordinates)[3], "ggproto")
})

test_that("facet", {
  expect_equal(class(am_barplot(example_grid)$facet)[1], "FacetNull")
  expect_equal(class(am_barplot(example_grid)$facet)[2], "Facet")
  expect_equal(class(am_barplot(example_grid)$facet)[3], "ggproto")
  expect_equal(class(am_boxplot(example_grid)$facet)[1], "FacetNull")
  expect_equal(class(am_boxplot(example_grid)$facet)[2], "Facet")
  expect_equal(class(am_boxplot(example_grid)$facet)[3], "ggproto")
  expect_equal(class(am_dotplot(example_grid)$facet)[1], "FacetNull")
  expect_equal(class(am_dotplot(example_grid)$facet)[2], "Facet")
  expect_equal(class(am_dotplot(example_grid)$facet)[3], "ggproto")
})

test_that("layers", {
  expect_equal(class(am_barplot(example_grid)$layers)[1], "list")
  expect_equal(class(am_barplot(example_grid)$layers[[1]]$geom)[1], "GeomCol")
  expect_equal(class(am_barplot(example_grid)$layers[[1]]$geom)[2], "GeomRect")
  expect_equal(class(am_barplot(example_grid)$layers[[1]]$geom)[3], "Geom")
  expect_equal(class(am_barplot(example_grid)$layers[[1]]$geom)[4], "ggproto")
  expect_equal(class(am_boxplot(example_grid)$layers[[1]]$geom)[1], "GeomBoxplot")
  expect_equal(class(am_boxplot(example_grid)$layers[[1]]$geom)[2], "Geom")
  expect_equal(class(am_boxplot(example_grid)$layers[[1]]$geom)[3], "ggproto")
  expect_equal(class(am_dotplot(example_grid)$layers[[1]]$geom)[1], "GeomPoint")
  expect_equal(class(am_dotplot(example_grid)$layers[[1]]$geom)[2], "Geom")
  expect_equal(class(am_dotplot(example_grid)$layers[[1]]$geom)[3], "ggproto")
})

test_that("theme", {
  expect_equal(am_barplot(example_grid)$theme$plot.title$size, 19)
  expect_equal(am_barplot(example_grid)$theme$plot.title$vjust, 1)
  expect_equal(am_barplot(example_grid)$theme$plot.title$colour, NULL)
  expect_equal(am_barplot(example_grid)$theme$panel.background$fill , "white")
  expect_equal(am_barplot(example_grid)$theme$panel.grid.major$colour , "grey92")
  expect_equal(am_barplot(example_grid)$theme$panel.grid.minor$colour , "grey92")
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
  expect_equal(class(am_barplot(example_grid))[1], "am_plot")
  expect_equal(class(am_barplot(example_grid))[2], "gg")
  expect_equal(class(am_barplot(example_grid))[3], "ggplot")
  expect_equal(class(am_boxplot(example_grid))[1], "am_plot")
  expect_equal(class(am_boxplot(example_grid))[2], "gg")
  expect_equal(class(am_boxplot(example_grid))[3], "ggplot")
})

context("Class trouvelot plot")

test_that("Trouvelot_plot", {
  expect_equal(class(am_barplot(example_trouvelot))[1], "am_plot")
  expect_equal(class(am_barplot(example_trouvelot))[2], "gg")
  expect_equal(class(am_barplot(example_trouvelot))[3], "ggplot")
  expect_equal(class(am_barplot(example_trouvelot))[1], "am_plot")
  expect_equal(class(am_boxplot(example_trouvelot))[2], "gg")
  expect_equal(class(am_boxplot(example_trouvelot))[3], "ggplot")
})
