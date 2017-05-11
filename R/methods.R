#' am_summary object.
#' 
#' @param x dataset containing Trouvelot or Grid data
#' @examples
#' am_summary(example_grid)
#' am_summary(example_trouvelot)
#' @export
#' @import dplyr
am_summary <- function(x) UseMethod("am_summary")

#' am_barplot object.
#' 
#' @param x dataset containing Trouvelot or Grid data
#' @param ... ignored
#' @examples
#' am_barplot(example_grid)
#' am_barplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_barplot <- function(x, ...) UseMethod("am_barplot")

#' am_boxplot object.
#' 
#' @param x dataset containing Trouvelot or Grid data
#' @param ... ignored
#' @examples
#' am_boxplot(example_grid)
#' am_boxplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_boxplot <- function(x, ...) UseMethod("am_boxplot")


