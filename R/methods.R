#' am_summary object.
#' 
#' @param x dataset containing Trouvelot or Grid data
#' @examples
#' am_summary(example_grid)
#' am_summary(example_trouvelot)
#' @export
#' @import BiocStyle
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr tally
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
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

#' am_save object.
#' 
#' @param x summary or plot data from Trouvelot or Grid dataset
#' @param filename path where save the file
#' @param ... ignored
#' @seealso `ggsave` from ggplot2 package
#' @examples
#' am_save(am_summary(example_grid))
#' am_save(am_summary(example_trouvelot))
#' am_save(am_barplot(example_trouvelot))
#' @export
am_save <- function(x, filename, ...) UseMethod("am_save")

#' @export
am_save.am_summary <- function(x, filename = "Summary", ...){
	write.csv(x[[1]], paste0(filename, "_per_Replicate.csv"), row.names = FALSE)
	write.csv(x[[2]], paste0(filename, "_per_Sample.csv"), row.names = FALSE)
}

#' @export
#' @importFrom ggplot2 ggsave
am_save.am_plot <- function(x, filename = "Rplot.pdf", ...){
	ggsave(plot = x, filename, ...)
}
