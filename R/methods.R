#' am_summary object.
#' 
#' @param x dataset containing Trouvelot or Grid data
#' @examples
#' am_summary(example_grid)
#' am_summary(example_trouvelot)
#' @export
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
#' @usage am_barplot(x, cbPalette, leg = "none", main = "Colonization", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param leg this parameter allows to set the legend. Default is "none". The other
#' 			  possible parameters are: "left", "right", "top", "bottom".
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_barplot(example_grid)
#' am_barplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_barplot <- function(x, cbPalette, leg = "none", main = "Colonization", ...) UseMethod("am_barplot")

#' am_boxplot object.
#' 
#' @usage am_boxplot(x, cbPalette, leg = "none", main = "Colonization", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param leg this parameter allows to set the legend. Default is "none". The other
#' 			  possible parameters are: "left", "right", "top", "bottom".
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_boxplot(example_grid)
#' am_boxplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_boxplot <- function(x, cbPalette, leg = "none", main = "Colonization", ...) UseMethod("am_boxplot")

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
