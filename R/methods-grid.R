#' gsummary object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import dplyr
gsummary <- function(x) UseMethod("gsummary")

#' @export
gsummary.grid <- function(x){
	grid_summary(x)
}

#' gplot object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import tidyr ggplot2
gplot <- function(x) UseMethod("gplot")

#'@export
gplot.grid <- function(x, type = "boxplot", ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	if (type == "boxplot"){
		grid_boxplot(x)
	}
	if (type == "barplot"){
		grid_barplot(x)
		}
}
