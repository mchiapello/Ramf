#' am_summary object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import dplyr
am_summary <- function(x, ...) UseMethod("am_summary")

#' @export
am_summary.grid <- function(x){
	grid_summary(x)
}

#' am_plot object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import tidyr ggplot2
am_plot <- function(x, ...) UseMethod("am_plot")

#'@export
am_plot.grid <- function(x, type = NULL){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	if (is.null(type)){
		stop("type should be: boxplot or barplot")
	}
	gt_plot(x, type)
}
