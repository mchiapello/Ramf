##' Reads data
##'
##' This funtion reads trouvelot and grid data
##' 
##' @title Read colonization files.
##' @param infile the name of the file which the data are to be read from.
##' @return an object of class \code{grid} or \code{trouvelot}.
##' @export
##' @author Marco Chiapello <mc983@cam.ac.uk>
##' @keywords IO, file
##' @import methods utils stats readr

readData <- function(infile){
	x <- suppressMessages(read_csv(infile))
	## Genaral checks
	if (any(is.na(x))) {
			stop("The datasets contains NAs")
	}
	if (dim(x)[2] != 3 & dim(x)[2] != 7){
		x <- NULL
		stop("Incorrect dimentions")
	}
	## Trouvelot specific checks
	if (dim(x)[2] == 3){
		ttest1 <- TRUE
	}
	tnames <- c("scoring", "replicates", "samples")
	if (all(names(x) == tnames)){
		ttest2 <- TRUE
	} else {
		tmp <- names(x)[which(names(x) != tnames)]
		if (length(tmp) != 0){
			for (i in length(tmp)){
			message(paste("The column header ","'", names(x)[i], "' ", "should be ", tnames[i], "\n", sep = ""))
			}
		}
		x <- NULL
		stop("I can not import the data")
	}
	if (ttest1 == TRUE & ttest2 == TRUE){
		### dataset validation
		class(x) <- c("trouvelot", class(x))
	}
	if (dim(x)[2] == 7){
		class(x) <- c("grid", class(x))
		### dataset validation
		list <- c(rep("character", 2), rep("integer", 5))
				for (i in seq_along(list)){
					if (class(x[[i]]) != list[i]){
						stop(paste(colnames(x[i]), "column does not contain only", list[i]))
					}
				}
	}
	return(x)
}


