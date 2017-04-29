##' Reads data
##'
##' This funtion reads trouvelot and grid data
##' 
##' @title Read colonization files.
##' @param infile the name of the file which the data are to be read from.
##' @return an instance of \code{grid} or \code{trouvelot}.
##' @export
##' @examples
##' f <- dir(system.file("extdata",package="Ramf"))
##' f
##' aa <- readData(f[1])
##' aa
##' @author Marco Chiapello <mc983@cam.ac.uk>
##' @keywords IO, file
##' @import methods utils stats readr

readData <- function(infile){
	x <- suppressMessages(read_csv(infile))
	if (dim(x)[2] == 3){
		### dataset validation
		if (any(is.na(x))) {
				stop("The datasets contains NAs")
		}
		#         list <- c(rep("character", 2), rep("integer", 5))
		#         for (i in seq_along(list)){
		#             if (class(x[[i]]) != list[i]){
		#                 stop(paste(colnames(x[i]), "column does not contain only", list[i]))
		#             }
		class(x) <- c("trouvelot", class(x))
		}
	if (dim(x)[2] == 7){
		class(x) <- c("grid", class(x))
		### dataset validation
	}
	if (dim(x)[2] != 3 & dim(x)[2] != 7){
		x <- NULL
		stop("Incorrect dimentions")
	}
	return(x)
}


