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
		class(x) <- c("trouvelot", class(x))
	}
	if (dim(x)[2] == 7){
		class(x) <- c("grid", class(x))
	}
	return(x)
}



