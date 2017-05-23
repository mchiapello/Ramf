##' Reads data
##'
##' This funtion reads trouvelot and grid data
##' 
##' @title Read colonization files in.
##' @param infile the name of the file which the data are to be read from.
##' @return an object of class \code{grid} or \code{trouvelot}.
##' @format A grid dataset should have 7 variables:
##' \describe{
##'   It is mandatory that the dataset is formatted as follow
##'   \item{- replicates:}{Sample replicates:}
##'   \item{- samples:}{Sample names:}
##'   \item{- Total:}{Number of total fungal structures identified:}
##'   \item{- Hyphopodia:}{Number of hypopodia identified:}
##'   \item{- IntrHyphae:}{Number of interanl hyphae identified:}
##'   \item{- Arbuscule:}{Number of arbuscules identified:}
##'   \item{- Vesicles:}{Number of vesicles identified:}
##' }
##' A Trouvelot dataset should have 3 variables:
##' \describe{
##'   It is mandatory that the dataset is formatted as follow
##'   \item{- scoring:}{Scoring value}
##'   \item{- replicates:}{Sample replicates}
##'   \item{- samples:}{Sample names}
##' }
##'
##' 		 The order of the samples (in the "samples" column) is the order used for plot display. So,
##' 		 if you like to have a specific order, please sort the original data accordingly
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
	tnames <- c("scoring", "replicates", "samples")
	if (dim(x)[2] == 3){
		if (all(names(x) == tnames)){
			class(x) <- c("trouvelot", class(x))
		} else {
			ttmp <- which(names(x) != tnames)
			if (length(ttmp) != 0){
				message(paste("The column header (",ttmp, ") ", "'", names(x)[ttmp],
							  "' is not correct!\nIt should be: ",
							  tnames[ttmp], "\n", sep = ""))
			x <- NULL
			stop("The dataset has not been imported")
			}
		}
	}
	## Grid specific checks
	gnames <- c("replicates", "samples", "Total", "Hyphopodia", "IntrHyphae",
				"Arbuscule", "Vesicles")
	if (dim(x)[2] == 7){
		if(all(names(x) == gnames)){
			class(x) <- c("grid", class(x))
		} else {
			gtmp <- which(names(x) != gnames)
			if (length(gtmp) != 0){
				message(paste("The column header (",gtmp, ") ", "'", names(x)[gtmp],
							  "' is not correct!\nIt should be: ",
							  gnames[gtmp], "\n\n", sep = ""))
			x <- NULL
			stop("The dataset has not been imported")
			}
		}
	}
	return(x)
}


# it is nor working!!! do a nested if statement
# if (dim == 3){
#     if (names){
#         grid
#     } else {
#         message header
#         x <- NULL
#         stop
#     }
