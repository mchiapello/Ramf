##' Reads data
##'
##' This funtion reads trouvelot and grid data
##' 
##' @title Read colonization files in.
##' @param infile the name of the file which the data are to be read from.
##' @param type: grid or trouvelot
##' @examples
##' f <- dirsystem.file"extdata", package = "Ramf", full.names = TRUE, pattern = "grid.csv"
##' x <- readData(f, type = "grid")
##' @return an object of class \code{grid} or \code{trouvelot}.
##' @format A grid dataset should have 7 variables:
##' \describe{
##'   It is mandatory that the dataset is formatted as follow
##'   \item{- replicates:}{Sample replicates}
##'   \item{- samples:}{Sample names}
##'   \item{- Total:}{Number of total fungal structures identified}
##'   \item{- Hyphopodia:}{Number of hypopodia identified}
##'   \item{- IntrHyphae:}{Number of interanl hyphae identified}
##'   \item{- Arbuscule:}{Number of arbuscules identified}
##'   \item{- Vesicle:}{Number of vesicles identified}
##' }
##' A Trouvelot dataset should have 3 variables:
##' \describe{
##'   It is mandatory that the dataset is formatted as follow
##'   \item{- scoring:}{Scoring value.
##'                     The only possible terms in this column are:
##'                     0A0,1A3,2A3,3A3,4A3,5A3,1A2,2A2,3A2,4A2,5A2,1A1,2A1,
##'                     3A1,4A1,5A1,1A0,2A0,3A0,4A0,5A0.
##'                     If 0 is present, it will be converted into 0A0}
##'   \item{- replicates:}{Sample replicates}
##'   \item{- samples:}{Sample names}
##' }
##'
##'          The order of the samples (in the "samples" column) is the order used for plot display. So,
##'          if you like to have a specific order, please sort the original data accordingly
##' @export
##' @author Marco Chiapello <mc983@cam.ac.uk>
##' @keywords IO, file
##' @import methods utils stats readr

readData <- function(infile, type = c("none", "trouvelot", "grid")){
    type <- match.arg(type)
    if (type == "none"){
        stop('You need to specify the data type! Use type = "trouvelot" or type = "grid"')
    }
    x <- suppressMessages(read_csv(infile))
    ## Genaral checks
    if (any(is.na(x))) {
            stop("The datasets contains NAs")
    }
    if (dim(x)[2] < 3){
        x <- NULL
        stop("Incorrect dimentions: the dataset should contain at least 3 columns")
    }
    ## Trouvelot specific checks
    if (type == "trouvelot"){
        tnames <- c("samples", "replicates", "scoring")
        snames <- c("0A0", "1A3", "2A3", "3A3", "4A3", "5A3", "1A2", "2A2", "3A2",
                   "4A2", "5A2", "1A1", "2A1", "3A1", "4A1", "5A1", "1A0", "2A0",
                   "3A0", "4A0", "5A0", "0") 
        if (all(names(x) == tnames)){
            if (length(setdiff(x$scoring, snames)) != 0){
                message(paste("This term in column scoring is wrong:\n",
                              setdiff(x$scoring, snames), "\n", sep = ""))
                message("The only allowed terms are:")
                message(paste(snames, "", sep = ", "))
                #                 message("\n")
            x <- NULL
            stop("The dataset has not been imported")
            }
            x$scoring <- as.character(x$scoring)
            x$replicates <- as.character(x$replicates)
            x$samples <- as.character(x$samples)
            x$scoring <- gsub("^0$", "0A0", x$scoring)
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
    if (type == "grid"){
        gnames <- c("samples", "replicates", "Total", "Hyphopodia", "IntrHyphae",
                    "Arbuscule", "Vesicle")
        if(all(names(x)[1:2] == gnames[1:2]) & 
           all(names(x)[3:length(names(x))] %in% gnames[3:7])){
            x$samples <- as.character(x$samples)
            x$replicates <- as.character(x$replicates)
            if(any(names(x) == "Total")){
                x$Total <- as.numeric(x$Total)
            }
            if(any(names(x) == "Hyphopodia")){
                x$Hyphopodia <- as.numeric(x$Hyphopodia)
            }
            if(any(names(x) == "IntrHyphae")){
                x$IntrHyphae <-  as.numeric(x$IntrHyphae)
            }
            if(any(names(x) == "Arbuscule")){
                x$Arbuscule <- as.numeric(x$Arbuscule)
            }
            if(any(names(x) == "Vescicle")){
                x$Vesicle <- as.numeric(x$Vesicle)
            }
                class(x) <- c("grid", class(x))
        } else {
            gtmp <- which(!(names(x) %in% gnames))
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
