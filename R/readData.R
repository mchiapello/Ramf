#' Reads data
#'
#' This funtion reads trouvelot and grid data
#' 
#' @title Read colonization files in.
#' @usage readData(infile, type = c("none", "trouvelot", "grid"))
#' @param infile the name of the file which the data are to be read from.
#' @param type grid or trouvelot
#' @examples
#' f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid.csv")
#' x <- readData(f, type = "grid")
#' @return an object of class \code{grid} or \code{trouvelot}.
#' @format A grid dataset should have 7 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{- Samples:}{Sample names}
#'   \item{- Replicates:}{Sample Replicates}
#'   \item{- Total:}{Number of total fungal structures identified}
#'   \item{- Hyphopodia:}{Number of hypopodia identified}
#'   \item{- IntrHyphae:}{Number of interanl hyphae identified}
#'   \item{- Arbuscules:}{Number of arbuscules identified}
#'   \item{- Vesicles:}{Number of vesicles identified}
#' }
#' A Trouvelot dataset should have 3 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{- Samples:}{Sample names}
#'   \item{- Replicates:}{Sample Replicates}
#'   \item{- Scoring:}{Scoring value.
#'                     The only possible terms in this column are:
#'                     0A0,1A3,2A3,3A3,4A3,5A3,1A2,2A2,3A2,4A2,5A2,1A1,2A1,
#'                     3A1,4A1,5A1,1A0,2A0,3A0,4A0,5A0.
#'                     If 0 is present, it will be converted into 0A0}
#' }
#'
#'          The order of the Samples (in the "Samples" column) is the order used for plot display. So,
#'          if you like to have a specific order, please sort the original data accordingly
#' @export
#' @author Marco Chiapello <mc983@cam.ac.uk>
#' @keywords IO, file
#' @import methods utils stats readr
readData <- function(infile, type = c("none", "trouvelot", "grid")) UseMethod("readData")

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
        tnames <- c("Samples", "Replicates", "Scoring")
        snames <- c("0A0", "1A3", "2A3", "3A3", "4A3", "5A3", "1A2", "2A2", "3A2",
                   "4A2", "5A2", "1A1", "2A1", "3A1", "4A1", "5A1", "1A0", "2A0",
                   "3A0", "4A0", "5A0", "0") 
        if (all(names(x) == tnames)){
            if (length(setdiff(x$Scoring, snames)) != 0){
                message(paste("This term in column Scoring is wrong:\n",
                              setdiff(x$Scoring, snames), "\n", sep = ""))
                message("The only allowed terms are:")
                message(paste(snames, "", sep = ", "))
                #                 message("\n")
            x <- NULL
            stop("The dataset has not been imported")
            }
            x$Scoring <- as.character(x$Scoring)
            x$Replicates <- as.character(x$Replicates)
            x$Samples <- as.character(x$Samples)
            x$Scoring <- gsub("^0$", "0A0", x$Scoring)
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
        gnames <- c("Samples", "Replicates", "Total", "Hyphopodia", "IntrHyphae",
                    "Arbuscules", "Vesicles")
        if(all(names(x)[1:2] == gnames[1:2]) & 
           all(names(x)[3:length(names(x))] %in% gnames[3:7])){
            x$Samples <- as.character(x$Samples)
            x$Replicates <- as.character(x$Replicates)
            if(any(names(x) == "Total")){
                x$Total <- as.numeric(x$Total)
            }
            if(any(names(x) == "Hyphopodia")){
                x$Hyphopodia <- as.numeric(x$Hyphopodia)
            }
            if(any(names(x) == "IntrHyphae")){
                x$IntrHyphae <-  as.numeric(x$IntrHyphae)
            }
            if(any(names(x) == "Arbuscules")){
                x$Arbuscules <- as.numeric(x$Arbuscules)
            }
            if(any(names(x) == "Vescicles")){
                x$Vesicles <- as.numeric(x$Vesicles)
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
			if(all(names(x)[1:2] != c("Samples", "Replicates"))){
                message(paste("The column headers ('Samples' or 'Replicates') are not in the correct order!"))
            x <- NULL
            stop("The dataset has not been imported")
			}
        }
    }
    return(x)
}

#' Reads datatime 
#'
#' This funtion reads trouvelot and grid data time
#' 
#' @title Read colonization files in.
#' @usage readDataTime(infile, type = c("none", "trouvelot", "grid"))
#' @param infile the name of the file which the data are to be read from.
#' @param type grid or trouvelot
#' @examples
#' f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "gridTime.csv")
#' x <- readDataTime(f, type = "grid")
#' @return an object of class \code{gridTime} or \code{trouvelotTime}.
#' @format A grid dataset should have 7 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{- Samples:}{Sample names}
#'   \item{- Replicates:}{Sample Replicates}
#'   \item{- time:}{When the sample have been collected}
#'   \item{- Total:}{Number of total fungal structures identified}
#'   \item{- Hyphopodia:}{Number of hypopodia identified}
#'   \item{- IntrHyphae:}{Number of interanl hyphae identified}
#'   \item{- Arbuscule:}{Number of arbuscules identified}
#'   \item{- Vesicle:}{Number of vesicles identified}
#' }
#' A Trouvelot dataset should have 3 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{- Samples:}{Sample names}
#'   \item{- Replicates:}{Sample Replicates}
#'   \item{- time:}{When the sample have been collected}
#'   \item{- Scoring:}{Scoring value.
#'                     The only possible terms in this column are:
#'                     0A0,1A3,2A3,3A3,4A3,5A3,1A2,2A2,3A2,4A2,5A2,1A1,2A1,
#'                     3A1,4A1,5A1,1A0,2A0,3A0,4A0,5A0.
#'                     If 0 is present, it will be converted into 0A0}
#' }
#'
#'          The order of the Samples (in the "Samples" column) is the order used for plot display. So,
#'          if you like to have a specific order, please sort the original data accordingly
#' @export
#' @author Marco Chiapello <mc983@cam.ac.uk>
#' @keywords IO, file
#' @import methods utils stats readr
readDataTime <- function(infile, type = c("none", "trouvelot", "grid")) UseMethod("readDataTime")

readDataTime <- function(infile, type = c("none", "trouvelot", "grid")){
    type <- match.arg(type)
    if (type == "none"){
        stop('You need to specify the data type! Use type = "trouvelot" or type = "grid"')
    }
    x <- suppressMessages(read_csv(infile))
    ## Genaral checks
    if (any(is.na(x))) {
            stop("The datasets contains NAs")
    }
    if (names(x)[3] != "time"){
        x <- NULL
        stop("Incorrect dimentions: the 3th column of dataset should be labeled: 'time'")
    }
    if (dim(x)[2] < 4){
        x <- NULL
        stop("Incorrect dimentions: the dataset should contain at least 3 columns")
    }
################################################################################
################################################################################
    ## Trouvelot specific checks
    if (type == "trouvelot"){
        tnames <- c("Samples", "Replicates", "Scoring")
        snames <- c("0A0", "1A3", "2A3", "3A3", "4A3", "5A3", "1A2", "2A2", "3A2",
                   "4A2", "5A2", "1A1", "2A1", "3A1", "4A1", "5A1", "1A0", "2A0",
                   "3A0", "4A0", "5A0", "0") 
        if (all(names(x) == tnames)){
            if (length(setdiff(x$Scoring, snames)) != 0){
                message(paste("This term in column Scoring is wrong:\n",
                              setdiff(x$Scoring, snames), "\n", sep = ""))
                message("The only allowed terms are:")
                message(paste(snames, "", sep = ", "))
                #                 message("\n")
            x <- NULL
            stop("The dataset has not been imported")
            }
            x$Scoring <- as.character(x$Scoring)
            x$Replicates <- as.character(x$Replicates)
            x$Samples <- as.character(x$Samples)
            x$Scoring <- gsub("^0$", "0A0", x$Scoring)
            class(x) <- c("trouvelotTime", class(x))
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
################################################################################
################################################################################
    ## Grid specific checks
    if (type == "grid"){
        gnames <- c("Samples", "Replicates", "time", "Total", "Hyphopodia", "IntrHyphae",
                    "Arbuscule", "Vesicle")
        if(all(names(x)[1:3] == gnames[1:3]) & 
           all(names(x)[4:length(names(x))] %in% gnames[4:8])){
            x$Samples <- as.character(x$Samples)
            x$Replicates <- as.character(x$Replicates)
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
                class(x) <- c("gridTime", class(x))
        } else {
            gtmp <- which(!(names(x) %in% gnames))
            if (length(gtmp) != 0){
                message(paste("The column header (",gtmp, ") ", "'", names(x)[gtmp],
                              "' is not correct!\nIt should be one of these: ",
                              "Total, Hyphopodia, IntrHyphae, Arbuscule, Vesicle"
							  , "\n", sep = ""))
            x <- NULL
            stop("The dataset has not been imported")
            }
			if(all(names(x)[1:2] != c("Samples", "Replicates"))){
                message(paste("The column headers ('Samples' or 'Replicates') are not in the correct order!"))
            x <- NULL
            stop("The dataset has not been imported")
			}
        }
    }
    return(x)
}

