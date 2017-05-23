#' Grid system scoring table
#'
#' A dataset containing the data for 4 samples: one control and three mutants.
#' The scoring system is the Grid one.
#'
#' @format A data frame with 63 rows and 7 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{replicates}{Sample replicates}
#'   \item{samples}{Sample names}
#'   \item{Total}{Number of total fungal structures identified}
#'   \item{Hyphopodia}{Number of hypopodia identified}
#'   \item{IntrHyphae}{Number of interanl hyphae identified}
#'   \item{Arbuscule}{Number of arbuscules identified}
#'   \item{Vesicles}{Number of vesicles identified}
#' }
#' @source PhD Caroline Gutjahr
"example_grid"


#' Trouvelot system scoring table
#'
#' A dataset containing the data for s34 samples: one control and two mutants.
#' The scoring system is the Trouvelot one.
#'
#' @format A data frame with 1008 rows and 3 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{scoring}{Scoring value}
#'   \item{replicates}{Sample replicates}
#'   \item{samples}{Sample names}
#' }
#' @source Mara Novero
"example_trouvelot"
