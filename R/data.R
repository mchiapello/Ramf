#' Grid system scoring table
#'
#' A dataset containing the data for 4 Samples: one control and three mutants.
#' The scoring system is the Grid one.
#'
#' @format A data frame with 63 rows and 7 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{Replicates}{Sample Replicates}
#'   \item{Samples}{Sample names}
#'   \item{Total}{Number of total fungal structures identified}
#'   \item{Hyphopodia}{Number of hypopodia identified}
#'   \item{IntrHyphae}{Number of interanl hyphae identified}
#'   \item{Arbuscules}{Number of arbuscules identified}
#'   \item{Vesicles}{Number of vesicles identified}
#' }
#' @source PhD Caroline Gutjahr
"example_grid"


#' Trouvelot system scoring table
#'
#' A dataset containing the data for s34 Samples: one control and two mutants.
#' The scoring system is the Trouvelot one.
#'
#' @format A data frame with 1008 rows and 3 variables:
#' \describe{
#'   It is mandatory that the dataset is formatted as follow
#'   \item{Samples}{Sample names}
#'   \item{Replicates}{Sample Replicates}
#'   \item{Scoring}{Scoring value}
#' }
#' @source Mara Novero
"example_trouvelot"
