% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{am_stat}
\alias{am_stat}
\title{am_stat object.}
\usage{
am_stat(x, method = c("none","holm","hommel", "hochberg",
               "bonferroni", "BH", "BY", "fdr"),
               ...)
}
\arguments{
\item{x}{dataset containing Trouvelot or Grid data}

\item{method}{adjusts the p-value for multiple comparisons  (see p.adjust)
The default is no adjustment for multiple comparisons.}

\item{...}{ignored}
}
\description{
am_stat object.
}
\examples{
am_stat(example_grid)
am_stat(example_trouvelot)
}
