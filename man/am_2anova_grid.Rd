% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{am_2anova_grid}
\alias{am_2anova_grid}
\title{am_2anova_grid object.}
\usage{
am_2anova_grid(x, col = c("Total", "Hyphopodia", "IntrHyphae",
                           "Arbuscules", "Vesicles"), ...)
}
\arguments{
\item{x}{dataset containing Trouvelot or Grid data}

\item{col}{contains the column to test. Should be one among: "Total", 
"Hyphopodia", "IntrHyphae", "Arbuscules", "Vesicles"}

\item{...}{ignored}
}
\description{
The dataset should be prepared in order to have a column called
             "trt" that contains the treatments. If the column is not present
             the function will fail.
}
\examples{
x <- data.frame(Samples = c("Low_phosphate", "Low_phosphate",
                           "Low_phosphate", "Low_phosphate",
                           "Medium_phosphate", "Medium_phosphate",
                           "Medium_phosphate", "Medium_phosphate",
                           "High_phosphate", "High_phosphate",
                           "High_phosphate", "High_phosphate"),
                trt = c("Sand", "Sand", "Soil", "Soil",
                        "Sand", "Sand", "Soil", "Soil",
                        "Sand", "Sand", "Soil", "Soil"),
                Total = c(88, 95, 87, 74, 95, 93, 80, 79, 72, 52, 80,
                          53))
am_2anova_grid(x, col = "Total")
}
