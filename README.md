[![Build Status](https://travis-ci.org/mchiapello/Ramf.svg?branch=master)](https://travis-ci.org/mchiapello/Ramf)
[![codecov](https://codecov.io/gh/mchiapello/Ramf/branch/master/graph/badge.svg)](https://codecov.io/gh/mchiapello/Ramf)

# The **Ramf** package [pre-release]

## Overview
<img align = "right" src="inst/extdata/Ramf.png" height="200">

A package for arbuscular mycorrhyzal fungi colonization

## Installation


```r
devtools::install_github("mchiapello/Ramf")
```

If you get this error:

"ERROR: dependency 'BiocStyle' is not avalilable for package 'Ramf'"

Please, install the package from [Bioconductor](http://bioconductor.org/):

```r
source("https://bioconductor.org/biocLite.R")
biocLite("BiocStyle")
```

Repeat the Ramf package installation command.

## Usage


```r
## Load library
library(Ramf)

## Read vignette
vignette("Ramf")

## Read data in
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid.csv")
x <- readData(f)

## Summary of the data
sx <- am_summary(x)

## Plot
barx <- am_barplot(x)
boxx <- am_boxplot(x)
dotx <- am_dotplot(x)

## Statistics
sx <- am_stat(x)
sxc <- am_stat(x, methods = "BH")

## Plot with statistics
am_barplot(x, annots = "asterisks")

## Plot with statistics
am_barplot(x, annot = "letters")

## Save summary data
am_save(sx, "My_data") # 2 files will be save: "My_data_per_Sample.csv" and "My_data_per_Replicate.csv"

## Save plot data
am_save(boxx, "RPlot.jpg")
am_save(barx, "RPlot.pdf", unit = "cm", width = 20, height = 20, dpi = 300) # set image unit, dimention and quality
am_save(dotx, "RPlot.png", width = 7, height = 7)
```



If you encounter a bug, please report it on [github](https://github.com/mchiapello/Ramf/issues).

