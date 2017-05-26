[![Build Status](https://travis-ci.org/mchiapello/Ramf.svg?branch=master)](https://travis-ci.org/mchiapello/Ramf)
[![codecov](https://codecov.io/gh/mchiapello/Ramf/branch/master/graph/badge.svg)](https://codecov.io/gh/mchiapello/Ramf)

# The **Ramf** package [pre-release]

## Overview
<img align = "right" src="inst/extdata/Ramf.png" height="200">
<img align = "right" src="inst/extdata/Ramf2.png" height="200">

A package for arbuscular mycorrhyzal fungi colonization

## Installation


```r
devtools::install_github("mchiapello/Ramf")
```

## Usage


```r
## Load library
library(Ramf)

## Read data in
x <- readData("inst/extdata/grid.csv")

## Summary of the data
sx <- am_summary(x)

## Plot
barx <- am_barplot(x)
boxx <- am_boxplot(x)

## Save summary data
am_save(sx, "My_data") # 2 files will be save: "My_data_per_Sample.csv" and "My_data_per_Replicate.csv"

## Save plot data
am_save(boxx, "RPlot.jpg")
am_save(barx, "RPlot.pdf", unit = "cm", width = 20, height = 20, dpi = 300) # set image unit, dimention and quality
```



If you encounter a bug, please report it on [github](https://github.com/mchiapello/Ramf/issues).

