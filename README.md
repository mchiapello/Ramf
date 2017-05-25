<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build Status](https://travis-ci.org/mchiapello/Ramf.svg?branch=master)](https://travis-ci.org/mchiapello/Ramf)
[![codecov](https://codecov.io/gh/mchiapello/Ramf/branch/master/graph/badge.svg)](https://codecov.io/gh/mchiapello/Ramf)

# The **Ramf** package [pre-release]



## Overview

A package for arbuscular mycorrhyzal fungi colonization

## Installation


```r
devtools::install_github("mchiapello/Ramf")
```

## Usage


```r
## Read data in
x <- readData("inst/extdata/grid.csv")

## Summary of the data
am_summary(x)

## Plot
am_barplot(x)
am_boxplot(x)

## Save data
am_save(am_summary(x))
```



If you encounter a bug, please report it on [github](https://github.com/mchiapello/Ramf/issues).

