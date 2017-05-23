---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build Status](https://travis-ci.org/mchiapello/Ramf.svg?branch=master)](https://travis-ci.org/mchiapello/Ramf)
[![codecov.io](https://codecov.io/github/mchiapello/Ramf.svg?branch=master)](https://codecov.io/github/mchiapello/Ramf?branch=master)

# The `Ramf` package



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
```


If you encounter a bug, please report it on [github](https://github.com/mchiapello/Ramf/issues).

