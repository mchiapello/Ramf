# Ramf
[![Build Status](https://travis-ci.org/mchiapello/Ramf.svg?branch=master)](https://travis-ci.org/mchiapello/Ramf) [![codecov.io](https://codecov.io/github/mchiapello/Ramf.svg?branch=master)](https://codecov.io/github/mchiapello/Ramf?branch=master)

## Overview

A package for arbuscular mycorrhyzal fungi colonization

## Installation

```{r, eval = FALSE}
devtools::install_github("tidyverse/dplyr")
```

## Usage

```{r, eval = FALSE}
## Read data in
x <- readData("grid.csv")

## Summary of the data
am_summary(x)

## Plot
am_barplot(x)
am_boxplot(x)
```


If you encounter a bug, please report it on [github](https://github.com/mchiapello/Ramf/issues).
