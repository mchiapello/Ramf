[![codecov](https://codecov.io/gh/mchiapello/Ramf/branch/master/graph/badge.svg)](https://codecov.io/gh/mchiapello/Ramf)
[![Life cycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/)
![GitHub Release Date](https://img.shields.io/github/release-date/mchiapello/Ramf?style=plastic)
![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/mchiapello/Ramf?sort=semver&style=plastic)
![GitHub last commit](https://img.shields.io/github/last-commit/mchiapello/Ramf?style=plastic)


# The **Ramf** package <img src="man/figures/logo.png" align="right" alt="" width="120" />
A package for arbuscular mycorrhyzal fungi colonization

## Citation
If you use the package, please cite it:

```
Marco Chiapello , Debatosh Das, Caroline Gutjahr. Ramf: An open-source R package for statistical analysis and display of quantitative root colonization by arbuscular mycorrhiza fungi. _Frontiers Plant Science_. **In press**.
```

## Installation
```r
devtools::install_github("mchiapello/Ramf")
```

If you get this error:

"ERROR: dependency 'BiocStyle' is not avalilable for package 'Ramf'"

Please, install the package from [Bioconductor](http://bioconductor.org/):

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("BiocStyle")
```

Repeat the Ramf package installation command.

## Simple usage
Read the Reference page for more information.


```r
## Load library
library(Ramf)

## Read data in
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid.csv")
x <- readData(f, type = "grid")

## Summary of the data
am_summary(x)

## Plot
am_barplot(x)
am_boxplot(x)
am_dotplot(x)

# Plot with different display
am_barplot2(x)
am_boxplot2(x)
am_dotplot2(x)

## Statistics
am_stat(x)
am_stat(x, methods = "BH")

## Plot with statistics
am_barplot(x, annots = "asterisks")

## Plot with statistics
am_barplot(x, annot = "letters")

## Save summary data
am_save(am_summary(x), "My_data") # 2 files will be save: "My_data_per_Sample.csv" and "My_data_per_Replicate.csv"

## Save plot data
am_save(am_dotplot(x), "RPlot.jpg")
am_save(am_dotplot(x), "RPlot.pdf", unit = "cm", width = 20, height = 20, dpi = 300) # set image unit, dimention and quality
am_save(am_dotplot(x), "RPlot.png", width = 7, height = 7)
```



If you encounter a bug, please report it on [github](https://github.com/mchiapello/Ramf/issues).

