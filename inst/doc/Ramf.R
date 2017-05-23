## ----env, echo=FALSE, warnings=FALSE-------------------------------------
library("Ramf")
library("BiocStyle")

## ----loadData------------------------------------------------------------
# Grid
gr <- readData("../inst/extdata/grid.csv") 
# Trouvelot
tr <- readData("../inst/extdata/trouvelot.csv") 


## ----summary-------------------------------------------------------------
am_summary(gr)
am_summary(tr)

## ----plot----------------------------------------------------------------
am_barplot(gr)
am_boxplot(gr)
am_barplot(tr)
am_boxplot(tr)

