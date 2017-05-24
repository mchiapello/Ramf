## ----env, echo=FALSE, warnings=FALSE-------------------------------------
library("Ramf")
library("BiocStyle")

## ----loadData------------------------------------------------------------
# Grid
gr <- readData("../inst/extdata/grid.csv") 
# Trouvelot
tr <- readData("../inst/extdata/trouvelot.csv") 


## ----summary-------------------------------------------------------------
grs <- am_summary(gr)
trs <- am_summary(tr)
grs
trs

## ----plot----------------------------------------------------------------
p1 <- am_barplot(gr)
p2 <- am_boxplot(gr)
p3 <- am_barplot(tr)
p4 <- am_boxplot(tr)

## ----save----------------------------------------------------------------
am_save(grs, "example_grid.csv")
am_save(trs, "example_trouvelot.csv")
am_save(p1, "barplor_grid.pdf")
am_save(p4, "boxplor_trouvelot.pdf")

