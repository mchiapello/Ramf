## ----env, echo=FALSE, warnings=FALSE-------------------------------------
library("Ramf")
library("BiocStyle")

## ----loadData------------------------------------------------------------
# Grid
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid.csv")
gr <- readData(f)
# Trouvelot
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "trouvelot.csv")
tr <- readData(f) 


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
am_save(grs)
am_save(trs)
am_save(p1, "barplor_grid.pdf")
am_save(p4, "boxplor_trouvelot.pdf")

