## ----env, echo=FALSE, warnings=FALSE---------------------------------------
library("Ramf")
library("BiocStyle")

## ----loadData--------------------------------------------------------------
# Grid
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid.csv")
gr <- readData(f, type = "grid")
# Grid incomplete
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "grid_incomplete.csv")
gri <- readData(f, type = "grid")
# Trouvelot
f <- dir(system.file("extdata", package = "Ramf"), full.names = TRUE, pattern = "trouvelot.csv")
tr <- readData(f, type = "trouvelot") 

## ----summary---------------------------------------------------------------
grs <- am_summary(gr)
gris <- am_summary(gri)
trs <- am_summary(tr)
grs
gris
trs

## ----stat------------------------------------------------------------------
grst <- am_stat(gr)
grist <- am_stat(gri)
trst <- am_stat(tr)
grst
grist
trst

## ----stat2-----------------------------------------------------------------
am_stat(gr, method = "BH")
am_stat(gri, method = "BH")
am_stat(tr, method = "fdr")

## ----plot------------------------------------------------------------------
am_barplot(gr)
am_boxplot(gr)
am_dotplot(gr)
am_barplot(tr)
am_boxplot(tr)
am_dotplot(tr)

## ----plot2-----------------------------------------------------------------
am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'))
am_dotplot(gr, cbPalette = rep("black", 4))
am_barplot(gr, cbPalette = c('#ca0020', '#f4a582', '#92c5de', '#0571b0'))
am_boxplot(gr, cbPalette = c("#b4ddd4", "#02531d", "#34debb", "#d10f55"))
am_barplot(gri, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'))
am_dotplot(gri, cbPalette = rep("black", 4))
am_barplot(gri, cbPalette = c('#ca0020', '#f4a582', '#92c5de', '#0571b0'))
am_boxplot(gri, cbPalette = c("#b4ddd4", "#02531d", "#34debb", "#d10f55"))

## ----title-----------------------------------------------------------------
am_barplot(gr, main = "Grid plot")
am_barplot(gri, main = "Grid incomplete plot")
am_barplot(tr, main = "Trouvelot plot")

## ----title.position--------------------------------------------------------
library(ggplot2)
am_barplot(gr, main = "Grid plot") + theme(plot.title = element_text(hjust = .5))
am_barplot(tr, main = "Trouvelot plot") + theme(plot.title = element_text(hjust = 1))
am_barplot(gr, main = "Grid plot") + theme(panel.grid.major.y = element_line(size = 3, colour = "grey80"))
am_barplot(gr, main = "Grid plot") + theme(text=element_text(family="Avenir"))
am_barplot(gr, main = "Grid plot") + theme(axis.text = element_text(size = 18))

## ----stat.plot-------------------------------------------------------------
am_barplot(gr, annot = "asterisks", alpha = 0.05)
am_barplot(gri, annot = "asterisks", alpha = 0.05)
am_dotplot(tr, annot = "asterisks", alpha = 0.01)

## ----stat.plot2------------------------------------------------------------
am_barplot(gr, annot = "asterisks", method = "BH")

## ----stat.plot3------------------------------------------------------------
am_barplot(gr, annot = "letters", alpha = 0.05)
am_barplot(gri, annot = "letters", alpha = 0.05)
am_dotplot(tr, annot = "letters", alpha = 0.01)

## ----save------------------------------------------------------------------
# Save the summary
am_save(grs) # Two files will be saved: Summary_per_Replicate.csv and Summary_per_Sample.csv
am_save(trs, "Trouvelot") # Two files will be saved: Trouvelot_per_Replicate.csv and Trouvelot_per_Sample.csv

# Save the statisticas
am_save(am_stat(gr)) # One file called Summary_Stat.csv will be saved
am_save(am_stat(tr, method = "BH"), "Trouvelot") # One file called Trouvelot_stat.csv will be saved

# Save the plots
am_save(am_barplot(gr), "barplor_grid.pdf")
p1 <- am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'), main = "Trouvelot", annot = "letters", method = "BH")
am_save(p1, "dotplor_trouvelot.pdf", width = 21, height = 21, units = "cm", dpi = 300)

# Save jpg plot
am_save(p1, "dotplor_trouvelot.jpg")

