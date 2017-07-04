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
tr

## ----summary-------------------------------------------------------------
grs <- am_summary(gr)
trs <- am_summary(tr)
grs
trs

## ----stat----------------------------------------------------------------
grst <- am_stat(gr)
trst <- am_stat(tr)
grst
trst

## ----stat2---------------------------------------------------------------
am_stat(gr, method = "bh")
am_stat(tr, method = "sidak")

## ----plot----------------------------------------------------------------
am_barplot(gr)
am_boxplot(gr)
am_dotplot(gr)
am_barplot(tr)
am_boxplot(tr)
am_dotplot(tr)

## ----plot2---------------------------------------------------------------
am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'))
am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'), main = "Grid")
# Plot with statistical information
am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'), main = "Grid",
		   stats = "asterisks")
# Statistical test corrected
am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'), main = "Grid",
		   stats = "asterisks", method = "bh")

## ----save----------------------------------------------------------------
# Save the summary
am_save(grs) # Two files will be saved: Summary_per_Replicate.csv and Summary_per_Sample.csv
am_save(trs, "Trouvelot") # Two files will be saved: Trouvelot_per_Replicate.csv and Trouvelot_per_Sample.csv

# Save the statisticas
am_save(am_stat(gr)) # One file called Summary_Stat.csv will be saved
am_save(am_stat(tr, method = "bh"), "Trouvelot") # One file called Trouvelot_stat.csv will be saved

# Save the plots
am_save(am_barplot(gr), "barplor_grid.pdf")
p1 <- am_barplot(gr, cbPalette = c('#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696'), main = "Trouvelot",
		   stats = "asterisks", method = "bh")
am_save(p1, "dotplor_trouvelot.pdf", width = 21, height = 21, units = "cm", dpi = 300)

# Save jpg plot
am_save(p1, "dotplor_trouvelot.jpg")

