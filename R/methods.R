#' am_summary object.
#' 
#' @usage am_summary(x)
#' @param x dataset containing Trouvelot or Grid data
#' @examples
#' am_summary(example_grid)
#' am_summary(example_trouvelot)
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr tally
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr pull
#' @importFrom dplyr tbl_df
#' @importFrom dplyr summarise_if
#' @importFrom dplyr mutate_at
#' @importFrom dplyr funs
#' @importFrom dplyr vars
#' @importFrom dplyr contains
am_summary <- function(x) UseMethod("am_summary")

#' am_barplot object.
#' 
#' @usage am_barplot(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'                  annot_size = 5,
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'          legend = c("right", "left", "top", "bottom"),
#'          main = "Gridline intersect method", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the Samples groupping the Samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param annot_size annotation font size. Default 5.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param legend Indicate the legend position
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_barplot(example_grid)
#' am_barplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_barplot <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
                       annot_size = 5,
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
             legend = c("right", "left", "top", "bottom"),
					   main = "Gridline intersect method", ...) UseMethod("am_barplot")

#' am_barplot2 object.
#' 
#' @usage am_barplot2(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'                  annot_size = 5,
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'                   main = "Colonization", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the Samples groupping the Samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param annot_size annotation font size. Default 5.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_barplot2(example_grid)
#' am_barplot2(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_barplot2 <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
                       annot_size = 5,
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
					   main = "Colonization", ...) UseMethod("am_barplot2")

#' am_boxplot object.
#' 
#' @usage am_boxplot(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'                  annot_size = 5,
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'          legend = c("right", "left", "top", "bottom"),
#'          main = "Gridline intersect method", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the Samples groupping the Samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param annot_size annotation font size. Default 5.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param legend Indicate the legend position
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_boxplot(example_grid)
#' am_boxplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_boxplot <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
                       annot_size = 5,
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
             legend = c("right", "left", "top", "bottom"),
					   main = "Gridline intersect method", ...) UseMethod("am_boxplot")

#' am_boxplot object.
#' 
#' @usage am_boxplot2(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'                  annot_size = 5,
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'                   main = "Colonization", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the Samples groupping the Samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param annot_size annotation font size. Default 5.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_boxplot2(example_grid)
#' am_boxplot2(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_boxplot2 <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
                       annot_size = 5,
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
					   main = "Colonization", ...) UseMethod("am_boxplot2")

#' am_dotplot object.
#' 
#' @usage am_dotplot(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'                  annot_size = 5,
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'          legend = c("right", "left", "top", "bottom"),
#'          main = "Gridline intersect method", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the Samples groupping the Samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param annot_size annotation font size. Default 5.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param legend Indicate the legend position
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_dotplot(example_grid)
#' am_dotplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_dotplot <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
                       annot_size = 5,
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
             legend = c("right", "left", "top", "bottom"),
					   main = "Gridline intersect method", ...) UseMethod("am_dotplot")

#' am_dotplot2 object.
#' 
#' @usage am_dotplot2(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'                  annot_size = 5,
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'                   main = "Colonization", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the Samples groupping the Samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param annot_size annotation font size. Default 5.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param main Plot title. Default "Colonization".
#' @param ... ignored
#' @examples
#' am_dotplot2(example_grid)
#' am_dotplot2(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_dotplot2 <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
                       annot_size = 5,
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
					   main = "Colonization", ...) UseMethod("am_dotplot2")

#' am_stat object.
#' 
#' @usage am_stat(x, method = c("none","holm","hommel", "hochberg",
#'                "bonferroni", "BH", "BY", "fdr"),
#'                ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param method adjusts the p-value for multiple comparisons  (see p.adjust)
#'               The default is no adjustment for multiple comparisons.
#' @param ... ignored
#' @examples
#' am_stat(example_grid)
#' am_stat(example_trouvelot)
#' @export
#' @import agricolae
am_stat <- function(x, method = c("none","holm","hommel", "hochberg",
                                  "bonferroni", "BH", "BY", "fdr"),
                                  ...) UseMethod("am_stat")

#' am_anova_grid object.
#' 
#' @usage am_anova_grid(x, col = c("Total", "Hyphopodia", "IntrHyphae",
#'                            "Arbuscules", "Vesicles"), ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param col contains the column to test. Should be one among: "Total", 
#'            "Hyphopodia", "IntrHyphae", "Arbuscules", "Vesicles"
#' @param ... ignored
#' @examples
#' x <- data.frame(Samples = c("Low_phosphate", "Low_phosphate",
#'                            "Low_phosphate", "Low_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "High_phosphate", "High_phosphate",
#'                            "High_phosphate", "High_phosphate"),
#'                 trt = c("Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil"),
#'                 Total = c(88, 95, 87, 74, 95, 93, 80, 79, 72, 52, 80,
#'                           53))
#' am_anova_grid(x, col = "Total")
#' @export
#' @import agricolae 
am_anova_grid <- function(x, col = c("Total", "Hyphopodia", "IntrHyphae",
                                "Arbuscules", "Vesicles"), 
                     ...) UseMethod("am_anova_grid")


#' am_2anova_grid object.
#' 
#' @usage am_2anova_grid(x, col = c("Total", "Hyphopodia", "IntrHyphae",
#'                            "Arbuscules", "Vesicles"), ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param col contains the column to test. Should be one among: "Total", 
#'            "Hyphopodia", "IntrHyphae", "Arbuscules", "Vesicles"
#' @param ... ignored
#' @description The dataset should be prepared in order to have a column called
#'              "trt" that contains the treatments. If the column is not present
#'              the function will fail.
#' @examples
#' x <- data.frame(Samples = c("Low_phosphate", "Low_phosphate",
#'                            "Low_phosphate", "Low_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "High_phosphate", "High_phosphate",
#'                            "High_phosphate", "High_phosphate"),
#'                 trt = c("Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil"),
#'                 Total = c(88, 95, 87, 74, 95, 93, 80, 79, 72, 52, 80,
#'                           53))
#' am_2anova_grid(x, col = "Total")
#' @export
#' @import agricolae 
am_2anova_grid <- function(x, col = c("Total", "Hyphopodia", "IntrHyphae",
                                "Arbuscules", "Vesicles"), 
                     ...) UseMethod("am_2anova_grid")

########## ANOVA
# One way
#' @export
#' @importFrom graphics plot
am_anova_grid <- function(x, col = c("Total", "Hyphopodia", "IntrHyphae",
                                "Arbuscules", "Vesicles"), ...){
    col <- match.arg(col)
    pv <- kruskal(x[names(x) %in% col], (x$Samples))$statistics$p.chisq
    if(pv < 0.05){
        message(paste0("The pvalue is ", pv, ". There are significant differences in ", col, " feature."))
    } else{
        message(paste0("The pvalue is ", pv, ". There are NOT significant differences in ", col, " feature."))
    }
}

# Two way
#' @export
am_2anova_grid <- function(x, col = c("Total", "Hyphopodia", "IntrHyphae",
                                 "Arbuscules", "Vesicles"), ...){
    col <- match.arg(col)
    if ("trt" %in% names(x)){
    pp <- aov(as.data.frame(x)[,names(x) %in% col] ~ x$Samples + x$trt)
    plot(pp, 1)
    plot(pp, 2)
    pv <- summary(aov(as.data.frame(x)[,names(x) %in% col] ~ x$Samples * x$trt))[[1]][["Pr(>F)"]]
    if(pv[1] < 0.05){
        message(paste0("The pvalue for Samples is ", pv[1], ". There are significant differences in ", col, " feature.\n"))
    } else{
        message(paste0("The pvalue for Samples is ", pv[1], ". There are NOT significant differences in ", col, " feature.\n"))
    }
    if(pv[2] < 0.05){
        message(paste0("The pvalue for treatment is ", pv[2], ". There are significant differences in ", col, " feature.\n"))
    } else{
        message(paste0("The pvalue for treatment is ", pv[2], ". There are NOT significant differences in ", col, " feature\n."))
    }
    if(pv[3] < 0.05){
        message(paste0("The pvalue for Sample-treatment interaction is ", pv[2], ". There are significant differences in ", col, " feature.\n"))
    } else{
        message(paste0("The pvalue for Sample-treatment interaction is ", pv[2], ". There are NOT significant differences in ", col, " feature.\n"))
    }
    } else {
        stop('You do not have the "trt" (treatment) column. Please add it to compute the 2-way ANOVA\n')
    }
}

#' am_anova_trouvelot object.
#' 
#' @usage am_anova_trouvelot(x, col = c("F", "M", "a", "A"), ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param col contains the column to test. Should be one among: "F", "M", "a", "A" 
#' @param ... ignored
#' @examples
#' x <- data.frame(Samples = c("Low_phosphate", "Low_phosphate",
#'                            "Low_phosphate", "Low_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "High_phosphate", "High_phosphate",
#'                            "High_phosphate", "High_phosphate"),
#'                 trt = c("Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil"),
#'                 F = c(88, 95, 87, 74, 95, 93, 80, 79, 72, 52, 80,
#'                           53))
#' am_anova_trouvelot(x, col = "F")
#' @export
#' @import agricolae 
am_anova_trouvelot <- function(x, col = c("F", "M", "a", "A"), 
                     ...) UseMethod("am_anova_trouvelot")


#' am_2anova_trouvelot object.
#' 
#' @usage am_2anova_trouvelot(x, col = c("F", "M", "a", "A"), ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param col contains the column to test. Should be one among: "F", "M", "a", "A" 
#' @param ... ignored
#' @description The dataset should be prepared in order to have a column called
#'              "trt" that contains the treatments. If the column is not present
#'              the function will fail.
#' @examples
#' x <- data.frame(Samples = c("Low_phosphate", "Low_phosphate",
#'                            "Low_phosphate", "Low_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "Medium_phosphate", "Medium_phosphate",
#'                            "High_phosphate", "High_phosphate",
#'                            "High_phosphate", "High_phosphate"),
#'                 trt = c("Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil",
#'                         "Sand", "Sand", "Soil", "Soil"),
#'                 F = c(88, 95, 87, 74, 95, 93, 80, 79, 72, 52, 80,
#'                           53))
#' am_2anova_trouvelot(x, col = "F")
#' @export
#' @import agricolae 
am_2anova_trouvelot <- function(x, col = c("F", "M", "a", "A"), 
                     ...) UseMethod("am_2anova_trouvelot")

########## ANOVA
# One way
#' @export
#' @importFrom graphics plot
am_anova_trouvelot <- function(x, col = c("F", "M", "a", "A"), ...){
    col <- match.arg(col)
    pv <- kruskal(x[names(x) %in% col], (x$Samples))$statistics$p.chisq
    if(pv < 0.05){
        message(paste0("The pvalue is ", pv, ". There are significant differences in ", col, " feature."))
    } else{
        message(paste0("The pvalue is ", pv, ". There are NOT significant differences in ", col, " feature."))
    }
}

# Two way
#' @export
am_2anova_trouvelot <- function(x, col = c("F", "M", "a", "A"), ...){
    col <- match.arg(col)
    if ("trt" %in% names(x)){
    pp <- aov(as.data.frame(x)[,names(x) %in% col] ~ x$Samples + x$trt)
    plot(pp, 1)
    plot(pp, 2)
    pv <- summary(aov(as.data.frame(x)[,names(x) %in% col] ~ x$Samples * x$trt))[[1]][["Pr(>F)"]]
    if(pv[1] < 0.05){
        message(paste0("The pvalue for Samples is ", pv[1], ". There are significant differences in ", col, " feature.\n"))
    } else{
        message(paste0("The pvalue for Samples is ", pv[1], ". There are NOT significant differences in ", col, " feature.\n"))
    }
    if(pv[2] < 0.05){
        message(paste0("The pvalue for treatment is ", pv[2], ". There are significant differences in ", col, " feature.\n"))
    } else{
        message(paste0("The pvalue for treatment is ", pv[2], ". There are NOT significant differences in ", col, " feature\n."))
    }
    if(pv[3] < 0.05){
        message(paste0("The pvalue for Sample-treatment interaction is ", pv[2], ". There are significant differences in ", col, " feature.\n"))
    } else{
        message(paste0("The pvalue for Sample-treatment interaction is ", pv[2], ". There are NOT significant differences in ", col, " feature.\n"))
    }
    } else {
        stop('You do not have the "trt" (treatment) column. Please add it to compute the 2-way ANOVA\n')
    }
}

#' am_save object.
#' 
#' @param x summary, statistics or plot data from Trouvelot or Grid dataset
#' @param filename path where save the file
#' @param ... arguments from ggsave function from ggplot2 package
#' @details
#'    If you save the summary, two files will be saved corresponding to the
#'    summary two tables. The user can name the file but the save function
#'    will append "_per_Replicate.csv" and "_per_Sample.csv".
#'
#'    If you save the statistical summary one file will be saved with the
#'    "_Stat.csv" extention.
#' @seealso `ggsave` from ggplot2 package
#' @examples
#' am_save(am_summary(example_grid))
#' am_save(am_summary(example_trouvelot))
#' am_save(am_barplot(example_trouvelot))
#' am_save(am_stat(example_trouvelot))
#' @export
am_save <- function(x, filename, ...) UseMethod("am_save")

#' @export
am_save.am_summary <- function(x, filename = "Summary", ...){
	write.csv(x[[1]], paste0(filename, "_per_Replicate.csv"), row.names = FALSE)
	write.csv(x[[2]], paste0(filename, "_per_Sample.csv"), row.names = FALSE)
}

#' @export
am_save.am_stat <- function(x, filename = "Summary", ...){
	write.csv(x, paste0(filename, "_Stat.csv"), row.names = FALSE)
}

#' @export
am_save.am_statime <- function(x, filename = "SummaryTime", ...){
	write.csv(x, paste0(filename, "_StatTime.csv"), row.names = FALSE)
}

#' @export
#' @importFrom ggplot2 ggsave
am_save.am_plot <- function(x, filename = "Rplot.pdf", ...){
	ggsave(plot = x, filename, ...)
}
