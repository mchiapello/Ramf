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
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'                   main = "Colonization", lab = "days",...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the samples groupping the samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param main Plot title. Default "Colonization".
#' @param lab Parameter used in time series plots. It specified the label to write
#'            close the time on the plot. Default is "days".
#' @param ... ignored
#' @examples
#' am_barplot(example_grid)
#' am_barplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_barplot <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
					   main = "Colonization", lab = "days", ...) UseMethod("am_barplot")

#' am_boxplot object.
#' 
#' @usage am_boxplot(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'                   main = "Colonization", lab = "days", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the samples groupping the samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param main Plot title. Default "Colonization".
#' @param lab Parameter used in time series plots. It specified the label to write
#'            close the time on the plot. Default is "days".
#' @param ... ignored
#' @examples
#' am_boxplot(example_grid)
#' am_boxplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_boxplot <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
					   main = "Colonization", lab = "days", ...) UseMethod("am_boxplot")

#' am_dotplot object.
#' 
#' @usage am_dotplot(x, cbPalette,
#'					alpha = 0.05,
#'					annot = c("none", "asterisks", "letters"),
#'					method = c("none","holm","hommel", "hochberg",
#'							   "bonferroni", "BH", "BY", "fdr"),
#'                   main = "Colonization", lab = "days", ...)
#' @param x dataset containing Trouvelot or Grid data
#' @param cbPalette a vector of colors. Default is: c("#999999", "#E69F00", "#56B4E9",
#'				    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7").
#' 					Colorbrewer and Colorgorical are website where is possible
#' 					design nice color palettes.
#' @param alpha Significant threshold
#' @param annot Default is "none". If it is "asterisks" on the plot asterisks appear
#'            below the sample statistically different from the control. The control
#'            is the first sample in the input file. If is is "letters" on the plot
#'            letters appear below the samples groupping the samples based on the 
#'            statistical test. For statistical tests check `am_stat` function.
#' @param method adjusts the p-value for multiple comparisons using the Bonferroni, Holm,
#'               Hochberg, Bonferroni, Benjamini-Hochberg, Benjamini-Yekutieli or fdr 
#'               adjustment
#'               (see agricolae package for more details).
#'               The default is no adjustment for multiple comparisons.
#' @param main Plot title. Default "Colonization".
#' @param lab Parameter used in time series plots. It specified the label to write
#'            close the time on the plot. Default is "days".
#' @param ... ignored
#' @examples
#' am_dotplot(example_grid)
#' am_dotplot(example_trouvelot)
#' @export
#' @import tidyr ggplot2
am_dotplot <- function(x, cbPalette,
					   alpha = 0.05,
					   annot = c("none", "asterisks", "letters"),
					   method = c("none","holm","hommel", "hochberg",
									   "bonferroni", "BH", "BY", "fdr"),
					   main = "Colonization", lab = "days", ...) UseMethod("am_dotplot")

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
#' @importFrom ggplot2 ggsave
am_save.am_plot <- function(x, filename = "Rplot.pdf", ...){
	ggsave(plot = x, filename, ...)
}
