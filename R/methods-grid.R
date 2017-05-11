#' am_summary object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import dplyr
am_summary <- function(x) UseMethod("am_summary")

#' @export
am_summary.grid <- function(x){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	grid_summary(x)
}

#' am_barplot object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import tidyr ggplot2
am_barplot <- function(x, ...) UseMethod("am_barplot")

#' @export
am_barplot.grid <- function(x, ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- grid_summary(x)
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	# The palette with grey:
	cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
				   "#0072B2", "#D55E00", "#CC79A7")
	final <- z %>% group_by(samples, features) %>%
		  summarize(means = mean(values, na.rm = TRUE),
					se    = sd(values, na.rm = TRUE))
	final2 <- final %>% tidyr::gather(comp, values, -samples, -features)
	final3 <- final2 %>% dplyr::filter(grepl("mean", comp))
	se <- final2 %>% dplyr::filter(grepl("se", comp))
	g <- ggplot(data = final3, aes(x = interaction(factor(final3$samples, levels = unique(x$samples)),
											  factor(final3$features, levels = c("Arbuscule", "Hyphopodia",
																		   "IntrHyphae", "Vesicles", "Total"))),
											  y = values, fill = samples))
	g + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
		geom_errorbar(aes(ymin = values - se$values, ymax = values + se$values), width = .1) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank(),
			 text = element_text(family = "Avenir")) +
		geom_vline(xintercept = seq(length(unique(z$samples)) + .5, length(unique(z$samples)) * 4 + .5,
									length(unique(z$samples))), colour = "lightgrey") +
		labs(title = "Colonization", 
			 subtitle = "Grid method",
			 x = "",
			 y = "") +
		ylim(-0.5, max(z$values) + max(z$values) / 10) +
		annotate("text", x = seq(length(unique(z$samples)) * .5 + .5, length(unique(z$samples)) * 5 + .5,
								 length(unique(z$samples)))[1:5],
				 y = max(z$values) + max(z$values) / 10, label = c("Arbuscule", "Hyphopodia",
																  "Intr. Hyphae", "Vesicles", "Total")) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_fill_manual(values = cbPalette, breaks = levels(factor(final3$samples, levels = unique(x$samples))))
}

#' am_boxplot object.
#' 
#' @param x surveyor object
#' @param ... ignored
#' @export
#' @import tidyr ggplot2
am_boxplot <- function(x, ...) UseMethod("am_boxplot")

#' @export
am_boxplot.grid <- function(x, ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- grid_summary(x)
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	# The palette with grey:
	cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
				   "#0072B2", "#D55E00", "#CC79A7")
	g <- ggplot(data = z,
				aes(x = interaction(factor(z$samples, levels = unique(x$samples)),
									factor(z$features, levels = c("Arbuscule",
																  "Hyphopodia",
																  "IntrHyphae",
																  "Vesicles",
																  "Total")),
										  sep = ": "),
						  y = values))
	g +
	geom_boxplot(colour = "lightgrey", alpha = 0) +
	geom_point(aes(color = features, shape = replicates),
			   position = position_jitter(width = 0.2)) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
		  plot.title = element_text(size = 19),
		  panel.grid.major.y = element_blank(),
		  panel.grid.minor.y = element_blank(),
		  panel.grid.major.x = element_blank(),
		  panel.grid.minor.x = element_blank(),
		 text = element_text(family = "Avenir")) +
	geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
								length(unique(z$samples)) * 4 + .5,
								length(unique(z$samples))), colour = "lightgrey") +
	labs(title = "Colonization", 
		 subtitle = "Grid method",
		 x = "",
		 y = "") +
	ylim(-0.5, max(z$values) + max(z$values) / 10) +
	annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
							 length(unique(z$samples)) * 5 + .5,
							 length(unique(z$samples)))[1:5],
			 y = max(z$values) + max(z$values) / 10, label = c("Arbuscule",
															   "Hyphopodia",
															  "Intr. Hyphae",
															  "Vesicles",
															  "Total")) +
	scale_x_discrete(labels = rep(unique(x$samples), 5)) +
	scale_colour_manual(values = cbPalette, 
						breaks = levels(factor(z$features,
											   levels = c("Arbuscule",
														  "Hyphopodia",
														  "IntrHyphae",
														  "Vesicles",
														  "Total"))))
}
