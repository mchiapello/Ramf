#' @export
am_summary.grid <- function(x){
	Arbuscule <- Hyphopodia <- IntrHyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- num <- n <- NULL
	tmp <- grid_summary(x)
	final <- tmp %>%
	group_by(samples) %>%
	mutate(num = n()) %>%
	summarise(`Mean Total` = round(mean(Total, na.rm = TRUE), 2),
			  `Standard error Total` = round(sd(Total, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean Hyphopodia` = round(mean(Hyphopodia, na.rm = TRUE), 2),
			  `Standard error Hyphopodia` = round(sd(Hyphopodia, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean IntrHyphae` = round(mean(IntrHyphae, na.rm = TRUE), 2),
			  `Standard error IntrHyphae` = round(sd(IntrHyphae, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean Arbuscule` = round(mean(Arbuscule, na.rm = TRUE), 2),
			  `Standard error Arbuscule` = round(sd(Arbuscule, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean Vesicles` = round(mean(Vesicles, na.rm = TRUE), 2),
			  `Standard error Vesicles` = round(sd(Vesicles, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2)
			  )
	final <- final[match(unique(x$samples), final$samples), ]
	l <- list(tmp, final)
	names(l) <- c("Summary per Replicate", "Summary per Sample")
	class(l) <- c("am_summary", "list")
	return(l)
}

#' @export
am_barplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
											 "#009E73", "#F0E442", "#0072B2",
											 "#D55E00", "#CC79A7"),
							leg = c("none", "right", "left", "bottom", "top"),
							main = "Colonization", ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- grid_summary(x)
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	final <- z %>% group_by(samples, features) %>%
		  summarize(means = mean(values, na.rm = TRUE),
					se    = sd(values, na.rm = TRUE))
	final2 <- final %>% tidyr::gather(comp, values, -samples, -features)
	final3 <- final2 %>% dplyr::filter(grepl("mean", comp))
	se <- final2 %>% dplyr::filter(grepl("se", comp))
	g <- ggplot(data = final3, aes(x = interaction(factor(final3$samples, levels = unique(x$samples)),
											  factor(final3$features, levels = c("Total", "Hyphopodia",
																		   "IntrHyphae", "Arbuscule", "Vesicles"))),
											  y = values, fill = samples))
	a1 <- g + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
		geom_errorbar(aes(ymin = values - se$values, ymax = values + se$values), width = .1) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank(),
			  legend.position = leg[1]) +
		geom_vline(xintercept = seq(length(unique(z$samples)) + .5, length(unique(z$samples)) * 4 + .5,
									length(unique(z$samples))), colour = "lightgrey") +
		labs(title = main, 
			 #              subtitle = "Grid method",
			 x = "",
			 y = "") +
		ylim(ifelse(min(final3$values - se$values) < 0,
					min(final3$values - se$values), 0), max(z$values) + max(z$values) / 10) +
		annotate("text", x = seq(length(unique(z$samples)) * .5 + .5, length(unique(z$samples)) * 5 + .5,
								 length(unique(z$samples)))[1:5],
				 y = max(z$values) + max(z$values) / 10, label = c("Total", "Hyphopodia",
																   "IntrHyphae", "Arbuscule", "Vesicles")) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_fill_manual(values = cbPalette, breaks = levels(factor(final3$samples, levels = unique(x$samples))))
	class(a1) <- c("am_plot", class(a1))
	return(a1)
}

#' @export
am_boxplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
											 "#009E73", "#F0E442", "#0072B2",
											 "#D55E00", "#CC79A7"),
							leg = c("none", "right", "left", "bottom", "top"),
							main = "Colonization", ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- grid_summary(x)
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	g <- ggplot(data = z,
				aes(x = interaction(factor(z$samples, levels = unique(x$samples)),
									factor(z$features, levels = c("Total", "Hyphopodia",
																  "IntrHyphae", "Arbuscule", "Vesicles")),
										  sep = ": "),
						  y = values))
	a2 <- g +
		geom_boxplot(colour = "lightgrey", alpha = 0) +
		geom_point(aes(color = features),
				   position = position_jitter(width = 0.2)) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank(),
			  legend.position = leg[1]) +
		geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
									length(unique(z$samples)) * 4 + .5,
									length(unique(z$samples))), colour = "lightgrey") +
		labs(title = main,
			 #              subtitle = "Grid method",
			 x = "",
			 y = "") +
		ylim(-0.5, max(z$values) + max(z$values) / 10) +
		annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
								 length(unique(z$samples)) * 5 + .5,
								 length(unique(z$samples)))[1:5],
				 y = max(z$values) + max(z$values) / 10, label = c("Total", "Hyphopodia",
																   "IntrHyphae", "Arbuscule", "Vesicles")) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_colour_manual(values = cbPalette, 
							breaks = levels(factor(z$features,
												   levels = c("Total", "Hyphopodia",
															  "IntrHyphae", "Arbuscule", "Vesicles"))))
	class(a2) <- c("am_plot", class(a2))
	return(a2)
}
