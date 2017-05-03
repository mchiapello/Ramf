summary.grid <- function(x){
	grid_summary(x)
}

plot.grid <- function(x, type = "boxplot", ...){
	Arbuscules <- Hypopodia <-  Intr_Hyphae <- Total <- Vesicles <- NULL
	if (type == "boxplot"){
		# Create summary table
		y <- grid_summary(x)
	# Change table shape
		z <- y %>% gather(features, values, -samples, -replicates)
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
}
