## SUMMARY
# Grid
grid_summary <- function(x){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- x %>% 
			group_by(samples, replicates) %>%
			summarise(Total = mean(Total, na.rm = TRUE),
					  Hyphopodia = mean(Hypopodia, na.rm = TRUE),
					  IntrHyphae = mean(Intr_Hyphae, na.rm = TRUE),
					  Arbuscule = mean(Arbuscule, na.rm = TRUE),
					  Vesicles = mean(Vesicles, na.rm = TRUE)) %>%
			ungroup
	class(y) <- c("grid", class(y))
	return(y)
}

# Trouvelot
trouvelot_summary <- function(x){
	y <- x %>% 
		group_by(scoring, replicates, samples) %>%
		tally %>%
		ungroup
	# Prepare complete dataset
	theo_df <- tibble(scoring = rep(c("0A0", "1A3", "2A3", "3A3", "4A3", "5A3",
				 "1A2", "2A2", "3A2", "4A2", "5A2", "1A1",
				 "2A1", "3A1", "4A1", "5A1", "1A0", "2A0",
				 "3A0", "4A0", "5A0"), dim(table(y$replicates, y$samples))[1] * dim(table(y$replicates, y$samples))[2]),
				  replicates = rep(rep(rownames(table(y$replicates, y$samples)), each = 21), dim(table(y$replicates, y$samples))[2]),
				   samples = rep(names(table(y$samples)), each = 21 * dim(table(y$replicates, y$samples))[1])
	)
	# Merge complete dataset with user data
	complete_df <- left_join(theo_df, y, by = c("scoring", "replicates", "samples"))
	### Compute F
	N <- complete_df %>% 
		group_by(samples, replicates) %>%
		summarise(N = sum(n, na.rm = TRUE))
	n0 <- complete_df %>% 
		group_by(samples, replicates) %>%
		dplyr::filter(scoring == "0A0") %>%
		summarise(n0 = sum(n, na.rm = TRUE))
	z <- inner_join(N, n0, by = c("samples", "replicates"))
	z <- z %>% 
		mutate(F = round(100 * (N -n0) / N, 2))
	# Compute M
	yy <- complete_df %>% 
		mutate(Colonization = substring(scoring, 1, 1),
			   Abundance = substring(scoring, 2, 3)
			   )
	yy2 <- yy %>%
		group_by(samples, replicates, Colonization) %>%
		summarize(nn = sum(n, na.rm = TRUE)) %>%
		mutate(perc = c(0, 1, 5, 30, 70, 95))
	yy2 <- yy2 %>% 
	mutate(new = nn * perc) %>%
	ungroup %>%
	group_by(samples, replicates) %>%
	mutate(tot = sum(nn, na.rm = TRUE)) %>%
	summarise(M1 = sum(new, na.rm = TRUE),
			  tot2 = mean(tot, na.rm = TRUE)) %>%
	mutate(M = M1 / tot2)
	# Compute m
	yy3 <- yy %>%
		group_by(samples, replicates) %>%
		dplyr::filter(Abundance != "A0") %>%
		summarize(n_myc = sum(n, na.rm = TRUE))
	yy4 <- inner_join(yy2, yy3, by = c("samples", "replicates"))
	yy4 <- yy4 %>%
		mutate(m = M * tot2 / n_myc)
	# Compute a
	yy5 <- yy %>%
		dplyr::filter(Abundance != "A0") %>%
		group_by(samples, replicates, Abundance, Colonization) %>%
		summarise(mA = sum(n, na.rm = TRUE)) %>%
		mutate(perc = c(1, 5, 30, 70, 95)) %>%
		mutate(tmpa = mA * perc) %>%
		ungroup %>%
		group_by(samples, replicates, Abundance) %>%
		summarise(a = sum(tmpa, na.rm = TRUE))
	# Merge to add n_myc
	tmp1 <- left_join(yy5, yy3, by = c("samples", "replicates"))
	# Merge to add m
	tmp2 <- left_join(tmp1, yy4[,c(1,2,7)], by = c("samples", "replicates"))
	# Final "a" computation
	yy6 <- tmp2 %>%
		group_by(samples, replicates) %>%
		mutate(final_a = (a / n_myc) * 100 / m,
			   perc = c(10, 50, 100)) %>%
		summarise(a = sum(final_a * perc, na.rm = TRUE) / 100)
	# Compute A
	tmp <- inner_join(yy4, yy6, by = c("samples", "replicates")) %>%
		mutate(A = a * (M / 100))
	tmp <- inner_join(z, tmp, by = c("samples", "replicates")) %>% 
		select(samples, replicates, F, M, a, A)
	}

## PLOTS
gt_plot <- function(x, type){
	features <- replicates <- samples <- values <- comp <- NULL
		# Create summary table
		y <- grid_summary(x)
		# Change table shape
		z <- y %>% tidyr::gather(features, values, -samples, -replicates)
		# The palette with grey:
		cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
					   "#0072B2", "#D55E00", "#CC79A7")
	if (type == "boxplot"){
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
	if (type == "barplot"){
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
}
