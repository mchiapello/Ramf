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
							stats = c("none", "asterisks", "letters"),
							main = "Colonization", ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- n <- num <- means <- se <- NULL
	stats <- match.arg(stats)
	# Create summary table
	y <- grid_summary(x)
	if (stats == "none" | stats == "letters"){
		d <- rep("", length(unique(y$samples)) * 5)
	}
	if (stats == "asterisks"){
    	stat <- am_stat(x)
    	stat_ctr <- stat[stat$group1 == y$samples[1], ]
    	stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:7])) < 0.05, "*", "") 
    	ll <- split(stat_l, rep(1:5, each = length(unique(y$samples)) - 1))
    	d <- NULL
    	for (i in seq_along(ll)){
    		d <- append(d, c("", ll[[i]]))
    	}
	}
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	final <- z %>% group_by(samples, features) %>%
		  mutate(num = n()) %>%
		  summarize(means = mean(values, na.rm = TRUE),
					se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)))
	g <- ggplot(data = final, aes(x = interaction(factor(final$samples, levels = unique(x$samples)),
											  factor(final$features, levels = c("Total", "Hyphopodia",
																		   "IntrHyphae", "Arbuscule", "Vesicles"))),
											  y = means, fill = samples))
	a1 <- g + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
		geom_errorbar(aes(ymin = means - se, ymax = means + se), width = .1) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank()) +
		geom_vline(xintercept = seq(length(unique(z$samples)) + .5, length(unique(z$samples)) * 4 + .5,
									length(unique(z$samples))), colour = "lightgrey") +
		labs(title = main, 
			 #              subtitle = "Grid method",
			 x = "",
			 y = "") +
		annotate("text", x = seq(length(unique(z$samples)) * .5 + .5, length(unique(z$samples)) * 5 + .5,
								 length(unique(z$samples)))[1:5],
				 y = max(z$values) + max(z$values) / 10, label = c("Total", "Hyphopodia",
																   "IntrHyphae", "Arbuscule", "Vesicles")) +
		annotate("text", x = 1:(length(unique(y$samples)) * 5), y = -Inf, vjust = -0.5, label = d) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_y_continuous(limits = c(ifelse(min(final$means - final$se) < 0,
					min(final$means - final$se), 0), max(z$values) + max(z$values) / 10),
						   breaks = seq(0, 110, 20)) +
		scale_fill_manual(values = cbPalette, breaks = levels(factor(final$samples, levels = unique(x$samples))))
	class(a1) <- c("am_plot", class(a1))
	return(a1)
}

#' @export
am_boxplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
											 "#009E73", "#F0E442", "#0072B2",
											 "#D55E00", "#CC79A7"),
							stats = c("none", "asterisks", "letters"),
							main = "Colonization", ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	stats <- match.arg(stats)
	# Create summary table
	y <- grid_summary(x)
	if (stats == "none" | stats == "letters"){
		d <- rep("", length(unique(y$samples)) * 5)
	}
	if (stats == "asterisks"){
    	stat <- am_stat(x)
    	stat_ctr <- stat[stat$group1 == y$samples[1], ]
    	stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:7])) < 0.05, "*", "") 
    	ll <- split(stat_l, rep(1:5, each = length(unique(y$samples)) - 1))
    	d <- NULL
    	for (i in seq_along(ll)){
    		d <- append(d, c("", ll[[i]]))
    	}
	}
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	g <- ggplot(data = z,
				aes(x = interaction(factor(z$samples, levels = unique(x$samples)),
									factor(z$features, levels = c("Total", "Hyphopodia",
																  "IntrHyphae", "Arbuscule", "Vesicles")),
										  sep = ": "),
						  y = values, color = samples))
	a2 <- g +
		geom_boxplot() +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank()) +
		geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
									length(unique(z$samples)) * 4 + .5,
									length(unique(z$samples))), colour = "lightgrey") +
		labs(title = main,
			 #              subtitle = "Grid method",
			 x = "",
			 y = "") +
		annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
								 length(unique(z$samples)) * 5 + .5,
								 length(unique(z$samples)))[1:5],
				 y = max(z$values) + max(z$values) / 10, label = c("Total", "Hyphopodia",
																   "IntrHyphae", "Arbuscule", "Vesicles")) +
		annotate("text", x = 1:(length(unique(y$samples)) * 5), y = -Inf, vjust = -0.5, label = d) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_y_continuous(limits = c(-0.5, max(z$values) + max(z$values) / 10),
						   breaks = seq(0, 110, 20))+ 
		scale_colour_manual(values = cbPalette, 
							breaks = levels(factor(z$features,
												   levels = c("Total", "Hyphopodia",
															  "IntrHyphae", "Arbuscule", "Vesicles"))))
	class(a2) <- c("am_plot", class(a2))
	return(a2)
}

#' @export
am_dotplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
											 "#009E73", "#F0E442", "#0072B2",
											 "#D55E00", "#CC79A7"),
							stats = c("none", "asterisks", "letters"),
							main = "Colonization", ...){
	Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	stats <- match.arg(stats)
	# Create summary table
	y <- grid_summary(x)
	if (stats == "none" | stats == "letters"){
		d <- rep("", length(unique(y$samples)) * 5)
	}
	if (stats == "asterisks"){
    	stat <- am_stat(x)
    	stat_ctr <- stat[stat$group1 == y$samples[1], ]
    	stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:7])) < 0.05, "*", "") 
    	ll <- split(stat_l, rep(1:5, each = length(unique(y$samples)) - 1))
    	d <- NULL
    	for (i in seq_along(ll)){
    		d <- append(d, c("", ll[[i]]))
    	}
	}
	# Change table shape
	z <- y %>% tidyr::gather(features, values, -samples, -replicates)
	g <- ggplot(data = z,
				aes(x = interaction(factor(z$samples, levels = unique(x$samples)),
									factor(z$features, levels = c("Total", "Hyphopodia",
																  "IntrHyphae", "Arbuscule", "Vesicles")),
										  sep = ": "),
						  y = values, color = samples))
	a2 <- g +
		geom_point(position = position_jitter(width = 0.2)) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank()) +
		geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
									length(unique(z$samples)) * 4 + .5,
									length(unique(z$samples))), colour = "lightgrey") +
		labs(title = main,
			 #              subtitle = "Grid method",
			 x = "",
			 y = "") +
		annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
								 length(unique(z$samples)) * 5 + .5,
								 length(unique(z$samples)))[1:5],
				 y = max(z$values) + max(z$values) / 10, label = c("Total", "Hyphopodia",
																   "IntrHyphae", "Arbuscule", "Vesicles")) +
		annotate("text", x = 1:(length(unique(y$samples)) * 5), y = -Inf, vjust = -0.5, label = d) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_y_continuous(limits = c(-0.5, max(z$values) + max(z$values) / 10),
						   breaks = seq(0, 110, 20))+ 
		scale_colour_manual(values = cbPalette, 
							breaks = levels(factor(z$features,
												   levels = c("Total", "Hyphopodia",
															  "IntrHyphae", "Arbuscule", "Vesicles"))))
	class(a2) <- c("am_plot", class(a2))
	return(a2)
}

#' @export
am_stat.grid <- function(x, ...){
	V1 <- NULL
	sls <- am_summary(x)
	stat <- list()
	for(i in 3:7){
        capture.output(tmp <-
			conover.test(pull(sls[[1]], i),
						 paste0(rep(1:length(unique(sls[[1]]$samples)),
									rep(as.numeric(tapply(sls[[1]]$replicates,
														  factor(sls[[1]]$samples,
																 levels = unique(sls[[1]]$samples)),
														  length)),1)),
								"_", sls[[1]]$samples),
						 method = "bh", table = T), file=NULL)
		stat_tmp <- tbl_df(cbind(V1 = tmp$comparisons, pval = round(tmp$P.adjusted * 2, 3)))
		stat_tmp <- stat_tmp %>% separate(V1, c("group1", "group2"), " - ")
        stat[[c(1, 1, 1:5)[i]]] <- stat_tmp
    }
	stat <- do.call(cbind, stat)[-c(4, 5, 7, 8, 10, 11, 13, 14)]
	names(stat) <- c("group1", "group2", paste0(names(sls[[1]])[3:7], ".pval"))
	stat$group1 <- gsub("^\\d+_", "", stat$group1)
	stat$group2 <- gsub("^\\d+_", "", stat$group2)
	class(stat) <- c("am_stat", class(stat))
    return(stat)
}


