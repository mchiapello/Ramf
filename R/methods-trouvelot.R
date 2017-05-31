#' @export
am_summary.trouvelot <- function(x){
	A <- M <- a <- num <- samples <- n <- NULL
	tmp <- trouvelot_summary(x)
	# Final table
	final <- tmp %>%
	mutate(num = n()) %>%
	group_by(samples) %>%
	summarise(mean_F = round(mean(F, na.rm = TRUE), 2),
			  se_F = round(sd(F, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  mean_M = round(mean(M, na.rm = TRUE), 2),
			  se_M = round(sd(M, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  mean_a = round(mean(a, na.rm = TRUE), 2),
			  se_a = round(sd(a, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  mean_A = round(mean(A, na.rm = TRUE), 2),
			  se_A = round(sd(A, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2)
			  )
	final <- final[match(unique(x$samples), final$samples), ]
	l <- list(tmp, final)
	names(l) <- c("Summary per Replicate", "Summary per Sample")
	class(l) <- c("am_summary", "list")
	return(l)
}

#' @export
am_barplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
												  "#009E73", "#F0E442", "#0072B2",
												  "#D55E00", "#CC79A7"),
								 leg = c("none", "right", "left", "bottom", "top"),
								 main = "Colonization", ...){
	A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- n <- NULL
	values <- NULL
	tmp <- trouvelot_summary(x)
	final <- tmp %>%
		mutate(num = n()) %>%
		group_by(samples) %>%
		summarise(mean_F = mean(F, na.rm = TRUE),
				  se_F = sd(F, na.rm = TRUE) / mean(num, na.rm = TRUE),
				  mean_M = mean(M, na.rm = TRUE),
				  se_M = sd(M, na.rm = TRUE) / mean(num, na.rm = TRUE),
				  mean_a = mean(a, na.rm = TRUE),
				  se_a = sd(a, na.rm = TRUE) / mean(num, na.rm = TRUE),
				  mean_A = mean(A, na.rm = TRUE),
				  se_A = sd(A, na.rm = TRUE) / mean(num, na.rm = TRUE)
				  )
	final2 <- final %>% gather(feature, value, -samples)
	final3 <- final2 %>% dplyr::filter(grepl("mean", feature))
	se <- final2 %>% dplyr::filter(grepl("se", feature))
	g <- ggplot(data = final3, aes(x = interaction(factor(final3$samples, levels = unique(x$samples)),
												   factor(final3$feature, levels = c("mean_F", "mean_M", "mean_a", "mean_A"))),
												   y = value, fill = samples))
	a1 <- g + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
		geom_errorbar(aes(ymin = value - se$value, ymax = value + se$value), width = .1) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank(),
			  legend.position = leg[1]) +
		geom_vline(xintercept = seq(length(unique(final3$samples)) + .5, length(unique(final3$samples)) * 3 + .5,
									length(unique(final3$samples))), colour = "lightgrey") +
					  #         geom_hline(yintercept = 105, colour = "lightgrey") +
			labs(title = main, 
				 #                  subtitle = "Trouvelot method",
				 x = "",
				 y = "") +
			ylim(-0.5, 110) +
			annotate("text", x = seq(length(unique(final3$samples)) * .5 + .5, length(unique(final3$samples)) * 5 + .5,
									 length(unique(final3$samples)))[1:4],
					 y = 110, label = c("F%", "A%", "a%", "M%")) +
			scale_x_discrete(labels = rep(unique(x$samples), 5)) +
			scale_fill_manual(values = cbPalette, breaks = levels(factor(final3$samples, levels = unique(x$samples))))
	class(a1) <- c("am_plot", class(a1))
	return(a1)
}

#' @export
am_boxplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
												  "#009E73", "#F0E442", "#0072B2",
												  "#D55E00", "#CC79A7"),
								 leg = c("none", "right", "left", "bottom", "top"),
								 main = "Colonization", ...){
	A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- NULL
	values <- NULL
	tmp <- trouvelot_summary(x)
	fin <- tmp %>% gather(feature, value, -samples, -replicates)
	g <- ggplot(data = fin, aes(x = interaction(factor(fin$samples, levels = unique(x$samples)),
											  factor(fin$feature, levels = c("F", "A", "a", "M")),
											  sep = ": "),
							  y = value))
	a2 <- g +
		geom_boxplot(colour = "lightgrey", alpha = 0) +
		geom_point(aes(color = feature), position = position_jitter(width = 0.2)) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank(),
			  legend.position = leg[1]) +
		geom_vline(xintercept = seq(length(unique(fin$samples)) + .5, length(unique(fin$samples)) * 3 + .5,
									length(unique(fin$samples))), colour = "lightgrey") +
					  #     geom_hline(yintercept = 105, colour = "lightgrey") +
		labs(title = main, 
			 #              subtitle = "Trouvelot method",
			 x = "",
			 y = "") +
		ylim(-0.5, 110) +
		annotate("text", x = seq(length(unique(fin$samples)) * .5 + .5, length(unique(fin$samples)) * 5 + .5,
								 length(unique(fin$samples)))[1:4],
				 y = 110, label = c("F%", "A%", "a%", "M%")) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_colour_manual(values = cbPalette, breaks = levels(factor(fin$feature, levels = c("F", "A", "a", "M"))))
	class(a2) <- c("am_plot", class(a2))
	return(a2)
}
