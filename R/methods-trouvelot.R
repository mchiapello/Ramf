#' @export
am_summary.trouvelot <- function(x){
	A <- M <- a <- num <- samples <- n <- NULL
	tmp <- trouvelot_summary(x)
	# Final table
	final <- tmp %>%
	group_by(samples) %>%
	mutate(num = n()) %>%
	summarise(`Mean F` = round(mean(F, na.rm = TRUE), 2),
			  `Standard error F` = round(sd(F, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean M` = round(mean(M, na.rm = TRUE), 2),
			  `Standard error M` = round(sd(M, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean a` = round(mean(a, na.rm = TRUE), 2),
			  `Standar error a` = round(sd(a, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
			  `Mean A` = round(mean(A, na.rm = TRUE), 2),
			  `Standard error A` = round(sd(A, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2)
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
								 stats = c("none", "asterisks", "letters"),
								 main = "Colonization", ...){
	A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- n <- NULL
	values <- means <- se <- num <- NULL
	stats <- match.arg(stats)
	tmp <- trouvelot_summary(x)
	if (stats == "none" | stats == "letters"){
		d <- rep("", length(unique(tmp$samples)) * 4)
	}
	if (stats == "asterisks"){
    	stat <- am_stat(x)
    	stat_ctr <- stat[stat$group1 == tmp$samples[1], ]
    	stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < 0.05, "*", "") 
    	ll <- split(stat_l, rep(1:4, each = length(unique(tmp$samples)) - 1))
    	d <- NULL
    	for (i in seq_along(ll)){
    		d <- append(d, c("", ll[[i]]))
    	}
	}
	z <- tmp %>% tidyr::gather(features, values, -samples, -replicates)
	final <- z %>% group_by(samples, features) %>%
		  mutate(num = n()) %>%
		  summarize(means = mean(values, na.rm = TRUE),
					se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)))
	g <- ggplot(data = final, aes(x = interaction(factor(final$samples, levels = unique(x$samples)),
												   factor(final$features, levels = c("F", "M", "a", "A"))),
												   y = means, fill = samples))
	a1 <- g + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
		geom_errorbar(aes(ymin = means - se, ymax = means + se), width = .1) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank(),
			  legend.position = "none") +
		geom_vline(xintercept = seq(length(unique(final$samples)) + .5, length(unique(final$samples)) * 3 + .5,
									length(unique(final$samples))), colour = "lightgrey") +
					  #         geom_hline(yintercept = 105, colour = "lightgrey") +
			labs(title = main, 
				 #                  subtitle = "Trouvelot method",
				 x = "",
				 y = "") +
			annotate("text", x = seq(length(unique(final$samples)) * .5 + .5, length(unique(final$samples)) * 5 + .5,
									 length(unique(final$samples)))[1:4],
					 y = 110, label = c("F%", "M%", "a%", "A%")) +
			annotate("text", x = 1:(length(unique(tmp$samples)) * 4), y = -Inf, vjust = -0.5, label = d) +
			scale_x_discrete(labels = rep(unique(x$samples), 5)) +
			scale_y_continuous(limits = c(ifelse(min(final$means - final$se) < 0,
					min(final$means - final$se), 0), 110), breaks = seq(0, 110, 20)) +
			scale_fill_manual(values = cbPalette, breaks = levels(factor(final$samples, levels = unique(x$samples))))
	class(a1) <- c("am_plot", class(a1))
	return(a1)
}

#' @export
am_boxplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
												  "#009E73", "#F0E442", "#0072B2",
												  "#D55E00", "#CC79A7"),
								 stats = c("none", "asterisks", "letters"),
								 main = "Colonization", ...){
	A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- NULL
	values <- NULL
	stats <- match.arg(stats)
	tmp <- trouvelot_summary(x)
	if (stats == "none" | stats == "letters"){
		d <- rep("", length(unique(tmp$samples)) * 4)
	}
	if (stats == "asterisks"){
    	stat <- am_stat(x)
    	stat_ctr <- stat[stat$group1 == tmp$samples[1], ]
    	stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < 0.05, "*", "") 
    	ll <- split(stat_l, rep(1:4, each = length(unique(tmp$samples)) - 1))
    	d <- NULL
    	for (i in seq_along(ll)){
    		d <- append(d, c("", ll[[i]]))
    	}
	}
	fin <- tmp %>% gather(feature, value, -samples, -replicates)
	g <- ggplot(data = fin, aes(x = interaction(factor(fin$samples, levels = unique(x$samples)),
											  factor(fin$feature, levels = c("F", "A", "a", "M")),
											  sep = ": "),
							  y = value, color = samples))
	a2 <- g +
		geom_boxplot() +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank()) +
		geom_vline(xintercept = seq(length(unique(fin$samples)) + .5, length(unique(fin$samples)) * 3 + .5,
									length(unique(fin$samples))), colour = "lightgrey") +
					  #     geom_hline(yintercept = 105, colour = "lightgrey") +
		labs(title = main, 
			 #              subtitle = "Trouvelot method",
			 x = "",
			 y = "") +
		annotate("text", x = seq(length(unique(fin$samples)) * .5 + .5, length(unique(fin$samples)) * 5 + .5,
								 length(unique(fin$samples)))[1:4],
				 y = 110, label = c("F%", "M%", "a%", "A%")) +
		annotate("text", x = 1:(length(unique(tmp$samples)) * 4), y = -Inf, vjust = -0.5, label = d) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_y_continuous(limits = c(-0.5, 110), breaks = seq(0, 110, 20)) +
		scale_colour_manual(values = cbPalette, breaks = levels(factor(fin$feature, levels = c("F", "A", "a", "M"))))
	class(a2) <- c("am_plot", class(a2))
	return(a2)
}


#' @export
am_dotplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
												  "#009E73", "#F0E442", "#0072B2",
												  "#D55E00", "#CC79A7"),
								 stats = c("none", "asterisks", "letters"),
								 main = "Colonization", ...){
	A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- NULL
	values <- NULL
	stats <- match.arg(stats)
	tmp <- trouvelot_summary(x)
	if (stats == "none" | stats == "letters"){
		d <- rep("", length(unique(tmp$samples)) * 4)
	} 
	if (stats == "asterisks"){
    	stat <- am_stat(x)
    	stat_ctr <- stat[stat$group1 == tmp$samples[1], ]
    	stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < 0.05, "*", "") 
    	ll <- split(stat_l, rep(1:4, each = length(unique(tmp$samples)) - 1))
    	d <- NULL
    	for (i in seq_along(ll)){
    		d <- append(d, c("", ll[[i]]))
    	}
	}
	fin <- tmp %>% gather(feature, value, -samples, -replicates)
	g <- ggplot(data = fin, aes(x = interaction(factor(fin$samples, levels = unique(x$samples)),
											  factor(fin$feature, levels = c("F", "A", "a", "M")),
											  sep = ": "),
							  y = value, color = as.factor(samples)))
	a2 <- g +
		geom_point(position = position_jitter(width = 0.2)) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
			  plot.title = element_text(size = 19),
			  panel.grid.major.y = element_blank(),
			  panel.grid.minor.y = element_blank(),
			  panel.grid.major.x = element_blank(),
			  panel.grid.minor.x = element_blank()) +
		geom_vline(xintercept = seq(length(unique(fin$samples)) + .5, length(unique(fin$samples)) * 3 + .5,
									length(unique(fin$samples))), colour = "lightgrey") +
					  #     geom_hline(yintercept = 105, colour = "lightgrey") +
		labs(title = main, 
			 #              subtitle = "Trouvelot method",
			 x = "",
			 y = "") +
		annotate("text", x = seq(length(unique(fin$samples)) * .5 + .5, length(unique(fin$samples)) * 5 + .5,
								 length(unique(fin$samples)))[1:4],
				 y = 110, label = c("F%", "M%", "a%", "A%")) +
		annotate("text", x = 1:(length(unique(tmp$samples)) * 4), y = -Inf, vjust = -0.5, label = d) +
		scale_x_discrete(labels = rep(unique(x$samples), 5)) +
		scale_y_continuous(limits = c(-0.5, 110), breaks = seq(0, 110, 20)) +
		scale_colour_manual(values = cbPalette, breaks = levels(factor(fin$feature, levels = c("F", "A", "a", "M"))))
	class(a2) <- c("am_plot", class(a2))
	return(a2)
}

#' @export
am_stat.trouvelot <- function(x, method = c("none", "bonferroni", "sidak",
									   "hs", "bh", "by"), ...){
	method <- match.arg(method)
	V1 <- NULL
	sls <- am_summary(x)
	stat <- list()
	for(i in 3:6){
        capture.output(tmp <-
			conover.test(pull(sls[[1]], i),
						 paste0(rep(1:length(unique(sls[[1]]$samples)),
									rep(as.numeric(tapply(sls[[1]]$replicates,
														  factor(sls[[1]]$samples,
																 levels = unique(sls[[1]]$samples)),
														  length)),1)),
								"_", sls[[1]]$samples),
						 method = method, table = T), file=NULL)
		stat_tmp <- tbl_df(cbind(V1 = tmp$comparisons, pval = round(tmp$P.adjusted * 2, 3)))
		stat_tmp <- stat_tmp %>% separate(V1, c("group1", "group2"), " - ")
        stat[[c(1, 1, 1:5)[i]]] <- stat_tmp
    }
	stat <- do.call(cbind, stat)[-c(4, 5, 7, 8, 10, 11)]
	names(stat) <- c("group1", "group2", paste0(names(sls[[1]])[3:6], ".pval"))
	stat$group1 <- gsub("^\\d+_", "", stat$group1)
	stat$group2 <- gsub("^\\d+_", "", stat$group2)
	class(stat) <- c("am_stat", class(stat))
    return(stat)
}

