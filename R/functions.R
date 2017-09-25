## SUMMARY
# Grid
grid_summary <- function(x){
	Arbuscule <- Hyphopodia <- IntrHyphae <- Total <- Vesicle <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- x %>% 
			group_by(samples, replicates) %>%
			summarise(Total = round(mean(Total, na.rm = TRUE), 2),
					  Hyphopodia = round(mean(Hyphopodia, na.rm = TRUE), 2),
					  IntrHyphae = round(mean(IntrHyphae, na.rm = TRUE), 2),
					  Arbuscule = round(mean(Arbuscule, na.rm = TRUE), 2),
					  Vesicle = round(mean(Vesicle, na.rm = TRUE),2)) %>%
			ungroup
	y <- y %>% arrange(factor(samples, levels = unique(x$samples)), replicates)
	return(y)
}

# Trouvelot
trouvelot_summary <- function(x){
	A <- Abundance <- Colonization <- M <- M1 <- a <- cbPalette <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- n <- NULL
	values <- NULL
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
	tmp <- tmp %>% dplyr::filter(!is.na(F))
	tmp[, c("F", "M", "a", "A")] <- round(tmp[, c("F", "M", "a", "A")], 2)
	tmp <- tmp %>% arrange(factor(samples, levels = unique(x$samples)), replicates)
	return(tmp)
}

.grid_stat <- function(x, group = FALSE, method = method, alpha = 0.05, ...){
	V1 <- NULL
	sls <- am_summary(x)
	sls[[1]]$samples <- paste0(rep(1:length(unique(sls[[1]]$samples)),
						rep(as.numeric(tapply(sls[[1]]$replicates, factor(sls[[1]]$samples,
						levels = unique(sls[[1]]$samples)), length)),1)), "_", sls[[1]]$samples)
	stat <- list()
	if (group == FALSE){
		for(i in 3:7){
			tmp <- kruskal(pull(sls[[1]], i),
					pull(sls[[1]], 1),
					p.adj = method,
					group = group,
					console = FALSE)
			tmp2 <- tmp$comparison
			tmp2$V1 <- rownames(tmp2)
			stat_tmp <- tmp2 %>% separate(V1, c("group1", "group2"), " - ") %>%
				select("group1", "group2", "pvalue")
			rownames(stat_tmp) <- NULL
			stat[[c(1, 1, 1:5)[i]]] <- stat_tmp
		}
		stat <- do.call(cbind, stat)[-c(4, 5, 7, 8, 10, 11, 13, 14)]
		names(stat) <- c("group1", "group2", paste0(names(sls[[1]])[3:7], ".pval"))
		stat$group1 <- gsub("^\\d+_", "", stat$group1)
		stat$group2 <- gsub("^\\d+_", "", stat$group2)
		class(stat) <- c("am_stat", class(stat))
		return(stat)
	} else {
		for(i in 3:7){
			tmp <- kruskal(pull(sls[[1]], i),
					pull(sls[[1]], 1),
					alpha = alpha,
					p.adj = method,
					group = group,
					console = FALSE)
			tmp2 <- tmp$groups[, 1:2]
			tmp2$trt <- rownames(tmp2)
			tmp2 <- tmp2[order(tmp2$trt), ]
			stat[[c(1, 1, 1:5)[i]]] <- tmp2[2]
		}
		stat <- do.call(cbind, stat)
		stat$sample <- rownames(stat)
		stat <- stat[, c(6, 1:5)]
		names(stat) <- c("sample", paste0(names(sls[[1]])[3:7], ".group"))
		stat$sample <- gsub("^\\d+_", "", stat$sample)
		return(stat)
	}
}


.trouvelot_stat <- function(x, group = FALSE, method = method, alpha = 0.05, ...){
	V1 <- NULL
	sls <- am_summary(x)
	sls[[1]]$samples <- paste0(rep(1:length(unique(sls[[1]]$samples)),
						rep(as.numeric(tapply(sls[[1]]$replicates, factor(sls[[1]]$samples,
						levels = unique(sls[[1]]$samples)), length)),1)), "_", sls[[1]]$samples)
	stat <- list()
	if (group == FALSE){
		for(i in 3:6){
			tmp <- kruskal(pull(sls[[1]], i),
					pull(sls[[1]], 1),
					alpha = alpha,
					p.adj = method,
					group = group,
					console = FALSE)
			tmp2 <- tmp$comparison
			tmp2$V1 <- rownames(tmp2)
			stat_tmp <- tmp2 %>% separate(V1, c("group1", "group2"), " - ") %>%
				select("group1", "group2", "pvalue")
			rownames(stat_tmp) <- NULL
			stat[[c(1, 1, 1:5)[i]]] <- stat_tmp
		}
		stat <- do.call(cbind, stat)[-c(4, 5, 7, 8, 10, 11)]
		names(stat) <- c("group1", "group2", paste0(names(sls[[1]])[3:6], ".pval"))
		stat$group1 <- gsub("^\\d+_", "", stat$group1)
		stat$group2 <- gsub("^\\d+_", "", stat$group2)
		class(stat) <- c("am_stat", class(stat))
		return(stat)
	} else {
		for(i in 3:6){
			tmp <- kruskal(pull(sls[[1]], i),
					pull(sls[[1]], 1),
					alpha = alpha,
					p.adj = method,
					group = group,
					console = FALSE)
			tmp2 <- tmp$groups[, 1:2]
			tmp2$trt <- rownames(tmp2)
			tmp2 <- tmp2[order(tmp2$trt), ]
			stat[[c(1, 1, 1:5)[i]]] <- tmp2[2]
		}
		stat <- do.call(cbind, stat)
		stat$sample <- rownames(stat)
		stat <- stat[, c(5, 1:4)]
		names(stat) <- c("sample", paste0(names(sls[[1]])[3:6], ".group"))
		stat$sample <- gsub("^\\d+_", "", stat$sample)
		return(stat)
	}
}

