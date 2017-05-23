## SUMMARY
# Grid
grid_summary <- function(x){
	Arbuscule <- Hyphopodia <- IntrHyphae <- Total <- Vesicles <- comp <- NULL
	features <- replicates <- samples <- values <- NULL
	# Create summary table
	y <- x %>% 
			group_by(samples, replicates) %>%
			summarise(Total = mean(Total, na.rm = TRUE),
					  Hyphopodia = mean(Hyphopodia, na.rm = TRUE),
					  IntrHyphae = mean(IntrHyphae, na.rm = TRUE),
					  Arbuscule = mean(Arbuscule, na.rm = TRUE),
					  Vesicles = mean(Vesicles, na.rm = TRUE)) %>%
			ungroup
	class(y) <- c("grid", class(y))
	return(y)
}

# Trouvelot
trouvelot_summary <- function(x){
	A <- Abundance <- Colonization <- M <- M1 <- a <- cbPalette <- feature <- features <- final_a <- m <- NULL
	mA <- n_myc <- nn <- num <- perc <- replicates <- samples <- scoring <- tmpa <- tot <- tot2 <- value <- NULL
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
}
