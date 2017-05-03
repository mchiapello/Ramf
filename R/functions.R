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
	return(y)
}

shape <- function(y){
		z <- y %>% gather(features, values, -samples, -replicates)
		final <- z %>% group_by(samples, features) %>%
		  summarize(means = mean(values, na.rm = TRUE),
					se    = sd(values, na.rm = TRUE))
		final2 <- final %>% gather(comp, values, -samples, -features)
		final3 <- final2 %>% filter(grepl("mean", comp))
		se <- final2 %>% filter(grepl("se", comp))
}
