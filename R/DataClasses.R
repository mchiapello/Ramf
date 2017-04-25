setClass("trouvelot",
		 slots = c(scoring    = "character",
				   replicates = "character",
				   samples    = "character"
				   )
		 # Added validity
		 # Checks that all scores are inside
		 # the range of values scoring can take
		 # 0A0, 1A1, 1A2, ...
		 )
