#' @export
am_summary.grid <- function(x){
    Arbuscule <- Hyphopodia <- IntrHyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- num <- n <- num_mean <- NULL
    tmp <- grid_summary(x)
    final <- tmp %>%
        group_by(samples) %>%
        mutate(num = n()) %>%
        summarise_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
        mutate_at(vars(contains("_sd")), funs(. / sqrt(num_mean))) %>%
        select(-contains("num"))
    names(final)[grepl("_sd", names(final))] <- gsub("_sd", "_se",
                                                     names(final)[grepl("_sd", names(final))])
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
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Colonization", ...){
    Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- n <- num <- means <- se <- NULL
    dimen <- 0
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    if (annot == "none"){
        d <- rep("", length(unique(y$samples)) * num)
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- 3
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        dimen <- 3
    }
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -samples, -replicates)
    final <- z %>% group_by(samples, features) %>%
          mutate(num = n()) %>%
          summarize(means = mean(values, na.rm = TRUE),
                    se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)))
    g <- ggplot(data = final, aes(x = interaction(factor(final$samples, levels = unique(x$samples)),
                                              factor(final$features, levels = c("Total", "Hyphopodia",
                                                                           "IntrHyphae", "Arbuscule", "Vesicle"))),
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
        geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
                                    (length(unique(z$samples)) + .5) * (num - 1),
                                    length(unique(z$samples))),
                   colour = "lightgrey") +
        labs(title = main, 
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 length(unique(z$samples)) * num + .5,
                                 length(unique(z$samples))),
                 y = 110, label = unique(final$features[order(match(final$features,
                                                        factor(c("Total",
                                                                 "Hyphopodia",
                                                                 "IntrHyphae",
                                                                 "Arbuscule",
                                                                 "Vesicle"),
                                                               levels = c("Total",
                                                                          "Hyphopodia",
                                                                          "IntrHyphae",
                                                                          "Arbuscule",
                                                                          "Vesicle"))))])) +
        annotate("text", x = 1:(length(unique(y$samples)) * num),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20))+ 
        scale_fill_manual(values = cbPalette,
                          breaks = levels(factor(final$samples,
                                                 levels = unique(x$samples))))
    class(a1) <- c("am_plot", class(a1))
    return(a1)
}

#' @export
am_boxplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Colonization", ...){
    Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- NULL
    dimen <- 0
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    if (annot == "none"){
        d <- rep("", length(unique(y$samples)) * num)
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- 3
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        dimen <- 3
    }
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -samples, -replicates)
    g <- ggplot(data = z,
                aes(x = interaction(factor(z$samples, levels = unique(x$samples)),
                                    factor(z$features, levels = c("Total", "Hyphopodia",
                                                                  "IntrHyphae", "Arbuscule", "Vesicle")),
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
                                    (length(unique(z$samples)) + .5) * (num - 1),
                                    length(unique(z$samples))),
                   colour = "lightgrey") +
        labs(title = main,
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 length(unique(z$samples)) * num + .5,
                                 length(unique(z$samples))),
                 y = 110, label = unique(z$features[order(match(z$features,
                                                        factor(c("Total",
                                                                 "Hyphopodia",
                                                                 "IntrHyphae",
                                                                 "Arbuscule",
                                                                 "Vesicle"),
                                                               levels = c("Total",
                                                                          "Hyphopodia",
                                                                          "IntrHyphae",
                                                                          "Arbuscule",
                                                                          "Vesicle"))))])) +
        annotate("text", x = 1:(length(unique(y$samples)) * num),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20))+ 
        scale_colour_manual(values = cbPalette, 
                            breaks = levels(factor(z$features,
                                                   levels = c("Total", "Hyphopodia",
                                                              "IntrHyphae", "Arbuscule", "Vesicle"))))
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_dotplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Colonization", ...){
    Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- NULL
    dimen <- 0
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    if (annot == "none"){
        d <- rep("", length(unique(y$samples)) * num)
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- 3
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        dimen <- 3
    }
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -samples, -replicates)
    g <- ggplot(data = z,
                aes(x = interaction(factor(z$samples, levels = unique(x$samples)),
                                    factor(z$features, levels = c("Total", "Hyphopodia",
                                                                  "IntrHyphae", "Arbuscule", "Vesicle")),
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
                                    (length(unique(z$samples)) + .5) * (num - 1),
                                    length(unique(z$samples))),
                   colour = "lightgrey") +
        labs(title = main,
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 length(unique(z$samples)) * num + .5,
                                 length(unique(z$samples))),
                 y = 110, label = unique(z$features[order(match(z$features,
                                                        factor(c("Total",
                                                                 "Hyphopodia",
                                                                 "IntrHyphae",
                                                                 "Arbuscule",
                                                                 "Vesicle"),
                                                               levels = c("Total",
                                                                          "Hyphopodia",
                                                                          "IntrHyphae",
                                                                          "Arbuscule",
                                                                          "Vesicle"))))])) +
        annotate("text", x = 1:(length(unique(y$samples)) * num),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20))+ 
        scale_colour_manual(values = cbPalette, 
                            breaks = levels(factor(z$features,
                                                   levels = c("Total", "Hyphopodia",
                                                              "IntrHyphae", "Arbuscule", "Vesicle"))))
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_stat.grid <- function(x, method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                         ...){
    method <- match.arg(method)
    stat <- .grid_stat(x, method = method) 
    return(stat)
}

###############################################################################

#' @export
am_summary.gridTime <- function(x){
    Arbuscule <- Hyphopodia <- IntrHyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- num <- n <- num_mean <- NULL
    y <- x %>% 
            group_by(samples, time, replicates) %>%
            summarise_if(is.numeric, mean, na.rm = TRUE) %>%
            ungroup
    final <- y %>%
        group_by(samples, time) %>%
        mutate(num = n()) %>%
        summarise_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
        mutate_at(vars(contains("_sd")), funs(. / sqrt(num_mean))) %>%
        select(-contains("num"))
    names(final)[grepl("_sd", names(final))] <- gsub("_sd", "_se",
                                                     names(final)[grepl("_sd", names(final))])
    #     final <- final[match(unique(x$samples), final$samples), ]
    l <- list(y, final)
    names(l) <- c("Summary per Replicate", "Summary per Sample")
    class(l) <- c("am_summary", "list")
    return(l)
}

#' @export
am_stat.gridTime <- function(x, method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                         ...){
    samples <- comb <- NULL
    method <- match.arg(method)
    stat <- .grid_statime(x, method = method) 
    return(stat)
}

#' @export
am_barplot.gridTime <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Colonization", ...){
    Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- n <- num <- means <- se <- NULL
    dimen <- math <- sterr <- 0
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    final <- am_summary(x)[[2]]
    tmp <- final %>%
        select("samples", "time", contains("_mean")) %>%
        gather(features, means, -samples, -time) %>%
        separate(features, c("features", "math")) %>% select(-math)
    tmp2 <- final %>%
        select("samples", "time", contains("_se")) %>%
        gather(features, sterr, -samples, -time) %>%
        separate(features, c("features", "math")) %>% select(-math)
    final <- inner_join(tmp, tmp2, by = c("samples", "time", "features")) %>%
        mutate(group = paste(samples, features, time, sep = "_"))
    final <- final[order(match(final$features, c("Total", "Hyphopodia", "IntrHyphae",
                                                 "Arbuscule", "Vesicle")),
                         final$time,
                         match(final$samples,unique(x$samples ))), ]
    final$order <- 1:nrow(final)
    num <- ncol(x)-3
    if (annot == "none"){
        d <- rep("", nrow(final))
    }
    if (annot == "asterisks"){
        stop("Asterisks do not work with an object of class gridTime")
    }
    if (annot == "letters"){
        stat <- .grid_statime(x, method = method, group = TRUE, alpha = alpha)
        stat <- stat %>% separate(sample, c("sample", "time"), "_")
        stat <- stat[order(stat$time, match(stat$sample, unique(x$samples))), ]
        d <- as.vector(as.matrix(stat[,3:ncol(stat)]))
        dimen <- 3
    }
    # Change table shape
#     if (annot == "none"){
    g <- ggplot(data = final, aes(x = order, y = means, fill = samples))
    a1 <- g + geom_col() +
        theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
        geom_errorbar(aes(ymin = means - sterr, ymax = means + sterr), width = .1) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = "none") +
        geom_vline(xintercept = c(0.5, seq(length(unique(final$samples)) + .5,
                                    (length(unique(final$samples)) + .5) * 
                                    ((length(unique(final$features)) * length(unique(final$samples))) - 1),
                                    length(unique(final$samples)))),
                   colour = "lightgrey") +
        labs(title = main, 
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        scale_x_continuous(breaks = 1:nrow(final), labels = rep(unique(final$samples), 
                                    ((length(unique(final$features)) *
                                      length(unique(final$samples)))))) +
        annotate("text", x = seq(length(unique(final$samples)) * .5 + .5,
                                 (length(unique(final$samples)) *
                                  length(unique(final$features)) *
                                  length(unique(final$time)) + .5),
                                 length(unique(final$samples))),
                 y = 105, label = rep(unique(final$features), each = 3)) +
        annotate("text", x = seq(length(unique(final$samples)) * .5 + .5,
                                 (length(unique(final$samples)) *
                                  length(unique(final$features)) *
                                  length(unique(final$time)) + .5),
                                 length(unique(final$samples))),
                 y = 110, label = rep(unique(final$time), length(unique(final$features)))) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20)) + 
        scale_fill_manual(values = cbPalette,
                          breaks = levels(factor(final$samples,
                                                 levels = unique(x$samples)))) +
        annotate("text", x = 0, y = c(105, 110), label = c("Feature:", "Time:"),
                 size = 3, hjust = 1) +
        annotate("text", x = 1:nrow(final),
                 y = -Inf, vjust = -0.5, label = d, size = dimen)
    class(a1) <- c("am_plot", class(a1))
    return(a1)
}


#' @export
am_boxplot.gridTime <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Colonization",
                            lab = "days", ...){
    Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- NULL
    dimen <- 0
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    final <- am_summary(x)[[1]]
    #     tmp <- final %>%
    #         select("samples", "time", contains("_mean")) %>%
    #         gather(features, means, -samples, -time) %>%
    #         separate(features, c("features", "math")) %>% select(-math)
    #     tmp2 <- final %>%
    #         select("samples", "time", contains("_se")) %>%
    #         gather(features, sterr, -samples, -time) %>%
    #         separate(features, c("features", "math")) %>% select(-math)
    #     final <- inner_join(tmp, tmp2, by = c("samples", "time", "features")) %>%
    #         mutate(group = paste(samples, features, time, sep = "_"))
    #     final <- final[order(match(final$features, c("Total", "Hyphopodia", "IntrHyphae",
    #                                                  "Arbuscule", "Vesicle")),
    #                          final$time,
    #                          match(final$samples,unique(x$samples ))), ]
    #     final$order <- 1:nrow(final)
    num <- ncol(x)-3
    if (annot == "none"){
        d <- rep("", length(table(z$group)))
    }
    if (annot == "asterisks"){
        stop("Asterisks do not work with an object of class gridTime")
    }
    if (annot == "letters"){
        stat <- .grid_statime(x, method = method, group = TRUE, alpha = alpha)
        stat <- stat %>% separate(sample, c("sample", "time"), "_")
        stat <- stat[order(stat$time, match(stat$sample, unique(x$samples))), ]
        d <- as.vector(as.matrix(stat[,3:ncol(stat)]))
        dimen <- 3
    }
    # Change table shape
    z <- final %>% tidyr::gather(features, values, -samples, -time, -replicates)
    z$group <- paste(z$samples, z$time, z$features, sep = "_")
    z <- z[order(match(z$features, c("Total", "Hyphopodia", "IntrHyphae",
                                                 "Arbuscule", "Vesicle")),
                         z$time,
                         match(z$samples,unique(x$samples ))), ]
    z$order <- rep(1:length(table(z$group)), table(z$group))
    g <- ggplot(data = z,
                aes(x = factor(z$group, levels=unique(z$group)),
                          y = values, color = samples))
    a2 <- g +
        geom_boxplot() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = "none") +
        geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
                                    length(unique(z$features)) *
                                    length(unique(z$samples)) * 
                                    length(unique(z$time)),
                                length(unique(z$samples))),
                   colour = "lightgrey") +
        labs(title = main,
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        scale_x_discrete(labels = rep(unique(z$samples), 
                                    ((length(unique(z$features)) *
                                      length(unique(z$samples)))))) +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 (length(unique(z$samples)) *
                                  length(unique(z$features)) *
                                  length(unique(z$time)) + .5),
                                 length(unique(z$samples))),
                 y = 105, label = rep(unique(z$features), each = 3)) +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 (length(unique(z$samples)) *
                                  length(unique(z$features)) *
                                  length(unique(z$time)) + .5),
                                 length(unique(z$samples))),
                 y = 110, label = paste0(rep(unique(z$time),
                                            length(unique(z$features))),
                                        " ", lab)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20)) + 
        scale_colour_manual(values = cbPalette,
                          breaks = levels(factor(z$samples,
                                                 levels = unique(x$samples)))) +
#         annotate("text", x = 2, y = c(105, 110), label = c("Feature:", "Time:"),
#                  size = 3, hjust = 1) +
        annotate("text", x = 1:length(table(z$group)),
                 y = -Inf, vjust = -0.5, label = d, size = dimen)
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_dotplot.gridTime <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Colonization",
                            lab = "days", ...){
    Arbuscule <- Hypopodia <- Intr_Hyphae <- Total <- Vesicle <- comp <- NULL
    features <- replicates <- samples <- values <- NULL
    dimen <- 0
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    final <- am_summary(x)[[1]]
    #     tmp <- final %>%
    #         select("samples", "time", contains("_mean")) %>%
    #         gather(features, means, -samples, -time) %>%
    #         separate(features, c("features", "math")) %>% select(-math)
    #     tmp2 <- final %>%
    #         select("samples", "time", contains("_se")) %>%
    #         gather(features, sterr, -samples, -time) %>%
    #         separate(features, c("features", "math")) %>% select(-math)
    #     final <- inner_join(tmp, tmp2, by = c("samples", "time", "features")) %>%
    #         mutate(group = paste(samples, features, time, sep = "_"))
    #     final <- final[order(match(final$features, c("Total", "Hyphopodia", "IntrHyphae",
    #                                                  "Arbuscule", "Vesicle")),
    #                          final$time,
    #                          match(final$samples,unique(x$samples ))), ]
    #     final$order <- 1:nrow(final)
    num <- ncol(x)-3
    if (annot == "none"){
        d <- rep("", length(table(z$group)))
    }
    if (annot == "asterisks"){
        stop("Asterisks do not work with an object of class gridTime")
    }
    if (annot == "letters"){
        stat <- .grid_statime(x, method = method, group = TRUE, alpha = alpha)
        stat <- stat %>% separate(sample, c("sample", "time"), "_")
        stat <- stat[order(stat$time, match(stat$sample, unique(x$samples))), ]
        d <- as.vector(as.matrix(stat[,3:ncol(stat)]))
        dimen <- 3
    }
    # Change table shape
    z <- final %>% tidyr::gather(features, values, -samples, -time, -replicates)
    z$group <- paste(z$samples, z$time, z$features, sep = "_")
    z <- z[order(match(z$features, c("Total", "Hyphopodia", "IntrHyphae",
                                                 "Arbuscule", "Vesicle")),
                         z$time,
                         match(z$samples,unique(x$samples ))), ]
    z$order <- rep(1:length(table(z$group)), table(z$group))
    g <- ggplot(data = z,
                aes(x = factor(z$group, levels=unique(z$group)),
                          y = values, color = samples))
    a2 <- g +
        geom_jitter() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = "none") +
        geom_vline(xintercept = seq(length(unique(z$samples)) + .5,
                                    length(unique(z$features)) *
                                    length(unique(z$samples)) * 
                                    length(unique(z$time)),
                                length(unique(z$samples))),
                   colour = "lightgrey") +
        labs(title = main,
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        scale_x_discrete(labels = rep(unique(z$samples), 
                                    ((length(unique(z$features)) *
                                      length(unique(z$samples)))))) +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 (length(unique(z$samples)) *
                                  length(unique(z$features)) *
                                  length(unique(z$time)) + .5),
                                 length(unique(z$samples))),
                 y = 105, label = rep(unique(z$features), each = 3)) +
        annotate("text", x = seq(length(unique(z$samples)) * .5 + .5,
                                 (length(unique(z$samples)) *
                                  length(unique(z$features)) *
                                  length(unique(z$time)) + .5),
                                 length(unique(z$samples))),
                 y = 110, label = paste0(rep(unique(z$time),
                                            length(unique(z$features))),
                                        " ", lab)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20)) + 
        scale_colour_manual(values = cbPalette,
                          breaks = levels(factor(z$samples,
                                                 levels = unique(x$samples)))) +
#         annotate("text", x = 2, y = c(105, 110), label = c("Feature:", "Time:"),
#                  size = 3, hjust = 1) +
        annotate("text", x = 1:length(table(z$group)),
                 y = -Inf, vjust = -0.5, label = d, size = dimen)
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}


