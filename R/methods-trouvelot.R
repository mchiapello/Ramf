#' @export
am_summary.trouvelot <- function(x){
    A <- M <- a <- num <- Samples <- n <- NULL
    tmp <- trouvelot_summary(x)
    # Final table
    final <- tmp %>%
    group_by(Samples) %>%
    mutate(num = dplyr::n()) %>%
    summarise(`Mean F` = round(mean(F, na.rm = TRUE), 2),
              `Standard error F` = round(sd(F, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
              `Mean M` = round(mean(M, na.rm = TRUE), 2),
              `Standard error M` = round(sd(M, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
              `Mean a` = round(mean(a, na.rm = TRUE), 2),
              `Standar error a` = round(sd(a, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2),
              `Mean A` = round(mean(A, na.rm = TRUE), 2),
              `Standard error A` = round(sd(A, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)), 2)
              )
    final <- final[match(unique(x$Samples), final$Samples), ]
    l <- list(tmp, final)
    names(l) <- c("Summary per Replicate", "Summary per Sample")
    class(l) <- c("am_summary", "list")
    return(l)
}

#' @export
am_barplot2.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                                  "#009E73", "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                                 main = "Trouvelot method", ...){
    A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
    mA <- n_myc <- nn <- num <- perc <- Replicates <- Samples <- Scoring <- tmpa <- tot <- tot2 <- value <- n <- NULL
    values <- means <- se <- num <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    tmp <- trouvelot_summary(x)
    if (annot == "none"){
        d <- rep("", length(unique(tmp$Samples)) * 4)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .trouvelot_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == tmp$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:4, each = length(unique(tmp$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .trouvelot_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:5]))
        dimen <- annot_size
    }
    z <- tmp %>% tidyr::gather(features, values, -Samples, -Replicates)
    final <- z %>% group_by(Samples, features) %>%
          mutate(num = dplyr::n()) %>%
          summarize(means = mean(values, na.rm = TRUE),
                    se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE)))
    g <- ggplot(data = final, aes(x = interaction(factor(final$Samples, levels = unique(x$Samples)),
                                                   factor(final$features, levels = c("F", "M", "a", "A"))),
                                                   y = means, fill = Samples))
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
        geom_vline(xintercept = seq(length(unique(final$Samples)) + .5, length(unique(final$Samples)) * 3 + .5,
                                    length(unique(final$Samples))), colour = "lightgrey") +
                      #         geom_hline(yintercept = 105, colour = "lightgrey") +
            labs(title = main, 
                 #                  subtitle = "Trouvelot method",
                 x = "",
                 y = "") +
            annotate("text", x = seq(length(unique(final$Samples)) * .5 + .5, length(unique(final$Samples)) * 5 + .5,
                                     length(unique(final$Samples)))[1:4],
                     y = 110, label = c("F%", "M%", "a%", "A%")) +
        annotate("text", x = 1:(length(unique(tmp$Samples)) * 4),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
            scale_x_discrete(labels = rep(unique(x$Samples), 5)) +
            scale_y_continuous(limits = c(ifelse(min(final$means - final$se) < 0,
                    min(final$means - final$se), 0), 110), breaks = seq(0, 110, 20)) +
            scale_fill_manual(values = cbPalette,
                              breaks = levels(factor(final$Samples, levels = unique(x$Samples))),
                              name = "")
    class(a1) <- c("am_plot", class(a1))
    return(a1)
}

#' @export
am_boxplot2.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                                  "#009E73", "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                                 main = "Trouvelot method", ...){
    A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final <- m <- NULL
    mA <- n_myc <- nn <- num <- perc <- Replicates <- Samples <- Scoring <- tmpa <- tot <- tot2 <- value <- NULL
    values <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    tmp <- trouvelot_summary(x)
    if (annot == "none"){
        d <- rep("", length(unique(tmp$Samples)) * 4)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .trouvelot_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == tmp$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:4, each = length(unique(tmp$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .trouvelot_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:5]))
        dimen <- annot_size
    }
    final <- tmp %>% gather(feature, value, -Samples, -Replicates)
    g <- ggplot(data = final, aes(x = interaction(factor(final$Samples, levels = unique(x$Samples)),
                                              factor(final$feature, levels = c("F", "A", "a", "M")),
                                              sep = ": "),
                              y = value, color = Samples))
    a2 <- g +
        geom_boxplot() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        geom_vline(xintercept = seq(length(unique(final$Samples)) + .5, length(unique(final$Samples)) * 3 + .5,
                                    length(unique(final$Samples))), colour = "lightgrey") +
                      #     geom_hline(yintercept = 105, colour = "lightgrey") +
        labs(title = main, 
             #              subtitle = "Trouvelot method",
             x = "",
             y = "") +
        annotate("text", x = seq(length(unique(final$Samples)) * .5 + .5, length(unique(final$Samples)) * 5 + .5,
                                 length(unique(final$Samples)))[1:4],
                 y = 110, label = c("F%", "M%", "a%", "A%")) +
        annotate("text", x = 1:(length(unique(tmp$Samples)) * 4),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$Samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110), breaks = seq(0, 110, 20)) +
        scale_colour_manual(values = cbPalette, 
                          breaks = levels(factor(final$Samples,
                                                 levels = unique(x$Samples))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_dotplot2.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                                  "#009E73", "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                                 main = "Trouvelot method", ...){
    A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final <- m <- NULL
    mA <- n_myc <- nn <- num <- perc <- Replicates <- Samples <- Scoring <- tmpa <- tot <- tot2 <- value <- NULL
    values <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    tmp <- trouvelot_summary(x)
    if (annot == "none"){
        d <- rep("", length(unique(tmp$Samples)) * 4)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .trouvelot_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == tmp$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:4, each = length(unique(tmp$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .trouvelot_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:5]))
        dimen <- annot_size
    }
    final <- tmp %>% gather(feature, value, -Samples, -Replicates)
    g <- ggplot(data = final, aes(x = interaction(factor(final$Samples, levels = unique(x$Samples)),
                                              factor(final$feature, levels = c("F", "A", "a", "M")),
                                              sep = ": "),
                              y = value, color = as.factor(Samples)))
    a2 <- g +
        geom_point(position = position_jitter(width = 0.2)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        geom_vline(xintercept = seq(length(unique(final$Samples)) + .5, length(unique(final$Samples)) * 3 + .5,
                                    length(unique(final$Samples))), colour = "lightgrey") +
                      #     geom_hline(yintercept = 105, colour = "lightgrey") +
        labs(title = main, 
             #              subtitle = "Trouvelot method",
             x = "",
             y = "") +
        annotate("text", x = seq(length(unique(final$Samples)) * .5 + .5, length(unique(final$Samples)) * 5 + .5,
                                 length(unique(final$Samples)))[1:4],
                 y = 110, label = c("F%", "M%", "a%", "A%")) +
        annotate("text", x = 1:(length(unique(tmp$Samples)) * 4),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$Samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110), breaks = seq(0, 110, 20)) +
        scale_colour_manual(values = cbPalette, 
                          breaks = levels(factor(final$Samples,
                                                 levels = unique(x$Samples))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_stat.trouvelot <- function(x, method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"), ...){
    method <- match.arg(method)
    stat <- .trouvelot_stat(x, method = method)
    return(stat)
}

###############################################################################

#' @export
am_barplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                                  "#009E73", "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            legend = c("right", "left", "top", "bottom"),
                            main = "Trouvelot method", ...){
    A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
    mA <- n_myc <- nn <- num <- perc <- Replicates <- Samples <- Scoring <- tmpa <- tot <- tot2 <- value <- n <- NULL
    values <- means <- se <- num <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    legend <- match.arg(legend)
    # Create summary table
    y <- trouvelot_summary(x)
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates)
    final <- z %>% group_by(Samples, features) %>%
          mutate(num = dplyr::n()) %>%
          summarize(means = mean(values, na.rm = TRUE),
                    se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE))) %>%
          ungroup %>%
          mutate(features = factor(features, levels = c("F", "M", "a", "A")),
                 Samples = factor(Samples, levels = unique(x$Samples))) %>%
          arrange(features, Samples)
    # Add annotations
    if (annot == "none"){
        final  <- final %>%
            mutate(annot = rep("", nrow(final))) %>%
            group_by(Samples)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .trouvelot_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:4, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        final  <- final %>%
            mutate(annot = d) %>%
            group_by(Samples)
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .trouvelot_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:5]))
        final  <- final %>%
            mutate(annot = d) %>%
            group_by(Samples)
        dimen <- annot_size
    }
    g <- ggplot(data = final, aes(x = features, y = means, fill = Samples))
    dodge <- position_dodge(width=0.9)
    a1 <- g + geom_col(position = dodge) + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
        geom_errorbar(aes(ymin = means - se, ymax = means + se), width = .1, position = dodge) +
        theme_bw() +
        theme(plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = legend) +
        labs(title = main, 
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        geom_text(aes( label = annot, y = (means + se)), vjust = -0.5, position = dodge,
                  show.legend = FALSE, size = dimen)  +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20))+ 
        scale_fill_manual(values = cbPalette,
                          breaks = levels(factor(final$Samples,
                                                 levels = unique(x$Samples))),
                          name = "")
    class(a1) <- c("am_plot", class(a1))
    return(a1)
}

#' @export
am_boxplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                                  "#009E73", "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            legend = c("right", "left", "top", "bottom"),
                            main = "Trouvelot method", ...){
    A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
    mA <- n_myc <- nn <- num <- perc <- Replicates <- Samples <- Scoring <- tmpa <- tot <- tot2 <- value <- Var1 <- NULL
    values <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    legend <- match.arg(legend)
    # Create summary table
    y <- trouvelot_summary(x)
    final <- y %>% 
        gather(features, values, -Samples, -Replicates) %>%
        ungroup %>%
        mutate(features = factor(features, levels = c("F", "M", "a", "A")),
               Samples = factor(Samples, levels = unique(x$Samples))) %>%
        arrange(features, Samples)
    # Add annotations
    if (annot == "none"){
        tmp <- expand.grid(unique(final$features), unique(final$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = "")
        an <- final %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .trouvelot_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:4, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        tmp <- expand.grid(unique(final$features), unique(final$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- final %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .trouvelot_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:5]))
        tmp <- expand.grid(unique(final$features), unique(final$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- final %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    g <- ggplot(data = final, aes(x = features,
                              y = values, color = Samples))
    dodge <- position_dodge(width=0.75)
    a2 <- g +
        geom_boxplot() +
        theme_bw() +
        theme(plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = legend) +
        geom_vline(xintercept = seq(1.5, length(unique(an$features))-0.5, 1),
                   colour = "lightgrey") +
        labs(title = main,
             #              subtitle = "Grid method",
             x = "",
             y = "") +
        geom_text(data = an, aes(x = features, label = annot, y = values), vjust = -0.8, 
                  position = dodge, show.legend = FALSE, size = dimen) +
        scale_y_continuous(limits = c(-0.5, 105),
                           breaks = seq(0, 105, 20))+ 
        scale_colour_manual(values = cbPalette, 
                          breaks = levels(factor(final$Samples,
                                                 levels = unique(x$Samples))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_dotplot.trouvelot <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                                  "#009E73", "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            legend = c("right", "left", "top", "bottom"),
                            main = "Trouvelot method", ...){
    A <- Abundance <- Colonization <- M <- M1 <- a <- feature <- features <- final_a <- m <- NULL
    mA <- n_myc <- nn <- num <- perc <- Replicates <- Samples <- Scoring <- tmpa <- tot <- tot2 <- value <- Var1 <- NULL
    values <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    legend <- match.arg(legend)
    # Create summary table
    y <- trouvelot_summary(x)
    final <- y %>% 
        gather(features, values, -Samples, -Replicates) %>%
        ungroup %>%
        mutate(features = factor(features, levels = c("F", "M", "a", "A")),
               Samples = factor(Samples, levels = unique(x$Samples))) %>%
        arrange(features, Samples)
    # Add annotations
    if (annot == "none"){
        tmp <- expand.grid(unique(final$features), unique(final$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = "")
        an <- final %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .trouvelot_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:6])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:4, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        tmp <- expand.grid(unique(final$features), unique(final$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- final %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .trouvelot_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:5]))
        tmp <- expand.grid(unique(final$features), unique(final$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- final %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    g <- ggplot(data = final, aes(x = features,
                              y = values, color = Samples))
    dodge <- position_dodge(width=0.9)
    a2 <- g +
        geom_point(position = position_jitterdodge(dodge.width = 0.9,
                                                   jitter.width = 0.1)) +
        theme_bw() +
        theme(plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = legend) +
        geom_vline(xintercept = seq(1.5, length(unique(an$features))-0.5, 1),
                   colour = "lightgrey") +
        labs(title = main,
             x = "",
             y = "") +
        geom_text(data = an, aes(label = annot, y = values), vjust = -0.8, 
                  position = dodge, show.legend = FALSE, size = dimen) +
        scale_y_continuous(limits = c(-0.5, 105),
                           breaks = seq(0, 105, 20))+ 
        scale_colour_manual(values = cbPalette, 
                          breaks = levels(factor(final$Samples,
                                                 levels = unique(x$Samples))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

