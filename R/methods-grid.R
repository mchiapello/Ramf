#' @export
am_summary.grid <- function(x){
    Arbuscules <- Hyphopodia <- IntrHyphae <- Total <- Vesicles <- comp <-. <- NULL
    features <- Replicates <- Samples <- values <- num <- n <- num_mean <- NULL
    tmp <- grid_summary(x)
    final <- tmp %>%
        group_by(Samples) %>%
        mutate(num = dplyr::n()) %>%
        summarise_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
        mutate(across(contains("_sd"), ~(. / sqrt(num_mean)))) %>%
        select(-contains("num"))
    names(final)[grepl("_sd", names(final))] <- gsub("_sd", "_se",
                                                     names(final)[grepl("_sd", names(final))])
    final <- final[match(unique(x$Samples), final$Samples), ]
    l <- list(tmp, final)
    names(l) <- c("Summary per Replicate", "Summary per Sample")
    class(l) <- c("am_summary", "list")
    return(l)
}

#' @export
am_stat.grid <- function(x, method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                         ...){
    method <- match.arg(method)
    stat <- .grid_stat(x, method = method) 
    return(stat)
}

#' @export
am_barplot2.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Gridline intersect method", ...){
    Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- final <- NULL
    features <- Replicates <- Samples <- values <- n <- num <- means <- se <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    if (annot == "none"){
        d <- rep("", length(unique(y$Samples)) * num)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        dimen <- annot_size
    }
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates)
    final <- z %>% group_by(Samples, features) %>%
          mutate(num = dplyr::n()) %>%
          summarize(means = mean(values, na.rm = TRUE),
                    se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE))) %>%
          ungroup %>%
          mutate(features = factor(features, levels = c("Total", "Hyphopodia",
                                                        "IntrHyphae", "Arbuscules",
                                                        "Vesicles")),
                 Samples = factor(Samples, levels = unique(x$Samples))) %>%
          arrange(features, Samples) %>%
          group_by(Samples)
    g <- ggplot(data = final, aes(x = interaction(factor(final$Samples, levels = unique(x$Samples)),
                                              factor(final$features, levels = c("Total", "Hyphopodia",
                                                                           "IntrHyphae", "Arbuscules", "Vesicles"))),
                                              y = means, fill = Samples))
    a1 <- g + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
        geom_errorbar(aes(ymin = means - se, ymax = means + se), width = .1) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.margin=margin(10,10,10,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              panel.grid.minor.x = element_blank(),
              legend.position = "none") +
        geom_vline(xintercept = seq(length(unique(z$Samples)) + .5,
                                    (length(unique(z$Samples)) + .5) * (num - 1),
                                    length(unique(z$Samples))),
                   colour = "lightgrey") +
        labs(title = main, 
             color = "",
             x = "",
             y = "root length colonized [%]") +
        annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
                                 length(unique(z$Samples)) * num + .5,
                                 length(unique(z$Samples))),
                 y = 110, label = unique(final$features[order(match(final$features,
                                                        factor(c("Total",
                                                                 "Hyphopodia",
                                                                 "IntrHyphae",
                                                                 "Arbuscules",
                                                                 "Vesicles"),
                                                               levels = c("Total",
                                                                          "Hyphopodia",
                                                                          "IntrHyphae",
                                                                          "Arbuscules",
                                                                          "Vesicles"))))])) +
        annotate("text", x = 1:(length(unique(y$Samples)) * num),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$Samples), 5)) +
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
am_boxplot2.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Gridline intersect method", ...){
    Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
    features <- Replicates <- Samples <- values <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    if (annot == "none"){
        d <- rep("", length(unique(y$Samples)) * num)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        dimen <- annot_size
    }
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates) %>%
          mutate(features = factor(features, levels = c("Total", "Hyphopodia",
                                                        "IntrHyphae", "Arbuscules",
                                                        "Vesicles")),
                 Samples = factor(Samples, levels = unique(x$Samples))) %>%
          arrange(features, Samples)
    g <- ggplot(data = z,
                aes(x = interaction(factor(z$Samples, levels = unique(x$Samples)),
                                    factor(z$features, levels = c("Total", "Hyphopodia",
                                                                  "IntrHyphae", "Arbuscules", "Vesicles")),
                                          sep = ": "),
                          y = values, color = Samples))
    a2 <- g +
        geom_boxplot() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.margin=margin(10,10,10,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              panel.grid.minor.x = element_blank()) +
        geom_vline(xintercept = seq(length(unique(z$Samples)) + .5,
                                    (length(unique(z$Samples)) + .5) * (num - 1),
                                    length(unique(z$Samples))),
                   colour = "lightgrey") +
        labs(title = main,
             color = "",
             x = "",
             y = "root length colonized [%]") +
        annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
                                 length(unique(z$Samples)) * num + .5,
                                 length(unique(z$Samples))),
                 y = 110, label = unique(z$features[order(match(z$features,
                                                        factor(c("Total",
                                                                 "Hyphopodia",
                                                                 "IntrHyphae",
                                                                 "Arbuscules",
                                                                 "Vesicles"),
                                                               levels = c("Total",
                                                                          "Hyphopodia",
                                                                          "IntrHyphae",
                                                                          "Arbuscules",
                                                                          "Vesicles"))))])) +
        annotate("text", x = 1:(length(unique(y$Samples)) * num),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$Samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20))+ 
        scale_colour_manual(values = cbPalette, 
                            breaks = levels(factor(z$features,
                                                   levels = c("Total", "Hyphopodia",
                                                              "IntrHyphae", "Arbuscules", "Vesicles"))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_dotplot2.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            main = "Gridline intersect method", ...){
    Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
    features <- Replicates <- Samples <- values <- final <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    if (annot == "none"){
        d <- rep("", length(unique(y$Samples)) * num)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        dimen <- annot_size
    }
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates) %>%
          mutate(features = factor(features, levels = c("Total", "Hyphopodia",
                                                        "IntrHyphae", "Arbuscules",
                                                        "Vesicles")),
                 Samples = factor(Samples, levels = unique(x$Samples))) %>%
          arrange(features, Samples)
    g <- ggplot(data = z,
                aes(x = interaction(factor(z$Samples, levels = unique(x$Samples)),
                                    factor(z$features, levels = c("Total", "Hyphopodia",
                                                                  "IntrHyphae", "Arbuscules", "Vesicles")),
                                          sep = ": "),
                          y = values, color = Samples))
    a2 <- g +
        geom_point(position = position_jitter(width = 0.2)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.margin=margin(10,10,10,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              panel.grid.minor.x = element_blank()) +
        geom_vline(xintercept = seq(length(unique(z$Samples)) + .5,
                                    (length(unique(z$Samples)) + .5) * (num - 1),
                                    length(unique(z$Samples))),
                   colour = "lightgrey") +
        labs(title = main,
             color = "",
             x = "",
             y = "root length colonized [%]") +
        annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
                                 length(unique(z$Samples)) * num + .5,
                                 length(unique(z$Samples))),
                 y = 110, label = unique(z$features[order(match(z$features,
                                                        factor(c("Total",
                                                                 "Hyphopodia",
                                                                 "IntrHyphae",
                                                                 "Arbuscules",
                                                                 "Vesicles"),
                                                               levels = c("Total",
                                                                          "Hyphopodia",
                                                                          "IntrHyphae",
                                                                          "Arbuscules",
                                                                          "Vesicles"))))])) +
        annotate("text", x = 1:(length(unique(y$Samples)) * num),
                 y = -Inf, vjust = -0.5, label = d, size = dimen) +
        scale_x_discrete(labels = rep(unique(x$Samples), 5)) +
        scale_y_continuous(limits = c(-0.5, 110),
                           breaks = seq(0, 110, 20))+ 
        scale_colour_manual(values = cbPalette, 
                            breaks = levels(factor(z$features,
                                                   levels = c("Total", "Hyphopodia",
                                                              "IntrHyphae", "Arbuscules", "Vesicles"))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

###############################################################################

# #' @export
# am_summary.gridTime <- function(x){
#     Arbuscules <- Hyphopodia <- IntrHyphae <- Total <- Vesicles <- comp <- . <- NULL
#     features <- Replicates <- Samples <- values <- num <- n <- num_mean <- NULL
#     y <- x %>% 
#             group_by(Samples, time, Replicates) %>%
#             summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#             ungroup
#     final <- y %>%
#         group_by(Samples, time) %>%
#         mutate(num = n()) %>%
#         summarise_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
#         mutate_at(vars(contains("_sd")), funs(. / sqrt(num_mean))) %>%
#         select(-contains("num"))
#     names(final)[grepl("_sd", names(final))] <- gsub("_sd", "_se",
#                                                      names(final)[grepl("_sd", names(final))])
#     #     final <- final[match(unique(x$Samples), final$Samples), ]
#     l <- list(y, final)
#     names(l) <- c("Summary per Replicate", "Summary per Sample")
#     class(l) <- c("am_summary", "list")
#     return(l)
# }
# 
# #' @export
# am_stat.gridTime <- function(x, method = c("none","holm","hommel", "hochberg",
#                                        "bonferroni", "BH", "BY", "fdr"),
#                          ...){
#     Samples <- comb <- NULL
#     method <- match.arg(method)
#     stat <- .grid_statime(x, method = method) 
#     return(stat)
# }
# 
# #' @export
# am_barplot.gridTime <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
#                                              "#009E73", "#F0E442", "#0072B2",
#                                              "#D55E00", "#CC79A7"),
#                             alpha = 0.05,
#                             annot = c("none", "asterisks", "letters"),
#                             method = c("none","holm","hommel", "hochberg",
#                                        "bonferroni", "BH", "BY", "fdr"),
#                             main = "Gridline intersect method",
#                             lab = "days", ...){
#     Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
#     features <- Replicates <- Samples <- values <- n <- num <- means <- se <- NULL
#     dimen <- math <- sterr <- 0
#     alpha <- alpha
#     annot <- match.arg(annot)
#     method <- match.arg(method)
#     # Create summary table
#     final <- am_summary(x)[[2]]
#     tmp <- final %>%
#         select("Samples", "time", contains("_mean")) %>%
#         gather(features, means, -Samples, -time) %>%
#         separate(features, c("features", "math")) %>% select(-math)
#     tmp2 <- final %>%
#         select("Samples", "time", contains("_se")) %>%
#         gather(features, sterr, -Samples, -time) %>%
#         separate(features, c("features", "math")) %>% select(-math)
#     final <- inner_join(tmp, tmp2, by = c("Samples", "time", "features")) %>%
#         mutate(group = paste(Samples, features, time, sep = "_"))
#     final <- final[order(match(final$features, c("Total", "Hyphopodia", "IntrHyphae",
#                                                  "Arbuscules", "Vesicles")),
#                          final$time,
#                          match(final$Samples,unique(x$Samples ))), ]
#     final$order <- 1:nrow(final)
#     num <- ncol(x)-3
#     if (annot == "none"){
#         d <- rep("", nrow(final))
#     }
#     if (annot == "asterisks"){
#         stop("Asterisks do not work with an object of class gridTime")
#     }
#     if (annot == "letters"){
#         stat <- .grid_statime(x, method = method, group = TRUE, alpha = alpha)
#         stat <- stat %>% separate(sample, c("sample", "time"), "_")
#         stat <- stat[order(stat$time, match(stat$sample, unique(x$Samples))), ]
#         d <- as.vector(as.matrix(stat[,3:ncol(stat)]))
#         dimen <- 3
#     }
#     # Change table shape
# #     if (annot == "none"){
#     g <- ggplot(data = final, aes(x = as.factor(order), y = means, fill = Samples))
#     a1 <- g + geom_col() +
#         theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
#         geom_errorbar(aes(ymin = means - sterr, ymax = means + sterr), width = .1) +
#         theme_bw() +
#         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#               plot.title = element_text(size = 19),
#               panel.grid.major.y = element_blank(),
#               panel.grid.minor.y = element_blank(),
#               panel.grid.major.x = element_blank(),
#               panel.grid.minor.x = element_blank(),
#               legend.position = "none") +
#         geom_vline(xintercept = seq(length(unique(final$Samples)) + .5,
#                                     ((length(unique(final$Samples)) + .5) * 
#                                     ((length(unique(final$features)) *
#                                       length(unique(final$time))) - 1)) -1,
#                                     length(unique(final$Samples))),
#                    colour = "lightgrey") +
#         labs(title = main, 
#              #              subtitle = "Grid method",
#              x = "",
#              y = "root length colonized [%]") +
#         scale_x_discrete(labels = rep(unique(final$Samples),
#                                       length(unique(final$time)) *
#                                       length(unique(final$features)))) +
# #         scale_x_continuous(breaks = 1:nrow(final), labels = rep(unique(final$Samples), 
# #                                     ((length(unique(final$features)) *
# #                                       length(unique(final$Samples)))))) +
#         annotate("text", x = seq(length(unique(final$Samples)) * .5 + .5,
#                                  (length(unique(final$Samples)) *
#                                   length(unique(final$features)) *
#                                   length(unique(final$time)) + .5),
#                                  length(unique(final$Samples))),
#                  y = 110, label = rep(unique(final$features),
#                                       each = length(unique(final$time)))) +
#         annotate("text", x = seq(length(unique(final$Samples)) * .5 + .5,
#                                  (length(unique(final$Samples)) *
#                                   length(unique(final$features)) *
#                                   length(unique(final$time)) + .5),
#                                  length(unique(final$Samples))),
#                  y = 106.5, label = paste0(rep(unique(final$time),
#                                              length(unique(final$features))),
#                                          " ", lab), size = 3) +
#         scale_y_continuous(limits = c(-0.5, 110),
#                            breaks = seq(0, 110, 20)) + 
#         scale_fill_manual(values = cbPalette,
#                           breaks = levels(factor(final$Samples,
#                                                  levels = unique(x$Samples)))) +
#         annotate("text", x = 1:nrow(final),
#                  y = -Inf, vjust = -0.5, label = d, size = dimen)
#     class(a1) <- c("am_plot", class(a1))
#     return(a1)
# }
# 
# #' @export
# am_boxplot.gridTime <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
#                                              "#009E73", "#F0E442", "#0072B2",
#                                              "#D55E00", "#CC79A7"),
#                             alpha = 0.05,
#                             annot = c("none", "asterisks", "letters"),
#                             method = c("none","holm","hommel", "hochberg",
#                                        "bonferroni", "BH", "BY", "fdr"),
#                             main = "Gridline intersect method",
#                             lab = "days", ...){
#     Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
#     features <- Replicates <- Samples <- values <- NULL
#     dimen <- 0
#     alpha <- alpha
#     annot <- match.arg(annot)
#     method <- match.arg(method)
#     # Create summary table
#     final <- am_summary(x)[[1]]
#     num <- ncol(x)-3
#     if (annot == "none"){
#         d <- rep("", length(table(paste(final$Samples, final$time))) * num)
#     }
#     if (annot == "asterisks"){
#         stop("Asterisks do not work with an object of class gridTime")
#     }
#     if (annot == "letters"){
#         stat <- .grid_statime(x, method = method, group = TRUE, alpha = alpha)
#         stat <- stat %>% separate(sample, c("sample", "time"), "_")
#         stat <- stat[order(stat$time, match(stat$sample, unique(x$Samples))), ]
#         d <- as.vector(as.matrix(stat[,3:ncol(stat)]))
#         dimen <- 3
#     }
#     # Change table shape
#     z <- final %>% tidyr::gather(features, values, -Samples, -time, -Replicates)
#     z$group <- paste(z$Samples, z$time, z$features, sep = "_")
#     z <- z[order(match(z$features, c("Total", "Hyphopodia", "IntrHyphae",
#                                                  "Arbuscules", "Vesicles")),
#                          z$time,
#                          match(z$Samples,unique(x$Samples ))), ]
#     z$order <- rep(1:length(table(z$group)), table(z$group))
#     g <- ggplot(data = z,
#                 aes(x = factor(z$group, levels=unique(z$group)),
#                           y = values, color = Samples))
#     a2 <- g +
#         geom_boxplot() +
#         theme_bw() +
#         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#               plot.title = element_text(size = 19),
#               panel.grid.major.y = element_blank(),
#               panel.grid.minor.y = element_blank(),
#               panel.grid.major.x = element_blank(),
#               panel.grid.minor.x = element_blank(),
#               legend.position = "none") +
#         geom_vline(xintercept = seq(length(unique(z$Samples)) + .5,
#                                     length(unique(z$features)) *
#                                     length(unique(z$Samples)) * 
#                                     length(unique(z$time)),
#                                 length(unique(z$Samples))),
#                    colour = "lightgrey") +
#         labs(title = main,
#              #              subtitle = "Grid method",
#              x = "",
#              y = "root length colonized [%]") +
#         scale_x_discrete(labels = rep(unique(z$Samples), 
#                                     ((length(unique(z$features)) *
#                                       length(unique(z$Samples)))))) +
#         annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
#                                  (length(unique(z$Samples)) *
#                                   length(unique(z$features)) *
#                                   length(unique(z$time)) + .5),
#                                  length(unique(z$Samples))),
#                  y = 110, label = rep(unique(z$features),
#                                       each = length(unique(z$time)))) +
#         annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
#                                  (length(unique(z$Samples)) *
#                                   length(unique(z$features)) *
#                                   length(unique(z$time)) + .5),
#                                  length(unique(z$Samples))),
#                  y = 106.5, label = paste0(rep(unique(z$time),
#                                             length(unique(z$features))),
#                                         " ", lab)) +
#         scale_y_continuous(limits = c(-0.5, 110),
#                            breaks = seq(0, 110, 20)) + 
#         scale_colour_manual(values = cbPalette,
#                           breaks = levels(factor(z$Samples,
#                                                  levels = unique(x$Samples)))) +
#         annotate("text", x = 1:length(table(z$group)),
#                  y = -Inf, vjust = -0.5, label = d, size = dimen)
#     class(a2) <- c("am_plot", class(a2))
#     return(a2)
# }
# 
# #' @export
# am_dotplot.gridTime <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
#                                              "#009E73", "#F0E442", "#0072B2",
#                                              "#D55E00", "#CC79A7"),
#                             alpha = 0.05,
#                             annot = c("none", "asterisks", "letters"),
#                             method = c("none","holm","hommel", "hochberg",
#                                        "bonferroni", "BH", "BY", "fdr"),
#                             main = "Gridline intersect method",
#                             lab = "days", ...){
#     Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
#     features <- Replicates <- Samples <- values <- NULL
#     dimen <- 0
#     alpha <- alpha
#     annot <- match.arg(annot)
#     method <- match.arg(method)
#     # Create summary table
#     final <- am_summary(x)[[1]]
#     num <- ncol(x)-3
#     if (annot == "none"){
#         d <- rep("", length(table(paste(final$Samples, final$time))) * num)
#     }
#     if (annot == "asterisks"){
#         stop("Asterisks do not work with an object of class gridTime")
#     }
#     if (annot == "letters"){
#         stat <- .grid_statime(x, method = method, group = TRUE, alpha = alpha)
#         stat <- stat %>% separate(sample, c("sample", "time"), "_")
#         stat <- stat[order(stat$time, match(stat$sample, unique(x$Samples))), ]
#         d <- as.vector(as.matrix(stat[,3:ncol(stat)]))
#         dimen <- 3
#     }
#     # Change table shape
#     z <- final %>% tidyr::gather(features, values, -Samples, -time, -Replicates)
#     z$group <- paste(z$Samples, z$time, z$features, sep = "_")
#     # Order
#     z <- z[order(match(z$features, c("Total", "Hyphopodia", "IntrHyphae",
#                                                  "Arbuscules", "Vesicles")),
#                          z$time,
#                          match(z$Samples,unique(x$Samples ))), ]
#     z$order <- rep(1:length(table(z$group)), table(z$group))
#     # Plot
#     g <- ggplot(data = z,
#                 aes(x = factor(z$group, levels=unique(z$group)),
#                           y = values, color = Samples))
#     a2 <- g +
#         geom_jitter() +
#         theme_bw() +
#         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#               plot.title = element_text(size = 19),
#               panel.grid.major.y = element_blank(),
#               panel.grid.minor.y = element_blank(),
#               panel.grid.major.x = element_blank(),
#               panel.grid.minor.x = element_blank(),
#               legend.position = "none") +
#         geom_vline(xintercept = seq(length(unique(z$Samples)) + .5,
#                                     length(unique(z$features)) *
#                                     length(unique(z$Samples)) * 
#                                     length(unique(z$time)),
#                                 length(unique(z$Samples))),
#                    colour = "lightgrey") +
#         labs(title = main,
#              x = "",
#              y = "root length colonized [%]") +
#         scale_x_discrete(labels = rep(unique(z$Samples), 
#                                     ((length(unique(z$features)) *
#                                       length(unique(z$Samples)))))) +
#         annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
#                                  (length(unique(z$Samples)) *
#                                   length(unique(z$features)) *
#                                   length(unique(z$time)) + .5),
#                                  length(unique(z$Samples))),
#                  y = 110, label = rep(unique(z$features),
#                                       each = length(unique(z$time)))) +
#         annotate("text", x = seq(length(unique(z$Samples)) * .5 + .5,
#                                  (length(unique(z$Samples)) *
#                                   length(unique(z$features)) *
#                                   length(unique(z$time)) + .5),
#                                  length(unique(z$Samples))),
#                  y = 106.5, label = paste0(rep(unique(z$time),
#                                             length(unique(z$features))),
#                                         " ", lab), size = 3) +
#         scale_y_continuous(limits = c(-0.5, 110),
#                            breaks = seq(0, 110, 20)) + 
#         scale_colour_manual(values = cbPalette,
#                           breaks = levels(factor(z$Samples,
#                                                  levels = unique(x$Samples)))) +
#         annotate("text", x = 1:length(table(z$group)),
#                  y = -Inf, vjust = -0.5, label = d, size = dimen)
#     class(a2) <- c("am_plot", class(a2))
#     return(a2)
# }

###############################################################################

#' @export
am_barplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            legend = c("right", "left", "top", "bottom"),
                            main = "Gridline intersect method", ...){
    Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
    features <- Replicates <- Samples <- values <- n <- num <- means <- se <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    legend <- match.arg(legend)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates)
    final <- z %>% group_by(Samples, features) %>%
          mutate(num = dplyr::n()) %>%
          summarize(means = mean(values, na.rm = TRUE),
                    se    = sd(values, na.rm = TRUE) / sqrt(mean(num, na.rm = TRUE))) %>%
          ungroup %>%
          mutate(features = factor(features, levels = c("Total", "Hyphopodia",
                                                        "IntrHyphae", "Arbuscules",
                                                        "Vesicles")),
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
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$Samples)) - 1))
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
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        final  <- final %>%
            mutate(annot = d) %>%
            group_by(Samples)
        dimen <- annot_size
    }
    g <- ggplot(data = final, aes(x = features,
                                              y = means, fill = Samples))
    dodge <- position_dodge(width=0.9)
    a1 <- g + geom_col(position = dodge) + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
        geom_errorbar(aes(ymin = means - se, ymax = means + se), width = .1, position = dodge) +
        theme_bw() +
        theme(plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.margin=margin(10,10,10,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              panel.grid.minor.x = element_blank(),
              legend.position = legend) +
        labs(title = main, 
             fill = "", 
             x = "",
             y = "root length colonized [%]") +
        geom_text(aes( label = annot, y = (means + se)), vjust = -0.5, position = dodge,
                  show.legend = FALSE, size = dimen) +
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
am_boxplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            legend = c("right", "left", "top", "bottom"),
                            main = "Gridline intersect method", ...){
    Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
    features <- Replicates <- Samples <- values <- n <- num <- Var1 <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    legend <- match.arg(legend)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates) %>%
          mutate(features = factor(features, levels = c("Total", "Hyphopodia",
                                                        "IntrHyphae", "Arbuscules",
                                                        "Vesicles")),
                 Samples = factor(Samples, levels = unique(x$Samples))) %>%
          arrange(features, Samples)
    # Add annotations
    if (annot == "none"){
        tmp <- expand.grid(unique(z$features), unique(z$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = "")
        an <- z %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        tmp <- expand.grid(unique(z$features), unique(z$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- z %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        tmp <- expand.grid(unique(z$features), unique(z$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- z %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    g <- ggplot(data = z,
                aes(x = features,
                    y = values,
                    colour = Samples))
    dodge <- position_dodge(width=0.75)
    a2 <- g +
        geom_boxplot() +
        theme_bw() +
        theme(plot.title = element_text(size = 19),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.margin=margin(10,10,10,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              panel.grid.minor.x = element_blank(),
              legend.position = legend) +
        geom_vline(xintercept = seq(1.5, length(unique(an$features))-0.5, 1),
                   colour = "lightgrey") +
        labs(title = main,
             color = "",
             x = "",
             y = "root length colonized [%]") +
        scale_y_continuous(limits = c(-0.5, 105),
                           breaks = seq(0, 105, 20))+ 
        scale_colour_manual(values = cbPalette, 
                          breaks = levels(factor(z$Samples,
                                                 levels = unique(x$Samples))),
                            name = "") +
        geom_text(data = an, aes(x = features, label = annot, y = values), vjust = -0.8, 
                  position = dodge, show.legend = FALSE, size = dimen) 
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}

#' @export
am_dotplot.grid <- function(x, cbPalette = c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2",
                                             "#D55E00", "#CC79A7"),
                            alpha = 0.05,
                            annot = c("none", "asterisks", "letters"),
                            annot_size = 5,
                            method = c("none","holm","hommel", "hochberg",
                                       "bonferroni", "BH", "BY", "fdr"),
                            legend = c("right", "left", "top", "bottom"),
                            main = "Gridline intersect method", ...){
    Arbuscules <- Hypopodia <- Intr_Hyphae <- Total <- Vesicles <- comp <- NULL
    features <- Replicates <- Samples <- values <- Var1 <- NULL
    alpha <- alpha
    annot <- match.arg(annot)
    method <- match.arg(method)
    legend <- match.arg(legend)
    # Create summary table
    y <- grid_summary(x)
    num <- ncol(y)-2
    # Change table shape
    z <- y %>% tidyr::gather(features, values, -Samples, -Replicates) %>%
          mutate(features = factor(features, levels = c("Total", "Hyphopodia",
                                                        "IntrHyphae", "Arbuscules",
                                                        "Vesicles")),
                 Samples = factor(Samples, levels = unique(x$Samples))) %>%
          arrange(features, Samples)
    # Add annotations
    if (annot == "none"){
        tmp <- expand.grid(unique(z$features), unique(z$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = "")
        an <- z %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "asterisks"){
        stat <- .grid_stat(x, method = method, group = FALSE, alpha = alpha)
        stat_ctr <- stat[stat$group1 == y$Samples[1], ]
        stat_l <- ifelse(as.numeric(as.matrix(stat_ctr[, 3:ncol(y)])) < alpha, "*", "") 
        ll <- split(stat_l, rep(1:num, each = length(unique(y$Samples)) - 1))
        d <- NULL
        for (i in seq_along(ll)){
            d <- append(d, c("", ll[[i]]))
        }
        tmp <- expand.grid(unique(z$features), unique(z$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- z %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    if (annot == "letters"){
        stat <- .grid_stat(x, method = method, group = TRUE, alpha = alpha)
        d <- as.vector(as.matrix(stat[,2:ncol(stat)]))
        tmp <- expand.grid(unique(z$features), unique(z$Samples)) %>%
            arrange(Var1) %>%
            mutate(annot = d)
        an <- z %>%
            left_join(tmp, by = c("features" = "Var1", "Samples" = "Var2")) %>%
            group_by(Samples, features) %>%
            dplyr::filter(values == max(values)) %>%
            arrange(features, Samples) %>%
            dplyr::top_n(1, Replicates)
        dimen <- annot_size
    }
    g <- ggplot(data = z,
                aes(x = features,
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
              legend.margin=margin(10,10,10,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              panel.grid.minor.x = element_blank(),
              legend.position = legend) +
        geom_vline(xintercept = seq(1.5, length(unique(an$features))-0.5, 1),
                   colour = "lightgrey") +
        labs(title = main,
             color = "",
             x = "",
             y = "root length colonized [%]") +
        geom_text(data = an, aes(label = annot, y = values), vjust = -0.8, 
                  position = dodge, show.legend = FALSE, size = dimen) +
        scale_y_continuous(limits = c(-0.5, 105),
                           breaks = seq(0, 105, 20))+ 
        scale_colour_manual(values = cbPalette, 
                          breaks = levels(factor(z$Samples,
                                                 levels = unique(x$Samples))),
                            name = "")
    class(a2) <- c("am_plot", class(a2))
    return(a2)
}


