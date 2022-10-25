## the following method require the ecnlose env to have the following
# fire_running
# interrupted
# stop
#
#
#
require(bnlearn)
require(dplyr)

to_log <- function(bn_df, nodes) {
  bn_df.log = bn_df
  for (node in nodes) {
    bn_df.log[, node] = log1p(bn_df.log[, node])
  }
  bn_df.log
}

inverse_log <- function(bn_df.log, nodes) {
  bn_df.invlog = bn_df.log
  for (node in nodes) {
    bn_df.invlog[, node] = exp1m(bn_df.invlog[, node])
  }
  bn_df.invlog
}

nomralize_data <-
  function(bn_df_taxas,
           bn_df_taxas.col_sum,
           bn_df_taxas.row_sum) {
    sample.names = rownames(bn_df_taxas)
    taxa.names = colnames(bn_df_taxas)
    for (sample in sample.names) {
      for (taxa in taxa.names) {
        raw_value = bn_df_taxas[sample, taxa]
        normalized.value = raw_value * bn_df_taxas.col_sum[taxa] / bn_df_taxas.row_sum[sample]
        bn_df_taxas[sample, taxa] = normalized.value
      }
    }
    invisible(bn_df_taxas)
  }

create_model <- function(
                    data_variables,
                    data_taxas,
                    expVar,
                    net_dir,
                    bl,
                    wl,
                    dismethod,
                    netscore,
                    thr_mi,
                    thr_bic,
                    filterTaxa,
                    filterThrG,
                    filterThrT,
                    filterOption,
                    filterVariable,
                    filterCountsT,
                    filterCountsG,
                    filterBA) {
    log_file <- file.path(net_dir, "log_file.txt")
    sink(log_file)
    fire_running("Reading input files")
    print("Reading input files", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }
    # data_variables = read.csv(file = input$data_variables$datapath, sep = ";", dec = ",", row.names = 1, header = T, stringsAsFactors = TRUE)
    # data_taxas = read.csv(file = input$data_taxas$datapath, sep = ";", dec = ",", row.names = 1, header = T, stringsAsFactors = TRUE)

    bn_df_variables <- data.frame(data_variables)
    bn_df_taxas <- data.frame(data_taxas)

    for (i in 1:ncol(bn_df_variables)) {
        c <- class(bn_df_variables[, i])
        if (c == "integer") {
            bn_df_variables[, i] <- as.numeric(bn_df_variables[, i])
        }
    }

    for (i in 1:ncol(bn_df_taxas)) {
        c <- class(bn_df_taxas[, i])
        if (c == "integer") {
            bn_df_taxas[, i] <- as.numeric(bn_df_taxas[, i])
        }
    }

    # FILTRADO POR PORCENTAJE BEFORE
    to_remove <- c()

    if (filterTaxa == 1) {
        if (filterBA == "Before") {
            if (filterOption == "Total") {
                min_samples <- round(nrow(bn_df_taxas) * filterThrT / 100)
                for (i in 1:ncol(bn_df_taxas)) {
                    counts <- sum(as.numeric(bn_df_taxas[, i]) >= as.numeric(filterCountsT))
                    if (counts < min_samples) {
                        to_remove <- c(to_remove, i)
                    }
                }
            } else if (filterOption == "Group") {
                columns_var <- ncol(bn_df_variables)
                df_complete <- cbind(bn_df_variables, bn_df_taxas)
                filterThrG <- str_replace_all(filterThrG, c("/" = ".", " " = "."))
                filterThrG_sep <- strsplit(filterThrG, ",")
                filterThrG_sep2 <- c()
                for (i in filterThrG_sep) {
                    sp <- strsplit(i, "-")
                    filterThrG_sep2 <- c(filterThrG_sep2, sp)
                }
                filterVariable <- str_replace_all(filterVariable, c("/" = ".", " " = ".", "-" = "."))
                df_divided <- split(df_complete, df_complete[[filterVariable]])
                col_tax <- ncol(bn_df_variables) + 1
                for (i in filterThrG_sep2) {
                    filt <- df_divided[[i[1]]]
                    min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
                    for (j in col_tax:ncol(df_complete)) {
                        counts <- sum(as.numeric(filt[, j]) >= as.numeric(filterCountsG))
                        if (counts < min_samples) {
                            to_remove <- c(to_remove, as.numeric(j) - columns_var)
                        }
                    }
                }
            }
            to_remove <- unique(to_remove)
            # bn_df_taxas <- bn_df_taxas[, -to_remove]
            output_new_taxa <- file.path(net_dir, "filtered_taxa.csv")
            write.table(bn_df_taxas, file = output_new_taxa, dec = ",", sep = ";")
        }
    }

    # expVar <- strsplit(input$exp_var, ",")[[1]]

    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }


    if (length(expVar) != 0) {
        fire_running("Discretizing experimental continuous variables")
        print("Discretizing experimental continuous variables", quote = FALSE)
        if (length(expVar) == 1) {
            if (dismethod != "hartemink") {
                var_discretize <- discretize(as.data.frame(bn_df_variables[[expVar]]), method = dismethod, breaks = 5, ordered = FALSE)
                bn_df_variables[[expVar]] <- var_discretize[, 1]
                bn_df_variables[[expVar]] <- factor(bn_df_variables[[expVar]], levels = rev(unique(var_discretize[, 1])), ordered = FALSE)
            } else {
                fire_running("Can't use hartemink, at least two variables are needed to compute mutual information. Using quantile method instead.")
                var_discretize <- discretize(as.data.frame(bn_df_variables[[expVar]]), method = "quantile", breaks = 5, ordered = FALSE)
                bn_df_variables[[expVar]] <- factor(bn_df_variables[[expVar]], levels = rev(unique(var_discretize[, 1])), ordered = FALSE)
            }
        } else {
            if (dismethod != "hartemink") {
                for (i in expVar) {
                    if (class(bn_df_variables[[i]]) == "numeric") {
                        var_discretize <- discretize(as.data.frame(bn_df_variables[[i]]), method = dismethod, breaks = 5, ordered = FALSE)
                        bn_df_variables[[i]] <- var_discretize[, 1]
                        bn_df_variables[[i]] <- factor(bn_df_variables[[i]], levels = rev(unique(var_discretize[, 1])), ordered = FALSE)
                    }
                }
            } else {
                df_dis <- select(bn_df_variables, all_of(expVar))
                var_discretize <- discretize(as.data.frame(df_dis), method = dismethod, breaks = 5, ordered = FALSE)
                bn_df_variables[names(var_discretize)] <- var_discretize
            }
        }
    }

    for (i in 1:ncol(bn_df_variables)) {
        if (class(bn_df_variables[, i]) == "numeric") {
            sp <- shapiro.test(bn_df_variables[, i])
            if (sp$p.value < 0.05) {
                bn_df_variables[, i] <- log1p(bn_df_variables[, i])
            }
        }
    }

    # bn_df_variables[bn_df_variables=="-Inf"]<- -1000

    data_raw <- cbind(bn_df_variables, bn_df_taxas)
    bn_df_raw <- as.data.frame(data_raw)

    dis_exp_variables <- bn_df_variables %>% select_if(is.factor)
    con_exp_variables <- bn_df_variables %>% select_if(is.numeric)

    fire_running("Creating model and training datasets")
    print("Creating model and training datasets", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    combinations_var <- distinct(dis_exp_variables)

    # data_model <- data.frame()
    # data_training <- data.frame()

    # l_var <- list()

    # for (i in 1:nrow(combinations_var)) {
    # for (j in 1:length(combinations_var[i,])) {
    # l_var[[names(combinations_var)[j]]] <- combinations_var[i,j]
    # }
    # comb_df <- bn_df_variables

    # for (j in 1:length(l_var)) {
    # comb_df <- comb_df[comb_df[[names(l_var[j])]] == l_var[[j]][1], ]
    # }
    # df1 <- comb_df[1:round(nrow(comb_df)/2), ]
    # df2 <- comb_df[round(nrow(comb_df)/2)+1:nrow(comb_df), ]
    # data_training <- rbind(data_training, df1)
    # data_model <- rbind(data_model, df2)
    # }

    # data_model <- data_model[complete.cases(data_model), ]
    # data_training <- data_training[complete.cases(data_training), ]

    fire_running("Normalizing taxa raw counts")
    print("Normalizing taxa raw counts", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    # bn_df_taxas_norm <- bn_df_taxas

    bn_df_taxas.col_sum <- colSums(bn_df_taxas)
    bn_df_taxas.row_sum <- rowSums(bn_df_taxas)

    bn_df_taxas_norm <- nomralize_data(bn_df_taxas, bn_df_taxas.col_sum, bn_df_taxas.row_sum)

    # for (r in 1:nrow(bn_df_taxas)) {
    #   sample_sum = sum(bn_df_taxas[r,])
    #   for (c in 1:ncol(bn_df_taxas)) {
    #     col_sum = sum(bn_df_taxas[, c])
    #     norm_value = bn_df_taxas[r,c]*col_sum/sample_sum
    #     if (!is.na(norm_value)){
    #       bn_df_taxas_norm[r,c] <- norm_value
    #     } else {
    #       bn_df_taxas_norm[r,c] <- 0
    #     }

    #   }
    # }

    output_norm <- file.path(net_dir, "taxa_norm_counts.csv")
    write.table(bn_df_taxas_norm, file = output_norm, dec = ",", sep = ";")

    fire_running("Applying log scale to normalized taxa data")
    print("Applying log scale to normalized taxa data", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    bn_df_taxa_norm_log <- to_log(bn_df_taxas_norm, colnames(bn_df_taxas_norm))

    output_log <- file.path(net_dir, "taxa_norm_log_counts.csv")
    write.table(bn_df_taxa_norm_log, file = output_log, dec = ",", sep = ";")


    if (filterTaxa == 1) {
        if (filterBA == "Before") {
            bn_df_taxas <- bn_df_taxas[, -to_remove]
            bn_df_taxas_norm <- bn_df_taxas_norm[, -to_remove]
            bn_df_taxa_norm_log <- bn_df_taxa_norm_log[, -to_remove]
        }
    }

    # bn_df_model <- cbind(bn_df_variables[rownames(data_model),],bn_df_taxas_norm[rownames(data_model),])
    # data_model <- bn_df_model
    # bn_df_training <- cbind(bn_df_variables[rownames(data_training),],bn_df_taxas_norm[rownames(data_training),])
    # data_training <- bn_df_training

    bn_df_norm <- cbind(bn_df_variables, bn_df_taxa_norm_log)

    fire_running("Creating network model")
    print("Creating network model", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    ## this need checking
    netscore.g <- tolower(paste(netscore, "-CG", sep = ""))
    # cont_variables <- try(unlist(lapply(bn_df_variables, is.numeric)))

    # if (class(cont_variables) == "try-error") {
    #     netscore.g <- tolower(paste(netscore, "-CG", sep = ""))
    # } else {
    #     if (length(cont_variables) == ncol(bn_df_variables)) {
    #         netscore.g <- tolower(paste(netscore, "-G", sep = ""))
    #     } else {
    #         netscore.g <- tolower(paste(netscore, "-CG", sep = ""))
    #     }
    # }

    print(netscore.g)



    # if (blacklist == 1 && whitelist == 1) {
    #     result <- hc(bn_df_norm, optimized = TRUE, whitelist = wl, blacklist = bl, score = netscore.g)
    # }

    # if (blacklist == 1 && whitelist != 1) {
    #     result <- hc(bn_df_norm, optimized = TRUE, blacklist = bl, score = netscore.g)
    # }

    # if (blacklist != 1 && whitelist == 1) {
    #     result <- hc(bn_df_norm, optimized = TRUE, whitelist = wl, score = netscore.g)
    # }

    # if (blacklist != 1 && whitelist != 1) {
    #     result <- hc(bn_df_norm, optimized = TRUE, score = netscore.g)
    # }

    result <- hc(bn_df_norm, optimized = TRUE, score = netscore.g , whitelist = wl , blacklist = bl)


    fire_running("Network model done! Filtering model by link strength")
    print("Network model done! Filtering model by link strength", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    remove_arcs <- data.frame()
    result_filt <- result

    arc_st_bic <- arc.strength(result, bn_df_norm, criterion = "bic-cg")
    arc_st_mi <- arc.strength(result, bn_df_norm, criterion = "mi-cg")

    n <- 0

    for (l in 1:nrow(arc_st_bic)) {
        if ((arc_st_bic[l, 3] < thr_bic) && (arc_st_mi[l, 3] < thr_mi)) {
            n <- n + 1
        } else {
            row <- c(arc_st_bic[l, 1], arc_st_bic[l, 2])
            remove_arcs <- rbind(remove_arcs, row)
        }
    }

    out_remove <- file.path(net_dir, "removed_arcs.txt")
    write.table(remove_arcs, out_remove, sep = "\t", dec = ",")

    if (length(remove_arcs) != 0) {
        for (i in 1:nrow(remove_arcs)) {
            d <- data.frame(from = remove_arcs[i, 1], to = remove_arcs[i, 2])
            comparison <- compare::compare(d, wl, allowAll = TRUE)
            if (isFALSE(comparison)) {
                result_filt <- drop.arc(result_filt, remove_arcs[i, 1], remove_arcs[i, 2])
            }
        }
    }

    fire_running("Training model")
    print("Training model", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    fittedbn <- bn.fit(result_filt, data = bn_df_norm, replace.unidentifiable = TRUE)

    fire_running("Writing output files")
    print("Writing output files", quote = FALSE)
    if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
    }

    strength_mi <- file.path(net_dir, "arc_strength_mi.txt")
    write.table(arc_st_mi, strength_mi, sep = "\t", dec = ",")

    strength_bic <- file.path(net_dir, "arc_strength_bic.txt")
    write.table(arc_st_bic, strength_bic, sep = "\t", dec = ",")

    out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_complete_network.RData", sep = "")
    output_file_net <- file.path(net_dir, out_net_name)
    save(list = ls(), file = output_file_net, envir = environment())

    # FILTRADO POR PORCENTAJE AFTER

    if (filterTaxa == 1) {
        if (filterBA == "After") {
            if (filterOption == "Total") {
                filterThrT_sep <- strsplit(filterThrT, ",")
                for (thr in filterThrT_sep[[1]]) {
                    print(thr)
                    to_remove <- c()
                    min_samples <- round(nrow(bn_df_taxas) * as.numeric(thr) / 100)
                    for (i in 1:ncol(bn_df_taxas)) {
                        counts <- sum(as.numeric(bn_df_taxas[, i]) >= as.numeric(filterCountsT))
                        if (counts < min_samples) {
                            to_remove <- c(to_remove, i)
                        }
                    }
                    to_remove <- unique(to_remove)
                    output_to_remove <- file.path(net_dir, paste("taxa_to_be_removed_by_filter_", thr, ".csv", sep = ""))
                    write.table(bn_df_taxas, file = output_to_remove, dec = ",", sep = ";")

                    result_removed <- result_filt
                    for (n in to_remove) {
                        result_removed <- remove.node(result_removed, colnames(bn_df_taxas)[[n]])
                    }
                    result1 <- result
                    bn_df_norm1 <- bn_df_norm
                    bn_df_norm_removed <- subset(bn_df_norm, select = nodes(result_removed))
                    output_kept <- file.path(net_dir, paste("kept_taxa_by_", thr, "_filter.csv", sep = ""))
                    write.table(bn_df_norm_removed, file = output_kept, dec = ",", sep = ";")
                    fittedbn <- bn.fit(result_removed, data = bn_df_norm_removed, replace.unidentifiable = TRUE)
                    out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_", thr, "_network.RData", sep = "")
                    output_file_net <- file.path(net_dir, out_net_name)
                    save(list = ls(), file = output_file_net, envir = environment())
                    result <- result1
                    bn_df_norm <- bn_df_norm1
                }
            } else if (filterOption == "Group") {
                columns_var <- ncol(bn_df_variables)
                df_complete <- cbind(bn_df_variables, bn_df_taxas)
                filterThrG <- str_replace_all(filterThrG, c("/" = ".", " " = "."))
                filterThrG_sep <- strsplit(filterThrG, ",")
                filterThrG_sep2 <- c()
                for (i in filterThrG_sep) {
                    sp <- strsplit(i, "-")
                    filterThrG_sep2 <- c(filterThrG_sep2, sp)
                }
                filterVariable <- str_replace_all(filterVariable, c("/" = ".", " " = ".", "-" = "."))
                df_divided <- split(df_complete, df_complete[[filterVariable]])
                col_tax <- ncol(bn_df_variables) + 1
                for (i in filterThrG_sep2) {
                    filt <- df_divided[[i[1]]]
                    min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
                    for (j in col_tax:ncol(df_complete)) {
                        counts <- sum(as.numeric(filt[, j]) >= as.numeric(filterCountsG))
                        if (counts < min_samples) {
                            to_remove <- c(to_remove, as.numeric(j) - columns_var)
                        }
                    }
                }
                result1 <- result
                bn_df_norm1 <- bn_df_norm
                to_remove <- unique(to_remove)
                # bn_df_taxas <- bn_df_taxas[, -to_remove]
                output_to_remove <- file.path(net_dir, "taxa_to_be_removed_by_group_filter.csv")
                write.table(bn_df_taxas, file = output_to_remove, dec = ",", sep = ";")
                result_removed <- result
                for (n in to_remove) {
                    result_removed <- remove.node(result_removed, colnames(bn_df_taxas)[[n]])
                }
                bn_df_norm_removed <- subset(bn_df_norm, select = nodes(result_removed))
                output_kept <- file.path(net_dir, "kept_taxa_by_group_filter.csv")
                write.table(bn_df_norm_removed, file = output_kept, dec = ",", sep = ";")
                fittedbn <- bn.fit(result_removed, data = bn_df_norm_removed, replace.unidentifiable = TRUE)
                result <- result_removed
                bn_df_norm <- bn_df_norm_removed
                out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_filtGroup_network.RData", sep = "")
                output_file_net <- file.path(net_dir, out_net_name)
                save(list = ls(), file = output_file_net, envir = environment())
                result <- result1
                bn_df_norm <- bn_df_norm1
            }
        }
    }

    fire_running("DONE!")
    print("DONE!", quote = FALSE)
    sink()
}