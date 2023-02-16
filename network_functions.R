## the following method require the ecnlose env to have the following
# fire_running
# interrupted
# stop





require(bnlearn)
require(dplyr)

to_log <- function(bn_df, nodes) {
  bn_df.log <- bn_df
  for (node in nodes) {
    bn_df.log[, node] <- log1p(bn_df.log[, node])
  }
  bn_df.log
}

inverse_log <- function(bn_df.log, nodes) {
  bn_df.invlog <- bn_df.log
  for (node in nodes) {
    bn_df.invlog[, node] <- exp1m(bn_df.invlog[, node])
  }
  bn_df.invlog
}


nomralize_data <-
  function(bn_df_taxas,
           bn_df_taxas.col_sum,
           bn_df_taxas.row_sum) {
    sample.names <- rownames(bn_df_taxas)
    taxa.names <- colnames(bn_df_taxas)
    for (sample in sample.names) {
      for (taxa in taxa.names) {
        raw_value <- bn_df_taxas[sample, taxa]
        normalized.value <- raw_value * bn_df_taxas.col_sum[taxa] / bn_df_taxas.row_sum[sample]
        bn_df_taxas[sample, taxa] <- normalized.value
      }
    }
    invisible(bn_df_taxas)
  }
#########################################################################################
bn_to_dagitty <- function(bn_fit_obj) {
  net_nodes <- bnlearn::nodes(bn_fit_obj)
  new_dag <- "dag{\n"
  for (node in net_nodes) {
    node_obj <- bn_fit_obj[[node]]
    new_dag <- paste(new_dag, node, ";\n")
    for (parent in node_obj$parents) {
      new_dag <- paste(new_dag, parent, "->", node, ";\n")
    }
  }
  new_dag <- paste(new_dag, "}")
  dagitty::dagitty(new_dag)
}

#####################################################################

######################   Network methods ############################
VAR_TYPE_NOMINAL <- "Nominal"
VAR_TYPE_NUMERIC <- "Numeric"

VAR_ROLE_EXP_CONR <- "Exp Control"
VAR_ROLE_EXP_OBSERVATION <- "Observed"
VAR_ROLE_EXP_OUTCOME <- "Outcome"

VAR_ROLES <- c(VAR_ROLE_EXP_CONR, VAR_ROLE_EXP_OBSERVATION, VAR_ROLE_EXP_OUTCOME)

generate_variables_summary <- function(bn_df_variables) {
  ## get all variable name and data type
  variables_name <- colnames(bn_df_variables)

  # nominal_variables <- bn_df_variables %>% select_if(is.factor)
  # numeric_variables <- bn_df_variables %>% select_if(is.numeric)
  variables_cls <- sapply(bn_df_variables, class)
  vars_desc <- list()
  for (var in variables_name) {
    if (variables_cls[var] == "factor") {
      vars_desc[[var]]$type <- VAR_TYPE_NOMINAL
      vars_desc[[var]]$var_values <- dplyr::n_distinct(bn_df_variables[[var]])
      vars_desc[[var]]$max_value <- 0
      vars_desc[[var]]$min_value <- 0
    }
    if (variables_cls[var] == "numeric") {
      vars_desc[[var]]$type <- VAR_TYPE_NUMERIC
      vars_desc[[var]]$var_values <- sd(bn_df_variables[[var]])
      vars_desc[[var]]$max_value <- max(bn_df_variables[[var]])
      vars_desc[[var]]$min_value <- min(bn_df_variables[[var]])
    }
    vars_desc[[var]]$role <- VAR_ROLE_EXP_CONR
    vars_desc[[var]]$scale_transformation <- ""
    vars_desc[[var]]$to_include <- TRUE
    vars_desc[[var]]$warning <- ""
  }
  res_df <- data.frame(matrix(unlist(vars_desc), nrow = length(vars_desc), byrow = TRUE))

  names(res_df) <- names(vars_desc[[1]])
  rownames(res_df) <- names(vars_desc)
  res_df
}




create_evidence_cpquery <- function(combination_vars) {
  #
  # create evidence from cpquery
  #
  for (l in 1:length(combination_vars)) {
    if (l == 1) {
      evidence_cpquery <- paste("(", names(combination_vars[l]), " == ", '"', combination_vars[1, l], '")', sep = "")
    } else {
      evidence_cpquery <- paste(evidence_cpquery, " & ", "(", names(combination_vars[l]), " == ", '"', combination_vars[1, l], '")', sep = "")
    }
  }
  evidence_cpquery
}
create_evidence_cpdist <- function(combination_vars) {
  #
  # create evidence from cpdist and sampling function
  #
  l_var <- list()
  for (j in 1:length(combination_vars[1, ])) {
    l_var[[names(combination_vars)[j]]] <- combination_vars[1, j]
  }
  l_var
}

filter_samples_by_weights <- function(in_samples, min_weight) {
  ## TODO :: input must have weights attr otherwise fail
  in_samples_w <- attr(in_samples, "weights")
  in_samples <- in_samples[in_samples_w >= min_weight]
  in_samples_w <- in_samples_w[in_samples_w >= min_weight]
  attr(in_samples, "weights") <- in_samples_w
  in_samples
}

n_matchs_to_weight <- function(n, c = 1) {
  w <- c / n
  1 / (1 + exp(-((w - 0.5) * 11)))
}

data.sampling <- function(in.df,
                          node, evidence,
                          n.samples = 1000,
                          min_weight = 0.5) {
  ## TODO :: add min mismatch as it is strait to assign weight to n mismatch outside
  require(dplyr)
  samples.cols <- c(node, names(evidence))
  in.df <- in.df[, samples.cols]
  x.samples <- in.df %>% slice_sample(n = n.samples * 2, replace = TRUE)
  x.samples$samples_weights__ <- 0
  for (v in names(evidence)) {
    matched.samples <- x.samples[v] == as.character(evidence[[v]])
    x.samples$samples_weights__[matched.samples] <- x.samples$samples_weights__[matched.samples] + 1
  }
  x.samples$samples_weights__ <- x.samples$samples_weights__ / length(evidence)
  # x.samples$samples_weights__ = (x.samples$samples_weights__ - 0.5)*5
  x.samples$samples_weights__ <- round(1 / (1 + exp(-((x.samples$samples_weights__ - 0.5) * 11))), 2)
  samples <- x.samples[[node]]
  samples <- samples[x.samples$samples_weights__ >= min_weight]
  samples.w <- x.samples$samples_weights__[x.samples$samples_weights__ >= min_weight]
  if (length(samples) > n.samples) {
    samples <- samples[1:n.samples]
    samples.w <- samples.w[1:n.samples]
  }
  attr(samples, "weights") <- samples.w
  samples
}

bn.network.sampling <- function(fittedbn, nodes, evidence, n.samples = 10000, min_weight = 0.5) {
  all.predicted.values <- c()
  all.predicted.values.w <- c()
  predicted.values <- try(cpdist(fittedbn, nodes = nodes, evidence = evidence, method = "lw", n = n.samples))
  if (class(predicted.values)[1] == "try-error") {
    all.predicted.values <- NULL
  } else {
    while (TRUE) {
      ## remove NA first
      # print("Round")
      predicted.values.no_na <- complete.cases(predicted.values) # !is.na(predicted.values)
      # print(length(predicted.values.no_na))

      w <- attr(predicted.values, "weights")
      w <- w[predicted.values.no_na]
      predicted.values <- predicted.values[predicted.values.no_na, ]
      # print(nrow(predicted.values))
      predicted.values <- predicted.values[w >= min_weight, ]
      # print(nrow(predicted.values))
      w <- w[w >= min_weight]
      non_zeros <- apply(predicted.values >= 0, 1, function(x) sum(x) / length(x) > 0.1)
      ## TODO :: with a random p of 0.7 reject some of  zero
      w <- w[non_zeros]
      predicted.values <- predicted.values[non_zeros, ]
      # print(nrow(predicted.values))
      predicted.values[predicted.values < 0] <- 0
      all.predicted.values <- rbind(all.predicted.values, predicted.values)
      all.predicted.values.w <- c(all.predicted.values.w, w)
      if (nrow(all.predicted.values) >= n.samples) {
        break
      }
      predicted.values <- try(cpdist(fittedbn, nodes = nodes, evidence = evidence, method = "lw", n = n.samples))
    }
    all.predicted.values <- all.predicted.values[1:n.samples, ]
    attr(all.predicted.values, "weights") <- all.predicted.values.w[1:n.samples]
  }
  all.predicted.values
}

network.sampling <- function(fittedbn, node, evidence, n.samples = 10000, min_weight = 0.5) {
  all.predicted.values <- c()
  all.predicted.values.w <- c()
  predicted.values <- try(cpdist(fittedbn, nodes = node, evidence = evidence, method = "lw", n = n.samples))
  if (class(predicted.values)[1] == "try-error") {
    all.predicted.values <- NULL
  } else {
    while (TRUE) {
      ## remove NA first
      predicted.values.no_na <- !is.na(predicted.values)
      w <- attr(predicted.values, "weights")
      w <- w[predicted.values.no_na]
      predicted.values <- predicted.values[predicted.values.no_na]
      predicted.values <- predicted.values[w >= min_weight]
      w <- w[w >= min_weight]

      ## TODO :: with a random p of 0.7 reject some of  zero
      w <- w[predicted.values >= 0]
      predicted.values <- predicted.values[predicted.values >= 0]

      all.predicted.values <- c(all.predicted.values, predicted.values)
      all.predicted.values.w <- c(all.predicted.values.w, w)
      if (length(all.predicted.values) >= n.samples) {
        break
      }
      predicted.values <- try(cpdist(fittedbn, nodes = node, evidence = evidence, method = "lw", n = n.samples))
    }
    all.predicted.values <- all.predicted.values[1:n.samples]
    attr(all.predicted.values, "weights") <- all.predicted.values.w[1:n.samples]
  }
  all.predicted.values
}

get.mixed.samples <- function(fittedbn,
                              org.data, node,
                              evidence,
                              n.samples = 10000,
                              samples.w = 0.5,
                              org_data_min_weight = 1,
                              samples_min_weight = 0.5,
                              HPDI_correct = TRUE,
                              all.network.samples.values = NULL) {
  ## TODO :: check on samples.w
  require(Hmisc)
  require(rethinking)
  if (is.null(all.network.samples.values)) {
    network.samples.values <- network.sampling(fittedbn, node, evidence, n.samples = n.samples, min_weight = samples_min_weight)
  } else {
    network.samples.values <- all.network.samples.values[[node]]
    attr(network.samples.values, "weights") <- attr(all.network.samples.values, "weights")
  }
  if (!is.null(network.samples.values)) {
    network.samples.weights <- attr(network.samples.values, "weights")

    if (HPDI_correct) {
      ## correct for %97
      HPDI_correct_value <- 0.99
      if (is.numeric(HPDI_correct)) {
        HPDI_correct_value <- HPDI_correct
      }
      network.samples.95HPDI <- HPDI(network.samples.values, prob = HPDI_correct_value)
      # network.samples.weights <-network.samples.weights[network.samples.values > network.samples.95HPDI[1] &
      #                                                  network.samples.values  < network.samples.95HPDI[2]]
      # network.samples.values <- network.samples.values[network.samples.values > network.samples.95HPDI[1] &
      #                                                      network.samples.values < network.samples.95HPDI[2]]
      network.samples.weights <- network.samples.weights[network.samples.values < network.samples.95HPDI[2]]
      network.samples.values <- network.samples.values[network.samples.values < network.samples.95HPDI[2]]
    }

    network.samples.raw.values <- network.samples.values
    network.samples.values <- round(expm1(network.samples.values))
    attr(network.samples.raw.values, "weights") <- network.samples.weights
    attr(network.samples.values, "weights") <- network.samples.weights

    network.samples.quantile <- wtd.quantile(network.samples.values, weights = network.samples.weights)
  }

  # wtd.mean(expm1(network.samples.values),weights = network.samples.weights)
  org.samples.raw.values <- data.sampling(org.data, node, evidence, n.samples = n.samples, min_weight = org_data_min_weight)
  org.samples.weights <- attr(org.samples.raw.values, "weights")
  org.samples.values <- round(expm1(org.samples.raw.values))
  attr(org.samples.values, "weights") <- org.samples.weights
  org.data.quantile <- wtd.quantile(org.samples.values, weights = org.samples.weights)

  if (!is.null(network.samples.values)) {
    list(
      network.samples.raw.values = network.samples.raw.values,
      network.samples.values = network.samples.values,
      network.samples.weights = network.samples.weights,
      org.samples.raw.values = org.samples.raw.values,
      org.samples.values = org.samples.values,
      org.samples.weights = org.samples.weights,
      network.samples.average = wtd.mean(network.samples.values, network.samples.weights),
      network.samples.sd = sqrt(wtd.var(network.samples.values, network.samples.weights)),
      org.samples.average = wtd.mean(org.samples.values, org.samples.weights),
      org.samples.sd = sqrt(wtd.var(org.samples.values, org.samples.weights)),
      network.samples.quantile = network.samples.quantile,
      org.data.quantile = org.data.quantile,
      weighted.quantiles = round((org.data.quantile * (1 - samples.w) + network.samples.quantile * samples.w), 0)
    )
  } else {
    list(
      network.samples.values = NULL,
      org.samples.raw.values = org.samples.raw.values,
      org.samples.values = org.samples.values,
      org.samples.weights = org.samples.weights,
      org.samples.average = wtd.mean(org.samples.values, org.samples.weights),
      org.samples.sd = sqrt(wtd.var(org.samples.values, org.samples.weights)),
      org.data.quantile = org.data.quantile
    )
  }
}








calc.prob <- function(samples, start.value, end.value) {
  round(sum(samples >= start.value & samples <= end.value) / length(samples), 2)
}

get_posterior_dist <- function(network_samples, proir_data = NULL, adjust_proir = 0.7,
                               adjust_samples = 0.4, uniform_proir = NULL) {
  ## TODO :: adjust_proir : large value indicate weak proir and should be realiable
  ## smaller values : strong proir and will strongly influence the final prob
  ## adjust_samples goold values are between 0.7-0.3 : it just smooth the prediction
  if (!is.null(proir_data)) {
    hard_max <- min(max(network_samples), 31) ## is not possible to go ever 31
    max_range <- max(hard_max, max(proir_data)) + 1
    proir_data_w <- attr(proir_data, "weights")
    if (!is.null(proir_data_w)) {
      if (sum(proir_data_w) != 1) proir_data_w <- proir_data_w / sum(proir_data_w)
    }


    network_samples_w <- attr(network_samples, "weights")
    if (!is.null(network_samples_w)) {
      if (sum(network_samples_w) != 1) network_samples_w <- network_samples_w / sum(network_samples_w)
    }

    #
    dd_proir <- density(proir_data, adjust = adjust_proir, from = 0, t = max_range, n = 1000, weights = proir_data_w)
    dd_samples <- density(network_samples, adjust = adjust_samples, from = 0, t = max_range, n = 1000, weights = network_samples_w)

    proir_data_max <- max(proir_data)
    proir_data_min <- min(proir_data)
    ## TODO :: add uniform proir to the org data
    if (!is.null(uniform_proir)) dd_proir$y <- dd_proir$y + uniform_proir

    dd_proir_p <- round(dd_proir$y, 6)
    dd_lik_p <- round(dd_samples$y, 6)
    # if(ceiling(max(proir_data)) > 0)
    #  dd_proir_p[dd_proir$x > ceiling(max(proir_data)) & dd_proir_p > 0] <- 0.001

    # posterior <- round(dd_proir_p * dd_samples$y, 5)
    posterior <- dd_proir_p * dd_samples$y

    # posterior <- posterior / sum(posterior)

    list(
      data_value = dd_samples$x,
      posterior_w = posterior,
      posterior_p = posterior / sum(posterior)
    )
  } else {
    max_range <- max(network_samples)
    dd_samples <- density(network_samples, adjust = adjust_samples, from = 0, t = max_range, n = 1000)
    list(
      data_value = dd_samples$x,
      posterior_w = dd_samples$y,
      posterior_p = dd_samples$y / sum(dd_samples$y)
    )
  }
}

posterior_stats <- function(posterior_dist, link = expm1) {
  require(Hmisc)
  transfromed_data <- link(posterior_dist$data_value)
  post_samples <- sample(x = transfromed_data, size = 1000000, prob = posterior_dist$posterior_w, replace = TRUE)
  posterior_dist$post_samples <- post_samples
  # posterior_dist$posterior_mean = wtd.mean(link(posterior_dist$data_value),posterior_dist$posterior_w)
  # posterior_dist$posterior_mean = round(mean(post_samples))
  # posterior_dist$posterior_sd <- round(sd(post_samples))
  # posterior_dist$posterior_quantile <- round(quantile(post_samples))
  posterior_w <- round(posterior_dist$posterior_w, 6)
  # posterior_dist$posterior_mean = wtd.mean(link(posterior_dist$data_value),posterior_dist$posterior_w)

  posterior_dist$posterior_mean <- round(mean(post_samples))
  posterior_dist$posterior_sd <- round(sd(post_samples), 2)
  posterior_dist$posterior_quantile <- round(quantile(post_samples))



  # posterior_dist$posterior_mean <- round(wtd.mean(transfromed_data, posterior_dist$posterior_w))
  # posterior_dist$posterior_sd <- round(sqrt(wtd.var(transfromed_data, posterior_dist$posterior_w)), 2)
  # posterior_dist$posterior_quantile <- round(wtd.quantile(transfromed_data, posterior_w))


  posterior_dist
}


markovBlanket_dag <- function(x, v, cond = NULL, c.done = c()) {
  s1 <- dagitty::parents(x, v)
  s2 <- dagitty::children(x, v)
  ms <- union(s1, s2)
  if (length(s2) > 0) {
    for (c in s2) {
      ms <- union(ms, dagitty::parents(x, c))
    }
  }
  direct_ms <- ms
  if (!is.null(cond)) {
    for (c in ms) {
      if (c %in% cond && !c %in% c.done) {
        # ms <- setdiff(ms,c)
        ms <- union(ms, markovBlanket(x, c, cond, c(c.done, c)))
      }
    }
  }
  ms <- setdiff(ms, v)
  attr(ms, "direct") <- setdiff(direct_ms, v)
  attr(ms, "in_direct") <- setdiff(ms, direct_ms)

  ms
}

markovBlanket <- function(fittedbn, v, cond = NULL, c.done = c(v)) {
  #
  mb_set <- mb(fittedbn, v)
  direct_mb_set <- mb_set
  if (!is.null(cond)) {
    for (c in mb_set) {
      if (c %in% cond && !c %in% c.done) {
        # ms <- setdiff(ms,c)
        sub_set <- markovBlanket(fittedbn, c, NULL, c(c.done, c))
        mb_set <- union(mb_set, sub_set)
      }
    }
  }
  mb_set <- setdiff(mb_set, v)
  direct_mb_set <- setdiff(direct_mb_set, v)
  attr(mb_set, "direct") <- direct_mb_set
  attr(mb_set, "in_direct") <- setdiff(mb_set, direct_mb_set)

  mb_set
}

get_testable_implications <- function(dag_obj, outcome_variables) {
  testable_implications <- list()
  for (target_taxa in outcome_variables) {
    local_testable_implications <- get_testable_implications_per_taxa(dag_obj, target_taxa, outcome_variables)
    testable_implications[[target_taxa]] <- local_testable_implications[[target_taxa]]
  }
  testable_implications
}


get_testable_implications_per_taxa <- function(dag_obj, target_taxa, outcome_variables) {
  dag_obj <- dagitty::as.dagitty(dag_obj)
  dagitty::latents(dag_obj) <- setdiff(outcome_variables, target_taxa)
  imls <- dagitty::impliedConditionalIndependencies(dag_obj)
  testable_implications <- list()
  for (iml in imls) {
    ## only process Taxa
    taxa_node <- NULL
    if (iml$X == target_taxa) taxa_node <- iml$X
    if (iml$Y == target_taxa) taxa_node <- iml$Y
    if (!is.null(taxa_node)) {
      if (is.null(testable_implications[[taxa_node]])) testable_implications[[taxa_node]] <- list()
      testable_implications[[taxa_node]] <- append(testable_implications[[taxa_node]], list(iml))
    }
  }
  for (target_node in names(testable_implications)) {
    testable_implication <- testable_implications[[target_node]]
    m <- r2r::hashmap()
    mKeys <- list()
    for (lsd in testable_implication) {
      if (is.null(m[[lsd$Z]])) {
        m[[lsd$Z]] <- c()
        mKeys <- append(mKeys, list(lsd$Z))
      }

      t_node <- lsd$Y
      if (target_node == t_node) {
        t_node <- lsd$X
      }
      m[[lsd$Z]] <- union(m[[lsd$Z]], t_node)
    }
    final_result <- list()
    for (lkey in mKeys) {
      final_result <- append(final_result, list(list(Y = m[[lkey]], Z = lkey)))
    }
    testable_implications[[target_node]] <- final_result
  }
  testable_implications
}

## this version return testable_implications with out setting latent variable
get_testable_implications_v1 <- function(dag_obj, outcome_variables) {
  imls <- dagitty::impliedConditionalIndependencies(dag_obj)
  testable_implications <- list()
  for (iml in imls) {
    ## only process Taxa
    taxa_node <- NULL
    if (iml$X %in% outcome_variables) taxa_node <- iml$X
    if (iml$Y %in% outcome_variables) taxa_node <- iml$Y
    if (!is.null(taxa_node)) {
      if (is.null(testable_implications[[taxa_node]])) testable_implications[[taxa_node]] <- list()
      testable_implications[[taxa_node]] <- append(testable_implications[[taxa_node]], list(iml))
    }
  }
  for (target_node in names(testable_implications)) {
    testable_implication <- testable_implications[[target_node]]
    m <- r2r::hashmap()
    mKeys <- list()
    for (lsd in testable_implication) {
      if (is.null(m[[lsd$Z]])) {
        m[[lsd$Z]] <- c()
        mKeys <- append(mKeys, list(lsd$Z))
      }

      t_node <- lsd$Y
      if (target_node == t_node) {
        t_node <- lsd$X
      }
      m[[lsd$Z]] <- union(m[[lsd$Z]], t_node)
    }
    final_result <- list()
    for (lkey in mKeys) {
      final_result <- append(final_result, list(list(Y = m[[lkey]], Z = lkey)))
    }
    testable_implications[[target_node]] <- final_result
  }
  testable_implications
}

get_testable_implications_v2 <- function(dag_obj, outcome_variables) {
  imls <- dagitty::impliedConditionalIndependencies(dag_obj, "basis.set")
  testable_implications <- list()
  for (taxa_node in outcome_variables) {
    for (iml in imls) {
      ## only process Taxa
      if (taxa_node == iml$X | taxa_node %in% iml$Y) {
        if (is.null(testable_implications[[taxa_node]])) testable_implications[[taxa_node]] <- list()
        testable_implications[[taxa_node]] <- append(testable_implications[[taxa_node]], list(iml))
      }
    }
  }


  for (target_node in names(testable_implications)) {
    testable_implication <- testable_implications[[target_node]]
    m <- r2r::hashmap()
    mKeys <- list()
    for (lsd in testable_implication) {
      if (is.null(m[[lsd$Z]])) {
        m[[lsd$Z]] <- c()
        mKeys <- append(mKeys, list(lsd$Z))
      }

      t_node <- lsd$X
      if (target_node == t_node) {
        t_node <- lsd$Y
      }
      m[[lsd$Z]] <- union(m[[lsd$Z]], t_node)
    }
    final_result <- list()
    for (lkey in mKeys) {
      final_result <- append(final_result, list(list(Y = m[[lkey]], Z = lkey)))
    }
    testable_implications[[target_node]] <- final_result
  }
  testable_implications
}


#########################################################################################


taxa_count_filter_by_1 <- function(bn_df_taxas, min_count = 1, min_count_threshold = 5) {
  to_remove <- c()
  min_samples <- round(nrow(bn_df_taxas) * min_count_threshold / 100)
  for (i in seq_len(ncol(bn_df_taxas))) {
    counts <- sum(as.numeric(bn_df_taxas[, i]) >= min_count)
    if (counts < min_samples) {
      to_remove <- c(to_remove, i)
    }
  }
  to_remove <- unique(to_remove)
}

apply_taxa_before_filter <-
  function(bn_df_taxas, bn_df_variables, taxa_count_filters) {
    to_remove <- c()
    if (taxa_count_filters$filter_option == "Total") {
      filter_counts_t <- as.numeric(taxa_count_filters$filterCountsT)
      # min_samples <- round(nrow(bn_df_taxas) * taxa_count_filters$filterThrT / 100)
      # for (i in 1:ncol(bn_df_taxas)) {
      #     counts <- sum(as.numeric(bn_df_taxas[, i]) >= filter_counts_t)
      #     if (counts < min_samples) {
      #         to_remove <- c(to_remove, i)
      #     }
      # }
      to_remove <- taxa_count_filter_by_1(
        bn_df_taxas,
        filter_counts_t,
        taxa_count_filters$filterThrT
      )
    } else if (taxa_count_filters$filter_option == "Group") {
      columns_var <- ncol(bn_df_variables)
      df_complete <- cbind(bn_df_variables, bn_df_taxas)
      taxa_count_filters$filterThrG <- stringr::str_replace_all(
        taxa_count_filters$filterThrG,
        c("/" = ".", " " = ".")
      )
      filterThrG_sep <- strsplit(taxa_count_filters$filterThrG, ",")
      filterThrG_sep2 <- c()
      for (i in filterThrG_sep) {
        sp <- strsplit(i, "-")
        filterThrG_sep2 <- c(filterThrG_sep2, sp)
      }
      filterVariable <- stringr::str_replace_all(
        taxa_count_filters$filterVariable,
        c("/" = ".", " " = ".", "-" = ".")
      )
      df_divided <- split(df_complete, df_complete[[filterVariable]])
      col_tax <- ncol(bn_df_variables) + 1
      for (i in filterThrG_sep2) {
        filt <- df_divided[[i[1]]]
        min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
        for (j in col_tax:ncol(df_complete)) {
          counts <- sum(as.numeric(filt[, j]) >=
            as.numeric(taxa_count_filters$filterCountsG))
          if (counts < min_samples) {
            to_remove <- c(to_remove, as.numeric(j) - columns_var)
          }
        }
      }
      to_remove <- unique(to_remove)
    }
    to_remove
  }


discretize_data_variables <-
  function(bn_df_variables, exp_var, dismethod) {
    if (length(exp_var) == 1) {
      if (dismethod != "hartemink") {
        var_discretize <- discretize(
          as.data.frame(bn_df_variables[[exp_var]]),
          method = dismethod,
          breaks = 5,
          ordered = FALSE
        )
        bn_df_variables[[exp_var]] <- var_discretize[, 1]
        bn_df_variables[[exp_var]] <- factor(
          bn_df_variables[[exp_var]],
          levels = rev(unique(var_discretize[, 1])),
          ordered = FALSE
        )
      } else {
        fire_running("Can't use hartemink, at least two variables are needed to compute mutual information. Using quantile method instead.")
        var_discretize <- discretize(
          as.data.frame(bn_df_variables[[exp_var]]),
          method = "quantile", breaks = 5,
          ordered = FALSE
        )
        bn_df_variables[[exp_var]] <- factor(
          bn_df_variables[[exp_var]],
          levels = rev(unique(var_discretize[, 1])),
          ordered = FALSE
        )
      }
    } else {
      if (dismethod != "hartemink") {
        for (i in exp_var) {
          if (class(bn_df_variables[[i]]) == "numeric") {
            var_discretize <- discretize(
              as.data.frame(bn_df_variables[[i]]),
              method = dismethod,
              breaks = 5,
              ordered = FALSE
            )
            bn_df_variables[[i]] <- var_discretize[, 1]
            bn_df_variables[[i]] <- factor(
              bn_df_variables[[i]],
              levels = rev(unique(var_discretize[, 1])),
              ordered = FALSE
            )
          }
        }
      } else {
        df_dis <- select(bn_df_variables, all_of(exp_var))
        var_discretize <- discretize(
          as.data.frame(df_dis),
          method = dismethod,
          breaks = 5,
          ordered = FALSE
        )
        bn_df_variables[names(var_discretize)] <- var_discretize
      }
    }
    bn_df_variables
  }

#' Variable selection module server-side processing
#'
#' @param result_env a list contain orginal_bn_df_taxas ,  orginal_bn_df_taxas
#'
#' @return result_env conatain bn_df_taxas/bn_df_taxas_norm after filtering and normalizing
fitler_norm_count_data <- function(orginal_bn_df_taxas, orginal_bn_df_variables,
                                   taxa_count_filters) {
  # FILTRADO POR PORCENTAJE BEFORE
  result_env <- list()
  to_remove <- c()
  if (!is.null(taxa_count_filters) &&
    taxa_count_filters$filterBA == "Before") {
    ## apply filter
    to_remove <- apply_taxa_before_filter(
      orginal_bn_df_taxas,
      orginal_bn_df_variables,
      taxa_count_filters
    )
  }
  result_env$bn_df_taxas <- orginal_bn_df_taxas
  bn_df_taxas.col_sum <- colSums(result_env$bn_df_taxas)
  bn_df_taxas.row_sum <- rowSums(result_env$bn_df_taxas)

  result_env$bn_df_taxas_norm <- nomralize_data(
    result_env$bn_df_taxas,
    bn_df_taxas.col_sum,
    bn_df_taxas.row_sum
  )

  result_env$bn_df_taxas_norm_log <- log1p(result_env$bn_df_taxas_norm)

  if (length(to_remove) > 0) {
    result_env$bn_df_taxas <- result_env$bn_df_taxas[, -to_remove]
    result_env$bn_df_taxas_norm <- result_env$bn_df_taxas_norm[, -to_remove]
    result_env$bn_df_taxas_norm_log <- result_env$bn_df_taxas_norm_log[, -to_remove]
  }
  result_env$to_remove <- to_remove
  result_env
}


zero_infl_model_formula <<- function(node, parents, data, offset = NULL) {
  if (length(parents) == 0) {
    model_str <- "1"
    zero_str <- "1"
  } else {
    continuous.parents <- NULL
    if (length(parents) > 0) {
      discrete.parents <- names(which(sapply(data[, parents, drop = FALSE], is.factor)))
      continuous.parents <- setdiff(parents, discrete.parents)
    }
    if (length(discrete.parents) > 0) {
      model_str <- paste("0 + ", paste(discrete.parents, collapse = " + "))
      zero_str <- paste("0 + ", paste(discrete.parents, collapse = " + "))
    } else {
      model_str <- "1"
      zero_str <- "1"
    }

    for (continuous.parent in continuous.parents) {
      model_str <- paste(model_str, " + ", paste0("log1p(", continuous.parent, ")"))
    }
  }
  if (!is.null(offset)) {
    model_str <- paste(model_str, offset, sep = " + ")
    zero_str <- paste(zero_str, offset, sep = " + ")
  }
  zero_infl_model <- "1"
  # paste(model_str,zero_infl_model)

  # if(!is.null(offset)) {
  #   zero_infl_model = paste(zero_infl_model,offset,sep = " + ")
  # }
  # print(model_str)
  paste(node, "~", model_str, "|", zero_infl_model)
}

zero_infl_intr_model_formula <<- function(node, parents, data, offset = NULL) {
  if (length(parents) == 0) {
    model_str <- "1"
    zero_str <- "1"
  } else {
    continuous.parents <- NULL
    if (length(parents) > 0) {
      discrete.parents <- names(which(sapply(data[, parents, drop = FALSE], is.factor)))
      continuous.parents <- setdiff(parents, discrete.parents)
    }
    if (length(discrete.parents) > 0) {
      model_str <- paste("0 + (", paste(discrete.parents, collapse = "+"), " )")
      zero_str <- paste("0 + ", paste(discrete.parents, collapse = " + "))
      if (length(continuous.parents) > 0) model_str <- paste(model_str, "*")
    } else {
      model_str <- "1"
      zero_str <- "1"
      if (length(continuous.parents) > 0) model_str <- paste(model_str, "+")
    }

    if (length(continuous.parents) > 0) {
      model_str <- paste(model_str, "(")
      plus_sign <- ""
      for (continuous.parent in continuous.parents) {
        model_str <- paste(model_str, plus_sign, paste0("log1p(", continuous.parent, ")"))
        plus_sign <- " + "
      }
      model_str <- paste(model_str, ")")
    }
  }
  if (!is.null(offset)) {
    model_str <- paste(model_str, offset, sep = " + ")
    zero_str <- paste(zero_str, offset, sep = " + ")
  }
  zero_infl_model <- "1"
  # paste(model_str,zero_infl_model)

  # if(!is.null(offset)) {
  #   zero_infl_model = paste(zero_infl_model,offset,sep = " + ")
  # }
  # print(model_str)
  paste(node, "~", model_str, "|", zero_infl_model)
}


bn_score_model_formula <<- zero_infl_model_formula

calculate_Offset <- function(taxa_data, ...) {
  rowSums(taxa_data)
}

BN_SCORE_ZINB <<- "BIC-ZINB"

BN_SCORE_BIC <<- "BIC"
BN_SCORE_AIC <<- "AIC"
BN_SCORE_loglik <<- "Loglik"

BN_DIST_ZINB <<- "ZINB"
BN_DIST_LOG_NORMAL <<- "Log-Normal"

zinb.dispersion <- function(model) {
  E2 <- resid(model, type = "pearson")
  p <- length(coef(model))
  N_samples <- length(model$y)
  sum(E2^2) / (N_samples - p)
}

calc.model.score <- function(model, score = BN_SCORE_BIC, n_parents, add.regu = TRUE, dispersion.corr = TRUE, sign.corr = FALSE, rmse.corr = FALSE, discrete.parents.corr = NULL) {
  score_value <- NA

  if (is(model, "zeroinfl")) {
    if (score == BN_SCORE_BIC) {
      score_value <- -BIC(model) / 2
    }
    if (score == BN_SCORE_AIC) {
      score_value <- -AIC(model) / 2
    }
    if (score == BN_SCORE_loglik) {
      score_value <- loglik(model)
    }
    model_sammary <- summary(model)
    ## signif_level <- mean(c(model_sammary$coefficients$count[, 4], model_sammary$coefficients$zero[, 4]))
    # if(is.na(signif_level_valid)) score_value <- NA
    # signif_level_valid <-  mean(model_sammary$coefficients$count[, 4])
    signif_level_valid <- mean(c(model_sammary$coefficients$count[, 4], model_sammary$coefficients$zero[, 4]))
    if (add.regu) {
      regu.term <- 0
      n.terms <- 0
      if (sign.corr && !is.na(signif_level_valid)) {
        if (n_parents > 0) {
          n_coffs <- length(model_sammary$coefficients$count[, 4])
          # coffs <- model_sammary$coefficients$count[, 4][1:n_coffs-1]
          coffs <- c(model_sammary$coefficients$count[, 4][1:n_coffs - 1], model_sammary$coefficients$zero[, 4])
          # coffs <- model_sammary$coefficients$count[, 4]
          # print(coffs)
          signif_level <- sum(coffs)
          signif_count <- sum(coffs <= 0.05)
          if (signif_count > 0) signif_level <- signif_level / signif_count # else signif_level <- 1
          signif_level <- signif_level + model_sammary$coefficients$count[, 4][n_coffs]
          # score_value = score_value *  signif_level
          n.terms <- n.terms + 1
          regu.term <- regu.term + signif_level
        }
      }
      if (is.na(signif_level_valid)) {
        n.terms <- n.terms + 1
        regu.term <- regu.term + 3
      }
      if (dispersion.corr) {
        dispersion <- zinb.dispersion(model)
        # print(dispersion)
        if (is.nan(dispersion)) dispersion <- 3
        if (dispersion < 1) {
          dispersion <- 2 - dispersion
        }
        n.terms <- n.terms + 1
        regu.term <- regu.term + dispersion
      }
      if (rmse.corr) {
        rmse <- 1 + E_RMSE(node_model$y, model$fitted.values)
        rmse <- 1 - 1 / rmse
        regu.term <- regu.term + rmse
        n.terms <- n.terms + 1
      }
      # print(discrete.parents.corr)
      if (!is.null(discrete.parents.corr)) {
        regu.term <- regu.term + (1 - discrete.parents.corr)
        n.terms <- n.terms + 1
      }


      regu.term <- regu.term / n.terms
      # print(regu.term)
      # print(n.terms)
      if (n.terms > 1) {
        score_value <- score_value + score_value * regu.term
      }
      # print(score_value)
    }

    # if(sign.corr && !is.na(signif_level_valid)) {
    #  if(n_parents > 0) {
    #    # coffs <- c(model_sammary$coefficients$count[, 4], model_sammary$coefficients$zero[, 4])
    #    coffs <- model_sammary$coefficients$count[, 4]
    #
    #    signif_level <- sum(coffs)
    #    signif_count <- sum(coffs <= 0.05)
    #    if(signif_count > 0) signif_level <- signif_level/signif_count else signif_level <- 1
    #    score_value = score_value *  signif_level
    #  }
    #
    # }
    # if(FALSE) {
    #   res <- cor(model$fitted.values,node_model$y , method = "spearman" , use = "complete.obs")
    #   res <- 1-res
    #   score_value = score_value *  res
    # }

    ## add penalty
    if (!add.regu && is.na(signif_level_valid)) score_value <- score_value * 3
  }

  score_value
}

cust.fit.arc.strength <- function(fittedbn_custom, result_dag, arc_st_mi_nl) {
  # edgs.test <- arcs(result_dag)
  # edgs.test <- as.data.frame(edgs.test)
  # edgs.test$strength <- 1
  edgs.test <- arc_st_mi_nl
  for (i in 1:nrow(edgs.test)) {
    node_from <- edgs.test[i, 1]
    node_to <- edgs.test[i, 2]
    node_model <- fittedbn_custom[[node_to]]
    ## TODO :: replace with is(node_model, "zeroinfl") better
    if ("zeroinfl" %in% class(node_model)) {
      node_model_sum <- summary(node_model)
      nodes_from_names <- rownames(node_model_sum$coefficients$count)
      coffs_index <- grepl(node_from, nodes_from_names)
      values <- min(node_model_sum$coefficients$count[coffs_index, 4])
      if (is.na(values)) values <- 1
      edgs.test[i, 3] <- values
    }
  }
  edgs.test
}

## args should have
## org_data << dataframe with the offset data if we need it
## create_model_formula :: for model formulate creation
##
custom.glm.bic <- function(node, parents, data, args) {
  ###########################
  # data <- args$org_data
  test_score <- BN_SCORE_BIC
  with_offset <- FALSE
  offset_term <- NULL
  if (!is.null(args$Offset)) {
    data <- cbind(data, args$Offset)
    with_offset <- TRUE
    offset_term <- "offset(log(Offset))"
  }
  if (!is.null(args$net_score)) {
    test_score <- args$net_score
  }
  if (test_score == BN_SCORE_BIC) test_score.cg <- "bic"
  if (test_score == BN_SCORE_AIC) test_score.cg <- "aic"
  if (test_score == BN_SCORE_loglik) test_score.cg <- "loglik"

  create_model_formula <- args$create_model_formula
  total.discrete.nodes <- args$total_discrete_nodes
  ## BY default
  dispersion.corr <- TRUE
  sign.corr <- FALSE
  if (!is.null(args$dispersion.corr)) {
    dispersion.corr <- args$dispersion.corr
  }
  if (!is.null(args$sign.corr)) {
    sign.corr <- args$sign.corr
  } ## this is used for edges weights
  # if(!is.null(args$dispersion.corr))
  #  dispersion.corr = args$dispersion.corr
  ###########################
  INF_VALUE <<- 1e+30



  if (is.numeric(data[, node])) {
    if (length(parents) == 0) {
      model_str <- paste(node, "~ 1 ")
    } else {
      model_str <- paste(node, "~", paste(parents, collapse = "+"))
    }
  } else {
    if (length(parents) == 0) {
      model_str <- paste(node, "~ 1")
    } else {
      model_str <- paste(node, "~", paste(parents, collapse = "+"))
    }
  }

  if (is.numeric(data[, node])) {
    node_dist <- "glm.nb"
    # if (!is.null(nodes_dists[[node]])) {
    #   node_dist <- nodes_dists[[node]]
    # }
    if (node_dist == "glm.nb") {
      model_str <-
        create_model_formula(node, parents, data, offset_term)
      # print(model_str)
      final__model <<- as.formula(model_str)
    }
  } else {
    final__model <<- as.formula(model_str)
  }

  if (is.numeric(data[, node])) {
    ## only taxa
    if (node_dist == "glm.nb0") {
      glm.model <- try(MASS::glm.nb(final__model, data = data))
    } else if (node_dist == "glm.nb") {
      glm.model <- try(
        pscl::zeroinfl(final__model,
          data = data,
          dist = "negbin"
        )
      )
      # if(is.null(glm.model)) return (-INF_VALUE)
      if ("try-error" %in% class(glm.model)) {
        # print(model_str)
        # print(glm.model)
        ## ###
        return(-INF_VALUE)
      }
      model_sammary <- summary(glm.model)

      # print(model_str)
      ## final_score <-  (-BIC(glm.model) / 2) * mean(c(model_sammary$coefficients$count[, 4], model_sammary$coefficients$zero[, 4]))
      discrete.parents.corr <- NULL
      if (!is.null(total.discrete.nodes)) {
        discrete.parents <- 0
        if (length(parents) > 0) {
          discrete.parents <- names(which(sapply(data[, parents, drop = FALSE], is.factor)))
        }
        discrete.parents.corr <- length(discrete.parents) / total.discrete.nodes
      }

      final_score <- calc.model.score(glm.model, test_score, length(parents),
        discrete.parents.corr = discrete.parents.corr,
        dispersion.corr = dispersion.corr,
        sign.corr = sign.corr
      )
      if (is.na(final_score)) final_score <- -INF_VALUE
      # print(final_score)
      return(final_score)
    } else {
      ## normal
    }
  } else if (is.factor(data[, node])) {
    continuous.parents <- NULL
    if (length(parents) > 0) {
      discrete.parents <- names(which(sapply(data[, parents, drop = FALSE], is.factor)))
      continuous.parents <- setdiff(parents, discrete.parents)
    }
    if (length(continuous.parents) > 0) {
      # print(node)
      return(-INF_VALUE)
    } else {
      dummy.dag <- empty.graph(c(node, parents))
      for (par in parents) {
        dummy.dag <- set.arc(dummy.dag, from = par, to = node)
      }
      return.score <- try(score(dummy.dag, data[, c(node, parents), drop = FALSE], by.node = TRUE, type = test_score.cg)[node])
      if (class(return.score) == "try-error") {
        # print(node)
        # print(return.score)
        return(-INF_VALUE)
      } else {
        if (length(parents) > 0) {
          return(return.score)
        } else {
          return(return.score)
        }
      }
    }
  }
}


bn.fit.custom.fit <- function(bn_data, bn_dag, bn_fit, nodes_to_fit, args = list()) {
  offset_term <- NULL
  with_offset <- FALSE
  if (!is.null(args$Offset)) {
    bn_data <- cbind(bn_data, args$Offset)
    with_offset <- TRUE
    offset_term <- "offset(log(Offset))"
  }
  create_model_formula <- args$create_model_formula
  ## TODO create_model_formula must not be null
  # if(!is.null(bn_data$Offset)) {
  #   offset_term <- "offset(log(Offset))"
  # }

  take_aways_nodes <- setdiff(colnames(bn_data), c(nodes_to_fit, "Offset"))
  custom_fit <- list()
  for (node in take_aways_nodes) {
    custom_fit[[node]] <- bn_fit[[node]]
  }
  for (node in nodes_to_fit) {
    bn_dag$nodes[[node]]$parents
    model_str <-
      create_model_formula(node, bn_dag$nodes[[node]]$parents, bn_data, offset_term)
    final__model <<- as.formula(model_str)
    glm.model <- try(pscl::zeroinfl(final__model,
      data = bn_data,
      dist = "negbin"
    ))
    if ("try-error" %in% class(glm.model)) {
      print(paste("can not fit zeroinfl model for node:", node))
      glm.model <- NULL
    }

    custom_fit[[node]] <- glm.model
  }
  custom_fit
}

get.sampling.path <- function(bn_dag, target_node = NULL) {
  final_path <- c()
  if (is.null(target_node)) {
    all_nodes <- bnlearn::nodes(bn_dag)
    while (length(all_nodes) > 0) {
      for (node in all_nodes) {
        node.parents <- bnlearn::parents(bn_dag, node)
        if (length(node.parents) == 0) {
          final_path <- c(final_path, node)
        } else if (sum(node.parents %in% final_path) == length(node.parents)) {
          final_path <- c(final_path, node)
        }
      }
      all_nodes <- setdiff(all_nodes, final_path)
    }
  } else {
    node.parents <- bnlearn::parents(bn_dag, target_node)
    for (p_node in node.parents) {
      final_path <- union(final_path, sampleing.path(bn_dag, p_node))
    }
    final_path <- union(final_path, node.parents)
    final_path <- c(final_path, target_node)
  }
  final_path
}


get_samples_all <- function(custom_fit, bn_fit, input_evidence, sampling.path, observed_variables, average.offset, nSamples = 100000, incProgress = NULL) {
  ##
  cp_observed_variables <- intersect(observed_variables, sampling.path)
  cp_observed_variables <- union(cp_observed_variables, names(input_evidence))
  ## this could be all diff com
  init_samples <- cpdist(bn_fit, nodes = cp_observed_variables, evidence = input_evidence, method = "lw", n = nSamples)
  ## what is a better way to get the init samples ??
  # start_samples <- cbind(start_samples,input_evidence)

  if (length(average.offset) > 1) {
    init_samples$Offset <- sample(average.offset, nrow(init_samples), replace = TRUE)
  } else {
    init_samples$Offset <- average.offset
  }


  init_samples_EX <- init_samples
  for (node in sampling.path) {
    if (!is.null(incProgress)) {
      incProgress(1 / length(sampling.path), detail = "Sampleing ...")
    }
    if (node %in% observed_variables) {
      ## ignore
      next
    }
    node_model <- custom_fit[[node]]

    ## capture varitains in the data
    zero_comp <- predict(node_model, init_samples, type = "zero")
    count_comp <- predict(node_model, init_samples, type = "count")
    final_counts <- sapply(1:length(count_comp), function(i) {
      ZIM::rzinb(1, k = node_model$theta, lambda = count_comp[i], zero_comp[i])
    })
    init_samples[[node]] <- final_counts

    # start_samples[[node]] <- round( MASS::rnegbin(predict(node_model,start_samples),theta = node_model$theta ))
    init_samples_EX[[node]] <- round(predict(node_model, init_samples_EX, type = "response"))
  }
  # sub_samples <- filter_by_evidence(start_samples,input_evidence)

  list(
    all_sampels = init_samples,
    exp_counts = init_samples_EX
  )
}


do.bn.cv <- function(data, bn, targets, from, resul_list = list()) {
  k <- 3
  runs <- 15
  if (is.null(resul_list[["all"]]$loss_values)) resul_list[["all"]]$loss_values <- c()
  if (is.null(resul_list[["all"]]$loss)) resul_list[["all"]]$loss <- 0
  n_nodes <- length(targets)
  for (node in targets) {
    xval <- try(bnlearn::bn.cv(data, bn, loss = "cor-lw-cg", loss.args = list(target = node, from = from, n = 200), k = 3, runs = 15))
    if ("try-error" %in% class(xval)) {
      res <- c(NA)
    } else {
      res <- c()
      for (i in 1:runs) {
        for (j in 1:k) {
          res <- c(res, xval[[i]][[j]]$loss)
        }
      }
    }

    resul_list[[node]]$loss_values <- res[!is.na(res)]
    resul_list[[node]]$loss <- mean(res, na.rm = TRUE)
    resul_list[["all"]]$loss_values <- c(resul_list[["all"]]$loss_values, res)
    resul_list[["all"]]$loss <- resul_list[["all"]]$loss + resul_list[[node]]$loss / n_nodes
  }
  resul_list
}

do.zinb.bn.cv <- function(data, bn, custom_fit, targets, from, resul_list = list()) {
  k <- 3
  runs <- 15
  if (is.null(resul_list[["all"]]$loss_values)) resul_list[["all"]]$loss_values <- c()
  if (is.null(resul_list[["all"]]$loss)) resul_list[["all"]]$loss <- 0
  n_nodes <- length(targets)
  for (node in targets) {
    ## TODO :: perfom a cv here
    node_model <- custom_fit[[node]]
    if (is.null(node_model)) {
      res <- NA
    } else {
      res <- cor(node_model$fitted.values, node_model$model[[node]], use = "complete.obs") # method = "spearman"
    }
    # loss_cor <- c()
    # for(i in 1:100) {
    #   zero_comp <- predict(node_model,type="zero")
    #   count_comp <- predict(node_model,type="count")
    #   final_counts  <- sapply(1:length(count_comp), function(i) {
    #     ZIM::rzinb(1,k = node_model$theta, lambda = count_comp[i],zero_comp[i])
    #   } )
    #   loss_cor <- c(loss_cor,cor(final_counts,node_model$model[[node]] , method = "spearman" , use = "complete.obs"))
    # }
    # res <- mean(loss_cor)
    ## TODO :: calculation here is not fair
    if (is.na(res)) res <- 0
    resul_list[[node]]$loss_values <- c(res)
    resul_list[[node]]$loss <- res
    resul_list[["all"]]$loss_values <- c(resul_list[["all"]]$loss_values, res)
    resul_list[["all"]]$loss <- resul_list[["all"]]$loss + resul_list[[node]]$loss / n_nodes
  }
  resul_list
}


E_MSE <- function(O, P, in_log = FALSE) {
  if (in_log) {
    link_fun <- expm1
    O <- link_fun(O)
    P <- link_fun(P)
  }
  d <- O - P
  sum(d * d) / length(O)
}

E_RMSE <- function(O, P, in_log = FALSE) {
  sqrt(E_MSE(O, P, in_log))
}

bn.dispersion.parm <- function(model, E2, N_samples) {
  # E2 <- resid(model, type = "pearson")
  p <- length(coef(model))
  sum(E2^2) / (N_samples - p)
}
E_R2 <- function(pred, obs, formula = "corr", link = NULL, na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  if (!is.null(link)) {
    pred <- link(pred)
    obs <- link(obs)
  }

  switch(formula,
    corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
    traditional = 1 - (sum((obs - pred)^2, na.rm = na.rm) / ((n - 1) * var(obs, na.rm = na.rm)))
  )
}

bn.collect.metrics <- function(bn_data, bn_fit, targets, result_list = list()) {
  n_sampels <- nrow(bn_data)
  n_nodes <- length(targets)
  if (is.null(result_list[["all"]]$MSE)) {
    result_list[["all"]]$MSE <- 0
    result_list[["all"]]$RMSE <- 0
    result_list[["all"]]$dispersion <- 0
    result_list[["all"]]$residuals <- c()
    result_list[["all"]]$R2 <- 0

    result_list[["all"]]$MSE_lgs <- 0
    result_list[["all"]]$RMSE_lgs <- 0
    result_list[["all"]]$dispersion_lgs <- 0
    result_list[["all"]]$residuals_lgs <- c()
    result_list[["all"]]$R2_lgs <- 0
  }

  # if(is.null(result_list[["all"]]$RMSE )) result_list[["all"]]$RMSE <- 0
  # if(is.null(result_list[["all"]]$dispersion )) result_list[["all"]]$dispersion <- 0
  # if(is.null(result_list[["all"]]$R2 )) result_list[["all"]]$R2 <- 0
  for (node in targets) {
    node_model <- bn_fit[[node]]
    observed_data <- bn_data[[node]]
    result_list[[node]]$MSE <- NA
    result_list[[node]]$RMSE <- NA
    result_list[[node]]$residuals <- NA
    result_list[[node]]$dispersion <- NA
    result_list[[node]]$R2 <- NA
    if (is(node_model, "bn.fit.cgnode") || is(node_model, "bn.fit.gnode")) {
      ## bn normal fit
      result_list[[node]]$MSE <- E_MSE(observed_data, node_model$fitted.values, in_log = TRUE)
      result_list[[node]]$RMSE <- E_RMSE(observed_data, node_model$fitted.values, in_log = TRUE)
      result_list[[node]]$residuals <- round(expm1(observed_data)) - round(expm1(node_model$fitted.values))
      E2 <- result_list[[node]]$residuals / sd(result_list[[node]]$residuals)
      result_list[[node]]$dispersion <- bn.dispersion.parm(node_model, E2, n_sampels)
      result_list[[node]]$R2 <- E_R2(observed_data, node_model$fitted.values, link = expm1)

      if (is.numeric(result_list[[node]]$MSE) && !(is.na(result_list[[node]]$MSE))) {
        result_list[["all"]]$MSE <- result_list[["all"]]$MSE + result_list[[node]]$MSE / n_nodes
      }
      if (is.numeric(result_list[[node]]$RMSE) && !(is.na(result_list[[node]]$RMSE))) {
        result_list[["all"]]$RMSE <- result_list[["all"]]$RMSE + result_list[[node]]$RMSE / n_nodes
      }
      if (is.numeric(result_list[[node]]$dispersion) && !(is.na(result_list[[node]]$dispersion))) {
        result_list[["all"]]$dispersion <- result_list[["all"]]$dispersion + result_list[[node]]$dispersion / n_nodes
      }
      if (is.numeric(result_list[[node]]$R2) && !(is.na(result_list[[node]]$R2))) {
        result_list[["all"]]$R2 <- result_list[["all"]]$R2 + result_list[[node]]$R2 / n_nodes
      }
      result_list[["all"]]$residuals <- c(result_list[["all"]]$residuals, result_list[[node]]$residuals)

      ## log scale
      result_list[[node]]$MSE_lgs <- E_MSE(observed_data, node_model$fitted.values, in_log = FALSE)
      result_list[[node]]$RMSE_lgs <- E_RMSE(observed_data, node_model$fitted.values, in_log = FALSE)
      result_list[[node]]$residuals_lgs <- node_model$residuals
      E2 <- result_list[[node]]$residuals_lgs / sd(result_list[[node]]$residuals_lgs)
      result_list[[node]]$dispersion_lgs <- bn.dispersion.parm(node_model, E2, n_sampels)
      result_list[[node]]$R2_lgs <- E_R2(observed_data, node_model$fitted.values)


      if (is.numeric(result_list[[node]]$MSE_lgs) && !(is.na(result_list[[node]]$MSE_lgs))) {
        result_list[["all"]]$MSE_lgs <- result_list[["all"]]$MSE_lgs + result_list[[node]]$MSE_lgs / n_nodes
      }
      if (is.numeric(result_list[[node]]$RMSE_lgs) && !(is.na(result_list[[node]]$RMSE_lgs))) {
        result_list[["all"]]$RMSE_lgs <- result_list[["all"]]$RMSE_lgs + result_list[[node]]$RMSE_lgs / n_nodes
      }
      if (is.numeric(result_list[[node]]$dispersion_lgs) && !(is.na(result_list[[node]]$dispersion_lgs))) {
        result_list[["all"]]$dispersion_lgs <- result_list[["all"]]$dispersion_lgs + result_list[[node]]$dispersion_lgs / n_nodes
      }
      if (is.numeric(result_list[[node]]$R2_lgs) && !(is.na(result_list[[node]]$R2_lgs))) {
        result_list[["all"]]$R2_lgs <- result_list[["all"]]$R2_lgs + result_list[[node]]$R2_lgs / n_nodes
      }
      result_list[["all"]]$residuals_lgs <- c(result_list[["all"]]$residuals_lgs, result_list[[node]]$residuals_lgs)
    }
  }

  result_list
}

zinb.bn.collect.metrics <- function(bn_data, bn_fit, targets, result_list = list()) {
  n_sampels <- nrow(bn_data)
  n_nodes <- length(targets)
  if (is.null(result_list[["all"]]$MSE)) {
    result_list[["all"]]$MSE <- 0
    result_list[["all"]]$RMSE <- 0
    result_list[["all"]]$dispersion <- 0
    result_list[["all"]]$residuals <- c()
    result_list[["all"]]$R2 <- 0
  }

  # if(is.null(result_list[["all"]]$RMSE )) result_list[["all"]]$RMSE <- 0
  # if(is.null(result_list[["all"]]$dispersion )) result_list[["all"]]$dispersion <- 0
  # if(is.null(result_list[["all"]]$R2 )) result_list[["all"]]$R2 <- 0
  for (node in targets) {
    node_model <- bn_fit[[node]]

    result_list[[node]]$MSE <- NA
    result_list[[node]]$RMSE <- NA
    result_list[[node]]$residuals <- NA
    result_list[[node]]$dispersion <- NA
    result_list[[node]]$R2 <- NA
    if (is(node_model, "zeroinfl")) {
      observed_data <- node_model$model[[node]]
      ## bn normal fit
      result_list[[node]]$MSE <- E_MSE(observed_data, node_model$fitted.values)
      result_list[[node]]$RMSE <- E_RMSE(observed_data, node_model$fitted.values)
      result_list[[node]]$residuals <- node_model$residuals
      E2 <- residuals(node_model, type = "pearson")
      result_list[[node]]$dispersion <- bn.dispersion.parm(node_model, E2, n_sampels)
      result_list[[node]]$R2 <- E_R2(observed_data, node_model$fitted.values)

      if (is.numeric(result_list[[node]]$MSE) && !(is.na(result_list[[node]]$MSE))) {
        result_list[["all"]]$MSE <- result_list[["all"]]$MSE + result_list[[node]]$MSE / n_nodes
      }
      if (is.numeric(result_list[[node]]$RMSE) && !(is.na(result_list[[node]]$RMSE))) {
        result_list[["all"]]$RMSE <- result_list[["all"]]$RMSE + result_list[[node]]$RMSE / n_nodes
      }
      if (is.numeric(result_list[[node]]$dispersion) && !(is.na(result_list[[node]]$dispersion))) {
        result_list[["all"]]$dispersion <- result_list[["all"]]$dispersion + result_list[[node]]$dispersion / n_nodes
      }
      if (is.numeric(result_list[[node]]$R2) && !(is.na(result_list[[node]]$R2))) {
        result_list[["all"]]$R2 <- result_list[["all"]]$R2 + result_list[[node]]$R2 / n_nodes
      }
      result_list[["all"]]$residuals <- c(result_list[["all"]]$residuals, result_list[[node]]$residuals)
    }
  }

  result_list
}

#' Variable selection module server-side processing
#'
#' @param result_env a list contain :
#' orginal_bn_df_taxas ,
#' orginal_bn_df_variables
#' bn_df_taxas_norm
#' bn_df_variables
#'
#' network_build_option list contains :
#' net_dir
#'
#' @return result_env conatain bn_df_taxas/bn_df_taxas_norm after filtering and normalizing
build_bn_model <- function(result_env,
                           network_build_option) {
  if (!exists("fire_running")) { ## dummy interface
    set_status <- function(msg) {
    }

    fire_interrupt <- function() {
    }

    fire_ready <- function() {

    }

    fire_running <- function(perc_complete) {

    }

    interrupted <- function() {
      FALSE
    }
  }



  net_dir <- network_build_option$net_dir
  log_file <- file.path(net_dir, "log_file.txt")
  sink(log_file)
  fire_running("Reading input files")
  print("Reading input files", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }





  fire_running("Creating model and training datasets")
  print("Creating model and training datasets", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }

  # combinations_var <- distinct(dis_exp_variables)

  fire_running("Writing Normalized taxa raw counts")
  print("Writing Normalized taxa raw counts", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }


  output_norm <- file.path(net_dir, "taxa_norm_counts.csv")
  write.table(result_env$bn_df_taxas_norm, file = output_norm, dec = ",", sep = ";")

  fire_running("Writing log scale normalized taxa data")
  print("Writing log scale normalized taxa data", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }


  output_log <- file.path(net_dir, "taxa_norm_log_counts.csv")
  write.table(result_env$bn_df_taxas_norm_log, file = output_log, dec = ",", sep = ";")


  # if (length(result_env$to_remove) > 0) {
  #   bn_df_taxas_norm_log <- result_env$bn_df_taxas_norm_log[, -result_env$to_remove]
  # } else {
  #   bn_df_taxas_norm_log <- result_env$bn_df_taxas_norm_log
  # }

  if (!is.null(result_env$taxa_names_df)) {
    old_names <- colnames(result_env$bn_df_taxas_norm_log)
    new_names <- result_env$taxa_names_df[
      result_env$taxa_names_df[, 2] %in% old_names,
      1
    ]
    colnames(result_env$bn_df_taxas_norm_log) <- new_names
    colnames(result_env$bn_df_taxas_norm) <- new_names
    colnames(result_env$bn_df_taxas) <- new_names
  }


  # bn_df_model <- cbind(bn_df_variables[rownames(data_model),],bn_df_taxas_norm[rownames(data_model),])
  # data_model <- bn_df_model
  # bn_df_training <- cbind(bn_df_variables[rownames(data_training),],bn_df_taxas_norm[rownames(data_training),])
  # data_training <- bn_df_training

  result_env$bn_df_norm <- cbind(
    result_env$bn_df_variables,
    result_env$bn_df_taxas_norm_log
  )

  fire_running("Creating network model")
  print("Creating network model", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }

  ## this need checking
  netscore.g <- network_build_option$netscore
  net_dist <- network_build_option$net_dist
  # netscore.g <- tolower(paste(network_build_option$netscore, "-CG", sep = ""))
  print(netscore.g)
  USE_OFFSET <- FALSE
  if (net_dist == BN_DIST_ZINB) {
    if (!is.null(network_build_option$use_offset)) {
      USE_OFFSET <- network_build_option$use_offset
    }

    if (USE_OFFSET) {
      result_env$Offset <- calculate_Offset(result_env$bn_df_taxas)

      result_env$input_bn_df <- cbind(
        result_env$bn_df_variables,
        result_env$bn_df_taxas
      )
    } else {
      ## ZINB only accept integers
      result_env$Offset <- NULL
      result_env$input_bn_df <- cbind(
        result_env$bn_df_variables,
        round(result_env$bn_df_taxas_norm)
      )
    }

    n_discrete.nodes <- NULL
    n_discrete.nodes <- ncol(result_env$bn_df_variables)

    fire_running("Starting model building")
    print("Starting model building", quote = FALSE)

    result_env$result <- hc(
      result_env$input_bn_df,
      score = "custom",
      fun = custom.glm.bic,
      restart = 1,
      optimized = TRUE,
      args = list(
        Offset = result_env$Offset,
        create_model_formula = bn_score_model_formula,
        net_score = netscore.g,
        total_discrete_nodes = n_discrete.nodes,
        sign.corr = FALSE,
        dispersion.corr = TRUE
      ),
      whitelist = network_build_option$wl,
      blacklist = network_build_option$bl
    )
    fire_running("Structure Search Finish")
    print("Structure Search Finish", quote = FALSE)
    ## plug here filter logic
    ## TODO :: set all to 1
    # result_env$arc_st_mi <- c()
    # result_env$arc_st_bic <- c()
    result_env$result_filt <- result_env$result

    result_env$arc_st_bic <- arc.strength(result_env$result, result_env$input_bn_df, fun = custom.glm.bic, args = list(
      Offset = result_env$Offset,
      create_model_formula = bn_score_model_formula,
      net_score = netscore.g,
      total_discrete_nodes = n_discrete.nodes,
      sign.corr = TRUE,
      dispersion.corr = TRUE
    ))
    fire_running("arc strength Finish")
    print("arc strength Finish", quote = FALSE)
    tmp_fittedbn <- bn.fit(result_env$result_filt, data = result_env$bn_df_norm, replace.unidentifiable = TRUE)
    df_input <- result_env$input_bn_df
    if (USE_OFFSET) {
      df_input <- cbind(result_env$input_bn_df, result_env$Offset)
    }
    nodes_to_fit <- colnames(result_env$bn_df_taxas)

    fire_running("try fitting and measuring strength")
    print("try fitting and measuring strength", quote = FALSE)

    tmp_fittedbn_custom <- bn.fit.custom.fit(
      df_input,
      result_env$result_filt,
      tmp_fittedbn,
      nodes_to_fit,
      args = list(
        Offset = result_env$Offset,
        create_model_formula = bn_score_model_formula
      )
    )

    arc_st_mi_nl <- arc.strength(
      result_env$result_filt,
      result_env$bn_df_norm,
      criterion = "mi-cg"
    )

    result_env$arc_st_mi <- cust.fit.arc.strength(tmp_fittedbn_custom, result_env$result, arc_st_mi_nl)

    fire_running("Finish measuring strength")
    print("Finish measuring strength", quote = FALSE)

    remove_arcs <- data.frame(matrix(data = NA, nrow = 0, ncol = 2))
    colnames(remove_arcs) <- c("from", "to")
    n <- 0

    for (l in 1:nrow(result_env$arc_st_bic)) {
      if ((result_env$arc_st_bic[l, 3] < network_build_option$thr_bic) &&
        (result_env$arc_st_mi[l, 3] < network_build_option$thr_mi)) {
        n <- n + 1
      } else {
        row <- c(result_env$arc_st_bic[l, 1], result_env$arc_st_bic[l, 2])
        remove_arcs <- rbind(remove_arcs, row)
      }
    }

    out_remove <- file.path(net_dir, "removed_arcs.txt")
    write.table(remove_arcs, out_remove, sep = "\t", dec = ",")


    # wl_df <-as.data.frame( result_build_env$network_build_option$wl)
    # if (nrow(remove_arcs) != 0) {
    #   for (i in 1:nrow(remove_arcs)) {
    #     d <- data.frame(from = remove_arcs[i, 1], to = remove_arcs[i, 2])
    #     #comparison <- compare::compare(d, network_build_option$wl, allowAll = TRUE)
    #     comparison <- plyr::match_df(remove_arcs,wl_df)
    #     if (nrow(comparison) == 0 ) {
    #       result_env$result_filt <- drop.arc(result_env$result_filt, remove_arcs[i, 1], remove_arcs[i, 2])
    #     }
    #   }
    # }
    result_env$remove_arcs <- remove_arcs



    strength_mi <- file.path(net_dir, "arc_strength_mi.txt")
    write.table(result_env$arc_st_mi, strength_mi, sep = "\t", dec = ",")

    strength_bic <- file.path(net_dir, "arc_strength_bic.txt")
    write.table(result_env$arc_st_bic, strength_bic, sep = "\t", dec = ",")
  } else {
    netscore.g <- "bic-cg"
    if (network_build_option$netscore == BN_SCORE_BIC) {
      netscore.g <- "bic-cg"
    }
    if (network_build_option$netscore == BN_SCORE_loglik) {
      netscore.g <- "loglik-cg"
    }
    if (network_build_option$netscore == BN_SCORE_AIC) {
      netscore.g <- "aic-cg"
    }
    # netscore.g <- tolower(paste(network_build_option$netscore, "-CG", sep = ""))
    result_env$result <- hc(
      result_env$bn_df_norm,
      optimized = TRUE,
      score = netscore.g,
      whitelist = network_build_option$wl,
      blacklist = network_build_option$bl
    )



    fire_running("Network model done! Filtering model by link strength")
    print("Network model done! Filtering model by link strength", quote = FALSE)
    if (interrupted()) {
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }




    result_env$arc_st_bic <- arc.strength(
      result_env$result,
      result_env$bn_df_norm,
      criterion = "bic-cg"
    )
    result_env$arc_st_mi <- arc.strength(
      result_env$result,
      result_env$bn_df_norm,
      criterion = "mi-cg"
    )

    result_env$result_filt <- result_env$result
    remove_arcs <- data.frame(matrix(data = NA, nrow = 0, ncol = 2))
    colnames(remove_arcs) <- c("from", "to")
    n <- 0

    for (l in seq_len(nrow(result_env$arc_st_bic))) {
      if ((result_env$arc_st_bic[l, 3] < network_build_option$thr_bic) &&
        (result_env$arc_st_mi[l, 3] < network_build_option$thr_mi)) {
        n <- n + 1
      } else {
        row <- c(result_env$arc_st_bic[l, 1], result_env$arc_st_bic[l, 2])
        remove_arcs <- rbind(remove_arcs, row)
      }
    }

    out_remove <- file.path(net_dir, "removed_arcs.txt")
    write.table(remove_arcs, out_remove, sep = "\t", dec = ",")


    wl_df <- as.data.frame(result_env$network_build_option$wl)
    if (nrow(remove_arcs) != 0) {
      for (i in seq_len(nrow(remove_arcs))) {
        d <- data.frame(from = remove_arcs[i, 1], to = remove_arcs[i, 2])
        # comparison <- compare::compare(d, network_build_option$wl, allowAll = TRUE)

        comparison <- plyr::match_df(remove_arcs, wl_df)
        if (nrow(comparison) == 0) {
          result_env$result_filt <- drop.arc(result_env$result_filt, remove_arcs[i, 1], remove_arcs[i, 2])
        }
      }
    }
    result_env$remove_arcs <- remove_arcs


    strength_mi <- file.path(net_dir, "arc_strength_mi.txt")
    write.table(result_env$arc_st_mi, strength_mi, sep = "\t", dec = ",")

    strength_bic <- file.path(net_dir, "arc_strength_bic.txt")
    write.table(result_env$arc_st_bic, strength_bic, sep = "\t", dec = ",")

    fire_running("Performing Cross Validation on the result Network")
    print("Performing Cross Validation on the result Network", quote = FALSE)
    result_env$taxa_metrics <- do.bn.cv(
      result_env$bn_df_norm,
      result_env$result_filt,
      colnames(result_env$bn_df_taxas),
      colnames(result_env$bn_df_variables)
    )
  }


  fire_running("Training model")
  print("Training model", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }

  result_env$fittedbn <- bn.fit(result_env$result_filt, data = result_env$bn_df_norm, replace.unidentifiable = TRUE)

  if (net_dist == BN_DIST_ZINB) {
    df_input <- result_env$input_bn_df
    if (USE_OFFSET) {
      df_input <- cbind(result_env$input_bn_df, result_env$Offset)
    }

    nodes_to_fit <- colnames(result_env$bn_df_taxas)

    result_env$fittedbn_custom <- bn.fit.custom.fit(
      df_input,
      result_env$result_filt,
      result_env$fittedbn,
      nodes_to_fit,
      args = list(
        Offset = result_env$Offset,
        create_model_formula = bn_score_model_formula
      )
    )
    fire_running("Collecting fitting metrics")
    print("Collecting fitting metrics", quote = FALSE)
    taxa_metrics <- do.zinb.bn.cv(
      result_env$input_bn_df,
      result_env$result_filt,
      result_env$fittedbn_custom,
      colnames(result_env$bn_df_taxas),
      colnames(result_env$bn_df_variables)
    )
    taxa_metrics <- zinb.bn.collect.metrics(
      result_env$input_bn_df,
      result_env$fittedbn_custom,
      colnames(result_env$bn_df_taxas),
      taxa_metrics
    )
    result_env$taxa_metrics <- taxa_metrics
  } else {
    fire_running("Collecting fitting metrics")
    print("Collecting fitting metrics", quote = FALSE)
    result_env$taxa_metrics <- bn.collect.metrics(
      result_env$bn_df_norm,
      result_env$fittedbn,
      colnames(result_env$bn_df_taxas),
      result_env$taxa_metrics
    )
  }

  fire_running("Writing output files")
  print("Writing output files", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }





  out_net_name <- paste(
    format(Sys.time(), "%F_%H.%M.%S"),
    "_complete_network.RData",
    sep = ""
  )

  result_env$exposure_variables <- colnames(result_env$bn_df_variables)
  result_env$outcome_variables <- colnames(result_env$bn_df_taxas)
  result_env$dagitty <- bn_to_dagitty(result_env$fittedbn)

  dagitty::exposures(result_env$dagitty) <- result_env$exposure_variables
  dagitty::outcomes(result_env$dagitty) <- result_env$outcome_variables

  result_env$testable_implications_taxa_vars <- get_testable_implications_v2(result_env$dagitty, result_env$outcome_variables)

  fire_running("DONE!")
  print("DONE!", quote = FALSE)
  sink()
  output_file_net <- file.path(net_dir, out_net_name)
  # save(list = ls(), file = output_file_net, envir = environment())
  saveRDS(result_env, file = output_file_net)
  result_env
}

#' Variable selection module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{xvar}{reactive character string indicating x variable selection}
#'   \item{yvar}{reactive character string indicating y variable selection}
#' }
create_model <- function(data_variables,
                         data_taxas,
                         # expVar,
                         net_dir,
                         # bl,
                         # wl,
                         # dismethod,
                         network_build_option,
                         variable_data_options,
                         taxa_count_filters
                         # filterTaxa,
                         # filterThrG,
                         # filterThrT,
                         # filterOption,
                         # filterVariable,
                         # filterCountsT,
                         # filterCountsG,
                         # filterBA
) {
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
  if (!is.null(taxa_count_filters) &&
    taxa_count_filters$filterBA == "Before") {
    ## apply filter
    to_remove <- apply_taxa_before_filter(
      bn_df_taxas,
      bn_df_variables,
      taxa_count_filters
    )
  }
  # if (filterTaxa == 1) {
  #     if (filterBA == "Before") {
  #         if (filterOption == "Total") {
  #             min_samples <- round(nrow(bn_df_taxas) * filterThrT / 100)
  #             for (i in 1:ncol(bn_df_taxas)) {
  #                 counts <- sum(as.numeric(bn_df_taxas[, i]) >= as.numeric(filterCountsT))
  #                 if (counts < min_samples) {
  #                     to_remove <- c(to_remove, i)
  #                 }
  #             }
  #         } else if (filterOption == "Group") {
  #             columns_var <- ncol(bn_df_variables)
  #             df_complete <- cbind(bn_df_variables, bn_df_taxas)
  #             filterThrG <- str_replace_all(filterThrG, c("/" = ".", " " = "."))
  #             filterThrG_sep <- strsplit(filterThrG, ",")
  #             filterThrG_sep2 <- c()
  #             for (i in filterThrG_sep) {
  #                 sp <- strsplit(i, "-")
  #                 filterThrG_sep2 <- c(filterThrG_sep2, sp)
  #             }
  #             filterVariable <- str_replace_all(filterVariable, c("/" = ".", " " = ".", "-" = "."))
  #             df_divided <- split(df_complete, df_complete[[filterVariable]])
  #             col_tax <- ncol(bn_df_variables) + 1
  #             for (i in filterThrG_sep2) {
  #                 filt <- df_divided[[i[1]]]
  #                 min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
  #                 for (j in col_tax:ncol(df_complete)) {
  #                     counts <- sum(as.numeric(filt[, j]) >= as.numeric(filterCountsG))
  #                     if (counts < min_samples) {
  #                         to_remove <- c(to_remove, as.numeric(j) - columns_var)
  #                     }
  #                 }
  #             }
  #         }
  #         to_remove <- unique(to_remove)

  #     }
  # }
  orginal_bn_df_taxas <- bn_df_taxas
  # bn_df_taxas <- bn_df_taxas[, -to_remove]
  # output_new_taxa <- file.path(net_dir, "filtered_taxa.csv")
  # write.table(bn_df_taxas, file = output_new_taxa, dec = ",", sep = ";")



  # expVar <- strsplit(input$exp_var, ",")[[1]]

  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }


  expVar <- variable_data_options$discretize_exp_variables
  if (length(expVar) != 0) {
    fire_running("Discretizing experimental continuous variables")
    print("Discretizing experimental continuous variables", quote = FALSE)
    bn_df_variables <- discretize_data_variables(
      bn_df_variables,
      expVar,
      variable_data_options$dismethod
    )
  }

  ## should be done per request not all data variable should be transformed to log
  for (i in 1:ncol(bn_df_variables)) {
    if (class(bn_df_variables[, i]) == "numeric") {
      sp <- shapiro.test(bn_df_variables[, i])
      if (sp$p.value < 0.05) {
        bn_df_variables[, i] <- log1p(bn_df_variables[, i])
      }
    }
  }

  # bn_df_variables[bn_df_variables=="-Inf"]<- -1000

  # data_raw <- cbind(bn_df_variables, bn_df_taxas)
  # bn_df_raw <- as.data.frame(data_raw)

  # rm(data_raw)

  # dis_exp_variables <- bn_df_variables %>% select_if(is.factor)
  # con_exp_variables <- bn_df_variables %>% select_if(is.numeric)

  fire_running("Creating model and training datasets")
  print("Creating model and training datasets", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }

  # combinations_var <- distinct(dis_exp_variables)

  fire_running("Normalizing taxa raw counts")
  print("Normalizing taxa raw counts", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }

  # bn_df_taxas_norm <- bn_df_taxas

  bn_df_taxas.col_sum <- colSums(bn_df_taxas)
  bn_df_taxas.row_sum <- rowSums(bn_df_taxas)

  bn_df_taxas_norm <- nomralize_data(
    bn_df_taxas,
    bn_df_taxas.col_sum,
    bn_df_taxas.row_sum
  )

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

  bn_df_taxas_norm_log <- to_log(bn_df_taxas_norm, colnames(bn_df_taxas_norm))

  output_log <- file.path(net_dir, "taxa_norm_log_counts.csv")
  write.table(bn_df_taxas_norm_log, file = output_log, dec = ",", sep = ";")


  if (length(to_remove) > 0) {
    bn_df_taxas <- bn_df_taxas[, -to_remove]
    bn_df_taxas_norm <- bn_df_taxas_norm[, -to_remove]
    bn_df_taxas_norm_log <- bn_df_taxas_norm_log[, -to_remove]
  }

  # bn_df_model <- cbind(bn_df_variables[rownames(data_model),],bn_df_taxas_norm[rownames(data_model),])
  # data_model <- bn_df_model
  # bn_df_training <- cbind(bn_df_variables[rownames(data_training),],bn_df_taxas_norm[rownames(data_training),])
  # data_training <- bn_df_training

  bn_df_norm <- cbind(bn_df_variables, bn_df_taxas_norm_log)

  fire_running("Creating network model")
  print("Creating network model", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }



  ## this need checking
  netscore.g <- tolower(paste(network_build_option$netscore, "-CG", sep = ""))
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

  result <- hc(
    bn_df_norm,
    optimized = TRUE,
    score = netscore.g,
    whitelist = network_build_option$wl,
    blacklist = network_build_option$bl
  )


  fire_running("Network model done! Filtering model by link strength")
  print("Network model done! Filtering model by link strength", quote = FALSE)
  if (interrupted()) {
    print("Stopping...", quote = FALSE)
    stop("User Interrupt")
  }

  remove_arcs <- data.frame(matrix(data = NA, nrow = 0, ncol = 2))
  colnames(remove_arcs) <- c("from", "to")
  result_filt <- result

  arc_st_bic <- arc.strength(result, bn_df_norm, criterion = "bic-cg")
  arc_st_mi <- arc.strength(result, bn_df_norm, criterion = "mi-cg")

  n <- 0

  for (l in 1:nrow(arc_st_bic)) {
    if ((arc_st_bic[l, 3] < network_build_option$thr_bic) && (arc_st_mi[l, 3] < network_build_option$thr_mi)) {
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
      comparison <- compare::compare(d, network_build_option$wl, allowAll = TRUE)
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

  out_net_name <- paste(
    format(Sys.time(), "%F_%H.%M.%S"),
    "_complete_network.RData",
    sep = ""
  )
  output_file_net <- file.path(net_dir, out_net_name)
  save(list = ls(), file = output_file_net, envir = environment())

  # FILTRADO POR PORCENTAJE AFTER

  ## after filters can be function with the app
  ## after loading the network to filter more nodes
  if (!is.null(taxa_count_filters) &&
    taxa_count_filters$filterBA == "After") {
    if (taxa_count_filters$filter_option == "Total") {
      filterThrT_sep <- strsplit(taxa_count_filters$filterThrT, ",")
      for (thr in filterThrT_sep[[1]]) {
        print(thr)
        to_remove <- c()
        min_samples <- round(nrow(bn_df_taxas) * as.numeric(thr) / 100)
        for (i in seq_len(ncol(bn_df_taxas))) {
          counts <- sum(as.numeric(bn_df_taxas[, i]) >=
            as.numeric(taxa_count_filters$filterCountsT))
          if (counts < min_samples) {
            to_remove <- c(to_remove, i)
          }
        }
        to_remove <- unique(to_remove)
        output_to_remove <- file.path(
          net_dir,
          paste(
            "taxa_to_be_removed_by_filter_",
            thr,
            ".csv",
            sep = ""
          )
        )
        write.table(
          bn_df_taxas,
          file = output_to_remove,
          dec = ",",
          sep = ";"
        )

        result_removed <- result_filt
        for (n in to_remove) {
          result_removed <- remove.node(
            result_removed,
            colnames(bn_df_taxas)[[n]]
          )
        }
        result1 <- result
        bn_df_norm1 <- bn_df_norm
        bn_df_norm_removed <- subset(
          bn_df_norm,
          select = nodes(result_removed)
        )
        output_kept <- file.path(
          net_dir,
          paste("kept_taxa_by_", thr, "_filter.csv", sep = "")
        )
        write.table(
          bn_df_norm_removed,
          file = output_kept,
          dec = ",",
          sep = ";"
        )
        fittedbn <- bn.fit(
          result_removed,
          data = bn_df_norm_removed,
          replace.unidentifiable = TRUE
        )
        out_net_name <- paste(
          format(
            Sys.time(),
            "%F_%H.%M.%S"
          ),
          "_",
          thr,
          "_network.RData",
          sep = ""
        )
        output_file_net <- file.path(net_dir, out_net_name)
        save(list = ls(), file = output_file_net, envir = environment())
        result <- result1
        bn_df_norm <- bn_df_norm1
      }
    } else if (taxa_count_filters$filterOption == "Group") {
      columns_var <- ncol(bn_df_variables)
      df_complete <- cbind(bn_df_variables, bn_df_taxas)
      filterThrG <- stringr::str_replace_all(
        taxa_count_filters$filterThrG,
        c("/" = ".", " " = ".")
      )
      filterThrG_sep <- strsplit(taxa_count_filters$filterThrG, ",")
      filterThrG_sep2 <- c()
      for (i in filterThrG_sep) {
        sp <- strsplit(i, "-")
        filterThrG_sep2 <- c(filterThrG_sep2, sp)
      }
      filterVariable <- stringr::str_replace_all(
        taxa_count_filters$filterVariable,
        c("/" = ".", " " = ".", "-" = ".")
      )
      df_divided <- split(df_complete, df_complete[[filterVariable]])
      col_tax <- ncol(bn_df_variables) + 1
      for (i in filterThrG_sep2) {
        filt <- df_divided[[i[1]]]
        min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
        for (j in col_tax:ncol(df_complete)) {
          counts <- sum(as.numeric(
            filt[, j]
          ) >= as.numeric(
            taxa_count_filters$filterCountsG
          ))
          if (counts < min_samples) {
            to_remove <- c(to_remove, as.numeric(j) - columns_var)
          }
        }
      }
      result1 <- result
      bn_df_norm1 <- bn_df_norm
      to_remove <- unique(to_remove)
      # bn_df_taxas <- bn_df_taxas[, -to_remove]
      output_to_remove <- file.path(
        net_dir,
        "taxa_to_be_removed_by_group_filter.csv"
      )
      write.table(
        bn_df_taxas,
        file = output_to_remove,
        dec = ",",
        sep = ";"
      )
      result_removed <- result
      for (n in to_remove) {
        result_removed <- remove.node(
          result_removed,
          colnames(bn_df_taxas)[[n]]
        )
      }
      bn_df_norm_removed <- subset(
        bn_df_norm,
        select = nodes(result_removed)
      )
      output_kept <- file.path(net_dir, "kept_taxa_by_group_filter.csv")
      write.table(
        bn_df_norm_removed,
        file = output_kept,
        dec = ",",
        sep = ";"
      )
      fittedbn <- bn.fit(
        result_removed,
        data = bn_df_norm_removed,
        replace.unidentifiable = TRUE
      )
      result <- result_removed
      bn_df_norm <- bn_df_norm_removed
      out_net_name <- paste(
        format(Sys.time(), "%F_%H.%M.%S"),
        "_filtGroup_network.RData",
        sep = ""
      )
      output_file_net <- file.path(net_dir, out_net_name)
      save(list = ls(), file = output_file_net, envir = environment())
      result <- result1
      bn_df_norm <- bn_df_norm1
    }
  }

  fire_running("DONE!")
  print("DONE!", quote = FALSE)
  sink()
}