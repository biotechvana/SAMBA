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
all_roots <- function(f, interval,
                      lower = min(interval), upper = max(interval), n = 100L, ...) {
  x <- seq(lower, upper, len = n + 1L)
  fx <- f(x, ...)
  roots <- x[which(fx == 0)]
  fx2 <- fx[seq(n)] * fx[seq(2L, n + 1L, by = 1L)]
  index <- which(fx2 < 0)
  for (i in index) {
    roots <- c(roots, uniroot(f, lower = x[i], upper = x[i + 1L], ...)$root)
  }
  return(sort(roots))
}

calc.falpha <- function(x = NULL, den, alpha, nn = 5000) {
  # Calculates falpha needed to compute HDR of density den.
  # Also finds approximate mode.
  # Input: den = density on grid.
  #          x = independent observations on den
  #      alpha = level of HDR
  # Called by hdr.box and hdr.conf

  if (is.null(x)) {
    calc.falpha(x = sample(den$x, nn, replace = TRUE, prob = den$y), den, alpha)
  } else {
    fx <- approx(den$x, den$y, xout = x, rule = 2)$y
    falpha <- quantile(sort(fx), alpha)
    mode <- den$x[den$y == max(den$y)]
    return(list(falpha = falpha, mode = mode, fx = fx))
  }
}

hdr.ends <- function(den, falpha) {
  miss <- is.na(den$x) # | is.na(den$y)
  den$x <- den$x[!miss]
  den$y <- den$y[!miss]
  n <- length(den$x)
  # falpha is above the density, so the HDR does not exist
  if (falpha > max(den$y)) {
    return(list(falpha = falpha, hdr = NA))
  }
  f <- function(x, den, falpha) {
    approx(den$x, den$y - falpha, xout = x)$y
  }
  intercept <- all_roots(f, interval = range(den$x), den = den, falpha = falpha)
  ni <- length(intercept)
  # No roots -- use the whole line
  if (ni == 0L) {
    intercept <- c(den$x[1], den$x[n])
  } else {
    # Check behaviour outside the smallest and largest intercepts
    if (f(0.5 * (head(intercept, 1) + den$x[1]), den, falpha) > 0) {
      intercept <- c(den$x[1], intercept)
    }
    if (f(0.5 * (tail(intercept, 1) + den$x[n]), den, falpha) > 0) {
      intercept <- c(intercept, den$x[n])
    }
  }
  # Check behaviour -- not sure if we need this now
  if (length(intercept) %% 2) {
    warning("Some HDRs are incomplete")
  }
  #  intercept <- sort(unique(intercept))
  return(list(falpha = falpha, hdr = intercept))
}
BoxCox <- function(x, lambda) {
  if (is.list(x)) {
    x <- x[[1]]
  }
  if (lambda == 0) {
    log(x)
  } else {
    (x^lambda - 1) / lambda
  }
}
InvBoxCox <- function(x, lambda) {
  if (is.list(x)) {
    x <- x[[1]]
  }
  if (lambda == 0) {
    exp(x)
  } else {
    (x * lambda + 1)^(1 / lambda)
  }
}
tdensity <- function(x, bw = "SJ", lambda = 1) {
  if (is.list(x)) {
    x <- x[[1]]
  }
  if (lambda == 1) {
    return(density(x, bw = bw, n = 1001))
  } else if (lambda < 0 | lambda > 1) {
    stop("lambda must be in [0,1]")
  }
  # Proceed with a Box-Cox transformed density
  y <- BoxCox(x, lambda)
  g <- density(y, bw = bw, n = 1001)
  j <- g$x > 0.1 - 1 / lambda # Stay away from the edge
  g$y <- g$y[j]
  g$x <- g$x[j]
  xgrid <- InvBoxCox(g$x, lambda) # x
  g$y <- c(0, g$y * xgrid^(lambda - 1))
  g$x <- c(0, xgrid)
  return(g)
}

c_hdr <- function(x = NULL, prob = c(50, 95, 99), den = NULL, lambda = 1, nn = 5000, all.modes = FALSE) {
  if (!is.null(x)) {
    r <- diff(range(x))
    if (r == 0) {
      stop("Insufficient data")
    }
  }
  if (is.null(den)) {
    den <- tdensity(x, lambda = lambda)
  }
  alpha <- sort(1 - prob / 100)
  falpha <- calc.falpha(x, den, alpha, nn = nn)
  hdr.store <- matrix(NA, length(alpha), 100)
  for (i in 1:length(alpha)) {
    junk <- hdr.ends(den, falpha$falpha[i])$hdr
    if (length(junk) > 100) {
      junk <- junk[1:100]
      warning("Too many sub-intervals. Only the first 50 returned.")
    }
    hdr.store[i, ] <- c(junk, rep(NA, 100 - length(junk)))
  }
  cj <- colSums(is.na(hdr.store))
  hdr.store <- matrix(hdr.store[, cj < nrow(hdr.store)], nrow = length(prob))
  rownames(hdr.store) <- paste(100 * (1 - alpha), "%", sep = "")
  if (all.modes) {
    y <- c(0, den$y, 0)
    n <- length(y)
    idx <- ((y[2:(n - 1)] > y[1:(n - 2)]) & (y[2:(n - 1)] >
      y[3:n])) | (den$y == max(den$y))
    mode <- den$x[idx]
  } else {
    mode <- falpha$mode
  }
  return(list(hdr = hdr.store, mode = mode, falpha = falpha$falpha))
}



######################   Network methods ############################
VAR_TYPE_NOMINAL <- "Nominal"
VAR_TYPE_NUMERIC <- "Numeric"

VAR_ROLE_EXP_CONR <- "Exp Control"
VAR_ROLE_EXP_OBSERVATION <- "Observed"
VAR_ROLE_EXP_OUTCOME <- "Outcome"

VAR_ROLES <- c(VAR_ROLE_EXP_CONR,VAR_ROLE_EXP_OBSERVATION,VAR_ROLE_EXP_OUTCOME)

generate_variables_summary <- function(bn_df_variables) {
  ## get all variable name and data type
  variables_name <- colnames(bn_df_variables)

  #nominal_variables <- bn_df_variables %>% select_if(is.factor)
  #numeric_variables <- bn_df_variables %>% select_if(is.numeric)
  variables_cls <-  sapply(bn_df_variables, class)
  vars_desc <- list()
  for (var in variables_name) {
    if(variables_cls[var] == "factor") {
      vars_desc[[var]]$type <- VAR_TYPE_NOMINAL
      vars_desc[[var]]$var_values <- dplyr::n_distinct(bn_df_variables[[var]])
      vars_desc[[var]]$max_value <- 0
      vars_desc[[var]]$min_value <- 0
    }
    if(variables_cls[var] == "numeric") {
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
  res_df <- data.frame(matrix(unlist(vars_desc), nrow=length(vars_desc), byrow=TRUE))

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
                              HPDI_correct = TRUE) {
  ## TODO :: check on samples.w
  require(Hmisc)
  require(rethinking)
  network.samples.values <- network.sampling(fittedbn, node, evidence, n.samples = n.samples, min_weight = samples_min_weight)
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
    max_range <- max(max(network_samples), max(proir_data)) + 1
    proir_data_w <- attr(proir_data, "weights")
    if (!is.null(proir_data_w)) {
      if (sum(proir_data_w) != 1) proir_data_w <- proir_data_w / sum(proir_data_w)
    }


    network_samples_w <- attr(network_samples, "weights")
    if (!is.null(network_samples_w)) {
      if (sum(network_samples_w) != 1) network_samples_w <- network_samples_w / sum(network_samples_w)
    }


    dd_proir <- density(proir_data, adjust = adjust_proir, from = 0, t = max_range, n = 1000, weights = proir_data_w)
    dd_samples <- density(network_samples, adjust = adjust_samples, from = 0, t = max_range, n = 1000, weights = network_samples_w)
    ## TODO :: add uniform proir to the org data
    if (!is.null(uniform_proir)) dd_proir$y <- dd_proir$y + uniform_proir

    dd_proir_p <- dd_proir$y
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

  # posterior_dist$posterior_mean = wtd.mean(link(posterior_dist$data_value),posterior_dist$posterior_w)
  posterior_dist$posterior_mean <- round(wtd.mean(transfromed_data, posterior_dist$posterior_w))
  posterior_dist$posterior_sd <- round(sqrt(wtd.var(transfromed_data, posterior_dist$posterior_w)), 2)
  posterior_dist$posterior_quantile <- round(wtd.quantile(transfromed_data, posterior_dist$posterior_w))


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
  # # browser()
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
  paste(node, "~", model_str, "|", zero_infl_model)
}

bn_score_model_formula <<- zero_infl_model_formula

calculate_Offset <- function(taxa_data, ...) {
  rowSums(taxa_data)
}

BN_CUSTOM_SCORE_ZINB <<- "BIC-ZINB"
BN_CUSTOM_SCORE_BIC <<- "BIC-Log-Normal"
BN_CUSTOM_SCORE_AIC  <<-  "AIC-Log-Normal"
BN_CUSTOM_SCORE_loglik <<-  "loglik-Log-Normal"
custom.glm.bic <- function(node, parents, data, args) {
  ###########################
  data <- args$org_data
  create_model_formula <- args$create_model_formula
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
        create_model_formula(node, parents, data, "offset(log(Offset))")
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
        return(-INF_VALUE)
      }
      model_sammary <- summary(glm.model)

      return((-BIC(glm.model) / 2) * mean(c(model_sammary$coefficients$count[, 4], model_sammary$coefficients$zero[, 4])))
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
      return(-INF_VALUE)
    } else {
      dummy.dag <- empty.graph(c(node, parents))
      for (par in parents) {
        dummy.dag <- set.arc(dummy.dag, from = par, to = node)
      }
      return.score <- try(score(dummy.dag, data[, c(node, parents), drop = FALSE], by.node = TRUE)[node])
      if (class(return.score) == "try-error") {
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


bn.fit.custom.fit <- function(bn_data, bn_dag, bn_fit, nodes_to_fit ) {
  take_aways_nodes <- setdiff(colnames(bn_data),c(nodes_to_fit,"Offset"))
  custom_fit <- list()
  for(node in take_aways_nodes) {
    custom_fit[[node]] <- bn_fit[[node]]
  }
  for(node in nodes_to_fit) {
    bn_dag$nodes[[node]]$parents
    model_str <-
      bn_score_model_formula(node, bn_dag$nodes[[node]]$parents,bn_data, "offset(log(Offset))")
    final__model <<- as.formula(model_str)
    custom_fit[[node]] <- pscl::zeroinfl(final__model,
                   data = bn_data,
                   dist = 'negbin')
  }
  custom_fit
}

get.sampling.path <- function(bn_dag,target_node = NULL  ) {
  final_path <- c()
  if(is.null(target_node)) {
    all_nodes <- bnlearn::nodes(bn_dag)
    while(length(all_nodes) > 0) {
      for(node in all_nodes) {
        node.parents <- bnlearn::parents(bn_dag,node)
        if(length(node.parents) == 0){
          final_path <- c(final_path,node)
        } else if ( sum(node.parents %in% final_path) == length(node.parents) ) {
          final_path <- c(final_path,node)
        }
      }
      all_nodes <- setdiff(all_nodes,final_path)
    }
  } else {
    node.parents <- bnlearn::parents(bn_dag,target_node) 
    for (p_node in node.parents) {
      final_path <- union(final_path, sampleing.path(bn_dag,p_node))
    }
    final_path <- union(final_path,node.parents)
    final_path <- c(final_path,target_node)
  }
  final_path
}


get_samples_all <- function(custom_fit,bn_fit,input_evidence, sampling.path, observed_variables, average.offset, nSamples = 100000 , incProgress = NULL ) {
  ## 
  cp_observed_variables <- intersect(observed_variables,sampling.path)
  cp_observed_variables <- union(cp_observed_variables,names(input_evidence))
  ## this could be all diff com
  init_samples <- cpdist(bn_fit, nodes = cp_observed_variables, evidence = input_evidence,method = "lw",n=nSamples)
  ## what is a better way to get the init samples ??
  #start_samples <- cbind(start_samples,input_evidence)
  
  if(length(average.offset) > 1) {
    init_samples$Offset <- sample(average.offset,nrow(init_samples),replace = TRUE)
  } else {
    init_samples$Offset <- average.offset
  }
  
  
  init_samples_EX <- init_samples
  for(node in sampling.path) {
    if(!is.null(incProgress)) {
      incProgress(1 / length(sampling.path), detail = "Sampleing ...")
    }
    if(node %in% observed_variables) {
      ## ignore
      next
    }
    node_model <- custom_fit[[node]]
    
    ## capture varitains in the data
    zero_comp <- predict(node_model,init_samples,type="zero")
    count_comp <- predict(node_model,init_samples,type="count")
    final_counts  <- sapply(1:length(count_comp), function(i) {
      ZIM::rzinb(1,k = node_model$theta, lambda = count_comp[i],zero_comp[i])
    } )
    init_samples[[node]] <- final_counts
    
    #start_samples[[node]] <- round( MASS::rnegbin(predict(node_model,start_samples),theta = node_model$theta ))
    init_samples_EX[[node]] <-  round( predict(node_model,init_samples_EX,type="response"))
  }
  #sub_samples <- filter_by_evidence(start_samples,input_evidence)
  
 list (
   all_sampels = init_samples,
   exp_counts = init_samples_EX
 )
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
  # netscore.g <- tolower(paste(network_build_option$netscore, "-CG", sep = ""))
  print(netscore.g)

  if (netscore.g == BN_CUSTOM_SCORE_ZINB) {
    result_env$input_bn_df_Offset <- cbind(
      result_env$bn_df_variables,
      Offset = calculate_Offset(result_env$bn_df_taxas),
      result_env$bn_df_taxas
    )

    result_env$input_bn_df <- cbind(
      result_env$bn_df_variables,
      result_env$bn_df_taxas
    )

    result_env$result <- hc(
      result_env$input_bn_df,
      score = "custom",
      fun = custom.glm.bic,
      restart = 1,
      optimized = TRUE,
      args = list(org_data = result_env$input_bn_df_Offset, create_model_formula = bn_score_model_formula),
      whitelist = network_build_option$wl,
      blacklist = network_build_option$bl
    )

    ## plug here filter logic
    ## TODO :: set all to 1
    result_env$arc_st_mi <- c() 
    result_env$arc_st_bic <- c()
    result_env$result_filt <- result_env$result



  } else {
    netscore.g <- "bic-cg"
    if(network_build_option$netscore == BN_CUSTOM_SCORE_BIC)
      netscore.g <- "bic-cg"
    if(network_build_option$netscore == BN_CUSTOM_SCORE_loglik)
      netscore.g <- "loglik-cg"
    if(network_build_option$netscore == BN_CUSTOM_SCORE_AIC)
      netscore.g <- "aic-cg"
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
    remove_arcs <- data.frame()
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

    if (length(remove_arcs) != 0) {
      for (i in 1:nrow(remove_arcs)) {
        d <- data.frame(from = remove_arcs[i, 1], to = remove_arcs[i, 2])
        comparison <- compare::compare(d, network_build_option$wl, allowAll = TRUE)
        if (isFALSE(comparison)) {
          result_env$result_filt <- drop.arc(result_env$result_filt, remove_arcs[i, 1], remove_arcs[i, 2])
        }
      }
    }

   

    strength_mi <- file.path(net_dir, "arc_strength_mi.txt")
    write.table(result_env$arc_st_mi, strength_mi, sep = "\t", dec = ",")

    strength_bic <- file.path(net_dir, "arc_strength_bic.txt")
    write.table(result_env$arc_st_bic, strength_bic, sep = "\t", dec = ",")
  }


    fire_running("Training model")
    print("Training model", quote = FALSE)
    if (interrupted()) {
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }

    result_env$fittedbn <- bn.fit(result_env$result_filt, data = result_env$bn_df_norm, replace.unidentifiable = TRUE)

    if (netscore.g == BN_CUSTOM_SCORE_ZINB) {
      nodes_to_fit <- colnames(result_env$bn_df_taxas)
      result_env$fittedbn_custom <- bn.fit.custom.fit(
                  result_env$input_bn_df_Offset,
                  result_env$result_filt,
                  result_env$fittedbn,
                  nodes_to_fit
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


  output_file_net <- file.path(net_dir, out_net_name)
  # save(list = ls(), file = output_file_net, envir = environment())
  saveRDS(result_env, file = output_file_net)
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

  remove_arcs <- data.frame()
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
