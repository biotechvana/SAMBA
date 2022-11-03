taxa_count_filters <- list (
  filterTaxa = 1, ## apply the filter
  filterThrG = "",
  filterCountsG = 10,
  filterThrT = input$filter_thrT,
  filterOption = "Group",
  filterVariable = input$filter_variable,
  filterCountsT = input$filter_countsT,

  filterBA = input$before_after_filter
)

taxa_count_filters_group <- list (
  filterThrG = "",
  filterCountsG = 10,
  filter_option = "Group",
  filterVariable = "Experiments",
  filterBA = "Before"
)
##############################################################
taxa_count_filters_total <- list (
  filter_option = "Total",
  filterCountsT = 1,
  filterThrT = 10,
  filterBA = "Before"
)


taxa_count_filters <- taxa_count_filters_total
final_res <- apply_taxa_before_filter(bn_df_taxas,bn_df_variables,taxa_count_filters_total)
taxa_count_filters <- NULL
to_remove <- c()
if( !is.null(taxa_count_filters) &&
    taxa_count_filters$filterBA == "Before") {
  ## apply filter
  to_remove <- apply_taxa_before_filter(
    bn_df_taxas,
    bn_df_variables,
    taxa_count_filters)
}

bn_df_taxas.col_sum <- colSums(result_env$bn_df_taxas)
bn_df_taxas.row_sum <- rowSums(result_env$bn_df_taxas)

bn_df_taxas_norm <- nomralize_data(
  result_env$bn_df_taxas,
  bn_df_taxas.col_sum,
  bn_df_taxas.row_sum
)


nomralize_data_tss <-
  function(bn_df_taxas,
           bn_df_taxas.col_sum,
           bn_df_taxas.row_sum) {
    sample.names <- rownames(bn_df_taxas)
    taxa.names <- colnames(bn_df_taxas)
    for (sample in sample.names) {
      for (taxa in taxa.names) {
        raw_value <- bn_df_taxas[sample, taxa]
        normalized.value <- raw_value / bn_df_taxas.row_sum[sample]
        bn_df_taxas[sample, taxa] <- normalized.value
      }
    }
    invisible(bn_df_taxas)
  }

#####################################################
result_env <- list()
result_env$orginal_bn_df_taxas <- bn_df_taxas
result_env$orginal_bn_df_variables <- bn_df_variables



result_list <- fitler_norm_count_data(
  result_env$orginal_bn_df_taxas,
  result_env$orginal_bn_df_variables,
  taxa_count_filters_total)

result_env$bn_df_taxas <- result_list$bn_df_taxas
result_env$bn_df_taxas_norm <- result_list$bn_df_taxas_norm
result_env$bn_df_taxas_norm_log <- result_list$bn_df_taxas_norm_log
result_env$to_remove <- result_list$to_remove




