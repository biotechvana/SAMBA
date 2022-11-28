######################## TEST CASE 
count_file = "/home/data/bio/projects/bn_network/PROGENSA/PROGENSA/new_input/example_data_taxas_EWH_LSAQUA_GAINPre2.csv"
var_file <- "/home/data/bio/projects/bn_network/PROGENSA/PROGENSA/new_input/example_data_variables_EWH_LSAQUA_GAINPre2.csv"



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
  filterCountsT = 20,
  filterThrT = 20,
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
    taxa_count_filters_total)
}

bn_df_taxas.col_sum <- colSums(result_env$bn_df_taxas)
bn_df_taxas.row_sum <- rowSums(result_env$bn_df_taxas)

bn_df_taxas_norm <- nomralize_data(
  result_env$bn_df_taxas,
  bn_df_taxas.col_sum,
  bn_df_taxas.row_sum
)
bn_df_taxas_norm_tss <- nomralize_data_tss(
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
result_env$bn_df_variables <- bn_df_variables

result_list_no_filters <- result_env
result_list <- fitler_norm_count_data(
  result_env$orginal_bn_df_taxas,
  result_env$orginal_bn_df_variables,NULL)

result_list_no_filters$bn_df_taxas <- result_list$bn_df_taxas
result_list_no_filters$bn_df_taxas_norm <- result_list$bn_df_taxas_norm
result_list_no_filters$bn_df_taxas_norm_log <- result_list$bn_df_taxas_norm_log
result_list_no_filters$to_remove <- result_list$to_remove



result_list <- fitler_norm_count_data(
  result_env$orginal_bn_df_taxas,
  result_env$orginal_bn_df_variables,
  taxa_count_filters_total)




result_env$bn_df_taxas <- result_list$bn_df_taxas
result_env$bn_df_taxas_norm <- result_list$bn_df_taxas_norm
result_env$bn_df_taxas_norm_log <- result_list$bn_df_taxas_norm_log
result_env$to_remove <- result_list$to_remove


################################################################################



css_normalize <- function(result_list){
  metaSeqObject      = newMRexperiment(t(result_list$bn_df_taxas)) 
  metaSeqObject_CSS  = cumNorm( metaSeqObject , p=cumNormStatFast(metaSeqObject) )
  OTU_read_count_CSS = data.frame(MRcounts(metaSeqObject_CSS, norm=TRUE, log=FALSE))
  result_list$bn_df_taxas_norm <- as.data.frame(t(OTU_read_count_CSS))
  result_list$bn_df_taxas_norm_log <- log1p(result_list$bn_df_taxas_norm)
  
  if (length(result_list$to_remove) > 0) {
    result_list$bn_df_taxas <- result_list$bn_df_taxas[, -result_list$to_remove]
    result_list$bn_df_taxas_norm <- result_list$bn_df_taxas_norm[, -result_list$to_remove]
    result_list$bn_df_taxas_norm_log <- result_list$bn_df_taxas_norm_log[, -result_list$to_remove]
  }
  result_list
}
result_env_css <- css_normalize(result_env)

TT = "T470"
cor(result_env_css$bn_df_taxas[TT],result_env_css$bn_df_taxas_norm [TT])
cor(result_env$bn_df_taxas[TT],result_env$bn_df_taxas_norm[TT])


################################################################################

library(tictoc)
t_list <- result_env
start_nNodes <- ncol(t_list$orginal_bn_df_variables) 
bn_df_norm <- cbind(
  t_list$bn_df_variables,
  t_list$bn_df_taxas_norm_log
)
end_nNodes <- round(ncol(bn_df_norm)/4 )
n_steps <- 10
times_hc <- c()
for( n in seq(start_nNodes,end_nNodes,n_steps) ) {
  tic(paste("HC for ", n) )
  hc(bn_df_norm[,1:n],optimized = TRUE)
  x <- toc()
  times_hc <- c(times_hc,x)
}

times_tabu <- c()
for( n in seq(start_nNodes,end_nNodes,n_steps) ) {
  tic(paste("HC for ", n) )
  tabu(bn_df_norm[,1:n])
  x <- toc()
  times_tabu <- c(times_tabu,x)
}



times_h2pc <- c()
for( n in seq(start_nNodes,end_nNodes,n_steps) ) {
  tic(paste("HC for ", n) )
  h2pc(bn_df_norm[,1:n],maximize.args = list(optimized = TRUE))
  x <- toc()
  times_h2pc <- c(times_h2pc,x)
}

res_1 <- h2pc(bn_df_norm[,1:n],maximize.args = list(optimized = TRUE))
plot(res_1)

