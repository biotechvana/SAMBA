## do not source this here just run the script for each change
# source('dep_check.R')
################# For Jobs Control
jobs <- list()
# TODO :: save and load this list between app restarts
################# ##################
##############################################################################################

convert_ui_evidence_selection <- function(var_list, evidence_selection) {
    input_evidence <- list()

    for (i in evidence_selection) {
        res <- lapply(var_list, function(x) match(i, x))
        for (j in 1:length(res)) {
            if (!is.na(res[j])) {
                input_evidence[[names(res[j])]] <- i
            }
        }
    }
    input_evidence
}

filter_by_evidence <- function (bn_df_input,l_var){
  filt_data <- bn_df_input
  for (v in 1:length(l_var)) {
    filt_data <- filt_data[filt_data[[names(l_var[v])]] == l_var[[v]][1], ]
  }
  filt_data
}
library(shinycssloaders)



###############################################################################################
source("network_functions.R")
source('build_network-module.R')
source('mod-evidences.R')
source('mod-cpts.R')
source('mod-dags.R')