## do not source this here just run the script for each change
# source('dep_check.R')

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

###############################################################################################
source('mod-evidences.R')