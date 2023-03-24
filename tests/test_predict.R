file <- "/home/ahafez/Downloads/wetransfer_zinb-filt-diet_2023-03-10_1523/ZINB FILT DIET/2023-02-22_14.44.34_complete_network.RData"
result_env <- readRDS(file)
shared_session_info <- list()
#set_shared_session_info(result_env)
res<-result_env
shared_session_info$build_env <- result_env
session_data <- shared_session_info
local_data<-list()
if (!is.null(session_data$bn_df_variables)) {
  variables <- session_data$bn_df_variables[, sapply(session_data$bn_df_variables, class) == "factor"]
  var_list <- list()
  for (i in 1:ncol(variables)) {
    var_list[[colnames(variables)[i]]] <- levels(variables[, i])
  }
  local_data$var_list <- var_list
} else {
  local_data$var_list <- c()
}
input <- list()
input$evidence1 <- c("EGG.HYDRO")
input$evidence1 <- c("GAIN_PreChallenge")
var_list <- local_data$var_list

data_as_strong_proir <- TRUE
target_nodes <- colnames(session_data$bn_df_taxas)
simple_table <- FALSE
df <- custom_fit_prediction(input_evidence, c("T439"), FALSE, TRUE)

incProgress <- function(x, detail = "Sampling ..."){
  
}





##### Test Filters
taxa_count_filters <- list (
  filterBA = "After",
  filter_option = "Total",
  filterThrT = "25,50", # Select global filter threshold
  filterCountsT = 10 # Specify a minimum number of counts to apply this filter
)

taxa_count_filters <- list (
  filterBA = "After",
  filter_option = "Group",
  filterThrG =  "25,25,25",# Select filter threshold for each variable condition
  filterVariable = "Experiments",
  filterCountsG = 10 # "Specify a minimum number of counts to apply this filter"
)

