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
pickerInput_select <- "
.bs-select-all-disable .bs-select-all {
  display: none;
}"

pickerInput_deselect <- "
.bs-select-all-disable .bs-deselect-all {
  width: 100%;
}
"




options(max.print=999999)

# biocmanag <- "BiocManager"
# lapply(biocmanag, function(x) if(!require(x,character.only = TRUE)) install.packages(x, dependencies = TRUE))

# list_of_packages = c("shiny","shinydashboard","shinydashboardPlus","shinyFiles","bnlearn","shinyjs","DT","data.table","bnviewer","visNetwork","stringr","shinythemes", "purrr", "seqinr","future","ipc","assertr","promises","dplyr","callr", "parallel","this.path","progressr","compare","future.callr","graph","igraph","zip","tibble","tibble", "fresh", "colourpicker", "shinyBS","shinyalert","shinyjqui" )
# lapply(list_of_packages, function(x) if(!require(x,character.only = TRUE)) BiocManager::install(x, dependencies = TRUE))

# if (!require("reticulate")) remotes::install_github("rstudio/reticulate")
# if (!require("reticulate")) install.packages("reticulate")
# if (!require("shinyDirectoryInput")) remotes::install_github("wleepang/shiny-directory-input")


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFiles)
library(shinyWidgets)
library(bnlearn)
library(shinyjs)
library(dplyr)
library(DT)
library(data.table)
library(bnviewer)
library(visNetwork)
library(stringr)
library(shinythemes)
library(shinyDirectoryInput)
library(reticulate)
library(purrr)
library(seqinr)
library(future)
library(ipc)
library(promises)
library(progressr)
library(future.callr)
library(parallel)
library(this.path)
library(compare)
library(graph)
library(igraph)
library(zip)
library(plyr)
# #
library(fresh)
library(colourpicker)
library(shinyBS)
library(shinyalert)
library(shinyjqui)

#cl <- parallel::makeCluster(2, setup_strategy = "sequential")
#if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
#    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
#  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
#}

## debug options
source("configs.R", local = TRUE)
options(shiny.fullstacktrace = TRUE)


## overwrite for testing
# options(shiny.error = browser)


###############################

# #
DT <- list( 
  size = 25, 
  width = 1,
  sel_width = 2,
  dashes = FALSE,
  direction = TRUE,
  bounce = FALSE,
  E_hidden = FALSE,
  opacity = 1,
  
  fix_x = FALSE,
  fix_y = FALSE,
  # Scaling
  scaling_min = 1,
  scaling_max = 1,
  scaling_label_enabled = FALSE,
  scaling_label_min = 14,
  scaling_label_max = 14,
  
  # Color
  color_background = "#84B8BD",
  color_border = "#616161",
  color_highlight = "#177782",
  shape = "dot", 
  
  # Font
  show_Nlabel = TRUE,
  show_Elabel = FALSE,
  font_color = "#343434",
  font_size = 14,
  font_face = "arial",
  font_background = NULL,
  font_strokeWidth = 1,
  font_strokeColor = "#ffffff",
  N_font_align = "center",
  E_font_align = "horizontal",
  
  # Shadow
  shadow_enabled = FALSE,
  shadow_color = "#FFFFFF",
  shadow_size = 10,
  shadow_x = 5,
  shadow_y = 5,
  st = " position: absolute; 
  top: 25px;
  text-align: center;
  background-color: white;
  color: black;
  border: 1px solid #e7e7e7;
  border-radius: 6px;
  padding: 8px 12px;
  ",
  
  # Otros
  # Design
  slider_color = "#95979A",
  digits = 3,
  # Grado para la seleccion
  grade = 0)

Ord_nod_tab <- NULL
Ord_edg_tab <- NULL

nodes <- NULL
edges <- NULL

#Rep <- 0
Sel_nod <- NULL
nodes_info <- NULL
edges_info <- NULL


#Groups <<- list(All = nodes$id, None = NULL)

# #
## put version info for later build options
samba_version <<- "1.0.0"

plan(multisession, workers = 12)

options(shiny.maxRequestSize=100*1024^2)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))



# pickerInput_select <- "
# .bs-select-all {
#   display: none;
# }"

# pickerInput_deselect <- "
# .bs-deselect-all {
#   width: 100%;
# }
# "


shiny::onStop(function() {
  cat("Doing application cleanup\n")
  output_job_db_file <- file.path(deploy_data, ".jobs.db.rdata")
  # save(list = ls(), file = output_file_net, envir = environment())
   cat("Saving Jobs ...\n")
  saveRDS(jobs, file = output_job_db_file)
})
cat("Loading Jobs List\n")
tryCatch(
  {
    output_job_db_file <- file.path(deploy_data, ".jobs.db.rdata")
    jobs <<- readRDS(output_job_db_file)
  },
  error = function(cond) {
    cat("Can not load Jobs List\n")
    print(cond)
    jobs <<- list()
  }
)



LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

css <- HTML(".margin-right{float: right !important;}")

js <- HTML("$(function(){
        setTimeout(function(){
           $('.dataTables_length').addClass('margin-right');
           }, 200);
           });")

shiny_handler <- make_progression_handler("shiny", reporter = list(
  update = function(config, state, progression, ...) {
    incProgress(progression$amount / config$max_steps)
  }
))

################################################################################################

debug_msg <- function(...) {
  if(exists("DEBUG") && DEBUG ) {
    is_local <- Sys.getenv('SHINY_PORT') == ""
    in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
    txt <- toString(list(...))
    if (is_local) message(txt)
    if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
  }
}

################################################################################################

source("network_functions.R")
source('build_network-module.R')
source('load_network-module.R')
source('prediction-module.R')
source('mod-evidences.R')
source('mod-cpts.R')
source('mod-dags.R')
source('mod-network-viewer.R')
source('download_result-module.R')