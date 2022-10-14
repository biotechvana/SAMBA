biocmanag <- "BiocManager"
lapply(biocmanag, function(x) if(!require(x,character.only = TRUE)) install.packages(x, dependencies = TRUE))

list_of_packages = c("shiny","shinydashboard","shinydashboardPlus","shinyFiles","bnlearn","shinyjs","DT","data.table","bnviewer","visNetwork","stringr","shinythemes", "purrr", "seqinr","future","ipc","assertr","promises","dplyr","callr", "parallel","this.path","progressr","compare","future.callr","graph","igraph","zip","tibble","tibble", "fresh", "colourpicker", "shinyBS","shinyalert","shinyjqui" )
lapply(list_of_packages, function(x) if(!require(x,character.only = TRUE)) BiocManager::install(x, dependencies = TRUE))

if (!require("reticulate")) remotes::install_github("rstudio/reticulate")
if (!require("reticulate")) install.packages("reticulate")
if (!require("shinyDirectoryInput")) remotes::install_github("wleepang/shiny-directory-input")
if (!require("rethinking")) devtools::install_github("rmcelreath/rethinking@slim")
if (!require("hdrcde")) install.packages("hdrcde")
