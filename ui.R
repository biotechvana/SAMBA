#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(max.print=999999)

biocmanag <- "BiocManager"
lapply(biocmanag, function(x) if(!require(x,character.only = TRUE)) install.packages(x, dependencies = TRUE))

list_of_packages = c("shiny","shinydashboard","shinydashboardPlus","shinyFiles","bnlearn","shinyjs","DT","data.table","bnviewer","visNetwork","stringr","shinythemes", "purrr", "seqinr","future","ipc","assertr","promises","dplyr","callr", "parallel","this.path","progressr","compare","future.callr","graph","igraph","zip","tibble")
lapply(list_of_packages, function(x) if(!require(x,character.only = TRUE)) BiocManager::install(x, dependencies = TRUE))

if (!require("reticulate")) remotes::install_github("rstudio/reticulate")
if (!require("reticulate")) install.packages("reticulate")
if (!require("shinyDirectoryInput")) remotes::install_github("wleepang/shiny-directory-input")


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

#cl <- parallel::makeCluster(2, setup_strategy = "sequential")
#if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
#    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
#  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
#}

plan(multisession, workers = 12)

options(shiny.maxRequestSize=100*1024^2)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


pickerInput_select <- "
.bs-select-all {
  display: none;
}"

pickerInput_deselect <- "
.bs-deselect-all {
  width: 100%;
}
"

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

to_log <- function(bn_df, nodes){
  bn_df.log = bn_df
  for (node in nodes) {
    bn_df.log[,node] = log1p(bn_df.log[,node])
  }
  bn_df.log
}

inverse_log <- function(bn_df.log, nodes){
  bn_df.invlog = bn_df.log
  for (node in nodes) {
    bn_df.invlog[,node] = exp1m(bn_df.invlog[,node])
  }
  bn_df.invlog
}

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("yeti"),
                 collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class = "active" href = "#"><b>SAMBA: Scanning Aquaculture Microbiome via Bayesian Approach</b></a>'),
                 id="nav",
                 windowTitle = HTML("<b>Metagenomic network</b>"),
                 tabPanel(HTML("<b>Learning and training</b>"),
                          sidebarPanel(width = 12,
                                       tags$label(h3('Input files, filters and parameters')),
                                       hr(),
                                       fileInput("data_variables", "CSV file for experimental variables", accept = ".csv"),
                                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_data_variables")),
                                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em",fileInput("data_taxas", "CSV file for taxa counts", accept = ".csv")),
                                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_data_taxas")),
                                       tags$style("#filter_taxa {height: 35px;}"),
                                       div(style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;",checkboxInput("filter_taxa", label = HTML('<h5 style="position: relative;">Filter taxa data by its presence in samples</h5>') , value = FALSE, width= NULL),
                                           conditionalPanel(
                                             condition = "input.filter_taxa == 1",
                                             pickerInput("filter_option", "Select Group to filter by a variables or Total to take into account all variables", choices = c("Total", "Group")),
                                             conditionalPanel(
                                               condition = "input.filter_option == 'Total'",
                                               numericInput("filter_countsT", "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
                                               div(style = "padding: 50 px 0 px; width: 100 px", textInput("filter_thrT", "Select global filter threshold", placeholder = "25,50"))
                                             ),
                                             conditionalPanel(
                                               condition = "input.filter_option == 'Group'",
                                               textInput("filter_variable", "Specify a variable name to apply this filter"),
                                               numericInput("filter_countsG", "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
                                               div(style = "padding: 50 px 0 px; width: 100 px", textInput("filter_thrG", "Select filter threshold for each variable condition", placeholder = "condition1-50,condition2-30..."))
                                             )
                                             
                                           )),
                                       div(style = "font-size: 10px; padding: 0px 0px;",textInput("exp_var", "Specify experimental variables to discretize", placeholder = "variableX,variableY")),
                                       selectInput("dis_method", label = "Select discretization method", choices = c("quantile", "interval", "hartemink"), selected = "quantile"),
                                       selectInput("net_score", label = "Network score", choices = c("AIC", "BIC", "loglik"), selected = "BIC"),
                                       numericInput("mi_thr", "Link strength: Mutual Information (MI) threshold", value = 0.05, min = 0, max = 50, step = 0.0005),
                                       numericInput("bic_thr", "Link strength: Bayesian Information Criterion (BIC) threshold", value = 0, min = -10000000, max = 10000000, step = 0.5),
                                       tags$style("#blacklist {height: 35px; width: 50 px;}"),
                                       div(style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;", checkboxInput("blacklist", label = HTML('<h5 style="position: relative; top: -50 px">Blacklist: Links not to be included in the network</h5>') , value = FALSE, width= NULL),
                                           conditionalPanel(
                                             condition = "input.blacklist == 1",
                                             fileInput("black_file", "Upload blacklist file"),
                                             div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_bl")),
                                           )),
                                       tags$style("#whitelist {height: 35px; width: 50 px;}"),
                                       div(style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;", checkboxInput("whitelist", label = HTML('<h5 style="position: relative; top: -50 px">Whitelist: Links to be included in the network</h5>') , value = FALSE, width= NULL),
                                           conditionalPanel(
                                             condition = "input.whitelist == 1",
                                             fileInput("white_file", "Upload whitelist file"),
                                             div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_wl"))
                                           )),
                                       directoryInput('directory_net', label = 'Select an output folder'),
                                       div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "start_net", label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
                                       div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "stop_net", label = "Stop process", style = "float", color = "primary", size = "sm", icon = icon("window-close"))),
                                       div(class = "buttonagency", style = "display:inline-block", actionBttn(inputId = "check_net", label = "Check status", style = "float", color = "primary", size = "sm", icon = icon("check-square")))
                          )),
                 
                 tabPanel(HTML("<b>Prediction and metagenomic inference</b>"),
                          selectInput("prediction_mode", "Select mode: ", choices = c("Automatic", "Customized"), selected = "Automatic"),
                          conditionalPanel(condition = "input.prediction_mode == 'Automatic'",
                                           sidebarPanel(width = 12,
                                                        tags$label(h3('Input files and parameters')),
                                                        hr(),
                                                        tags$style(".buttonagency .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7; margin-top:1em;}"),
                                                        fileInput("network_auto", "Network", accept = ".RData"),
                                                        div(style = "font-size: 10px; padding: 0px 0px; margin-top:-1em",numericInput("iterations_auto", "Iterations", value = 1, min = 1, max = 50, step = 0.5)),
                                                        div(style = "font-size: 10px; padding: 0px 0px; margin-top:3em",fileInput("seqs_auto", "Sequences", accept = c(".fasta", ".fa", ".fas"))),
                                                        div(style = "font-size: 10px; padding: 0px 0px; margin-top:-1em",numericInput("error_network_auto", "Conditional probability allowed +- error", value = 0.3, min = 0, max = 1, step = 0.05)),
                                                        div(style = "font-size: 10px; padding: 0px 0px; margin-top:3em",directoryInput('directory_auto', label = 'Select an output folder')),
                                                        div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "button_auto", label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
                                                        div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "stop_button_auto", label = "Stop process", style = "float", color = "primary", size = "sm", icon = icon("window-close"))),
                                                        div(class = "buttonagency", style = "display:inline-block", actionBttn(inputId = "check_button_auto", label = "Check status", style = "float", color = "primary", size = "sm", icon = icon("check-square")))
                                           ),
                                           
                                           mainPanel(shinyjs::useShinyjs(),
                                                     tags$head(tags$style(HTML(".shiny-notification {width: 800px; position:fixed; top: calc(98% - 50px);; left: calc(50% - 400px);;}"))),
                                                     tags$head(tags$style("#automated_pred{text-align:justify; overflow-y:scroll; overflow-x:hidden; max-height: 575px; background: #F8F8F8;}")),
                                                     textOutput("automated_pred")) ),
                          conditionalPanel(condition = "input.prediction_mode == 'Customized'",
                                           fluidRow(
                                             column(3,
                                                    wellPanel(tags$label(h3('Input network')),
                                                              #hr(),
                                                              fileInput("network", "Network", accept = ".RData"))),
                                             
                                             column(6,
                                                    wellPanel(tags$label(h3('Predict taxa values')),
                                                              # hr(),
                                                              textInput("taxas", "Taxas", value= "", placeholder = 'Example: Bacteria_X,Bacteria_Y. Write "all" for all taxas.'),
                                                              #textInput("evidence", "Evidence", value= "", placeholder = 'Example: season=S,tissue=AI'),
                                                              #verbatimTextOutput("directorypath"),
                                                              #shinyDirButton('directory', 'Select a folder', 'Please select a folder', FALSE),
                                                              #textInput("outputFolder", "Output folder", value= getwd()),
                                                              #div(style = "display:inline-block; float:right;", actionButton("button1", "Submit")))),
                                                              numericInput("iterations", "Iterations", value = 1, min = 1, max = 50, step = 0.5),
                                                              numericInput("error_network", "Conditional probability allowed +- error", value = 0.3, min = 0, max = 1, step = 0.05),
                                                              tags$style(HTML(pickerInput_select)),
                                                              tags$style(HTML(pickerInput_deselect)),
                                                              uiOutput("selector", container = div),
                                                              div(class = "buttonagency", actionBttn(inputId = "button1", label = "Submit", style = "float", color = "primary", size = "sm")))
                                             ),
                                             
                                             column(3,
                                                    wellPanel(tags$label(h3('Display conditional probability tables')),
                                                              #hr(),
                                                              textInput("nodes", "Nodes", value= "", placeholder = 'Example: Bacteria_X,Bacteria_Y,season,tissue'),
                                                              #numericInput("shiny_width", "Width", value= "1000"),
                                                              #numericInput("shiny_heigth", "Heigth", value= "1000"),
                                                              height = 12,
                                                              div(class = "buttonagency", actionBttn(inputId = "button2", label = "Submit", style = "float", color = "primary", size = "sm"))))
                                             
                                           ),
                                           mainPanel(tags$hr(style="margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"), 
                                                     div(style="align: center", tabsetPanel(type = "pills", 
                                                                                            tabPanel(strong("Predicted value"),
                                                                                                     tags$hr(style="margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                                                                                                     dataTableOutput("predicted_value")),
                                                                                            tabPanel(strong("Conditional probability table"),
                                                                                                     tags$hr(style="margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                                                                                                     uiOutput("selector_cpt", container = div),
                                                                                                     downloadButton("save_cpt_all", "Download all tables", class = "butt"),
                                                                                                     tags$head(tags$style(".butt{background-color:#F7F7F7;} .butt{color: black;}")),
                                                                                                     downloadButton("save_cpt", "Download current table", class = "butt"),
                                                                                                     tags$head(tags$style("#conditional_table{overflow: auto; padding: 20px; text-align:justify; overflow-y:scroll; overflow-x:scroll; max-height: 575px; background: #F8F8F8;}")),
                                                                                                     verbatimTextOutput("conditional_table")),
                                                                                            tabPanel(strong("Infer metagenome"),
                                                                                                     tags$hr(style="margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                                                                                                     fileInput("counts", "Raw counts text file", accept = ".txt"),
                                                                                                     div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_counts")),
                                                                                                     div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", fileInput("seqs", "Sequences fasta file", accept = c(".fa", ".fasta"))),
                                                                                                     div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_seqs")),
                                                                                                     #shinyDirButton('directory', 'Select an output folder', 'Please select a folder'),
                                                                                                     div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", directoryInput('directory', label = 'Select an output folder')),
                                                                                                     span(tags$i(h6(HTML("<b>Remember to remove white spaces in fasta headers.</b>"))), style="color:#52C4DD"),
                                                                                                     tags$style(".buttonpicrust .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7;}"),
                                                                                                     div(class = "buttonpicrust", style = "font-size: 10px; padding: 0px 0px; margin-top:2em;", actionBttn(inputId = "button_picrust", label = "Launch", style = "float", color = "primary", size = "sm",  icon = icon("rocket"))),
                                                                                                     mainPanel(fluidRow(align = "center", shinyjs::useShinyjs(), style = "background-color:#F8F8F8;",
                                                                                                                        textOutput("predicted_metagenome")), width = 12)
                                                                                            )
                                                     )), style = 'width: 1000px; height: 1000px')
                                           
                          )),
                 tabPanel(HTML("<b>Graph panel</b>"),
                          tags$label(h3('Input files')),
                          sidebarPanel(width = 12,
                                       fileInput("network_plot", "Network", accept = ".RData"),
                                       textInput("nodes_plot", "Nodes", value= "", placeholder = 'Example: Bacteria_X,Bacteria_Y,season,tissue'),
                                       div(class = "buttonagency", actionBttn(inputId = "button_plot", label = "Submit", style = "float", color = "primary", size = "sm"))),
                          hr(),
                          mainPanel(
                            downloadButton("save_plot", "Save network plot"),
                            downloadButton("save_relationships", "Save network relationships"),
                            br(),
                            visNetworkOutput("plot_graph", width = "100%", height = "100vh"), style = 'width: 100%; height: 100%'))
))

