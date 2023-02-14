#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# local_test <- TRUE
deploy_dir = "/srv/shiny-server/samba_files/"
# for local test
# deploy_dir <- "/home/data/git/samba/files/"

options(max.print=999999)

# biocmanag <- "BiocManager"
# lapply(biocmanag, function(x) if(!require(x,character.only = TRUE)) install.packages(x, dependencies = TRUE))

# list_of_packages = c("shiny","shinydashboard","shinydashboardPlus","shinyFiles","bnlearn","shinyjs","DT","data.table","bnviewer","visNetwork","stringr","shinythemes", "purrr", "seqinr","future","ipc","assertr","promises","dplyr","callr", "parallel","this.path","progressr","compare","future.callr","graph","igraph","zip","tibble", "fresh", "colourpicker", "shinyBS","shinyalert","shinyjqui" )
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

# Full-screen
js_fs <- "
function openFullscreen(elem) {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}

function closeFullscreen(elem) {
  if (document.exitFullscreen) {
    document.exitFullscreen();
  } else if (elem.mozExitFullscreen) { /* Firefox */
    document.mozExitFullScreen();
  } else if (elem.webkitExitFullscreen) { /* Chrome, Safari and Opera */
    document.webkitExitFullscreen();
  } else if (elem.msExitFullscreen) { /* IE/Edge */
    document.msExitFullscreen();
  }
}"

css_fs <- "
#network_proxy:-webkit-full-screen {
  height: 100%;
  margin: 0;
}
#network_proxy:-ms-fullscreen {
  height: 100%;
}
#network_proxy:fullscreen {
  height: 100%;
}

#mynetwork {
  width: 600px;
  height: 200px;
}
"  
 
# https://stackoverflow.com/questions/69220191/easiest-way-to-set-different-background-images-to-shiny-panels
# https://stackoverflow.com/questions/45178843/how-to-alter-navigation-button-color-in-vis-js/45307412#45307412


dropdownBlock_Rox <- function (..., id, icon = NULL, title = NULL, badgeStatus = "danger") 
{
  if (!is.null(badgeStatus)) 
    #validateStatus(badgeStatus)
  items <- c(list(...))
  dropdownClass <- paste0("dropdown")
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- dashboardLabel(status = badgeStatus)
  }
  shiny::tags$li(shiny::singleton(shiny::tags$head(shiny::tags$script(shiny::HTML(paste0("$(document).ready(function(){\n                $('#", 
                                                                                         id, "').find('ul').click(function(e){\n                  e.stopPropagation();\n                });\n              });\n              "))))), 
                 class = dropdownClass, id = id, shiny::tags$a(href = "#", 
                                                               class = "dropdown-toggle", `data-toggle` = "dropdown", 
                                                               icon, title, badge), shiny::tags$ul(class = "dropdown-menu", 
                                                                                                   style = "left: 0; right: auto;", shiny::tags$li(shiny::tags$ul(class = "menu", 
                                                                                                                                                                  shiny::tags$div(style = "margin-left: auto; margin-right: auto; width: 80%;", 
                                                                                                                                                                                  items)))))
}

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
  # Diseño UI
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

plan(multisession, workers = 12)

options(shiny.maxRequestSize=100*1024^2)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))




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


learning_training_panel <- tabPanel(
  HTML("<b>Learning and training</b>"),
  sidebarPanel(
    width = 12,
    tags$label(h3("Input files, filters and parameters")),
    hr(),
    fileInput("data_variables", "CSV file for experimental variables", accept = ".csv"),
    div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_data_variables")),
    div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", fileInput("data_taxas", "CSV file for taxa counts", accept = ".csv")),
    div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_data_taxas")),
    tags$style("#filter_taxa {height: 35px;}"),
    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;", checkboxInput("filter_taxa", label = HTML('<h5 style="position: relative;">Filter taxa data by its presence in samples</h5>'), value = FALSE, width = NULL),
      conditionalPanel(
        condition = "input.filter_taxa == 1",
        pickerInput("before_after_filter", "Select applying abundance filter before or after building the network", choices = c("Before", "After")),
        pickerInput("filter_option", "Select Group to filter by a variables or Total to take into account all variables", choices = c("Total", "Group")),
        conditionalPanel(
          condition = "input.filter_option == 'Total'",
          numericInput("filter_countsT", "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
          conditionalPanel(
            condition = "input.before_after_filter == 'After'",
            div(style = "padding: 50 px 0 px; width: 100 px", textInput("filter_thrT", "Select global filter threshold", placeholder = "25,50"))
          ),
          conditionalPanel(
            condition = "input.before_after_filter == 'Before'",
            div(style = "padding: 50 px 0 px; width: 100 px", numericInput("filter_thrT", "Select global filter threshold", value = 50, min = 0, max = 100, step = 0.5))
          )
        ),
        conditionalPanel(
          condition = "input.filter_option == 'Group'",
          textInput("filter_variable", "Specify a variable name to apply this filter"),
          numericInput("filter_countsG", "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
          div(style = "padding: 50 px 0 px; width: 100 px", textInput("filter_thrG", "Select filter threshold for each variable condition", placeholder = "condition1-50,condition2-30..."))
        )
      )
    ),
    div(style = "font-size: 10px; padding: 0px 0px;", textInput("exp_var", "Specify experimental variables to discretize", placeholder = "variableX,variableY")),
    selectInput("dis_method", label = "Select discretization method", choices = c("quantile", "interval", "hartemink"), selected = "quantile"),
    selectInput("net_score", label = "Network score", choices = c("AIC", "BIC", "loglik"), selected = "BIC"),
    numericInput("mi_thr", "Link strength: Mutual Information (MI) threshold", value = 0.05, min = 0, max = 50, step = 0.0005),
    numericInput("bic_thr", "Link strength: Bayesian Information Criterion (BIC) threshold", value = 0, min = -10000000, max = 10000000, step = 0.5),
    tags$style("#blacklist {height: 35px; width: 50 px;}"),
    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;", checkboxInput("blacklist", label = HTML('<h5 style="position: relative; top: -50 px">Blacklist: Links not to be included in the network</h5>'), value = FALSE, width = NULL),
      conditionalPanel(
        condition = "input.blacklist == 1",
        fileInput("black_file", "Upload blacklist file"),
        div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_bl")),
      )
    ),
    tags$style("#whitelist {height: 35px; width: 50 px;}"),
    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;", checkboxInput("whitelist", label = HTML('<h5 style="position: relative; top: -50 px">Whitelist: Links to be included in the network</h5>'), value = FALSE, width = NULL),
      conditionalPanel(
        condition = "input.whitelist == 1",
        fileInput("white_file", "Upload whitelist file"),
        div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_wl"))
      )
    ),
    div(style = "font-size: 10px; padding: 0px 0px;", textInput("directory_net", "Specify an output folder", placeholder = "experiment_1")),
    # directoryInput('directory_net', label = 'Select an output folder'),
    div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "start_net", label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
    div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "stop_net", label = "Stop process", style = "float", color = "primary", size = "sm", icon = icon("window-close"))),
    div(class = "buttonagency", style = "display:inline-block", actionBttn(inputId = "check_net", label = "Check status", style = "float", color = "primary", size = "sm", icon = icon("check-square")))
  )
)

load_network_panel <- tabPanel(
  HTML("<b>Load Network</b>"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$label(h3("Input network")),
      p("Load network from a previous build"),
      # hr(),
      fileInput("input_network_file", NULL, accept = c(".RData", ".RDATA", ".rdata"))
    ),
    mainPanel(
      width = 9,
      h3("Network Summary : "),
      # h4("This is a classic simple layout that is often used to keep"),
      uiOutput("current_network_info")
    )
  )
)


display_cpts_panel <- tabPanel(
  HTML("<b>CPTs</b>"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$label(h3("Conditional Probability Tables")),
      p("Select a Node to display CPT"),
      uiOutput("selector_cpt", container = div),
      hr(),
      downloadButton("save_cpt_all", "Download all tables", class = "butt"),
      tags$head(tags$style(".butt{background-color:#F7F7F7;} .butt{color: black;}")),
      downloadButton("save_cpt", "Download current table", class = "butt"),
    ),
    mainPanel(
      width = 9,
      strong("Conditional probability table"),
      tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
      tags$head(tags$style("#conditional_table{overflow: auto; padding: 20px; text-align:justify; overflow-y:scroll; overflow-x:scroll; max-height: 575px; background: #F8F8F8;}")),
      verbatimTextOutput("conditional_table")
    )
  )
)


# set_network_evidence_panel <- tabPanel(HTML("<b>Evidence/Control</b>"),
# uiOutput("network_evidence_info_ui"))



prediction_panel <- tabPanel(
  HTML("<b>Prediction</b>"),
  conditionalPanel(
    condition = "output.show_graph",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$label(h3("Predict taxa values")),
        # hr(),
        # ", "Taxas", value = "", placeholder = 'Example: Bacteria_X,Bacteria_Y. Write "all" for all taxas.'),
        uiOutput("selector_predict_taxas", container = div),
        # textInput("evidence", "Evidence", value= "", placeholder = 'Example: season=S,tissue=AI'),
        # verbatimTextOutput("directorypath"),
        # shinyDirButton('directory', 'Select a folder', 'Please select a folder', FALSE),
        # textInput("outputFolder", "Output folder", value= getwd()),
        # div(style = "display:inline-block; float:right;", actionButton("button1", "Submit")))),
        #numericInput("iterations", "Iterations", value = 1, min = 1, max = 50, step = 0.5),
        #numericInput("error_network", "Conditional probability allowed +- error", value = 0.3, min = 0, max = 1, step = 0.05),
        tags$style(HTML(pickerInput_select)),
        tags$style(HTML(pickerInput_deselect)),
        uiOutput("evidence_selector", container = div),
        checkboxInput("use_data_as_strong_proir", label = "Use Input Data as strong proir for network prediction", value = TRUE, width = NULL),
        checkboxInput("show_org_data_dist", label = "Show Original Data summary", value = TRUE, width = NULL),
        div(class = "buttonagency", actionBttn(inputId = "button1", label = "Submit", style = "float", color = "primary", size = "sm"))
      ),
      mainPanel(
        width = 9,
        #tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; heigth: auto; object-fit: contain;"),
        div(style = "align: center", 
        tabsetPanel(
          type = "pills",
          tabPanel(
            strong("Predict abundances"),
            strong("Predict abundances"),
            tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
            dataTableOutput("predicted_value")
          ),
          # tabPanel(
          #   strong("Conditional probability table"),
          #   tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
          #   uiOutput("selector_cpt", container = div),
          #   downloadButton("save_cpt_all", "Download all tables", class = "butt"),
          #   tags$head(tags$style(".butt{background-color:#F7F7F7;} .butt{color: black;}")),
          #   downloadButton("save_cpt", "Download current table", class = "butt"),
          #   tags$head(tags$style("#conditional_table{overflow: auto; padding: 20px; text-align:justify; overflow-y:scroll; overflow-x:scroll; max-height: 575px; background: #F8F8F8;}")),
          #   verbatimTextOutput("conditional_table")
          # ),
          tabPanel(
            strong("Predict Metagenomes"),
            strong("Predict Metagenomes"),
            tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
            fileInput("counts", "Raw counts text file", accept = ".txt"),
            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_counts")),
            div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", fileInput("seqs", "Sequences fasta file", accept = c(".fa", ".fasta"))),
            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_seqs")),
            # shinyDirButton('directory', 'Select an output folder', 'Please select a folder'),
            div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", directoryInput("directory", label = "Select an output folder")),
            span(tags$i(h6(HTML("<b>Remember to remove white spaces in fasta headers.</b>"))), style = "color:#52C4DD"),
            tags$style(".buttonpicrust .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7;}"),
            div(class = "buttonpicrust", style = "font-size: 10px; padding: 0px 0px; margin-top:2em;", actionBttn(inputId = "button_picrust", label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
            mainPanel(fluidRow(
              align = "center", shinyjs::useShinyjs(), style = "background-color:#F8F8F8;",
              textOutput("predicted_metagenome")
            ), width = 12)
          )
        ))
        # ,
        # style = "width: 100%; height: 1000px"
      )
    )
  )
)



# Define UI for application that draws a histogram
shinyUI(  
  
  fluidPage(
    tags$style(".buttonagency .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7; margin-top:1em;}"),

    navbarPage(
      theme = shinytheme("yeti"),
      collapsible = TRUE,
      HTML( paste('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class = "active" href = "#"><b>','SAMBA</b></a>')),
      HTML( paste('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class = "active" href = "#"><b>','SAMBA</b></a>')),
      id = "nav",
      windowTitle = HTML("Metagenomic network"),
      navbarMenu(HTML("<b>Build</b>"),
      navbarMenu(HTML("<b>Build</b>"),
                      # learning_training_panel,
                      build_network_ui(),
                      load_network_ui(),
                      load_network_ui(),
                      evidence_info_ui("evidence_ui")),
      navbarMenu(HTML("<b>Inference</b>"),
      navbarMenu(HTML("<b>Inference</b>"),
                      nodes_cpts_ui("nodes_cpts_ui"),
                      nodes_dags_ui("nodes_dags_ui")),
      network_prediction_ui(),
      network_viewer_ui(),
      tabPanel(
        HTML("<b>Downloads</b>"),
        HTML("<b>Downloads</b>"),
        tags$label(h3("Experiment results")),
        # radioButtons("down_files","Download Files", choiceNames = as.list(list.files('/srv/shiny-server/samba/files/', full.names = FALSE)), choiceValues = as.list(list.files('/srv/shiny-server/samba/files/', full.names = FALSE)), selected = "")
        selectInput("down_files", "Download Files", as.list(list.files(deploy_dir, full.names = FALSE)), selected = ""),
        downloadButton("downloadResults", "Download")
      )
    )
  )
)
