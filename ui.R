#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
local_test <- TRUE
#deploy_dir = "/srv/shiny-server/samba_files/"
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
                                       div(style = "font-size: 10px; padding: 0px 0px;",textInput("directory_net", "Specify an output folder", placeholder = "experiment_1")),
                                       #directoryInput('directory_net', label = 'Select an output folder'),
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
                                                        div(style = "font-size: 10px; padding: 0px 0px;",textInput("directory_auto", "Specify an output folder", placeholder = "experiment_1")),
                                                        #div(style = "font-size: 10px; padding: 0px 0px; margin-top:3em",directoryInput('directory_auto', label = 'Select an output folder')),
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
                                       #textInput("nodes_plot", "Nodes", value= "", placeholder = 'Example: Bacteria_X,Bacteria_Y,season,tissue'), 
                                       div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "button_plot", label = "Submit", style = "float", color = "primary", size = "sm")),
                                       div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", downloadBttn(outputId = "save_relationships", label = "Save network relationships", style = "float", color = "primary", size = "sm", icon = icon("download"))),
                          ), 
                          hr(),
                          mainPanel(width = 12, 
                                    chooseSliderSkin("Shiny", color = DT$slider_color), # Slider imput style
                                    conditionalPanel( condition = "input.button_plot",
                                                      shinydashboardPlus::dashboardPage(  
                                                        skin = "black",
                                                        dashboardHeader( 
                                                          title = " ",
                                                          #disable = TRUE,
                                                          # Save:
                                                          tags$li(class = "dropdown", downloadLink(outputId = "Output_SVG", label = " ",  
                                                                                                   style = 
                                                                                                     "background-color: transparent;
                                             border-color: transparent;
                                             font-size: 24px;")),
                                                          tags$li(class = "dropdown", actionButton(inputId = "Capture",label = "", 
                                                                                                   icon = icon("camera", lib = "glyphicon"),
                                                                                                   style = 
                                                                                                     "background-color: transparent;
                                             border-color: transparent;
                                             font-size: 24px;")), 
                                                          tags$li(class = "dropdown", actionButton(inputId = "Save",label = "", 
                                                                                                   icon = icon("save", lib = "glyphicon"),
                                                                                                   style = 
                                                                                                     "background-color: transparent;
                                             border-color: transparent;
                                             font-size: 24px;"))
                                                          
                                                        ),
                                                        dashboardSidebar(id = "sidebar",
                                                                         collapsed = TRUE,
                                                                         minified = TRUE, 
                                                                         # Edit menue #·························································#
                                                                         div( id= "Edit_menu", 
                                                                              sidebarMenu(id = "Edit",  
                                                                                          fluidRow(
                                                                                            column(10, offset = 2,
                                                                                                   tags$h3(icon("edit"),"  Edit:"))),  
                                                                                          menuItem("Nodes",tabName = "Nodes", 
                                                                                                   icon = icon("chevron-right"), 
                                                                                                   menuItem("Shape and size",
                                                                                                            icon = icon("resize-full", lib="glyphicon"),
                                                                                                            ## Forma : 
                                                                                                            fixedRow(column(3, br(), tags$p("Shape") ), 
                                                                                                                     column(9,selectInput( inputId = "N_shape",
                                                                                                                                           #label = "Nodes shape",
                                                                                                                                           label = NULL,
                                                                                                                                           selected = DT$shape, 
                                                                                                                                           choices =  c( "box", "database","ellipse","diamond", "dot", "hexagon", 
                                                                                                                                                         "square", "star","text", "triangle", "triangleDown"))
                                                                                                                     )),
                                                                                                            #HTML("<hr>"), 
                                                                                                            ## Autosize
                                                                                                            checkboxInput(inputId = "N_autosize",
                                                                                                                          label = "Auto size",
                                                                                                                          value = FALSE),
                                                                                                            conditionalPanel(condition = "input.N_autosize == true",
                                                                                                                             radioButtons(inputId = "N_autosize_type", label = "Set nodes size acording to the number of:", 
                                                                                                                                          choices = list("All connected edges" = "C_E","Edges from the node" = "E_F", "Edges to the node" = "E_T"),
                                                                                                                                          selected = "C_E"), 
                                                                                                                             sliderInput(inputId = "N_size_range", "Size range:",
                                                                                                                                         min = 0, max = 100, value = c(10,50))
                                                                                                                             
                                                                                                            ),
                                                                                                            #HTML("<hr>"), 
                                                                                                            ## Tamaño
                                                                                                            conditionalPanel(condition = "input.N_autosize == false",
                                                                                                                             sliderInput(inputId = "N_size", label = "Size",
                                                                                                                                         min = 10,  max = 100, value = DT$size)),
                                                                                                            sliderInput(inputId = "N_borderWidth", label = "Border Width",
                                                                                                                        min = 0,  max = 10, value = DT$width),
                                                                                                            sliderInput(inputId = "N_borderWidthSelected", label = "Selected Border Width",
                                                                                                                        min = 0,  max = 10, value = DT$sel_width)
                                                                                                   ), 
                                                                                                   ## Color de los nodos
                                                                                                   menuItem("Color",tabName = "Nodes_color", 
                                                                                                            icon = icon("tint"),
                                                                                                            tags$h5("Nodes color"),
                                                                                                            fixedRow(column(4, br(), tags$p("Fill") ), 
                                                                                                                     column(8,colourInput(inputId = "N_color_background", 
                                                                                                                                          label = NULL, value = DT$color_background)
                                                                                                                     )),
                                                                                                            fixedRow(column(4, br(), tags$p("Border") ), 
                                                                                                                     column(8,colourInput(inputId = "N_color_border", 
                                                                                                                                          label = NULL, value = DT$color_border)
                                                                                                                     )) ,
                                                                                                            #HTML("<hr>"), 
                                                                                                            
                                                                                                            ## Color selección 
                                                                                                            tags$h5("Selected nodes color"),
                                                                                                            fixedRow(column(4, br(), tags$p("Fill") ), 
                                                                                                                     column(8,colourInput(inputId = "N_color_highlight_background", 
                                                                                                                                          label = NULL, value = DT$color_highlight)
                                                                                                                     )),
                                                                                                            fixedRow(column(4, br(), tags$p("Border") ), 
                                                                                                                     column(8,colourInput(inputId = "N_color_highlight_border", 
                                                                                                                                          label = NULL, value = DT$color_border)
                                                                                                                     )),
                                                                                                            # HTML("<hr>"), 
                                                                                                            ## Opacidad
                                                                                                            sliderInput(inputId = "N_opacity", label = "Opacity",
                                                                                                                        min = 0,  max = 1, value = DT$opacity)
                                                                                                   ),  
                                                                                                   menuItem("Labels",tabName = "N_labels",
                                                                                                            icon = icon("tags"),
                                                                                                            # Show/Hide labels
                                                                                                            checkboxInput(inputId = "N_label",
                                                                                                                          label = "Show labels",
                                                                                                                          value = DT$show_Nlabel),
                                                                                                            # Color
                                                                                                            fixedRow(column(4, br(), tags$p("Color") ), 
                                                                                                                     column(8,
                                                                                                                            colourInput(inputId = "N_label_color", 
                                                                                                                                        label = NULL, value = DT$font_color))
                                                                                                            ),
                                                                                                            # Tamaño
                                                                                                            sliderInput(inputId = "N_label_size", 
                                                                                                                        label = "Size",
                                                                                                                        min = 1,  max = 100, 
                                                                                                                        value = DT$font_size
                                                                                                            ),
                                                                                                            # Fuente
                                                                                                            fixedRow(column(4, br(), tags$p("Font") ), 
                                                                                                                     column(8,
                                                                                                                            selectInput( inputId = "N_label_face",
                                                                                                                                         #label = "Font",
                                                                                                                                         label = NULL,
                                                                                                                                         selected = DT$font_face, 
                                                                                                                                         choices =  c( "Arial", "Arial Black",
                                                                                                                                                       "Calibri","Century Gothic",
                                                                                                                                                       "Comic Sanz","Courier New", 
                                                                                                                                                       "Impact","Lucida Sans",
                                                                                                                                                       "Times New Roman" ))
                                                                                                                     )),
                                                                                                            #background
                                                                                                            
                                                                                                            # Fondo
                                                                                                            sliderInput(inputId = "N_label_strokeWidth", label = "Background width",
                                                                                                                        min = 0,  max = 100, value = DT$font_strokeWidth),
                                                                                                            # Fondo color
                                                                                                            fixedRow(column(4, tags$p("Background"), tags$p("  color") ), 
                                                                                                                     column(8,
                                                                                                                            colourInput(inputId = "N_label_strokeColor",
                                                                                                                                        label = NULL, value = DT$font_strokeColor)
                                                                                                                     )),
                                                                                                            
                                                                                                            # Align
                                                                                                            fixedRow(column(4, br(), tags$p("Position") ), 
                                                                                                                     column(8,
                                                                                                                            selectInput( inputId = "N_label_align",
                                                                                                                                         label = NULL,
                                                                                                                                         #label = "Position",
                                                                                                                                         selected = DT$N_font_align, 
                                                                                                                                         choices =  c( "center", "left"))
                                                                                                                     ))
                                                                                                   ),
                                                                                                   menuItem( "Shadow",tabName = "N_shadow",
                                                                                                             icon = icon("adjust"),
                                                                                                             fluidRow(
                                                                                                               checkboxInput(inputId = "N_show_shadow",
                                                                                                                             label = "Show shadow",
                                                                                                                             value = DT$shadow_enabled),
                                                                                                               shiny::conditionalPanel(
                                                                                                                 condition = "input.N_show_shadow==true",
                                                                                                                 sliderInput(inputId = "N_shadow_x", label = "X",
                                                                                                                             min = -25,  max = 25, value = DT$shadow_x),
                                                                                                                 sliderInput(inputId = "N_shadow_y", label = "Y",
                                                                                                                             min = -25,  max = 25, value = DT$shadow_y)
                                                                                                                 
                                                                                                               )
                                                                                                             )
                                                                                                   ) 
                                                                                          ) ,
                                                                                           
                                                                                          menuItem("Edges",tabName = "Edges",
                                                                                                   icon = icon("chevron-right"), 
                                                                                                   checkboxInput(inputId = "E_hidden",
                                                                                                                 label = "Hide  all links",
                                                                                                                 value = DT$E_hidden), 
                                                                                                   menuItem("Shape and size",
                                                                                                            icon = icon("resize-full", lib="glyphicon"),
                                                                                                            
                                                                                                            checkboxInput(inputId = "E_direction",
                                                                                                                          label = "Directional",
                                                                                                                          value = DT$direction),
                                                                                                            checkboxInput(inputId = "E_dashes",
                                                                                                                          label = "Dashed line",
                                                                                                                          value = DT$dashes),
                                                                                                            sliderInput(inputId = "E_width",
                                                                                                                        label = "Width",
                                                                                                                        min = 0,
                                                                                                                        max = 25,
                                                                                                                        value = DT$width),
                                                                                                            sliderInput(inputId = "E_selected_width",
                                                                                                                        label = "Selected width",
                                                                                                                        min = 0,
                                                                                                                        max = 25,
                                                                                                                        value = DT$sel_width)), 
                                                                                                   menuItem("Color",tabName = "Edges_color",
                                                                                                            icon = icon("tint"),
                                                                                                            tags$h5("Edges color"),
                                                                                                            # Color
                                                                                                            fixedRow(column(4, br(), tags$p("Color") ), 
                                                                                                                     column(8,colourInput(inputId = "E_color", 
                                                                                                                                          label = NULL, value = DT$color_border))),
                                                                                                            fixedRow(column(4, br(), tags$p("Selected color") ), 
                                                                                                                     column(8,colourInput(inputId = "E_color_highlight", 
                                                                                                                                          label = NULL, value = DT$color_border))), 
                                                                                                            ## Opacidad
                                                                                                            sliderInput(inputId = "E_opacity",
                                                                                                                        label = "Opacity",
                                                                                                                        min = 0,
                                                                                                                        max = 1,
                                                                                                                        value = DT$opacity)), 
                                                                                                   # Labels
                                                                                                   menuItem("Labels",tabName = "E_labels",
                                                                                                            icon = icon("tags"),
                                                                                                            # Show/Hide labels
                                                                                                            checkboxInput(inputId = "E_label",
                                                                                                                          label = "Show labels",
                                                                                                                          value = DT$show_Elabel),
                                                                                                            # Color
                                                                                                            fixedRow(column(4, br(), tags$p("Color") ), 
                                                                                                                     column(8,
                                                                                                                            colourInput(inputId = "E_label_color", 
                                                                                                                                        label = NULL, 
                                                                                                                                        value = DT$font_color)
                                                                                                                     )),
                                                                                                            # Tamaño
                                                                                                            sliderInput(inputId = "E_label_size", label = "Size",
                                                                                                                        min = 1,  max = 30, value = DT$font_size,
                                                                                                            ),
                                                                                                            # Fuente
                                                                                                            fixedRow(column(4, br(), tags$p("Font") ), 
                                                                                                                     column(8,
                                                                                                                            selectInput( inputId = "E_label_face",
                                                                                                                                         #label = "Font",
                                                                                                                                         label = NULL,
                                                                                                                                         selected = DT$font_face, 
                                                                                                                                         choices =  c( "Arial", "Arial Black", "Calibri",
                                                                                                                                                       "Century Gothic", "Comic Sanz",
                                                                                                                                                       "Courier New", "Impact",
                                                                                                                                                       "Lucida Sans", "Times New Roman"))
                                                                                                                     )),
                                                                                                            #background
                                                                                                            
                                                                                                            # Fondo
                                                                                                            sliderInput(inputId = "E_label_strokeWidth", label = "Background",
                                                                                                                        min = 0,  max = 100, value = DT$font_strokeWidth),
                                                                                                            # Fondo color
                                                                                                            fixedRow(column(4, br(), tags$p("Background"), tags$p("  color") ), 
                                                                                                                     column(8,
                                                                                                                            colourInput(inputId = "E_label_strokeColor",
                                                                                                                                        label = NULL, 
                                                                                                                                        value = DT$font_strokeColor)
                                                                                                                     )),
                                                                                                            
                                                                                                            # Align
                                                                                                            fixedRow(column(4, br(), tags$p("Position") ), 
                                                                                                                     column(8,
                                                                                                                            selectInput( inputId = "E_label_align",
                                                                                                                                         # label = "Position",
                                                                                                                                         label = NULL,
                                                                                                                                         selected = DT$E_font_align, 
                                                                                                                                         choices =  c( "horizontal", "top",
                                                                                                                                                       "middle", "bottom"))
                                                                                                                     ))
                                                                                                   ), 
                                                                                                   menuItem( "Shadow",tabName = "E_shadow",
                                                                                                             icon = icon("adjust"),
                                                                                                             fluidRow(
                                                                                                               checkboxInput(inputId = "E_show_shadow",
                                                                                                                             label = "Show shadow",
                                                                                                                             value = DT$shadow_enabled),
                                                                                                               shiny::conditionalPanel(
                                                                                                                 condition = "input.E_show_shadow==true",
                                                                                                                 sliderInput(inputId = "E_shadow_x", label = "X",
                                                                                                                             min = -25,  max = 25, value = DT$shadow_x),
                                                                                                                 sliderInput(inputId = "E_shadow_y", label = "Y",
                                                                                                                             min = -25,  max = 25, value = DT$shadow_y)
                                                                                                                 
                                                                                                               )
                                                                                                             )
                                                                                                   )
                                                                                          ), 
                                                                                          menuItem("Physics",tabName = "Physics", 
                                                                                                   icon = icon("chevron-right"),
                                                                                                   # Estatico
                                                                                                   checkboxInput(inputId = "N_physics",
                                                                                                                 label = "Bounce", ### Cambiar nombre
                                                                                                                 value = DT$bounce) ,
                                                                                                   # Fijar eje X
                                                                                                   checkboxInput(inputId = "N_fix_x",
                                                                                                                 label = "Fix X", ### Cambiar nombre
                                                                                                                 value = DT$fix_x) ,
                                                                                                   # Fijar eje y
                                                                                                   checkboxInput(inputId = "N_fix_y",
                                                                                                                 label = "Fix Y", ### Cambiar nombre
                                                                                                                 value = DT$fix_y) 
                                                                                          )
                                                                              ) ,
                                                                              
                                                                              ###### 
                                                                              #Cambio de seleccion  
                                                                              sidebarMenu(id = "Menu_selecion",  
                                                                                          fluidRow(
                                                                                            column(10, offset = 2,
                                                                                                   tags$h3(icon("tasks"),"  Select:"))),
                                                                                          menuItem("Select menu",tabName = "Select" ,
                                                                                                   icon = icon("chevron-right"),
                                                                                                   fluidRow(column(4, offset = 1, br(),
                                                                                                                   tags$body(" Select the grade and the "), br(),
                                                                                                                   tags$body ("direction and click 'Set selection'"), br(),
                                                                                                                   tags$body(" to fix the selection."), style = "font-size:12px;"
                                                                                                   )),
                                                                                                   
                                                                                                   actionButton(inputId = "Set_sel", label = "Set selection", icon = icon("pushpin")), 
                                                                                                   sliderInput(inputId = "Select_grade", label = "Increase selection by grade",
                                                                                                               min = 0,  max = 5, value = DT$grade),  
                                                                                                   radioButtons( inputId = "Select_direction", label = "Direction",
                                                                                                                 choices = c("All", "From", "To"), selected = "All",
                                                                                                                 inline = TRUE)#, 
                                                                                                   # Selecionamos todos los nodos filtrados de la tabla
                                                                                                   #actionButton(inputId = "Tab_nod_sel", label = "Select all table nodes"),
                                                                                                   #Seleccionamos todos los nodos conectados por los edges visibles en la tabla 
                                                                                                   #actionButton(inputId = "Tab_edg_sel", label = "No se que poner")
                                                                                          )
                                                                              ) ,
                                                                              ######  
                                                                              # filtrado
                                                                              sidebarMenu(id = "Menu_filtrado",  
                                                                                          fluidRow(
                                                                                            column(10, offset = 2,
                                                                                                   tags$h3(icon("tasks"),"  Filter:"))),
                                                                                          menuItem("Filter menu",tabName = "Filter" ,
                                                                                                   icon = icon("chevron-right"), 
                                                                                                   fluidRow(
                                                                                                     column(width = 5,checkboxGroupInput(inputId = "Filter_Menu", label = "Filter", 
                                                                                                                                         choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"))
                                                                                                     ),
                                                                                                     column(width = 6, checkboxGroupInput(inputId = "Filter_Menu_By", label = "By", 
                                                                                                                                          choices = list("Selected" = "By_sel","Group" = "By_group", "Number of interactions" = "By_num") )
                                                                                                            
                                                                                                     )),
                                                                                                   fixedRow (
                                                                                                     column(width = 9, uiOutput(outputId = "Filter_by")), 
                                                                                                     column(width = 1, absolutePanel(uiOutput("Remove_group"), right = 8, top = 32)
                                                                                                     )
                                                                                                   ),
                                                                                                   uiOutput(outputId = "Filter_grade"),
                                                                                                   uiOutput(outputId = "Filter_number"),
                                                                                                   uiOutput(outputId = "Filter_direction"),
                                                                                                   
                                                                                                   fluidRow(column(4, offset = 1, br(),
                                                                                                                   tags$body(" Select a group of nodes"), br(),
                                                                                                                   tags$body (" and click 'Add group'"), br(),
                                                                                                                   tags$body(" to define a new group."), style = "font-size:12px;"
                                                                                                   )),
                                                                                                   actionButton(inputId = "Add_sel_group", label = "Add selected group", icon = icon("plus")), 
                                                                                                   # actionButton(inputId = "Undo_", label = "undo", icon = icon("minus")) ,
                                                                                                   textInput("nodes_input", "Input a list of nodes", value= "", placeholder = 'Example: Bacteria_X,Bacteria_Y,season,tissue'), 
                                                                                                   actionButton(inputId = "Add_input_group", label = "Add input group", icon = icon("plus")) 
                                                                                          )
                                                                              ))
                                                                         ###### 
                                                                         
                                                        ),
                                                        dashboardBody( #shinyjqui::jqui_draggable(
                                                              shinydashboardPlus :: box( id = "Graph_output", 
                                                                                         #shinyjqui::jqui_resizable(
                                                                                           #options = list( alsoResize  = "Graph_output", minWidth ='1000px', maxWidth ='850px'), 
                                                                   visNetworkOutput("network_proxy" , width = "100%", height = "500px"
                                                                   #) 
                                                                   ), width = NULL, height = NULL
                                                                   #)
                                                              ) ,
                                                          ####### Lo que estaba
                                                          #downloadButton("save_plot", "Save network plot"),
                                                          #downloadButton("save_relationships", "Save network relationships"),
                                                          #actionBttn(inputId = "SVG", label = "SVG", style = "float", color = "primary", size = "sm"),
                                                          #br(),
                                                          #########
                                                          fluidRow(  column(4,
                                                                            fluidRow(
                                                                              # Informacion del nodo tras hacer doble click
                                                                              #shinyjqui::jqui_resizable(
                                                                               # jqui_draggable(
                                                                                  shinydashboardPlus::box(id="Info_nodes",
                                                                                                          title = "Node info",
                                                                                                          footer = "Double click on a node to display the information.",
                                                                                                          icon = icon("bookmark"),
                                                                                                          width = 12,
                                                                                                          status = "black", 
                                                                                                          collapsible = TRUE,
                                                                                                          solidHeader = TRUE, 
                                                                                                          # Ponemos la información:  
                                                                                                          uiOutput("Node_info"),
                                                                                                          # column(6, h4(textOutput("N1"))),
                                                                                                          # column(6, h4(textOutput("N2")))  
                                                                                                          uiOutput(outputId = "Info_graph_nodes") #))
                                                                              )),
                                                                            fluidRow(
                                                                              #shinyjqui::jqui_resizable(
                                                                               # jqui_draggable(
                                                                                  shinydashboardPlus::box(id="Info_edges",
                                                                                                          title = "Edge info",
                                                                                                          footer = "Double click on an edge to display the information.",
                                                                                                          icon = icon("bookmark"),
                                                                                                          width = 12,
                                                                                                          status = "black", 
                                                                                                          collapsible = TRUE,
                                                                                                          solidHeader = TRUE, 
                                                                                                          uiOutput("Edge_info")
                                                                                                          #conditionalPanel(condition = "(  input.doubleClick_edges_selection.length > 0)" ,
                                                                                                          # Ponemos la información: 
                                                                                                          
                                                                                                          #h4("Causal Relationships:"),
                                                                                                          #h5(textOutput("E1"))'
                                                                                  )#))
                                                                            )
                                                                            
                                                          ),
                                                          
                                                          # Informacion del nodo tras hacer doble click 
                                                          # Tabla de nodos y edges.
                                                          #shinyjqui::jqui_resizable(
                                                           # jqui_draggable(
                                                              tabBox(id="Data",
                                                                     selected = "Nodes",
                                                                     width = 8,
                                                                     tabPanel(title = "Nodes",
                                                                              fluidRow(
                                                                                column( width = 8,
                                                                                        checkboxGroupInput(inputId = "Filter_Tab_N", label = "Select an option to emphasize or show only selected nodes", 
                                                                                                           choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"), selected = NULL,
                                                                                                           width = "100%")),
                                                                                column(width = 4, 
                                                                                       checkboxInput(inputId = "Reorder_N",
                                                                                                     label = "Reorder (selected first)",
                                                                                                     value = TRUE),
                                                                                       #checkboxInput(inputId = "Select_Tab_N", label = "Select/ deselect all table visible nodes.", 
                                                                                       #            value = FALSE)
                                                                                )),
                                                                              #fluidRow( column(1),
                                                                              #         column(2,offset = 8,
                                                                              #                 switchInput(inputId = "Emphasize", 
                                                                              #                             label = "Emphasize", 
                                                                              #                             value = FALSE,
                                                                              #                             onLabel = "ON",
                                                                              #                             offLabel = "OFF",
                                                                              #                             onStatus = "success",
                                                                              #                             offStatus = "danger") 
                                                                              #          )        
                                                                              #),
                                                                              # Tabla con la información de los nodos
                                                                              dataTableOutput("Tab_nodes")), 
                                                                     tabPanel(title = "Edges", 
                                                                              column(width = 8,
                                                                                     checkboxGroupInput(inputId = "Filter_Tab_E", label = "Select an option to emphasize or show only selected nodes", 
                                                                                                        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"), selected = NULL,
                                                                                                        width = "100%")),
                                                                              column(width = 4, 
                                                                                     checkboxInput(inputId = "Reorder_E",
                                                                                                   label = "Reorder (selected first)",
                                                                                                   value = TRUE),
                                                                                     uiOutput(outputId = "Value_decimals")),
                                                                              # Tabla con la información de los nodos
                                                                              dataTableOutput("Tab_edges"),
                                                                              tags$style(type = "text/css",
                                                                                         ".noUi-connect {background: #95979A;}")
                                                                     )
                                                              ) #))
                                                          ), 
                                                          # Pop up de previsualización del guardado
                                                          bsModal(id = "Pop_up", title = "Preview", 
                                                                  trigger = "Save", size = "large", 
                                                                  fluidRow(conditionalPanel(condition= "input.Out_type!='HTML'",
                                                                                            fluidRow(
                                                                                              column(4, 
                                                                                                     textInput(inputId = "Title_main", 
                                                                                                               label = "Main title: ", 
                                                                                                               value = "")),
                                                                                              column(4, 
                                                                                                     textInput(inputId = "Title_submain",
                                                                                                               label = "Submain: ", 
                                                                                                               value = "")),
                                                                                              column(4, 
                                                                                                     textInput(inputId = "Title_footer", 
                                                                                                               label = "Footer: ", 
                                                                                                               value = ""))
                                                                                            ))),
                                                                  fluidRow( column(12,
                                                                                   visNetworkOutput("network_save", width = "100%", height = "100vh") 
                                                                  )
                                                                  ), 
                                                                  fluidRow(  
                                                                    column(4,
                                                                           textInput( inputId = "Out_name", label = "File name",
                                                                                      value = "Output")),
                                                                    column(4, selectInput( inputId = "Out_type",
                                                                                           label = "Format", 
                                                                                           selected = "png", 
                                                                                           choices =  c("HTML", "png", 
                                                                                                        "jpeg", "pdf")),
                                                                    ) ,
                                                                    column(4, 
                                                                           uiOutput(outputId = "HTML_Button"))
                                                                  )
                                                          ) ,
                                                          
                                                          
                                                          ######  
                                                          ######   
                                                          style = 'width: 100%; height: 100%')) 
                                    )
                          )
                   
                 ),
                 tabPanel(HTML("<b>Results</b>"),
                          tags$label(h3('Experiment results')),
                          #radioButtons("down_files","Download Files", choiceNames = as.list(list.files('/srv/shiny-server/samba/files/', full.names = FALSE)), choiceValues = as.list(list.files('/srv/shiny-server/samba/files/', full.names = FALSE)), selected = "")
                          selectInput("down_files","Download Files", as.list(list.files(deploy_dir, full.names = FALSE)), selected = ""),
                          downloadButton("downloadResults", "Download")
                 )
))

