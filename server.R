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


# Define server logic required to use the functions
shinyServer(function(input, output, session) {
  
  status_file <- tempfile()
  
  get_status <- function(){
    scan(status_file, what = "character",sep="\n")
  }
  
  set_status <- function(msg){
    write(msg, status_file)
  }
  
  fire_interrupt <- function(){
    set_status("interrupt")
  }
  
  fire_ready <- function(){
    set_status("Ready")
  }
  
  fire_running <- function(perc_complete){
    if(missing(perc_complete))
      msg <- "Running..."
    else
      msg <- paste0("Running... ", perc_complete)
    set_status(msg)
  }
  
  interrupted <- function(){
    get_status() == "interrupt"
  }
  
  # Delete file at end of session
  onStop(function(){
    print(status_file)
    if(file.exists(status_file))
      unlink(status_file)
  })
  
  # Create Status File
  fire_ready()
  
  ## Learning and training network model
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory_net
    },
    handlerExpr = {
      if (input$directory_net > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'directory_net'),
                          caption="Choose a directory...")
        updateDirectoryInput(session, 'directory_net', value = path)
      }
    }
  )
  
  output$directory_net = renderText({
    readDirectoryInput(session, 'directory_net')
  })
  
  
  create_model <- function(data_variables, data_taxas, expVar, net_dir, blacklist, whitelist, bl, wl, dismethod, netscore, thr_mi, thr_bic, filterTaxa, filterThrG, filterThrT, filterOption, filterVariable, filterCountsT, filterCountsG) ({
    log_file <- file.path(net_dir, "log_file.txt")
    sink(log_file)
    fire_running("Reading input files")
    print("Reading input files", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    #data_variables = read.csv(file = input$data_variables$datapath, sep = ";", dec = ",", row.names = 1, header = T, stringsAsFactors = TRUE)
    #data_taxas = read.csv(file = input$data_taxas$datapath, sep = ";", dec = ",", row.names = 1, header = T, stringsAsFactors = TRUE)
    
    bn_df_variables = data.frame(data_variables)
    bn_df_taxas = data.frame(data_taxas)
    
    for (i in 1:ncol(bn_df_variables)) {
      c <- class(bn_df_variables[,i])
      if ( c == "integer") {
        bn_df_variables[,i] <- as.numeric(bn_df_variables[,i])
      }
    }
    
    for (i in 1:ncol(bn_df_taxas)) {
      c <- class(bn_df_taxas[,i])
      if ( c == "integer") {
        bn_df_taxas[,i] <- as.numeric(bn_df_taxas[,i])
      }
    }
    
    #expVar <- strsplit(input$exp_var, ",")[[1]]
    
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    
    if(length(expVar) != 0) {
      fire_running("Discretizing experimental continuous variables")
      print("Discretizing experimental continuous variables", quote = FALSE)
      if(length(expVar) == 1) {
        if (dismethod != "hartemink") {
          var_discretize <- discretize(as.data.frame(bn_df_variables[[expVar]]), method = dismethod, breaks = 5, ordered = FALSE)
          bn_df_variables[[expVar]] <- var_discretize[,1]
          bn_df_variables[[expVar]] <- factor(bn_df_variables[[expVar]], levels = rev(unique(var_discretize[,1])), ordered = FALSE)
        } else {
          fire_running("Can't use hartemink, at least two variables are needed to compute mutual information. Using quantile method instead.")
          var_discretize <- discretize(as.data.frame(bn_df_variables[[expVar]]), method = "quantile", breaks = 5, ordered = FALSE)
          bn_df_variables[[expVar]] <- factor(bn_df_variables[[expVar]], levels = rev(unique(var_discretize[,1])), ordered = FALSE)
        }
      } else {
        if (dismethod != "hartemink") {
          for (i in expVar) {
            if (class(bn_df_variables[[i]]) == "numeric") {
              var_discretize <- discretize(as.data.frame(bn_df_variables[[i]]), method = dismethod, breaks = 5, ordered = FALSE)
              bn_df_variables[[i]] <- var_discretize[,1]
              bn_df_variables[[i]] <- factor(bn_df_variables[[i]], levels = rev(unique(var_discretize[,1])), ordered = FALSE)
            }
          }
        } else {
          df_dis <- select(bn_df_variables, all_of(expVar))
          var_discretize <- discretize(as.data.frame(df_dis), method = dismethod, breaks = 5, ordered = FALSE)
          bn_df_variables[names(var_discretize)] <- var_discretize
        }
      }
      
    }
    
    for (i in 1:ncol(bn_df_variables)) {
      if (class(bn_df_variables[,i]) == "numeric") {
        sp = shapiro.test(bn_df_variables[,i])
        if (sp$p.value < 0.05) {
          bn_df_variables[,i] <- log1p(bn_df_variables[,i])
        }
      }
    }
    
    bn_df_variables[bn_df_variables=="-Inf"]<- -1000
    
    data_raw <- cbind(bn_df_variables, bn_df_taxas)
    bn_df_raw<- as.data.frame(data_raw)
    
    dis_exp_variables <- bn_df_variables %>% select_if(is.factor)
    con_exp_variables <- bn_df_variables %>% select_if(is.numeric)
    
    fire_running("Creating model and training datasets")
    print("Creating model and training datasets", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    combinations_var <- distinct(dis_exp_variables)
    
    #data_model <- data.frame()
    #data_training <- data.frame()
    
    #l_var <- list()
    
    #for (i in 1:nrow(combinations_var)) {
    #for (j in 1:length(combinations_var[i,])) {
    #l_var[[names(combinations_var)[j]]] <- combinations_var[i,j]
    #}
    #comb_df <- bn_df_variables
    
    #for (j in 1:length(l_var)) {
    #comb_df <- comb_df[comb_df[[names(l_var[j])]] == l_var[[j]][1], ]
    #}
    #df1 <- comb_df[1:round(nrow(comb_df)/2), ]
    #df2 <- comb_df[round(nrow(comb_df)/2)+1:nrow(comb_df), ]
    #data_training <- rbind(data_training, df1)
    #data_model <- rbind(data_model, df2)
    #}
    
    #data_model <- data_model[complete.cases(data_model), ]
    #data_training <- data_training[complete.cases(data_training), ]
    
    fire_running("Normalizing taxa raw counts")
    print("Normalizing taxa raw counts", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    bn_df_taxas_norm <- bn_df_taxas
    
    for (r in 1:nrow(bn_df_taxas)) {
      sample_sum = sum(bn_df_taxas[r,])
      for (c in 1:ncol(bn_df_taxas)) {
        col_sum = sum(bn_df_taxas[, c])
        log_value = log1p(bn_df_taxas[r,c])*log1p(col_sum)/log1p(sample_sum)
        norm_value <- expm1(log_value)
        if (!is.na(norm_value)){
          bn_df_taxas_norm[r,c] <- norm_value
        } else {
          bn_df_taxas_norm[r,c] <- 0
        }
        
      }
    }
    
    output_norm <- file.path(net_dir, "taxa_norm_counts.csv")
    write.table(bn_df_taxas_norm, file = output_norm, dec = ",", sep = ";")
    
    fire_running("Applying log scale to normalized taxa data")
    print("Applying log scale to normalized taxa data", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    bn_df_taxa_norm_log <- to_log(bn_df_taxas_norm, colnames(bn_df_taxas_norm))
    
    output_log <- file.path(net_dir, "taxa_norm_log_counts.csv")
    write.table(bn_df_taxa_norm_log, file = output_log, dec = ",", sep = ";")
    
    #bn_df_model <- cbind(bn_df_variables[rownames(data_model),],bn_df_taxas_norm[rownames(data_model),])
    #data_model <- bn_df_model
    #bn_df_training <- cbind(bn_df_variables[rownames(data_training),],bn_df_taxas_norm[rownames(data_training),])
    #data_training <- bn_df_training
    
    bn_df_norm <- cbind(bn_df_variables, bn_df_taxa_norm_log)
    
    fire_running("Creating network model")
    print("Creating network model", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    cont_variables <- try(unlist(lapply(bn_df_variables), is.numeric))
    
    if (class(cont_variables) == "try-error") {
      
      netscore.g <- tolower(paste(netscore, "-CG", sep = ""))
    } else {
      if (length(cont_variables) == ncol(bn_df_variables)) {
        
        netscore.g <- tolower(paste(netscore, "-G", sep = ""))
      } else {
        
        netscore.g <- tolower(paste(netscore, "-CG", sep = ""))
      }
    }
    
    print(netscore.g)
    
    if (blacklist == 1 && whitelist == 1) {
      result <- hc(bn_df_norm, optimized = TRUE, whitelist = wl, blacklist = bl, score = netscore.g)
    }
    
    if (blacklist == 1 && whitelist != 1) {
      result <- hc(bn_df_norm, optimized = TRUE, blacklist = bl, score = netscore.g)
    }
    
    if (blacklist != 1 && whitelist == 1) {
      result <- hc(bn_df_norm, optimized = TRUE, whitelist = wl, score = netscore.g)
    }
    
    if (blacklist != 1 && whitelist != 1) {
      result <- hc(bn_df_norm, optimized = TRUE, score = netscore.g)
    }
    
    fire_running("Network model done! Filtering model by link strength")
    print("Network model done! Filtering model by link strength", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    remove_arcs = data.frame()
    result_filt <- result
    
    arc_st_bic = arc.strength(result, bn_df_norm, criterion = "bic-cg")
    arc_st_mi = arc.strength(result, bn_df_norm, criterion = "mi-cg")
    
    n = 0
    
    for (l in 1:nrow(arc_st_bic)) {
      if ((arc_st_bic[l,3] < thr_bic) && (arc_st_mi[l,3] < thr_mi)) {
        n = n + 1
      } else {
        row = c(arc_st_bic[l,1], arc_st_bic[l,2])
        remove_arcs <- rbind(remove_arcs, row)
      }
    }
    
    out_remove <- file.path(net_dir, "removed_arcs.txt")
    write.table(remove_arcs, out_remove, sep = "\t", dec = ",")
    
    if (length(remove_arcs) != 0) {
      for (i in 1:nrow(remove_arcs)) {
        d <- data.frame(from = remove_arcs[i,1], to = remove_arcs[i,2])
        comparison <- compare::compare(d, wl, allowAll = TRUE)
        if(isFALSE(comparison)) {
          result_filt = drop.arc(result_filt, remove_arcs[i,1], remove_arcs[i,2])
        }
        
      }
    }
    
    fire_running("Training model")
    print("Training model", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    fittedbn <- bn.fit(result_filt, data = bn_df_norm, replace.unidentifiable = TRUE)
    
    fire_running("Writing output files")
    print("Writing output files", quote = FALSE)
    if(interrupted()){ 
      print("Stopping...", quote = FALSE)
      stop("User Interrupt")
    }
    
    strength_mi <- file.path(net_dir, "arc_strength_mi.txt")
    write.table(arc_st_mi, strength_mi, sep = "\t", dec = ",")
    
    strength_bic <- file.path(net_dir, "arc_strength_bic.txt")
    write.table(arc_st_bic, strength_bic, sep = "\t", dec = ",")
    
    out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_complete_network.RData", sep = "")
    output_file_net <- file.path(net_dir, out_net_name)
    save(list = ls(), file = output_file_net, envir = environment())
    
    #FILTRADO POR PORCENTAJE
    
    if(filterTaxa == 1) {
      if(filterOption == "Total") {
        filterThrT_sep <- strsplit(filterThrT, ",")
        for (thr in filterThrT_sep[[1]]) {
          print(thr)
          to_remove <- c()
          min_samples <- round(nrow(bn_df_taxas)*as.numeric(thr)/100)
          for(i in 1:ncol(bn_df_taxas)) {
            counts <- sum(as.numeric(bn_df_taxas[,i]) >= as.numeric(filterCountsT))
            if(counts < min_samples) {
              to_remove <- c(to_remove, i)
            }
          }
          to_remove <- unique(to_remove)
          output_to_remove <- file.path(net_dir, paste("taxa_to_be_removed_by_filter_", thr, ".csv", sep = ""))
          write.table(bn_df_taxas, file = output_to_remove, dec = ",", sep = ";")
          
          result_removed <- result_filt
          for (n in to_remove) {
            result_removed <- remove.node(result_removed, colnames(bn_df_taxas)[[n]])
          }
          result1 <- result
          bn_df_norm1 <- bn_df_norm
          bn_df_norm_removed <- subset(bn_df_norm, select = nodes(result_removed))
          output_kept <- file.path(net_dir, paste("kept_taxa_by_", thr, "_filter.csv", sep = ""))
          write.table(bn_df_norm_removed, file = output_kept, dec = ",", sep = ";")
          fittedbn <- bn.fit(result_removed, data = bn_df_norm_removed, replace.unidentifiable = TRUE)
          out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_", thr,"_network.RData", sep = "")
          output_file_net <- file.path(net_dir, out_net_name)
          save(list = ls(), file = output_file_net, envir = environment())
          result <- result1
          bn_df_norm <- bn_df_norm1
        }
        
      } else if(filterOption == "Group") {
        columns_var <- ncol(bn_df_variables)
        df_complete <- cbind(bn_df_variables, bn_df_taxas)
        filterThrG <- str_replace_all(filterThrG, c("/" = ".", " " = "."))
        filterThrG_sep <- strsplit(filterThrG, ",")
        filterThrG_sep2 <- c()
        for (i in filterThrG_sep) {
          sp <- strsplit(i, "-")
          filterThrG_sep2 <- c(filterThrG_sep2, sp)
        }
        filterVariable <- str_replace_all(filterVariable, c("/" = ".", " " = ".", "-" = "."))
        df_divided <- split(df_complete, df_complete[[filterVariable]])
        col_tax <- ncol(bn_df_variables)+1
        for (i in filterThrG_sep2) {
          filt <- df_divided[[i[1]]]
          min_samples <- round(nrow(filt)*as.numeric(i[2])/100)
          for(j in col_tax:ncol(df_complete)) {
            counts <- sum(as.numeric(filt[,j]) >= as.numeric(filterCountsG))
            if(counts < min_samples) {
              to_remove <- c(to_remove, as.numeric(j)-columns_var)
            }
          }
        }
        result1 <- result
        bn_df_norm1 <- bn_df_norm
        to_remove <- unique(to_remove)
        #bn_df_taxas <- bn_df_taxas[, -to_remove]
        output_to_remove <- file.path(net_dir, "taxa_to_be_removed_by_group_filter.csv")
        write.table(bn_df_taxas, file = output_to_remove, dec = ",", sep = ";")
        result_removed <- result
        for (n in to_remove) {
          result_removed <- remove.node(result_removed, colnames(bn_df_taxas)[[n]])
        }
        bn_df_norm_removed <- subset(bn_df_norm, select = nodes(result_removed))
        output_kept <- file.path(net_dir, "kept_taxa_by_group_filter.csv")
        write.table(bn_df_norm_removed, file = output_kept, dec = ",", sep = ";")
        fittedbn <- bn.fit(result_removed, data = bn_df_norm_removed, replace.unidentifiable = TRUE)
        result <- result_removed
        bn_df_norm <- bn_df_norm_removed
        out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"),"_filtGroup_network.RData", sep = "")
        output_file_net <- file.path(net_dir, out_net_name)
        save(list = ls(), file = output_file_net, envir = environment())
        result <- result1
        bn_df_norm <- bn_df_norm1
      }
    }
    
    fire_running("DONE!")
    print("DONE!", quote = FALSE)
    sink()
    
  })
  
  create_model_button <- observeEvent(input$start_net, {
    disable("start_net")
    #create_model()
    current_dir <- this.dir()
    data_variables <- fread(file = input$data_variables$datapath, sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE)
    data_taxas = fread(file = input$data_taxas$datapath, sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE)
    data_variables <- data.frame(data_variables, row.names = 1)
    data_taxas <- data.frame(data_taxas, row.names = 1)
    expVar <- strsplit(input$exp_var, ",")[[1]]
    blacklist <- input$blacklist
    whitelist <- input$whitelist
    bl <- ""
    wl <- ""
    filterTaxa <- input$filter_taxa
    filterThrG <- input$filter_thrG
    filterThrT <- input$filter_thrT
    filterOption <- input$filter_option
    filterVariable <- input$filter_variable
    filterCountsT <- input$filter_countsT
    filterCountsG <- input$filter_countsG
    
    if (blacklist == 1) {
      bl = fread(file = input$black_file$datapath, sep = "auto", dec = ".", header = T)
      #bl[] <- lapply(bl, function(x) gsub("/", ".", x))
      #bl[] <- lapply(bl, function(x) gsub(" ", ".", x))
      #bl[] <- lapply(bl, function(x) gsub("-", ".", x))
    }
    
    if (whitelist == 1) {
      wl = fread(file = input$white_file$datapath, sep = "auto", dec = ".", header = T)
      #wl[] <- lapply(wl, function(x) gsub("/", ".", x))
      #wl[] <- lapply(wl, function(x) gsub(" ", ".", x))
      #wl[] <- lapply(wl, function(x) gsub("-", ".", x))
    }
    
    dismethod <- input$dis_method
    netscore <- input$net_score
    thr_mi = input$mi_thr
    thr_bic = input$bic_thr
    
    #net_dir <- readDirectoryInput(session, 'directory_net')
    net_dir <- "/srv/shiny-server/samba/files"
    f <- future({create_model(data_variables, data_taxas, expVar, net_dir, blacklist, whitelist, bl, wl, dismethod, netscore, thr_mi, thr_bic, filterTaxa, filterThrG, filterThrT, filterOption, filterVariable, filterCountsT, filterCountsG)}, seed = TRUE)
    #return(NULL)
    f <- catch(f, function(e) {
      print(e$message)
      showNotification(e$message)
    })
    f <- finally(f,
                 function(){
                   fire_ready() 
                   enable("start_net")
                 })
    enable("start_net")
  })
  
  observeEvent(input$stop_net, {
    print("Cancel")
    fire_interrupt()
    enable("button_auto")
  })
  
  observeEvent(input$check_net, {
    print("Status")
    showNotification(get_status())
  })
  
  output$example_bl <- renderUI({
    tags$a(href="example_blacklist_whitelist.csv", target = "blank", "Example blacklist/whitelist file", download = "example_blacklist_whitelist.csv")
  })
  
  output$example_wl <- renderUI({
    tags$a(href="example_blacklist_whitelist.csv", target = "blank", "Example blacklist/whitelist file", download = "example_blacklist_whitelist.csv")
  })
  
  output$example_data_variables <- renderUI({
    tags$a(href="example_data_variables.csv", target = "blank", "Example variables file", download = "example_data_variables.csv")
  })
  
  output$example_data_taxas <- renderUI({
    tags$a(href="example_data_taxas.csv", target = "blank", "Example taxas file", download = "example_data_taxas.csv")
  })
  
  ## Automated taxa values prediction
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory_auto
    },
    handlerExpr = {
      if (input$directory_auto > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'directory_auto'),
                          caption="Choose a directory...")
        updateDirectoryInput(session, 'directory_auto', value = path)
      }
    }
  )
  
  output$directory_auto = renderText({
    readDirectoryInput(session, 'directory_auto')
  })
  
  ### Read network using observeEvent
  observeEvent(input$network_auto,{
    if ( is.null(input$network_auto)) return(NULL)
    inFile <- isolate({input$network_auto })
    file <- inFile$datapath
    load(file, envir = .GlobalEnv)
    
    taxas <- nodes(fittedbn)
    taxas <- taxas[ncol(data_variables)+1:length(taxas)]
    taxas <- na.omit(taxas)
    
    error_cp <- input$error_network_auto
    
    automated_prediction <- function(sequences, out_dir, it, current_dir) ({
      #withProgressShiny(message = "Calculation in progress", detail = "Wait...", value = 0, {
      #p1 <- progressor(steps = nrow(combinations_var))
      nodes_complete <- nodes(fittedbn)
      
      files <- NULL
      for (i in nodes_complete) {
        if (nchar(i) > 200) {
          name <- substr(i, 1, 200)
        }else {
          name <- i
        }
        fileName <- paste(name, "_CPT.txt", sep = "")
        sink(fileName)
        print(fittedbn[[i]])
        sink()
        files <- c(fileName, files)
      }
      zipname <- paste(out_dir, "nodes_CPT.zip", sep = "//")
      zip::zip(zipname, files)
      
      for (i in 1:nrow(combinations_var)) {
        filename <- paste("combination", i, sep = "_")
        if (.Platform$OS.type == "windows"){
          filepath <- paste(out_dir, filename, sep = "\\")
          filepath <- paste(filepath, "\\", filename, ".log", sep = "")
          filepath <- gsub("\\\\", "/", filepath)
          path_file <- paste(out_dir, filename, sep = "\\")
          path_file <- gsub("\\\\", "/", path_file)
        } else {
          filepath <- paste(out_dir, filename, sep = "")
          filepath <- paste(filepath, "/", filename, ".log", sep = "")
          path_file <- paste(out_dir, filename, sep = "")
        }
        
        if (.Platform$OS.type == "windows"){
          setwd(out_dir)
          ifelse(!dir.exists(path_file), dir.create(filename), FALSE)
          setwd(current_dir)
        } else {
          ifelse(!dir.exists(path_file), dir.create(path_file), FALSE)
        }
        
        sink(filepath)
        ronda <- paste(i, " of ", nrow(combinations_var), sep = "")
        #p1(sprintf(ronda))
        #print(i)
        
        evidence_cpquery = ""
        for (l in 1:length(combinations_var)) {
          if (l == 1) {
            evidence_cpquery = paste("(", names(combinations_var[l]), " == ", '"', combinations_var[i,l], '")', sep = "")
          } else {
            evidence_cpquery = paste(evidence_cpquery, " & ", "(", names(combinations_var[l]), " == ", '"', combinations_var[i,l], '")', sep = "")
          }
        }
        
        l_var <- list()
        for (j in 1:length(combinations_var[i,])) {
          l_var[[names(combinations_var)[j]]] <- combinations_var[i,j]
        }
        
        df <- matrix(data = NA, nrow = length(taxas), ncol = 6)
        df <- data.frame(df)
        colnames(df) <- c("Taxa", "Predicted.normalized.value", "Raw.counts.average", "Standard.deviation", "Range.of.normalized.values","Conditional.probability")
        bn_df_total <- bn_df_norm
        bn_df_raw_filt <- bn_df_raw
        
        bn_df_total_taxas <- bn_df_taxas_norm
        #total <- complete_sum
        
        bn_df_raw_taxas <- bn_df_taxas
        
        for (j in 1:length(l_var)) {
          bn_df_raw_filt <- bn_df_raw_filt[bn_df_raw_filt[[names(l_var[j])]] == l_var[[j]][1], ]
        }
        
        if (nrow(bn_df_raw_filt) == 0) {
          next
        } else {
          bn_df_raw_filt_taxas <- select(bn_df_raw_filt, colnames(bn_df_taxas))
          total_raw_counts <- try(sum(bn_df_raw_filt_taxas)/nrow(bn_df_raw_filt_taxas))
          
          ## For loop to add data to dataframe
          #withProgressShiny(message = "Predicting taxa values and predicting metagenome ", detail = "Wait...", value = 0, {
          #p2 <- progressr::progressor(steps = 20, initiate = TRUE, scale = 1L, offset = 0L)
          for (t in 1:length(taxas)) {
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            ronda2 <- paste("Experimental variables combination ", ronda, " & Taxa ", t, " of ", length(taxas), sep = "")
            fire_running(ronda2)
            #p(sprintf(ronda2))
            print(ronda2, quote = FALSE)
            #p2(sprintf(ronda2))
            df[t,1] <- taxas[t]
            taxas[t] <- str_replace_all(taxas[t], c("/" = ".", " " = ".", "-" = "."))
            predict <- try(cpdist(fittedbn, nodes = taxas[t], evidence = l_var, method = "lw", n = 100000))
            if (class(predict)[1] == "try-error") {
              df[t,2] <- as.character("Cannot be calculated")
            } else {
              no_na <- !is.na(predict)
              pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
              if (!is.nan(pred_val)) {
                vector <- c(as.numeric(pred_val))
                #it <- input$iterations_auto
                if (it != 1) {
                  for (v in 2:it) {
                    predict <- cpdist(fittedbn, nodes = taxas[t], evidence = l_var, method = "lw", n = 100000)
                    no_na <- !is.na(predict)
                    pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
                    if (!is.nan(pred_val)) {
                      vector <- c(vector, as.numeric(pred_val))
                    }
                  }
                  pred_value_log <- median(vector)
                  if (expm1(pred_log_value) >= 0) {
                    df[t,2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
                  } else {
                    df[t,2] <- 0
                  }
                } else {
                  pred_value_log <- pred_val
                  if (expm1(pred_log_value) >= 0) {
                    df[t,2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
                  } else {
                    df[t,2] <- 0
                  }
                }
              } else {
                df[t,2] <- as.character("NaN")
              }
            }
            if (class(total_raw_counts) == "numeric") {
              media <- mean(bn_df_raw_filt_taxas[[taxas[t]]])
              df[t,3] <- round(media, digits = 0)
            } else {
              df[t,3] <- as.character("Cannot be calculated")
            }
            
            if (class(total_raw_counts) == "numeric") {
              desves <- sd(bn_df_raw_filt_taxas[[taxas[t]]])
              df[t,4] <- round(desves, digits = 2)
            } else {
              df[t,4] <- as.character("Cannot be calculated")
            }
            
            filt_data <- bn_df_norm
            for (v in 1:length(l_var)) {
              filt_data <- filt_data[filt_data[[names(l_var[v])]] == l_var[[v]][1], ]
            }
            
            average_norm <- round(mean(expm1(filt_data[[taxas[t]]])), digits = 2)
            desvest_norm <- round(sd(expm1(filt_data[[taxas[t]]])), digits = 2)
            
            if ((average_norm - desvest_norm) < 0) {
              low_range <- 0
            } else {
              low_range <- average_norm - desvest_norm
            }
            
            high_range <- average_norm + desvest_norm
            
            df[t,5] <- paste(low_range, "-", high_range)
            
            if ((as.numeric(df[t,3]) == 0) & (as.numeric(df[t,4]) == 0)) {
              df[t,2] <- as.character(0)
            }
            if ((df[t,2] != "Cannot be calculated") & (df[t,2] != "NaN")) {
              event_cpquery = paste("((", taxas[t], ">= ", (as.numeric(pred_value_log)-error_cp*as.numeric(pred_value_log)), ") & (", taxas[t], "<= ", (as.numeric(pred_value_log)+error_cp*as.numeric(pred_value_log)), "))", sep = "")
              result_cpquery <- try(eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', evidence_cpquery, ', method = "ls")'))))
              if (!is.nan(result_cpquery)) {
                vector3 <- c(as.numeric(result_cpquery))
                if (it!=1) {
                  for (v in 2:it) {
                    result_cpquery <- try(eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', evidence_cpquery, ', method = "ls")'))))
                    if (!is.nan(result_cpquery)) {
                      vector3 <- c(vector3, as.numeric(result_cpquery))
                    }
                  }
                  df[t,6] <- round(as.numeric(median(vector2)), digits = 2)
                } else {
                  df[t,6] <- round(as.numeric(result_cpquery), digits = 2)
                }
              } else {
                df[t,6] <- "NaN"
              }
            } else {
              df[t,6] <- "Cannot be calculated"
            }
          }
          
          if (df[t,6] != "Cannot be calculated") {
            #filename <- col_concat(combinations_var[i,], sep = "_")
            print(" ", quote = FALSE)
            print("Taxa value prediction done for the following combination: ")
            #print(colnames(combinations_var)[1])
            for (c in 1:length(combinations_var[i,])) {
              print(paste(colnames(combinations_var)[c], ": ", combinations_var[i,c], sep = ""), quote = FALSE)
            }
            #path_file <- paste(out_dir, filename, sep = "")
            #ifelse(!dir.exists(path_file), dir.create(path_file), FALSE)
            outfile <- paste(path_file, filename, sep = "/")
            outfile.table <- paste(outfile, ".csv", sep = "")
            write.table(df, outfile.table, dec = ",", row.names = FALSE)
            
            #df_filt <- select(df, "Taxa", "Predicted.raw.value")
            #df_filt <- df_filt[df_filt[,2] != "NaN", ]
            
            bn_df_raw_filt_taxas_t <- t(bn_df_raw_filt_taxas)
            bn_df_raw_filt_taxas_t <- as.data.frame(bn_df_raw_filt_taxas_t)
            bn_df_raw_filt_taxas_t_mod <- tibble::rownames_to_column(bn_df_raw_filt_taxas_t, "Sample")
            
            out_df_filt <- paste(path_file, "/raw_counts.txt", sep = "")
            write.table(bn_df_raw_filt_taxas_t_mod, out_df_filt, sep = "\t", row.names = FALSE, quote = FALSE)
            
            sequences_filt <- sequences[names(sequences) %in% rownames(bn_df_raw_filt_taxas_t)]
            out_sequences_filt <- paste(path_file, "/sequences.fasta", sep = "")
            write.fasta(sequences = sequences_filt, names = names(sequences_filt), nbchar = 80, file.out = out_sequences_filt)
            
            # PICRUST con los ficheros generados
            print(" ", quote = FALSE)
            print("Starting metagenome prediction...", quote = FALSE)
            fire_running(" metagenome prediction...")
            use_python("/usr/bin/python")
            use_condaenv(condaenv = "picrust2", required=TRUE)
            import("picrust2.place_seqs")
            import("picrust2.wrap_hsp")
            import("picrust2.metagenome_pipeline")
            import("picrust2.util")
            import("picrust2.pathway_pipeline")
            import("picrust2.default")
            path_python_scripts <- paste(current_dir, "/python/", sep = "")
            #path_python_scripts <- paste(dirname(rstudioapi::getSourceEditorContext()$path), "/python/", sep = "")
            
            setwd(path_file)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            
            print("1. Place study unaligned sequences (i.e. OTUs or ASVs) into a reference tree.", quote = FALSE)
            cmd <- paste(path_python_scripts, "place_seqs.py -s sequences.fasta -o out.tre -p 5 --intermediate intermediate/place_seqs", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE, quote = FALSE)
              stop("User Interrupt")
            }
            print("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.", quote = FALSE)
            cmd <- paste(path_python_scripts, "hsp.py -i 16S -t out.tre -o 16S_predicted_and_nsti.tsv.gz -p 5 -n", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            print("3. Predict the enzymes of gene families present in the predicted genome for each amplicon sequence variant.", quote = FALSE)
            cmd <- paste(path_python_scripts, "hsp.py -i EC -t out.tre -o EC_predicted.tsv.gz -p 5", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            print("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence. 
        The specified sequence abundance table will be normalized by the predicted number of marker gene copies.", quote = FALSE)
            cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i raw_counts.txt -m 16S_predicted_and_nsti.tsv.gz -f EC_predicted.tsv.gz -o metagenome_out/ --strat_out", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            print("5. Convert abundance table.", quote = FALSE)
            cmd <- paste(path_python_scripts, "convert_table.py metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o metagenome_out/pred_metagenome_unstrat.tsv.gz", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            print("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.", quote = FALSE)
            cmd <- paste(path_python_scripts, "pathway_pipeline.py -i metagenome_out/pred_metagenome_contrib.tsv.gz -o pathways_out/", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            print("7. Add description column to metagenome abundance table.", quote = FALSE)
            cmd <- paste(path_python_scripts, "add_descriptions.py -i metagenome_out/pred_metagenome_unstrat.tsv.gz -m EC -o metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz", sep = "")
            system(cmd)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            print("8. Add description column to pathways abundance table.", quote = FALSE)
            cmd <- paste(path_python_scripts, "add_descriptions.py -i pathways_out/path_abun_unstrat.tsv.gz -m METACYC -o pathways_out/path_abun_unstrat_descrip.tsv.gz", sep = "")
            system(cmd)
            print("Metagenome prediction DONE!", quote = FALSE)
            
            setwd(out_dir)
            
            if(interrupted()){ 
              print("Stopping...", quote = FALSE)
              stop("User Interrupt")
            }
            
            sink()
            
            
          }
          #})
        }
      }
      #})
    })
    
    auto_picrust_button <- observeEvent(input$button_auto, {
      disable("button_auto")
      
      current_dir <- this.dir()
      oldw <- getOption("warn")
      sequences <- suppressWarnings({read.fasta(file = input$seqs_auto$datapath)})
      out_dir <- readDirectoryInput(session, 'directory_auto')
      it <- input$iterations_auto
      f <- future({automated_prediction(sequences, out_dir, it, current_dir)}, seed = TRUE)
      #return(NULL)
      f <- catch(f, function(e) {
        print(e$message)
        showNotification(e$message)
      })
      f <- finally(f,
                   function(){
                     fire_ready() 
                     enable("button_auto")
                   })
      
      ## Bloquea el uso de app si pongo barra de progreso
      #withProgress(message = "Calculation in progress", detail = "This may take a while...", value = 0, {
      #with_progress({
      #p <- progressor(steps = 20)
      #f <- future({automated_prediction(sequences, out_dir, it, current_dir, p)}, seed = TRUE)
      #}, handlers = shiny_handler)
      #})
      
    })
    
    observeEvent(input$stop_button_auto, {
      print("Cancel")
      fire_interrupt()
      enable("button_auto")
    })
    
    observeEvent(input$check_button_auto, {
      print("Status")
      showNotification(get_status())
    })
    
    #auto_picrust_button <- observeEvent(input$button_auto, {withCallingHandlers({
    #shinyjs::html(id = "automated_pred", "")
    #sequences <- read.fasta(file = input$seqs_auto$datapath)
    #out_dir <- readDirectoryInput(session, 'directory_auto')
    #it <- input$iterations_auto
    #future({automated_prediction(sequences, out_dir, it)}) %...>% data_promise()
    #return(NULL)
    #},
    #message = function(m) {
    #shinyjs::html(id = "automated_pred", html = paste0(m$message, '<br>', '<br>'), add = TRUE)
    #})
    #})
    
    #auto_button <- eventReactive(input$button_auto, print(automated_prediction()))
    
  })
  
  ## Predict taxa values, CPTs and display graph
  
  ### Read network using observeEvent
  observeEvent(input$network,{
    if ( is.null(input$network)) return(NULL)
    inFile <- isolate({input$network })
    file <- inFile$datapath
    load(file, envir = .GlobalEnv)
    
    variables <- bn_df_variables[,sapply(bn_df_variables, class) == 'factor']
    var_list <- list()
    for (i in 1:ncol(variables)) {
      var_list[[colnames(variables)[i]]] <- levels(variables[,i])
    }
    
    output$selector <- renderUI({
      pickerInput("evidence", "Evidence", choices = var_list, selected = 1, multiple = TRUE,
                  options = pickerOptions("liveSearch" = FALSE, 
                                          # "max-options" = 2,
                                          "max-options-group" = 1,
                                          "selectOnTab" = TRUE,
                                          actionsBox = TRUE,
                                          style = ".bs-select-all { display: none;}"
                                          
                  ))
    })
    
    observeEvent(input$button2, {
      nodes <- strsplit(input$nodes, ",")[[1]]
      nodes <- str_replace_all(nodes, c("/" = ".", " " = ".", "-" = "."))
      
      output$selector_cpt <- renderUI({
        pickerInput("nodes_cpt", label = NULL, choices = nodes, selected = nodes[1], multiple = FALSE,
                    options = pickerOptions("liveSearch" = TRUE, 
                                            # "max-options" = 2,
                                            #"max-options-group" = 1,
                                            #"selectOnTab" = TRUE,
                                            actionsBox = TRUE,
                                            style = "btn-default"
                                            
                    )
        )
      })
      
      selection <- reactive({
        #shiny::validate(need(input$nodes_cpt, message=FALSE))
        return(input$nodes_cpt)})
      
      observeEvent(selection(), {
        output$conditional_table <- renderPrint(print(fittedbn[[selection()]]))
      })
      
      output$save_cpt_all <- downloadHandler(filename = "download_CPTs.zip",
                                             content = function(fname) {
                                               owd <- setwd(tempdir())
                                               on.exit(setwd(owd))
                                               files <- NULL;
                                               for (i in nodes) {
                                                 if (nchar(i) > 200) {
                                                   name <- substr(i, 1, 200)
                                                 } else {
                                                   name <- i
                                                 }
                                                 fileName = paste(name, "_CPT.txt", sep = "")
                                                 sink(fileName)
                                                 print(fittedbn[[i]])
                                                 sink()
                                                 files <- c(fileName, files)
                                               }
                                               zip::zip(fname, files)
                                             }, 
                                             contentType = "application/zip")
      
      output$save_cpt <- downloadHandler(filename = function(){
        if (nchar(selection()) > 200) {
          name <- substr(i, 1, 200)
        } else {
          name <- selection()
        }
        paste(name,"_CPT.txt", sep = "")},
        content = function (fname) {
          sink(fname)
          print(fittedbn[[selection()]])
          sink()
        },
        contentType = "text/csv")
      
    })
    
    #outputOptions(output, "selector", suspendWhenHidden = FALSE)
    
    tableInput <- function(){
      
      ## If statement to create taxa list
      if (input$taxas != "all") {
        taxas <- strsplit(input$taxas, ",")[[1]]
      } else {
        taxas <- nodes(fittedbn)
        taxas <- taxas[ncol(data_variables)+1:length(taxas)]
        taxas <- na.omit(taxas)
      }
      
      ## Split evidence string by comma and create a list
      
      ev <- list()
      
      for (i in input$evidence) {
        res = lapply(var_list, function(x) match(i, x))
        for (j in 1:length(res)) {
          if (!is.na(res[j])) {
            ev[[names(res[j])]] <- i
          }
        }
      }
      
      #evidence <- strsplit(input$evidence, ",")[[1]]
      #for (i in 1:length(evidence)) {
      #    evidence[i] <- strsplit(as.character(evidence[i]), "=")
      #}
      
      #ev <- list()
      
      #for (i in 1:length(evidence)) {
      #    ev[[evidence[[i]][1]]] <- evidence[[i]][2] 
      #}
      
      ## Create output dataframe
      df <- matrix(data = NA, nrow = length(taxas), ncol = 6)
      df <- data.frame(df)
      colnames(df) <- c("Taxa", "Predicted normalized value", "Raw counts average", "Standard deviation","Range of normalized values","Conditional probability")
      
      bn_df_total <- bn_df_norm
      bn_df_raw_filt <- bn_df_raw
      
      bn_df_total_taxas <- bn_df_taxas_norm
      #total <- complete_sum
      
      bn_df_raw_taxas <- bn_df_taxas
      
      for (i in 1:length(ev)) {
        bn_df_raw_filt <- bn_df_raw_filt[bn_df_raw_filt[[names(ev[i])]] %in% ev[[i]][1], ]
      }
      
      bn_df_raw_filt_taxas <- select(bn_df_raw_filt, colnames(bn_df_taxas))
      total_raw_counts <- try(sum(bn_df_raw_filt_taxas)/nrow(bn_df_raw_filt_taxas))
      
      it <- input$iterations
      error_cp <- input$error_network
      
      evidence_cpquery = ""
      for (l in 1:length(ev)) {
        if (l == 1) {
          evidence_cpquery = paste("(", names(ev[l]), " == ", '"', ev[l][[1]], '")', sep = "")
        } else {
          evidence_cpquery = paste(evidence_cpquery, " & ", "(", names(ev[l]), " == ", '"', ev[l][[1]], '")', sep = "")
        }
      }
      
      ## For loop to add data to dataframe
      withProgress(message = "Generating table", detail = "Wait...", value = 0, {
        incProgress(0/length(taxas), detail = paste("0", "/", length(taxas)))
        for (i in 1:length(taxas)){
          if (taxas[i] %in% nodes(fittedbn)){
            df[i,1] <- taxas[i]
            taxas[i] <- str_replace_all(taxas[i], c("/" = ".", " " = ".", "-" = "."))
            #taxas[i] <- sub("/",".", taxas[i])
            #taxas[i] <- sub(" ",".", taxas[i])
            #taxas[i] <- sub("-",".", taxas[i])
            predict <- try(cpdist(fittedbn, nodes = taxas[i], evidence = ev, method = "lw", n = 100000))
            if (class(predict)[1] == "try-error") {
              df[i,2] <- as.character("Cannot be calculated")
            } else {
              no_na <- !is.na(predict)
              pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
              if (!is.nan(pred_val)) {
                vector <- c(as.numeric(pred_val))
                if (it != 1) {
                  for (v in 2:it) {
                    predict <- cpdist(fittedbn, nodes = taxas[i], evidence = ev, method = "lw", n = 100000)
                    no_na <- !is.na(predict)
                    pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
                    if (!is.nan(pred_val)) {
                      vector <- c(vector, as.numeric(pred_val))
                    }
                  }
                  pred_value_log <- median(vector)
                  if (expm1(pred_log_value) >= 0) {
                    df[i,2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
                  } else {
                    df[i,2] <- 0
                  }
                  
                } else {
                  pred_value_log <- pred_val
                  if (expm1(pred_log_value) >= 0) {
                    df[i,2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
                  } else {
                    df[i,2] <- 0
                  }
                }
                
              } else {
                df[i,2] <- as.character("NaN")
              }
            }
            
            if (class(total_raw_counts) == "numeric") {
              media <- mean(bn_df_raw_filt_taxas[[taxas[i]]])
              df[i,3] <- round(media, digits = 0)
            } else {
              df[i,3] <- as.character("Cannot be calculated")
            }
            
            if (class(total_raw_counts) == "numeric") {
              desves <- sd(bn_df_raw_filt_taxas[[taxas[i]]])
              df[i,4] <- round(desves, digits = 2)
            } else {
              df[i,4] <- as.character("Cannot be calculated")
            }
            
            #df[i,2] <- as.numeric(sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na]))
            filt_data <- bn_df_norm
            for (v in 1:length(ev)) {
              filt_data <- filt_data[filt_data[[names(ev[v])]] == ev[[v]][1], ]
            }
            
            average_norm <- round(mean(expm1(filt_data[[taxas[i]]])), digits = 2)
            desvest_norm <- round(sd(expm1(filt_data[[taxas[i]]])), digits = 2)
            
            if ((average_norm - desvest_norm) < 0) {
              low_range <- 0
            } else {
              low_range <- average_norm - desvest_norm
            }
            
            high_range <- average_norm + desvest_norm
            
            df[i,5] <- paste(low_range, "-", high_range)
            
            if ((as.numeric(df[i,3]) == 0) & (as.numeric(df[i,4]) == 0)) {
              df[i,2] <- as.character(0)
            }
            
            if ((df[i,2] != "Cannot be calculated") & (df[i,2] != "NaN")) {
              event_cpquery = paste("((", taxas[i], ">= ", (as.numeric(pred_value_log)-error_cp*as.numeric(pred_value_log)), ") & (", taxas[i], "<= ", (as.numeric(pred_value_log)+error_cp*as.numeric(pred_value_log)), "))", sep = "")
              result_cpquery <- try(eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', evidence_cpquery, ', method = "ls")'))))
              #result_cpquery <- eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', list(ev), ', method = "lw")'))) ## lw method
              if (!is.nan(result_cpquery)) {
                vector2 <- c(as.numeric(result_cpquery))
                if (it != 1) {
                  for (v in 2:it) {
                    result_cpquery <- try(eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', evidence_cpquery, ', method = "ls")'))))
                    if (!is.nan(result_cpquery)) {
                      vector2 <- c(vector2, as.numeric(result_cpquery))
                    }
                  }
                  df[i,6] <- round(as.numeric(median(vector2)), digits = 2)
                } else {
                  df[i,6] <- round(as.numeric(result_cpquery), digits = 2)
                }
                
              } else {
                df[i,6] <- "NaN"
              }
            } else {
              if (df[i,2] == "Cannot be calculated") {
                df[i,6] <- "Cannot be calculated"
              } else {
                df[i,6] <- "NaN"
              }
            }
          } else {
            next
          }
          
          
          #df[i,5] <- eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', list(ev), ', method = "lw")')))
          incProgress(1/length(taxas), detail = paste(i, "/", length(taxas)))
        } 
      })
      
      
      return(df)
      
    }
    
    ## Display table when user clicks on button
    t_button <- eventReactive(input$button1, print(tableInput()))
    
    output$predicted_value <- renderDataTable(
      t_button(), 
      plugins = "natural",
      server = FALSE,
      extensions = c('Buttons'), 
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#696969', 'color': '#fff'});",
          "}"),
        columnDefs=list(list(targets=1:5, class="dt-center")),
        #language = list(lengthMenu = "_MENU_"),
        search = list(regex=TRUE, caseInsensitive=TRUE),
        columnDefs = list(list(type = 'natural', targets = 2)),
        dom = '<"#js"l>Bfrtip', 
        scrollY = TRUE, 
        scrollX = TRUE,
        select = list(style = "multi", items = "row"),
        lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")),
        pageLength = 10,
        buttons = list('copy',
                       list(extend = 'csv', filename = "table"),
                       list(extend = 'excel', filename = "table", title = NULL)))
    )
    
    
  })
  
  observeEvent(input$network_plot,{if ( is.null(input$network_plot)) return(NULL)
    inFile <- isolate({input$network_plot })
    file <- inFile$datapath
    load(file, envir = .GlobalEnv)
    
    ## Plot subgraph of the network
    plotInput <- function()({
      nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
      nodes <- strsplit(nodes, ",")[[1]]
      #for (i in 1:length(nodes)) {
      #nodes[i] <- sub("/",".", nodes[i])
      #nodes[i] <- sub(" ",".", nodes[i])
      #nodes[i] <- str_replace(nodes[i], "-",".")
      #}
      subgr <- bnlearn::subgraph(fittedbn, nodes)
      tryit <- try(strength.viewer(bayesianNetwork = subgr, 
                                   bayesianNetwork.boot.strength = arc_st_mi, 
                                   bayesianNetwork.arc.strength.label = TRUE, 
                                   bayesianNetwork.arc.strength.tooltip = TRUE,
                                   bayesianNetwork.edge.scale.min = 1,
                                   bayesianNetwork.edge.scale.max = 1,
                                   edges.dashes = FALSE,
                                   bayesianNetwork.layout = "layout_nicely"))
      if (inherits(tryit, "try-error")) {
        gr <- bn.to.igraph(subgr)
        p <- visIgraph(gr)
      } else {
        p <- strength.viewer(bayesianNetwork = subgr, 
                             bayesianNetwork.boot.strength = arc_st_mi, 
                             bayesianNetwork.arc.strength.label = TRUE, 
                             bayesianNetwork.arc.strength.tooltip = TRUE,
                             bayesianNetwork.edge.scale.min = 1,
                             bayesianNetwork.edge.scale.max = 1,
                             edges.dashes = FALSE,
                             bayesianNetwork.layout = "layout_nicely",
        )
      }
      
    })
    
    ## Link button to print image
    p_button <- eventReactive(input$button_plot, print(plotInput()))
    #cpt_button <- eventReactive(input$button2, table_cpt())
    
    output$plot_graph <- renderVisNetwork(
      p_button() %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, autoResize = TRUE)
    )
    
    width <- input$width
    height <- input$height
    
    ## Option to save image
    output$save_plot <- downloadHandler(filename = function() {"network.html"}, content = function(file) {
      p_button() %>% visSave(file, selfcontained = TRUE)
    })
    
    output$save_relationships <- downloadHandler(filename = "network_relationships.zip", content = function(fname) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
      nodes <- strsplit(nodes, ",")[[1]]
      subgr <- bnlearn::subgraph(fittedbn, nodes)
      fileName <- "network_relationships.txt"
      sink(fileName)
      print(arcs(subgr), quote = FALSE)
      sink()
      
      subgr.igraph <- as.igraph(subgr)
      fileName_igraph <- "network_relationships_igraph.dot"
      write_graph(subgr.igraph, fileName_igraph, format = "dot")
      
      files <- c(fileName, fileName_igraph)
      
      zip::zip(fname,files)
    }, contentType = "application/zip")
  })
  
  
  
  ## Function to select directory
  #volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  #shinyDirChoose(input, 'directory', roots = volumes)
  
  #observe({
  # cat("\ninput$directory value:\n\n")
  #print(input$directory)
  #})
  
  ## Function to show selected directory
  #output$directorypath <- renderText({
  # if (is.integer(input$directory)) {
  #  cat("No directory has been selected")
  #} else {
  # parseDirPath(volumes, input$directory)
  #}
  #})
  
  ## Infer metagenome
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'directory'),
                          caption="Choose a directory...")
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  output$directory = renderText({
    readDirectoryInput(session, 'directory')
  })
  
  
  output$example_counts <- renderUI({
    tags$a(href="example_raw_counts.txt", target = "blank", "Example raw counts file", download = "example_raw_counts.txt")
  })
  
  output$example_seqs <- renderUI({
    tags$a(href="example_sequences.fasta", target = "blank", "Example sequences file", download = "example_sequences.fasta")
  })
  
  picrust <- function(){
    use_python("/usr/bin/python")
    use_condaenv(condaenv = "picrust2", required=TRUE)
    import("picrust2.place_seqs")
    import("picrust2.wrap_hsp")
    import("picrust2.metagenome_pipeline")
    import("picrust2.util")
    import("picrust2.pathway_pipeline")
    import("picrust2.default")
    message(div(style = "text-align: center", h4(HTML("<b>Executing metagenome inference</b>")), ))
    Sys.sleep(2)
    path_python_scripts <- paste(dirname(rstudioapi::getSourceEditorContext()$path), "/python/", sep = "") ## dirname(rstudioapi::getSourceEditorContext()$path) --> Find location of current script
    message("1. Place study unaligned sequences (i.e. OTUs or ASVs) into a reference tree.")
    cmd <- paste(path_python_scripts, "place_seqs.py -s ", input$seqs$datapath, " -o ", readDirectoryInput(session, 'directory'), "out.tre -p 5 --intermediate ", readDirectoryInput(session, 'directory'), "intermediate/place_seqs", sep = "")
    system(cmd)
    message("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.")
    cmd <- paste(path_python_scripts, "hsp.py -i 16S -t ", readDirectoryInput(session, 'directory'), "out.tre -o ", readDirectoryInput(session, 'directory'), "16S_predicted_and_nsti.tsv.gz -p 5 -n", sep = "")
    system(cmd)
    message("3. Predict the enzymes of gene families present in the predicted genome for each amplicon sequence variant.")
    cmd <- paste(path_python_scripts, "hsp.py -i EC -t ", readDirectoryInput(session, 'directory'), "out.tre -o ", readDirectoryInput(session, 'directory'), "EC_predicted.tsv.gz -p 5", sep = "")
    system(cmd)
    message("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence. 
            The specified sequence abundance table will be normalized by the predicted number of marker gene copies.")
    cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i ", input$counts$datapath, " -m ", readDirectoryInput(session, 'directory'), "16S_predicted_and_nsti.tsv.gz -f ", readDirectoryInput(session, 'directory'), "EC_predicted.tsv.gz -o ", readDirectoryInput(session, 'directory'), "metagenome_out/ --strat_out", sep = "")
    system(cmd)
    message("5. Convert abundance table.")
    cmd <- paste(path_python_scripts, "convert_table.py ", readDirectoryInput(session, 'directory'), "metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o ", readDirectoryInput(session, 'directory'), "metagenome_out/pred_metagenome_unstrat.tsv.gz", sep = "")
    system(cmd)
    message("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.")
    cmd <- paste(path_python_scripts, "pathway_pipeline.py -i ", readDirectoryInput(session, 'directory'), "metagenome_out/pred_metagenome_contrib.tsv.gz -o ", readDirectoryInput(session, 'directory'), "pathways_out/", sep = "")
    system(cmd)
    message("7. Add description column to metagenome abundance table.")
    cmd <- paste(path_python_scripts, "add_descriptions.py -i ", readDirectoryInput(session, 'directory'), "metagenome_out/pred_metagenome_unstrat.tsv.gz -m EC -o ", readDirectoryInput(session, 'directory'), "metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz", sep = "")
    system(cmd)
    message("8. Add description column to pathways abundance table.")
    cmd <- paste(path_python_scripts, "add_descriptions.py -i ", readDirectoryInput(session, 'directory'), "pathways_out/path_abun_unstrat.tsv.gz -m METACYC -o ", readDirectoryInput(session, 'directory'), "pathways_out/path_abun_unstrat_descrip.tsv.gz", sep = "")
    system(cmd)
    message((h4(HTML("DONE!"))))
    
  }
  
  picrust_button <- observeEvent(input$button_picrust, {withCallingHandlers({
    shinyjs::html(id = "predicted_metagenome", "")
    picrust()
  },
  message = function(m) {
    shinyjs::html(id = "predicted_metagenome", html = paste0(m$message, '<br>', '<br>'), add = TRUE)
  })
  })
  
  #output$predicted_metagenome <- renderPrint(picrust_button())
  
})





