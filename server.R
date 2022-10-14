#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
deploy_dir = "/srv/shiny-server/samba_files/"
# deploy_dir <- "/home/data/git/samba/files/"

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
samba_version <- 1

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

#####################################################################
all_roots <- function (f, interval,
  lower = min(interval), upper = max(interval), n = 100L, ...) {
  x <- seq(lower, upper, len = n + 1L)
  fx <- f(x, ...)
  roots <- x[which(fx == 0)]
  fx2 <- fx[seq(n)] * fx[seq(2L,n+1L,by=1L)]
  index <- which(fx2 < 0)
  for (i in index)
    roots <- c(roots, uniroot(f, lower = x[i], upper = x[i+1L], ...)$root)
  return(sort(roots))
}

calc.falpha <- function(x=NULL, den, alpha, nn=5000) {
    # Calculates falpha needed to compute HDR of density den.
    # Also finds approximate mode.
    # Input: den = density on grid.
    #          x = independent observations on den
    #      alpha = level of HDR
    # Called by hdr.box and hdr.conf

    if(is.null(x))
        calc.falpha(x=sample(den$x, nn, replace=TRUE, prob=den$y), den, alpha)
    else
    {
        fx <- approx(den$x,den$y,xout=x,rule=2)$y
        falpha <- quantile(sort(fx), alpha)
        mode <- den$x[den$y==max(den$y)]
        return(list(falpha=falpha,mode=mode,fx=fx))
    }
}

hdr.ends <- function(den,falpha) {
    miss <- is.na(den$x) # | is.na(den$y)
    den$x <- den$x[!miss]
    den$y <- den$y[!miss]
    n <- length(den$x)
    # falpha is above the density, so the HDR does not exist
    if(falpha > max(den$y))
        return(list(falpha=falpha,hdr=NA) )
    f <- function(x, den, falpha) {
      approx(den$x, den$y-falpha, xout=x)$y
    }
    intercept <- all_roots(f, interval=range(den$x), den=den, falpha=falpha)
    ni <- length(intercept)
    # No roots -- use the whole line
    if(ni == 0L)
      intercept <- c(den$x[1],den$x[n])
    else {
      # Check behaviour outside the smallest and largest intercepts
      if(f(0.5*(head(intercept,1) + den$x[1]), den, falpha) > 0)
        intercept <- c(den$x[1],intercept)
      if(f(0.5*(tail(intercept,1) + den$x[n]), den, falpha) > 0)
        intercept <- c(intercept,den$x[n])
    }
    # Check behaviour -- not sure if we need this now
    if(length(intercept) %% 2)
      warning("Some HDRs are incomplete")
      #  intercept <- sort(unique(intercept))
    return(list(falpha=falpha,hdr=intercept))
}


c_hdr <- function (x = NULL, prob = c(50, 95, 99), den = NULL, h = hdrbw(BoxCox(x, lambda), mean(prob)), lambda = 1, nn = 5000, all.modes = FALSE)  {
    if (!is.null(x)) {
        r <- diff(range(x))
        if (r == 0) 
            stop("Insufficient data")
    }
    if (is.null(den)) 
        den <- tdensity(x, bw = h, lambda = lambda)
    alpha <- sort(1 - prob/100)
    falpha <- hdrcde::calc.falpha(x, den, alpha, nn = nn)
    hdr.store <- matrix(NA, length(alpha), 100)
    for (i in 1:length(alpha)) {
        junk <- hdr.ends(den, falpha$falpha[i])$hdr
        if (length(junk) > 100) {
            junk <- junk[1:100]
            warning("Too many sub-intervals. Only the first 50 returned.")
        }
        hdr.store[i, ] <- c(junk, rep(NA, 100 - length(junk)))
    }
    cj <- colSums(is.na(hdr.store))
    hdr.store <- matrix(hdr.store[, cj < nrow(hdr.store)], nrow = length(prob))
    rownames(hdr.store) <- paste(100 * (1 - alpha), "%", sep = "")
    if (all.modes) {
        y <- c(0, den$y, 0)
        n <- length(y)
        idx <- ((y[2:(n - 1)] > y[1:(n - 2)]) & (y[2:(n - 1)] > 
            y[3:n])) | (den$y == max(den$y))
        mode <- den$x[idx]
    }
    else mode <- falpha$mode
    return(list(hdr = hdr.store, mode = mode, falpha = falpha$falpha))
}



######################   Network methods ############################


to_log <- function(bn_df, nodes) {
  bn_df.log = bn_df
  for (node in nodes) {
    bn_df.log[, node] = log1p(bn_df.log[, node])
  }
  bn_df.log
}

inverse_log <- function(bn_df.log, nodes) {
  bn_df.invlog = bn_df.log
  for (node in nodes) {
    bn_df.invlog[, node] = exp1m(bn_df.invlog[, node])
  }
  bn_df.invlog
}

nomralize_data <-
  function(bn_df_taxas,
           bn_df_taxas.col_sum,
           bn_df_taxas.row_sum) {
    sample.names = rownames(bn_df_taxas)
    taxa.names = colnames(bn_df_taxas)
    for (sample in sample.names) {
      for (taxa in taxa.names) {
        raw_value = bn_df_taxas[sample, taxa]
        normalized.value = raw_value * bn_df_taxas.col_sum[taxa] / bn_df_taxas.row_sum[sample]
        bn_df_taxas[sample, taxa] = normalized.value
      }
    }
    invisible(bn_df_taxas)
  }




create_evidence_cpquery <- function(combination_vars) {
  #
  # create evidence from cpquery
  # 
  for (l in 1:length(combination_vars)) {
    if (l == 1) {
      evidence_cpquery = paste("(", names(combination_vars[l]), " == ", '"', combination_vars[1,l], '")', sep = "")
    } else {
      evidence_cpquery = paste(evidence_cpquery, " & ", "(", names(combination_vars[l]), " == ", '"', combination_vars[1,l], '")', sep = "")
    }
  }
  evidence_cpquery
}
create_evidence_cpdist <- function(combination_vars) {
  #
  # create evidence from cpdist and sampling function
  # 
  l_var <- list()
  for (j in 1:length(combination_vars[1,])) {
    l_var[[names(combination_vars)[j]]] <- combination_vars[1,j]
  }
  l_var
}

filter_samples_by_weights <- function(in_samples, min_weight) {
  ## TODO :: input must have weights attr otherwise fail
  in_samples_w  <- attr(in_samples, "weights")
  in_samples <- in_samples[in_samples_w >= min_weight]
  in_samples_w <- in_samples_w[in_samples_w >= min_weight]
  attr(in_samples, "weights") <- in_samples_w
  in_samples
}

n_matchs_to_weight <- function(n,c=1){
  w = c/n
  1/(1+exp(-(  (w-0.5)*11  )))
}

data.sampling <- function(
            in.df,
            node, evidence,
            n.samples = 1000,
            min_weight = 0.5 ) {
  ## TODO :: add min mismatch as it is strait to assign weight to n mismatch outside 
  require(dplyr)
  samples.cols <-  c(node,names(evidence))
  in.df <- in.df[,samples.cols]
  x.samples <- in.df %>% slice_sample(n = n.samples*2, replace = TRUE)
  x.samples$samples_weights__ = 0
  for (v in names(evidence)) {
    matched.samples <- x.samples[v] == as.character(evidence[[v]])
    x.samples$samples_weights__[matched.samples] <- x.samples$samples_weights__[matched.samples] +1
  }
  x.samples$samples_weights__ = x.samples$samples_weights__/length(evidence)
  #x.samples$samples_weights__ = (x.samples$samples_weights__ - 0.5)*5
  x.samples$samples_weights__ = round(1/(1+exp(-(  (x.samples$samples_weights__-0.5)*11  ))),2)
  samples <- x.samples[[node]]
  samples = samples[x.samples$samples_weights__ >= min_weight]
  samples.w = x.samples$samples_weights__[x.samples$samples_weights__ >= min_weight]
  if(length(samples) > n.samples) {
    samples <- samples[1:n.samples]
    samples.w <- samples.w[1:n.samples]
  }
  attr(samples, "weights") <- samples.w
  samples
}


network.sampling <- function(fittedbn, node, evidence, n.samples = 10000 , min_weight = 0.5 ) {
  all.predicted.values = c()
  all.predicted.values.w = c()
  predicted.values <- try(cpdist(fittedbn, nodes = node, evidence = evidence, method = "lw", n = n.samples))
  if (class(predicted.values)[1] == "try-error") {
    all.predicted.values <- NULL
  }
  else {
    while(TRUE) {
        ## remove NA first
        predicted.values.no_na <- !is.na(predicted.values)
        w =  attr(predicted.values, "weights")
        w = w[predicted.values.no_na]
        predicted.values = predicted.values[predicted.values.no_na]
        predicted.values = predicted.values[w >= min_weight]
        w = w[w >= min_weight]
        
        ## TODO :: with a random p of 0.7 reject some of  zero
        w = w[predicted.values>=0]
        predicted.values = predicted.values[predicted.values>=0]
        
        all.predicted.values = c(all.predicted.values,predicted.values)
        all.predicted.values.w = c(all.predicted.values.w,w)
        if(length(all.predicted.values) >= n.samples )
          break
        predicted.values <- try(cpdist(fittedbn, nodes = node, evidence = evidence, method = "lw", n = n.samples))
        
      }
      all.predicted.values <- all.predicted.values[1:n.samples]
      attr(all.predicted.values,"weights") <- all.predicted.values.w[1:n.samples]
  }
  all.predicted.values
}

get.mixed.samples <- function(fittedbn, 
                        org.data,node,
                        evidence, 
                        n.samples = 10000, 
                        samples.w = 0.5,
                        org_data_min_weight = 1,
                        samples_min_weight = 0.5,
                        HPDI_correct = TRUE
                        ){
  ## TODO :: check on samples.w
  require(Hmisc)
  require(rethinking)
  network.samples.values <- network.sampling(fittedbn, node, evidence, n.samples =  n.samples , min_weight = samples_min_weight)
  if( !is.null(network.samples.values)) {
    network.samples.weights <- attr(network.samples.values, "weights")
    
    if (HPDI_correct) {
      ## correct for %97
      HPDI_correct_value = 0.99
      if(is.numeric(HPDI_correct)){
        HPDI_correct_value  = HPDI_correct
      }
      network.samples.95HPDI = HPDI(network.samples.values ,prob = HPDI_correct_value)
      #network.samples.weights <-network.samples.weights[network.samples.values > network.samples.95HPDI[1] &
      #                                                  network.samples.values  < network.samples.95HPDI[2]]
      #network.samples.values <- network.samples.values[network.samples.values > network.samples.95HPDI[1] &
      #                                                      network.samples.values < network.samples.95HPDI[2]]
      network.samples.weights <-network.samples.weights[network.samples.values  < network.samples.95HPDI[2]]
      network.samples.values <- network.samples.values[network.samples.values < network.samples.95HPDI[2]]
    }

    network.samples.raw.values <- network.samples.values
    network.samples.values <- round(expm1(network.samples.values))
    attr(network.samples.raw.values, "weights") <- network.samples.weights
    attr(network.samples.values, "weights") <- network.samples.weights

    network.samples.quantile = wtd.quantile(network.samples.values,weights = network.samples.weights)

  }

  # wtd.mean(expm1(network.samples.values),weights = network.samples.weights)
  org.samples.raw.values <- data.sampling(org.data, node, evidence, n.samples =  n.samples , min_weight = org_data_min_weight)
  org.samples.weights <- attr(org.samples.raw.values, "weights")
  org.samples.values <- round(expm1(org.samples.raw.values))
  attr(org.samples.values, "weights") <- org.samples.weights
  org.data.quantile = wtd.quantile(org.samples.values,weights = org.samples.weights)

  if(!is.null(network.samples.values)) {
   list(
        network.samples.raw.values = network.samples.raw.values,
        network.samples.values = network.samples.values,
        network.samples.weights = network.samples.weights,
        org.samples.raw.values = org.samples.raw.values,
        org.samples.values = org.samples.values,
        org.samples.weights = org.samples.weights,
        network.samples.average = wtd.mean(network.samples.values,network.samples.weights),
        network.samples.sd = sqrt(wtd.var(network.samples.values,network.samples.weights)),
        org.samples.average = wtd.mean(org.samples.values,org.samples.weights),
        org.samples.sd = sqrt(wtd.var(org.samples.values,org.samples.weights)),
        network.samples.quantile = network.samples.quantile,
        org.data.quantile = org.data.quantile,
        weighted.quantiles = round((org.data.quantile*(1-samples.w)+network.samples.quantile*samples.w),0)
       )
  } else {
     list(network.samples.values = NULL,
      org.samples.raw.values = org.samples.raw.values,
       org.samples.values = org.samples.values,
       org.samples.weights = org.samples.weights,
       org.samples.average = wtd.mean(org.samples.values,org.samples.weights),
       org.samples.sd = sqrt(wtd.var(org.samples.values,org.samples.weights)),
       org.data.quantile = org.data.quantile
       )
  }
}



filter_by_evidence <- function (bn_df_input,l_var){
  filt_data <- bn_df_input
  for (v in 1:length(l_var)) {
    filt_data <- filt_data[filt_data[[names(l_var[v])]] == l_var[[v]][1], ]
  }
  filt_data
}




calc.prob <- function(samples,start.value,end.value){
  round(sum(samples>=start.value & samples<= end.value)/length(samples),2)
}

get_posterior_dist <- function(network_samples,proir_data = NULL , adjust_proir = 0.7,
        adjust_samples = 0.4, uniform_proir = NULL) {
          ## TODO :: adjust_proir : large value indicate weak proir and should be realiable
          ## smaller values : strong proir and will strongly influence the final prob
          ## adjust_samples goold values are between 0.7-0.3 : it just smooth the prediction 
   if(!is.null(proir_data)) {
    max_range <- max(max(network_samples), max(proir_data)) + 1
    proir_data_w <- attr(proir_data, "weights")
    if(!is.null(proir_data_w)  ) {
      if (sum(proir_data_w) != 1) proir_data_w <- proir_data_w/sum(proir_data_w)
    }


    network_samples_w <- attr(network_samples, "weights")
    if(!is.null(network_samples_w)  ) {
      if (sum(network_samples_w) != 1) network_samples_w <- network_samples_w/sum(network_samples_w)
    }
    
    
    dd_proir <- density(proir_data, adjust = adjust_proir, from=0, t = max_range , n = 1000 , weights = proir_data_w)
    dd_samples <- density(network_samples, adjust =adjust_samples, from=0, t = max_range , n = 1000 ,weights = network_samples_w)
    ## TODO :: add uniform proir to the org data
    if( !is.null(uniform_proir)) dd_proir$y = dd_proir$y + uniform_proir

    dd_proir_p <- dd_proir$y
    #if(ceiling(max(proir_data)) > 0)
    #  dd_proir_p[dd_proir$x > ceiling(max(proir_data)) & dd_proir_p > 0] <- 0.001

    posterior <- round(dd_proir_p * dd_samples$y,5)
    #posterior <- posterior / sum(posterior)

    list(data_value = dd_samples$x,
    posterior_w = posterior,
    posterior_p = posterior / sum(posterior))
   } else {
    max_range <- max(network_samples)
    dd_samples = density(network_samples, adjust =adjust_samples, from=0, t = max_range , n = 1000)
    list(data_value = dd_samples$x,
        posterior_w = dd_samples$y,
        posterior_p = dd_samples$y/sum(dd_samples$y))
   }
}

posterior_stats <- function(posterior_dist, link = expm1) {
  transfromed_data = link(posterior_dist$data_value)
  post_samples <- sample(x = transfromed_data, size = 1000000,prob = posterior_dist$posterior_w , replace = TRUE)
  posterior_dist$post_samples <- post_samples
  #posterior_dist$posterior_mean = wtd.mean(link(posterior_dist$data_value),posterior_dist$posterior_w)
  #posterior_dist$posterior_mean = round(mean(post_samples))
  #posterior_dist$posterior_sd <- round(sd(post_samples))
  #posterior_dist$posterior_quantile <- round(quantile(post_samples))
  
  #posterior_dist$posterior_mean = wtd.mean(link(posterior_dist$data_value),posterior_dist$posterior_w)
  posterior_dist$posterior_mean <- round(wtd.mean(transfromed_data,posterior_dist$posterior_w))
  posterior_dist$posterior_sd <- round(sqrt(wtd.var(transfromed_data,posterior_dist$posterior_w)),2)
  posterior_dist$posterior_quantile <- round(wtd.quantile(transfromed_data,posterior_dist$posterior_w))
  
  
  posterior_dist
}

#####################################################################


# Define server logic required to use the functions
shinyServer(function(input, output, session) {

  output$show_graph <- reactive({
       0
      })
  outputOptions(output, 'show_graph', suspendWhenHidden = FALSE)

  # group variable with a reactive object associated with the current session
  ## TODO :: what is the best way to represent this ?
  session_data <- reactive({
    res <- new.env()
    print("in session_data")
    #browser()
    res$fittedbn <- NULL
    res$input_network_file <- input$input_network_file
    if(!is.null(res$input_network_file)) {
      res$input_network_filename <- res$input_network_file$name
      inFile <- isolate({
      input$input_network_file
          })
      file <- inFile$datapath
      load(file, envir = res)

      # load(file, envir = session$userData)
      load(file, envir =.GlobalEnv)
      output$show_graph <- reactive({
        with (res, {
          show_graph_method()
          show_prediction_panel()
          show_cpts_panel()
          }
        )
        
        1
      })
      outputOptions(output, 'show_graph', suspendWhenHidden = FALSE)
    }

    #res$data_variables <- input$data_variables

    return(res)
  })

  output$current_network_info <- renderUI({
    #browser()
    if(is.null(session_data()$input_network_file)) {
      return(h4("No currect active Network"))
    }
    # if(is.null(session_data()$data_variables)) {
    #   return(h4("No currect active Network"))
    # }
    return(h4(paste0("IN  ", session_data()$input_network_file$name)))
    })

  status_file <- tempfile()

  get_status <- function() {
    scan(status_file, what = "character", sep = "\n")
  }

  set_status <- function(msg) {
    write(msg, status_file)
  }

  fire_interrupt <- function() {
    set_status("interrupt")
  }

  fire_ready <- function() {
    set_status("Ready")
  }

  fire_running <- function(perc_complete) {
    if (missing(perc_complete)) {
      msg <- "Running..."
    } else {
      msg <- paste0("Running... ", perc_complete)
    }
    set_status(msg)
  }

  interrupted <- function() {
    get_status() == "interrupt"
  }

  # Delete file at end of session
  onStop(function() {
    print(status_file)
    if (file.exists(status_file)) {
      unlink(status_file)
    }
  })

  # Create Status File
  fire_ready()

  ## Learning and training network model


  #  observeEvent(
  #    ignoreNULL = TRUE,
  #    eventExpr = {
  #      input$directory_net
  #    },
  #    handlerExpr = {
  #      if (input$directory_net > 0) {
  #        # condition prevents handler execution on initial app launch
  #        path = choose.dir(default = readDirectoryInput(session, 'directory_net'),
  #                          caption="Choose a directory...")
  #        updateDirectoryInput(session, 'directory_net', value = path)
  #      }
  #    }
  #  )
  #
  #  output$directory_net = renderText({
  #    readDirectoryInput(session, 'directory_net')
  #  })


  create_model <- function(data_variables, data_taxas, expVar, net_dir, blacklist, whitelist, bl, wl, dismethod, netscore, thr_mi, thr_bic, filterTaxa, filterThrG, filterThrT, filterOption, filterVariable, filterCountsT, filterCountsG, filterBA) {
    ({
      log_file <- file.path(net_dir, "log_file.txt")
      # browser()
      sink(log_file)
      fire_running("Reading input files")
      print("Reading input files", quote = FALSE)
      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }
      # data_variables = read.csv(file = input$data_variables$datapath, sep = ";", dec = ",", row.names = 1, header = T, stringsAsFactors = TRUE)
      # data_taxas = read.csv(file = input$data_taxas$datapath, sep = ";", dec = ",", row.names = 1, header = T, stringsAsFactors = TRUE)

      bn_df_variables <- data.frame(data_variables)
      bn_df_taxas <- data.frame(data_taxas)

      for (i in 1:ncol(bn_df_variables)) {
        c <- class(bn_df_variables[, i])
        if (c == "integer") {
          bn_df_variables[, i] <- as.numeric(bn_df_variables[, i])
        }
      }

      for (i in 1:ncol(bn_df_taxas)) {
        c <- class(bn_df_taxas[, i])
        if (c == "integer") {
          bn_df_taxas[, i] <- as.numeric(bn_df_taxas[, i])
        }
      }

      # FILTRADO POR PORCENTAJE BEFORE
      to_remove <- c()

      if (filterTaxa == 1) {
        if (filterBA == "Before") {
          if (filterOption == "Total") {
            min_samples <- round(nrow(bn_df_taxas) * filterThrT / 100)
            for (i in 1:ncol(bn_df_taxas)) {
              counts <- sum(as.numeric(bn_df_taxas[, i]) >= as.numeric(filterCountsT))
              if (counts < min_samples) {
                to_remove <- c(to_remove, i)
              }
            }
          } else if (filterOption == "Group") {
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
            col_tax <- ncol(bn_df_variables) + 1
            for (i in filterThrG_sep2) {
              filt <- df_divided[[i[1]]]
              min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
              for (j in col_tax:ncol(df_complete)) {
                counts <- sum(as.numeric(filt[, j]) >= as.numeric(filterCountsG))
                if (counts < min_samples) {
                  to_remove <- c(to_remove, as.numeric(j) - columns_var)
                }
              }
            }
          }
          to_remove <- unique(to_remove)
          # bn_df_taxas <- bn_df_taxas[, -to_remove]
          output_new_taxa <- file.path(net_dir, "filtered_taxa.csv")
          write.table(bn_df_taxas, file = output_new_taxa, dec = ",", sep = ";")
        }
      }

      # expVar <- strsplit(input$exp_var, ",")[[1]]

      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }


      if (length(expVar) != 0) {
        fire_running("Discretizing experimental continuous variables")
        print("Discretizing experimental continuous variables", quote = FALSE)
        if (length(expVar) == 1) {
          if (dismethod != "hartemink") {
            var_discretize <- discretize(as.data.frame(bn_df_variables[[expVar]]), method = dismethod, breaks = 5, ordered = FALSE)
            bn_df_variables[[expVar]] <- var_discretize[, 1]
            bn_df_variables[[expVar]] <- factor(bn_df_variables[[expVar]], levels = rev(unique(var_discretize[, 1])), ordered = FALSE)
          } else {
            fire_running("Can't use hartemink, at least two variables are needed to compute mutual information. Using quantile method instead.")
            var_discretize <- discretize(as.data.frame(bn_df_variables[[expVar]]), method = "quantile", breaks = 5, ordered = FALSE)
            bn_df_variables[[expVar]] <- factor(bn_df_variables[[expVar]], levels = rev(unique(var_discretize[, 1])), ordered = FALSE)
          }
        } else {
          if (dismethod != "hartemink") {
            for (i in expVar) {
              if (class(bn_df_variables[[i]]) == "numeric") {
                var_discretize <- discretize(as.data.frame(bn_df_variables[[i]]), method = dismethod, breaks = 5, ordered = FALSE)
                bn_df_variables[[i]] <- var_discretize[, 1]
                bn_df_variables[[i]] <- factor(bn_df_variables[[i]], levels = rev(unique(var_discretize[, 1])), ordered = FALSE)
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
        if (class(bn_df_variables[, i]) == "numeric") {
          sp <- shapiro.test(bn_df_variables[, i])
          if (sp$p.value < 0.05) {
            bn_df_variables[, i] <- log1p(bn_df_variables[, i])
          }
        }
      }

      # bn_df_variables[bn_df_variables=="-Inf"]<- -1000

      data_raw <- cbind(bn_df_variables, bn_df_taxas)
      bn_df_raw <- as.data.frame(data_raw)

      dis_exp_variables <- bn_df_variables %>% select_if(is.factor)
      con_exp_variables <- bn_df_variables %>% select_if(is.numeric)

      fire_running("Creating model and training datasets")
      print("Creating model and training datasets", quote = FALSE)
      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }

      combinations_var <- distinct(dis_exp_variables)

      # data_model <- data.frame()
      # data_training <- data.frame()

      # l_var <- list()

      # for (i in 1:nrow(combinations_var)) {
      # for (j in 1:length(combinations_var[i,])) {
      # l_var[[names(combinations_var)[j]]] <- combinations_var[i,j]
      # }
      # comb_df <- bn_df_variables

      # for (j in 1:length(l_var)) {
      # comb_df <- comb_df[comb_df[[names(l_var[j])]] == l_var[[j]][1], ]
      # }
      # df1 <- comb_df[1:round(nrow(comb_df)/2), ]
      # df2 <- comb_df[round(nrow(comb_df)/2)+1:nrow(comb_df), ]
      # data_training <- rbind(data_training, df1)
      # data_model <- rbind(data_model, df2)
      # }

      # data_model <- data_model[complete.cases(data_model), ]
      # data_training <- data_training[complete.cases(data_training), ]

      fire_running("Normalizing taxa raw counts")
      print("Normalizing taxa raw counts", quote = FALSE)
      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }

      # bn_df_taxas_norm <- bn_df_taxas

      bn_df_taxas.col_sum <- colSums(bn_df_taxas)
      bn_df_taxas.row_sum <- rowSums(bn_df_taxas)

      bn_df_taxas_norm <- nomralize.data(bn_df_taxas, bn_df_taxas.col_sum, bn_df_taxas.row_sum)

      # for (r in 1:nrow(bn_df_taxas)) {
      #   sample_sum = sum(bn_df_taxas[r,])
      #   for (c in 1:ncol(bn_df_taxas)) {
      #     col_sum = sum(bn_df_taxas[, c])
      #     norm_value = bn_df_taxas[r,c]*col_sum/sample_sum
      #     if (!is.na(norm_value)){
      #       bn_df_taxas_norm[r,c] <- norm_value
      #     } else {
      #       bn_df_taxas_norm[r,c] <- 0
      #     }

      #   }
      # }

      output_norm <- file.path(net_dir, "taxa_norm_counts.csv")
      write.table(bn_df_taxas_norm, file = output_norm, dec = ",", sep = ";")

      fire_running("Applying log scale to normalized taxa data")
      print("Applying log scale to normalized taxa data", quote = FALSE)
      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }

      bn_df_taxa_norm_log <- to_log(bn_df_taxas_norm, colnames(bn_df_taxas_norm))

      output_log <- file.path(net_dir, "taxa_norm_log_counts.csv")
      write.table(bn_df_taxa_norm_log, file = output_log, dec = ",", sep = ";")


      if (filterTaxa == 1) {
        if (filterBA == "Before") {
          bn_df_taxas <- bn_df_taxas[, -to_remove]
          bn_df_taxas_norm <- bn_df_taxas_norm[, -to_remove]
          bn_df_taxa_norm_log <- bn_df_taxa_norm_log[, -to_remove]
        }
      }

      # bn_df_model <- cbind(bn_df_variables[rownames(data_model),],bn_df_taxas_norm[rownames(data_model),])
      # data_model <- bn_df_model
      # bn_df_training <- cbind(bn_df_variables[rownames(data_training),],bn_df_taxas_norm[rownames(data_training),])
      # data_training <- bn_df_training

      bn_df_norm <- cbind(bn_df_variables, bn_df_taxa_norm_log)

      fire_running("Creating network model")
      print("Creating network model", quote = FALSE)
      if (interrupted()) {
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
      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }

      remove_arcs <- data.frame()
      result_filt <- result

      arc_st_bic <- arc.strength(result, bn_df_norm, criterion = "bic-cg")
      arc_st_mi <- arc.strength(result, bn_df_norm, criterion = "mi-cg")

      n <- 0

      for (l in 1:nrow(arc_st_bic)) {
        if ((arc_st_bic[l, 3] < thr_bic) && (arc_st_mi[l, 3] < thr_mi)) {
          n <- n + 1
        } else {
          row <- c(arc_st_bic[l, 1], arc_st_bic[l, 2])
          remove_arcs <- rbind(remove_arcs, row)
        }
      }

      out_remove <- file.path(net_dir, "removed_arcs.txt")
      write.table(remove_arcs, out_remove, sep = "\t", dec = ",")

      if (length(remove_arcs) != 0) {
        for (i in 1:nrow(remove_arcs)) {
          d <- data.frame(from = remove_arcs[i, 1], to = remove_arcs[i, 2])
          comparison <- compare::compare(d, wl, allowAll = TRUE)
          if (isFALSE(comparison)) {
            result_filt <- drop.arc(result_filt, remove_arcs[i, 1], remove_arcs[i, 2])
          }
        }
      }

      fire_running("Training model")
      print("Training model", quote = FALSE)
      if (interrupted()) {
        print("Stopping...", quote = FALSE)
        stop("User Interrupt")
      }

      fittedbn <- bn.fit(result_filt, data = bn_df_norm, replace.unidentifiable = TRUE)

      fire_running("Writing output files")
      print("Writing output files", quote = FALSE)
      if (interrupted()) {
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

      # FILTRADO POR PORCENTAJE AFTER

      if (filterTaxa == 1) {
        if (filterBA == "After") {
          if (filterOption == "Total") {
            filterThrT_sep <- strsplit(filterThrT, ",")
            for (thr in filterThrT_sep[[1]]) {
              print(thr)
              to_remove <- c()
              min_samples <- round(nrow(bn_df_taxas) * as.numeric(thr) / 100)
              for (i in 1:ncol(bn_df_taxas)) {
                counts <- sum(as.numeric(bn_df_taxas[, i]) >= as.numeric(filterCountsT))
                if (counts < min_samples) {
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
              out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_", thr, "_network.RData", sep = "")
              output_file_net <- file.path(net_dir, out_net_name)
              save(list = ls(), file = output_file_net, envir = environment())
              result <- result1
              bn_df_norm <- bn_df_norm1
            }
          } else if (filterOption == "Group") {
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
            col_tax <- ncol(bn_df_variables) + 1
            for (i in filterThrG_sep2) {
              filt <- df_divided[[i[1]]]
              min_samples <- round(nrow(filt) * as.numeric(i[2]) / 100)
              for (j in col_tax:ncol(df_complete)) {
                counts <- sum(as.numeric(filt[, j]) >= as.numeric(filterCountsG))
                if (counts < min_samples) {
                  to_remove <- c(to_remove, as.numeric(j) - columns_var)
                }
              }
            }
            result1 <- result
            bn_df_norm1 <- bn_df_norm
            to_remove <- unique(to_remove)
            # bn_df_taxas <- bn_df_taxas[, -to_remove]
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
            out_net_name <- paste(format(Sys.time(), "%F_%H.%M.%S"), "_filtGroup_network.RData", sep = "")
            output_file_net <- file.path(net_dir, out_net_name)
            save(list = ls(), file = output_file_net, envir = environment())
            result <- result1
            bn_df_norm <- bn_df_norm1
          }
        }
      }

      fire_running("DONE!")
      print("DONE!", quote = FALSE)
      sink()
    })
  }

  create_model_button <- observeEvent(input$start_net, {
    disable("start_net")
    # create_model()
    # if(local_test)
    #   current_dir = paste(getwd(),"/",sep = "")
    # else
    #   current_dir <- this.dir()
    data_variables <- fread(file = input$data_variables$datapath, sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE)
    data_taxas <- fread(file = input$data_taxas$datapath, sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE)
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
    filterBA <- input$before_after_filter
    if (blacklist == 1) {
      bl <- fread(file = input$black_file$datapath, sep = "auto", dec = ".", header = T)
      # bl[] <- lapply(bl, function(x) gsub("/", ".", x))
      # bl[] <- lapply(bl, function(x) gsub(" ", ".", x))
      # bl[] <- lapply(bl, function(x) gsub("-", ".", x))
    }

    if (whitelist == 1) {
      wl <- fread(file = input$white_file$datapath, sep = "auto", dec = ".", header = T)
      # wl[] <- lapply(wl, function(x) gsub("/", ".", x))
      # wl[] <- lapply(wl, function(x) gsub(" ", ".", x))
      # wl[] <- lapply(wl, function(x) gsub("-", ".", x))
    }

    dismethod <- input$dis_method
    netscore <- input$net_score
    thr_mi <- input$mi_thr
    thr_bic <- input$bic_thr
    # browser()
    # readDirectoryInput(session, 'directory_net')
    net_dir <- paste(deploy_dir, input$directory_net, sep = "")
    dir.create(net_dir)
    f <- future(
      {
        create_model(data_variables, data_taxas, expVar, net_dir, blacklist, whitelist, bl, wl, dismethod, netscore, thr_mi, thr_bic, filterTaxa, filterThrG, filterThrT, filterOption, filterVariable, filterCountsT, filterCountsG, filterBA)
      },
      seed = TRUE
    )
    # return(NULL)
    f <- catch(f, function(e) {
      print(e$message)
      showNotification(e$message)
    })
    f <- finally(
      f,
      function() {
        fire_ready()
        enable("start_net")
      }
    )
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
    tags$a(href = "example_blacklist_whitelist.csv", target = "blank", "Example blacklist/whitelist file", download = "example_blacklist_whitelist.csv")
  })

  output$example_wl <- renderUI({
    tags$a(href = "example_blacklist_whitelist.csv", target = "blank", "Example blacklist/whitelist file", download = "example_blacklist_whitelist.csv")
  })

  output$example_data_variables <- renderUI({
    tags$a(href = "example_data_variables.csv", target = "blank", "Example variables file", download = "example_data_variables.csv")
  })

  output$example_data_taxas <- renderUI({
    tags$a(href = "example_data_taxas.csv", target = "blank", "Example taxas file", download = "example_data_taxas.csv")
  })

  ## Automated taxa values prediction

  # observeEvent(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$directory_auto
  #   },
  #   handlerExpr = {
  #     if (input$directory_auto > 0) {
  #       # condition prevents handler execution on initial app launch
  #       path = choose.dir(default = readDirectoryInput(session, 'directory_auto'),
  #                         caption="Choose a directory...")
  #       updateDirectoryInput(session, 'directory_auto', value = path)
  #     }
  #   }
  # )

  output$directory_auto <- renderText({
    readDirectoryInput(session, "directory_auto")
  })

  ### Read network using observeEvent
  observeEvent(input$network_auto, {
    if (is.null(input$network_auto)) {
      return(NULL)
    }
    inFile <- isolate({
      input$network_auto
    })
    file <- inFile$datapath
    load(file, envir = .GlobalEnv)

    taxas <- nodes(fittedbn)
    taxas <- taxas[ncol(data_variables) + 1:length(taxas)]
    taxas <- na.omit(taxas)

    error_cp <- input$error_network_auto

    automated_prediction <- function(sequences, out_dir, it, current_dir) {
      ({
        # withProgressShiny(message = "Calculation in progress", detail = "Wait...", value = 0, {
        # p1 <- progressor(steps = nrow(combinations_var))
        nodes_complete <- nodes(fittedbn)
        files <- NULL
        tmpDir <- getwd()
        setwd(out_dir)
        for (i in nodes_complete) {
          if (nchar(i) > 200) {
            name <- substr(i, 1, 200)
          } else {
            name <- i
          }
          fileName <- paste(name, "_CPT.txt", sep = "")
          # fileName <- paste(current_dir,fileName,sep = "")
          sink(fileName)
          print(fittedbn[[i]])
          sink()
          files <- c(fileName, files)
        }
        zipname <- paste(out_dir, "nodes_CPT.zip", sep = "//")
        zip::zip(zipname, files)
        setwd(tmpDir)
        for (i in 1:nrow(combinations_var)) {
          filename <- paste("combination", i, sep = "_")
          if (.Platform$OS.type == "windows") {
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

          if (.Platform$OS.type == "windows") {
            setwd(out_dir)
            ifelse(!dir.exists(path_file), dir.create(filename), FALSE)
            setwd(current_dir)
          } else {
            ifelse(!dir.exists(path_file), dir.create(path_file), FALSE)
          }

          sink(filepath)
          ronda <- paste(i, " of ", nrow(combinations_var), sep = "")
          # p1(sprintf(ronda))
          # print(i)

          evidence_cpquery <- create_evidence_cpquery(combinations_var[i, ])
          # for (l in 1:length(combinations_var)) {
          #   if (l == 1) {
          #     evidence_cpquery = paste("(", names(combinations_var[l]), " == ", '"', combinations_var[i,l], '")', sep = "")
          #   } else {
          #     evidence_cpquery = paste(evidence_cpquery, " & ", "(", names(combinations_var[l]), " == ", '"', combinations_var[i,l], '")', sep = "")
          #   }
          # }

          l_var <- create_evidence_cpquery(combinations_var[i, ])
          # for (j in 1:length(combinations_var[i,])) {
          #   l_var[[names(combinations_var)[j]]] <- combinations_var[i,j]
          # }

          df <- matrix(data = NA, nrow = length(taxas), ncol = 6)
          df <- data.frame(df)
          colnames(df) <- c("Taxa", "Predicted.normalized.value", "Raw.counts.average", "Standard.deviation", "Range.of.normalized.values", "Conditional.probability")
          bn_df_total <- bn_df_norm
          bn_df_raw_filt <- bn_df_raw

          bn_df_total_taxas <- bn_df_taxas_norm
          # total <- complete_sum

          bn_df_raw_taxas <- bn_df_taxas

          for (j in 1:length(l_var)) {
            bn_df_raw_filt <- bn_df_raw_filt[bn_df_raw_filt[[names(l_var[j])]] == l_var[[j]][1], ]
          }

          if (nrow(bn_df_raw_filt) == 0) {
            next
          } else {
            bn_df_raw_filt_taxas <- select(bn_df_raw_filt, colnames(bn_df_taxas))
            total_raw_counts <- try(sum(bn_df_raw_filt_taxas) / nrow(bn_df_raw_filt_taxas))
            # browser()
            ## For loop to add data to dataframe
            # withProgressShiny(message = "Predicting taxa values and predicting metagenome ", detail = "Wait...", value = 0, {
            # p2 <- progressr::progressor(steps = 20, initiate = TRUE, scale = 1L, offset = 0L)
            for (t in 1:length(taxas)) {
              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              ronda2 <- paste("Experimental variables combination ", ronda, " & Taxa ", t, " of ", length(taxas), sep = "")
              fire_running(ronda2)
              # p(sprintf(ronda2))
              print(ronda2, quote = FALSE)
              # p2(sprintf(ronda2))
              df[t, 1] <- taxas[t]
              taxas[t] <- str_replace_all(taxas[t], c("/" = ".", " " = ".", "-" = "."))
              predict <- try(cpdist(fittedbn, nodes = taxas[t], evidence = l_var, method = "lw", n = 100000))
              if (class(predict)[1] == "try-error") {
                df[t, 2] <- as.character("Cannot be calculated")
              } else {
                no_na <- !is.na(predict)
                pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
                if (!is.nan(pred_val)) {
                  vector <- c(as.numeric(pred_val))
                  # it <- input$iterations_auto
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
                    if (expm1(pred_value_log) >= 0) {
                      df[t, 2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
                    } else {
                      df[t, 2] <- 0
                    }
                  } else {
                    pred_value_log <- pred_val
                    if (expm1(pred_value_log) >= 0) {
                      df[t, 2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
                    } else {
                      df[t, 2] <- 0
                    }
                  }
                } else {
                  df[t, 2] <- as.character("NaN")
                }
              }
              if (class(total_raw_counts) == "numeric") {
                media <- mean(bn_df_raw_filt_taxas[[taxas[t]]])
                df[t, 3] <- round(media, digits = 0)
              } else {
                df[t, 3] <- as.character("Cannot be calculated")
              }

              if (class(total_raw_counts) == "numeric") {
                desves <- sd(bn_df_raw_filt_taxas[[taxas[t]]])
                df[t, 4] <- round(desves, digits = 2)
              } else {
                df[t, 4] <- as.character("Cannot be calculated")
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

              df[t, 5] <- paste(low_range, "-", high_range)

              if ((as.numeric(df[t, 3]) == 0) & (as.numeric(df[t, 4]) == 0)) {
                df[t, 2] <- as.character(0)
              }
              if ((df[t, 2] != "Cannot be calculated") & (df[t, 2] != "NaN")) {
                event_cpquery <- paste("((", taxas[t], ">= ", (as.numeric(pred_value_log) - error_cp * as.numeric(pred_value_log)), ") & (", taxas[t], "<= ", (as.numeric(pred_value_log) + error_cp * as.numeric(pred_value_log)), "))", sep = "")
                result_cpquery <- try(eval(parse(text = paste("cpquery(fittedbn, event = ", event_cpquery, ", evidence = ", evidence_cpquery, ', method = "ls")'))))
                if (!is.nan(result_cpquery)) {
                  vector3 <- c(as.numeric(result_cpquery))
                  if (it != 1) {
                    for (v in 2:it) {
                      result_cpquery <- try(eval(parse(text = paste("cpquery(fittedbn, event = ", event_cpquery, ", evidence = ", evidence_cpquery, ', method = "ls")'))))
                      if (!is.nan(result_cpquery)) {
                        vector3 <- c(vector3, as.numeric(result_cpquery))
                      }
                    }
                    df[t, 6] <- round(as.numeric(median(vector2)), digits = 2)
                  } else {
                    df[t, 6] <- round(as.numeric(result_cpquery), digits = 2)
                  }
                } else {
                  df[t, 6] <- "NaN"
                }
              } else {
                df[t, 6] <- "Cannot be calculated"
              }
            }

            if (df[t, 6] != "Cannot be calculated") {
              # filename <- col_concat(combinations_var[i,], sep = "_")
              print(" ", quote = FALSE)
              print("Taxa value prediction done for the following combination: ")
              # print(colnames(combinations_var)[1])
              for (c in 1:length(combinations_var[i, ])) {
                print(paste(colnames(combinations_var)[c], ": ", combinations_var[i, c], sep = ""), quote = FALSE)
              }
              # path_file <- paste(out_dir, filename, sep = "")
              # ifelse(!dir.exists(path_file), dir.create(path_file), FALSE)
              outfile <- paste(path_file, filename, sep = "/")
              outfile.table <- paste(outfile, ".csv", sep = "")
              write.table(df, outfile.table, dec = ",", row.names = FALSE)

              # df_filt <- select(df, "Taxa", "Predicted.raw.value")
              # df_filt <- df_filt[df_filt[,2] != "NaN", ]

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
              use_condaenv(condaenv = "picrust2", required = TRUE)
              import("picrust2.place_seqs")
              import("picrust2.wrap_hsp")
              import("picrust2.metagenome_pipeline")
              import("picrust2.util")
              import("picrust2.pathway_pipeline")
              import("picrust2.default")
              path_python_scripts <- paste(current_dir, "/python/", sep = "")
              # path_python_scripts <- paste(dirname(rstudioapi::getSourceEditorContext()$path), "/python/", sep = "")

              setwd(path_file)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }

              print("1. Place study unaligned sequences (i.e. OTUs or ASVs) into a reference tree.", quote = FALSE)
              cmd <- paste(path_python_scripts, "place_seqs.py -s sequences.fasta -o out.tre -p 5 --intermediate intermediate/place_seqs", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE, quote = FALSE)
                stop("User Interrupt")
              }
              print("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.", quote = FALSE)
              cmd <- paste(path_python_scripts, "hsp.py -i 16S -t out.tre -o 16S_predicted_and_nsti.tsv.gz -p 5 -n", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              print("3. Predict the enzymes of gene families present in the predicted genome for each amplicon sequence variant.", quote = FALSE)
              cmd <- paste(path_python_scripts, "hsp.py -i EC -t out.tre -o EC_predicted.tsv.gz -p 5", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              print("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence.
        The specified sequence abundance table will be normalized by the predicted number of marker gene copies.", quote = FALSE)
              cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i raw_counts.txt -m 16S_predicted_and_nsti.tsv.gz -f EC_predicted.tsv.gz -o metagenome_out/ --strat_out", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              print("5. Convert abundance table.", quote = FALSE)
              cmd <- paste(path_python_scripts, "convert_table.py metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o metagenome_out/pred_metagenome_unstrat.tsv.gz", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              print("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.", quote = FALSE)
              cmd <- paste(path_python_scripts, "pathway_pipeline.py -i metagenome_out/pred_metagenome_contrib.tsv.gz -o pathways_out/", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              print("7. Add description column to metagenome abundance table.", quote = FALSE)
              cmd <- paste(path_python_scripts, "add_descriptions.py -i metagenome_out/pred_metagenome_unstrat.tsv.gz -m EC -o metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz", sep = "")
              system(cmd)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }
              print("8. Add description column to pathways abundance table.", quote = FALSE)
              cmd <- paste(path_python_scripts, "add_descriptions.py -i pathways_out/path_abun_unstrat.tsv.gz -m METACYC -o pathways_out/path_abun_unstrat_descrip.tsv.gz", sep = "")
              system(cmd)
              print("Metagenome prediction DONE!", quote = FALSE)

              setwd(out_dir)

              if (interrupted()) {
                print("Stopping...", quote = FALSE)
                stop("User Interrupt")
              }

              sink()
            }
            # })
          }
        }
        # })
      })
    }

    auto_picrust_button <- observeEvent(input$button_auto, {
      disable("button_auto")

      # current_dir <- this.dir()
      oldw <- getOption("warn")
      sequences <- suppressWarnings({
        read.fasta(file = input$seqs_auto$datapath)
      })
      # out_dir <- readDirectoryInput(session, 'directory_auto')
      # browser()
      out_dir <- paste(deploy_dir, input$directory_auto, sep = "")
      dir.create(net_dir)
      it <- input$iterations_auto
      # automated_prediction(sequences, out_dir, it, deploy_dir)
      f <- future(
        {
          automated_prediction(sequences, out_dir, it, deploy_dir)
        },
        seed = TRUE
      )
      # return(NULL)
      f <- catch(f, function(e) {
        print(e$message)
        showNotification(e$message)
      })
      f <- finally(
        f,
        function() {
          fire_ready()
          enable("button_auto")
        }
      )

      ## Bloquea el uso de app si pongo barra de progreso
      # withProgress(message = "Calculation in progress", detail = "This may take a while...", value = 0, {
      # with_progress({
      # p <- progressor(steps = 20)
      # f <- future({automated_prediction(sequences, out_dir, it, current_dir, p)}, seed = TRUE)
      # }, handlers = shiny_handler)
      # })
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

    # auto_picrust_button <- observeEvent(input$button_auto, {withCallingHandlers({
    # shinyjs::html(id = "automated_pred", "")
    # sequences <- read.fasta(file = input$seqs_auto$datapath)
    # out_dir <- readDirectoryInput(session, 'directory_auto')
    # it <- input$iterations_auto
    # future({automated_prediction(sequences, out_dir, it)}) %...>% data_promise()
    # return(NULL)
    # },
    # message = function(m) {
    # shinyjs::html(id = "automated_pred", html = paste0(m$message, '<br>', '<br>'), add = TRUE)
    # })
    # })

    # auto_button <- eventReactive(input$button_auto, print(automated_prediction()))
  })


  output$selector_predict_taxas <- renderUI({
      with(session_data(),{
        taxas <- c()
        selected_taxa <- NULL
        if(is.null(fittedbn)) {
          selected_taxa <- NULL
        } else {
          taxas <- nodes(fittedbn)
          taxas <- nodes(fittedbn)
          taxas <- taxas[ncol(data_variables) + 1:length(taxas)]
          taxas <- na.omit(taxas)
          selected_taxa <- NULL
        }
        
        pickerInput("predict_selected_taxas",
          label = NULL, choices = taxas, selected = selected_taxa,multiple = TRUE,
          options = pickerOptions(
            "liveSearch" = TRUE,
            # "max-options" = 2,
            # "max-options-group" = 1,
            # "selectOnTab" = TRUE,
           actionsBox = TRUE
          )
        )
      })

      })

   output$selector_cpt <- renderUI({
      with(session_data(),{
        nodes <- c()
        selected_node <- NULL
        if(is.null(fittedbn)) {
          selected_node <- NULL
        } else {
          nodes <- nodes(fittedbn)
          selected_node <- nodes[1]
        }
        
        pickerInput("nodes_cpt",
          label = NULL, choices = nodes, selected = selected_node, multiple = FALSE,
          options = pickerOptions(
            "liveSearch" = TRUE,
            # "max-options" = 2,
            # "max-options-group" = 1,
            # "selectOnTab" = TRUE,
            actionsBox = TRUE,
            style = "btn-default"
          )
        )
      })

      })


  show_cpts_panel <- function() {
    nodes <- nodes(fittedbn)


    selection <- reactive({
      # shiny::validate(need(input$nodes_cpt, message=FALSE))
      return(input$nodes_cpt)
    })

    # observeEvent(selection(), {
    #   output$conditional_table <- renderPrint(print(fittedbn[[selection()]]))
    # })

    output$conditional_table <- renderPrint({
      ## this the method to change a selection from the server using pickerInput
      # updatePickerInput(session = session, inputId = "nodesX_cpt",
      #             selected= input$nodes_cpt)
      if (!is.null(input$nodes_cpt)) fittedbn[[input$nodes_cpt]]
    })


    output$save_cpt_all <- downloadHandler(
      filename = "download_CPTs.zip",
      content = function(fname) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL
        for (i in nodes) {
          if (nchar(i) > 200) {
            name <- substr(i, 1, 200)
          } else {
            name <- i
          }
          fileName <- paste(name, "_CPT.txt", sep = "")
          sink(fileName)
          print(fittedbn[[i]])
          sink()
          files <- c(fileName, files)
        }
        zip::zip(fname, files)
      },
      contentType = "application/zip"
    )

    output$save_cpt <- downloadHandler(
      filename = function() {
        if (nchar(selection()) > 200) {
          name <- substr(i, 1, 200)
        } else {
          name <- selection()
        }
        paste(name, "_CPT.txt", sep = "")
      },
      content = function(fname) {
        sink(fname)
        print(fittedbn[[selection()]])
        sink()
      },
      contentType = "text/csv"
    )
  }
  ## Predict taxa values, CPTs and display graph

  ### Read network using observeEvent
  #observeEvent(output$show_graph, 
  show_prediction_panel <- function()
  {
    
    # if (is.null(input$network)) {
    #   return(NULL)
    # }
    # inFile <- isolate({
    #   input$network
    # })
    # file <- inFile$datapath
    # load(file, envir = .GlobalEnv)



    variables <- bn_df_variables[, sapply(bn_df_variables, class) == "factor"]
    var_list <- list()
    for (i in 1:ncol(variables)) {
      var_list[[colnames(variables)[i]]] <- levels(variables[, i])
    }

    

    output$evidence_selector <- renderUI({
      pickerInput("evidence", "Evidence",
        choices = var_list, selected = 1, multiple = TRUE,
        options = pickerOptions(
          "liveSearch" = FALSE,
          # "max-options" = 2,
          "max-options-group" = 1,
          "selectOnTab" = TRUE,
          actionsBox = TRUE,
          style = "bs-select-all-disable"
        )
      )
    })

    # observeEvent(input$button2, {
    #   browser()
    #   nodes <- strsplit(input$nodes, ",")[[1]]
    #   nodes <- str_replace_all(nodes, c("/" = ".", " " = ".", "-" = "."))

    # })

    # outputOptions(output, "selector", suspendWhenHidden = FALSE)

    generate_prediction_table <- function() {
      # browser()
      ## If statement to create taxa list
      ## if input$taxas == ""
      #browser()
      if (length(input$predict_selected_taxas) == 0) {
         stop("Select some taxa first")
      }
      if(length(input$evidence) == 0 ){
         stop("Select evidence first")
      }


      taxas <- input$predict_selected_taxas
      # if (input$predict_selected_taxas != "all") {
      #   taxas <- strsplit(input$predict_selected_taxas, ",")[[1]]
      # } else {
      #   taxas <- nodes(fittedbn)
      #   taxas <- taxas[ncol(data_variables) + 1:length(taxas)]
      #   taxas <- na.omit(taxas)
      # }

      data_as_strong_proir <- input$use_data_as_strong_proir
      # simple table for just prediction values
      simple_table <- !input$show_org_data_dist

      ## Split evidence string by comma and create a list

      input_evidence <- list()

      for (i in input$evidence) {
        res <- lapply(var_list, function(x) match(i, x))
        for (j in 1:length(res)) {
          if (!is.na(res[j])) {
            input_evidence[[names(res[j])]] <- i
          }
        }
      }

      # evidence <- strsplit(input$evidence, ",")[[1]]
      # for (i in 1:length(evidence)) {
      #    evidence[i] <- strsplit(as.character(evidence[i]), "=")
      # }

      # ev <- list()

      # for (i in 1:length(evidence)) {
      #    ev[[evidence[[i]][1]]] <- evidence[[i]][2]
      # }

      ## Create output dataframe
      df <- NULL
      if (simple_table) n_columns <- 11 else n_columns <- 18
      columns_names <- c(
          "Taxa",
          "Average",
          "SD",
          "Q-0%",
          "Q-25%",
          "Q-50%",
          "Q-75%",
          "Q-100%",
          "M+-SD",
          "P(M+-SD)",
          "HDR_50%"
        )
      if (!simple_table) columns_names <- c(columns_names,
                                      c("Data Average",
                                        "Data SD",
                                        "Data Q-0%",
                                        "Data Q-25%",
                                        "Data Q-50%",
                                        "Data Q-75%",
                                        "Data Q-100%")
                                        )
      df <- matrix(data = NA, nrow = length(taxas), ncol = n_columns)
      colnames(df) <- columns_names
      


      it <- input$iterations
      error_cp <- input$error_network
      bn_df_norm_filtered_evidence <- filter_by_evidence(bn_df_norm, input_evidence)
      org_data_min_weight <- 0.99
      ## TODO :: we need to check if we can sample from the netowrk with the given evidence if nrow is 0
      if (nrow(bn_df_norm_filtered_evidence) == 0) {
        org_data_min_weight <- n_matchs_to_weight(length(input_evidence), 1)
      }

      withProgress(message = "Generating table", detail = "Wait...", value = 0, {
        incProgress(0 / length(taxas), detail = paste("0", "/", length(taxas)))
        for (i in 1:length(taxas)) {
          df[i, 1] <- taxas[i]
          taxas[i] <- str_replace_all(taxas[i], c("/" = ".", " " = ".", "-" = "."))
          org_data_average <- NULL
          # taxas[i] <- sub("/",".", taxas[i])
          # taxas[i] <- sub(" ",".", taxas[i])
          # taxas[i] <- sub("-",".", taxas[i])
          # predict <- try(cpdist(fittedbn, nodes = taxas[i], evidence = ev, method = "lw", n = 100000))
          browser()
          HPDI_correct = FALSE
          if(data_as_strong_proir) {
            HPDI_correct = 0.98
          } else {
            HPDI_correct = 0.95
          }
          samples.result <- get.mixed.samples(fittedbn, bn_df_norm, taxas[i],
            evidence = input_evidence,
            n.samples = 100000,
            org_data_min_weight = org_data_min_weight,
            HPDI_correct = HPDI_correct
          )
          data_as_proir <- NULL
          if (nrow(bn_df_norm_filtered_evidence) > 0) {
            ## get orginal data
            data_as_proir <- bn_df_norm_filtered_evidence[[taxas[i]]]
          } else if (!is.null(samples.result$org.samples.values)) {
            data_as_proir <- samples.result$org.samples.raw.values
          }
          if (is.null(data_as_proir)) {
            ## can not do data proir here
            data_as_strong_proir <- FALSE
          }

          if (data_as_strong_proir) {
            if (!is.null(samples.result$network.samples.values) & !is.null(data_as_proir)) {
              posterior_dist <- get_posterior_dist(samples.result$network.samples.raw.values, data_as_proir,
                adjust_samples = 1, adjust_proir = 1
              )
              posterior_dist <- posterior_stats(posterior_dist)
              #browser()
              mean_value <- posterior_dist$posterior_mean
              sd_value <- posterior_dist$posterior_sd
              df[i, 2] <- mean_value
              df[i, 3] <- sd_value
              q_ranges <- posterior_dist$posterior_quantile
              df[i, 4] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
              df[i, 5] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
              df[i, 6] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
              df[i, 7] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
              df[i, 8] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])


              low_range <- mean_value - sd_value
              if (low_range < 0) low_range <- 0
              high_range <- mean_value + sd_value
              if(high_range == low_range) high_range <- high_range+1
              df[i, 9] <- paste0(round(low_range), "-", round(high_range))
              data_mask <- posterior_dist$data_value >= log1p(low_range) & posterior_dist$data_value <= log1p(high_range)
              df[i, 10] <- round(sum(posterior_dist$posterior_p[data_mask]),2)

              #hdr <- hdrcde::hdr.den((samples.result$network.samples.raw.values), prob = 50)

              #df[i, 11] <- paste(round(expm1(hdr$hdr[1])), "-", round(expm1(hdr$hdr[length(hdr$hdr)])))

              hdr_50 <- c_hdr(den = list(x = posterior_dist$data_value, y = posterior_dist$posterior_w), prob = 50)

              df[i, 11] <- paste0(round(expm1(hdr_50$hdr[1])), "-", round(expm1(hdr_50$hdr[length(hdr_50$hdr)])))
            }
            ## end if(data_as_strong_proir)
          } else {
            if (!is.null(samples.result$network.samples.values)) {
              mean_value <- round(samples.result$network.samples.average)
              sd_value <- round(samples.result$network.samples.sd, 2)
              df[i, 2] <- mean_value
              df[i, 3] <- sd_value
              q_ranges <- samples.result$network.samples.quantile
              df[i, 4] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
              df[i, 5] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
              df[i, 6] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
              df[i, 7] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
              df[i, 8] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])
              #browser()
              low_range <- mean_value - sd_value
              if (low_range < 0) low_range <- 0
              high_range <- mean_value + sd_value
              df[i, 9] <- paste0(round(low_range), "-", round(high_range))
              data_mask <- samples.result$network.samples.raw.values >= low_range & samples.result$network.samples.raw.values <= high_range
              df[i, 10] <- round(sum(data_mask) / length(samples.result$network.samples.raw.values),2)
              
              hdr_50 <- c_hdr(samples.result$network.samples.raw.values, prob = 50)
              df[i, 11] <- paste0(round(expm1(hdr_50$hdr[1])), "-", round(expm1(hdr_50$hdr[length(hdr_50$hdr)])))
        
            
            
            }
          }

          ## end if if(simple_table)
          if (!simple_table) {
            ## add rest of data here
            if (nrow(bn_df_norm_filtered_evidence) > 0) {
              ## use orginal data for this
              data_proir <- expm1(bn_df_norm_filtered_evidence[[taxas[i]]])
              org_data_average <- mean(data_proir)
              df[i, 12] <- round(org_data_average)
              df[i, 13] <- round(sd(data_proir), 2)
              q_ranges <- round(quantile(data_proir))
              df[i, 14] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
              df[i, 15] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
              df[i, 16] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
              df[i, 17] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
              df[i, 18] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])
            } else {
              if (!is.null(samples.result$org.samples.values)) {
                org_data_average <- samples.result$org.samples.average
                df[i, 12] <- round(org_data_average)
                df[i, 13] <- round(samples.result$org.samples.sd, 2)
                q_ranges <- samples.result$org.data.quantile
                df[i, 14] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
                df[i, 15] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
                df[i, 16] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
                df[i, 17] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
                df[i, 18] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])
              }
            }

            
          }


          # if (! is.null(samples.result$network.samples.values)) {
          #   average_norm_value <- samples.result$network.samples.average
          #   df[i, 2] <- round(average_norm_value)
          #   df[i, 3] <- round(samples.result$network.samples.sd, 2)
          #   q_ranges <- samples.result$network.samples.quantile
          #   df[i, 4] <- paste(q_ranges[1], "-", q_ranges[2])
          #   df[i, 5] <- paste(q_ranges[2], "-", q_ranges[3])
          #   df[i, 6] <- paste(q_ranges[3], "-", q_ranges[4])
          #   df[i, 7] <- paste(q_ranges[4], "-", q_ranges[5])
          # }
          # if (is.null(samples.result$org.samples.values)) {
          #   df[i, 8] <- as.character("Cannot be calculated")
          # } else {
          #   org_data_average <- samples.result$org.samples.average
          #   df[i, 8] <- round(org_data_average)
          #   df[i, 9] <- round(samples.result$org.samples.sd, 2)
          #   q_ranges <- samples.result$org.data.quantile
          #   df[i, 10] <- paste(q_ranges[1], "-", q_ranges[2])
          #   df[i, 11] <- paste(q_ranges[2], "-", q_ranges[3])
          #   df[i, 12] <- paste(q_ranges[3], "-", q_ranges[4])
          #   df[i, 13] <- paste(q_ranges[4], "-", q_ranges[5])
          # }

          # if (!is.null(samples.result$network.samples.values) & !is.null(org_data_average)) {

          #   # df[i,14] <- paste(low_range, "-", high_range)
          #   # df[i,15] <- calc.prob(samples.result$network.samples.values,low_range,high_range)
          #   posterior_dist <- get_posterior_dist(samples.result$network.samples.raw.values, data_as_proir,
          #     adjust_samples = 0.5, adjust_proir = 0.8
          #   )
          #   posterior_dist <- posterior_stats(posterior_dist)
          #   # browser()
          #   df[i, 14] <- posterior_dist$posterior_mean
          #   df[i, 15] <- posterior_dist$posterior_sd
          #   q_ranges <- posterior_dist$posterior_quantile
          #   df[i, 16] <- paste(q_ranges[1], "-", q_ranges[2])
          #   df[i, 17] <- paste(q_ranges[2], "-", q_ranges[3])
          #   df[i, 18] <- paste(q_ranges[3], "-", q_ranges[4])
          #   df[i, 19] <- paste(q_ranges[4], "-", q_ranges[5])

          #   mean_value <- posterior_dist$posterior_mean
          #   low_range <- mean_value - mean_value * error_cp
          #   if (low_range < 0) low_range <- 0
          #   high_range <- mean_value + mean_value * error_cp
          #   df[i, 20] <- paste(round(low_range), "-", round(high_range))
          #   data_mask <- posterior_dist$data_value >= log1p(low_range) & posterior_dist$data_value <= log1p(high_range)
          #   df[i, 21] <- sum(posterior_dist$posterior_p[data_mask])
          #   hdr <- hdrcde::hdr.den((samples.result$network.samples.raw.values), prob = 90)

          #   df[i, 20] <- paste(round(expm1(hdr$hdr[1])), "-", round(expm1(hdr$hdr[length(hdr$hdr)])))

          #   hdr <- hdrcde::hdr.den(den = list(x = posterior_dist$data_value, y = posterior_dist$posterior_w), prob = 90)

          #   df[i, 21] <- paste(round(expm1(hdr$hdr[1])), "-", round(expm1(hdr$hdr[length(hdr$hdr)])))
          # }

          # df[i,5] <- eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', list(ev), ', method = "lw")')))
          incProgress(1 / length(taxas), detail = paste(i, "/", length(taxas)))
        }
      })


      return(df)
    }

    ## Display table when user clicks on button
    t_button <- eventReactive(input$button1, generate_prediction_table())

    output$predicted_value <- renderDataTable(
      t_button(),
      plugins = "natural",
      editable = TRUE,
      server = FALSE,
      extensions = c("Buttons"),
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#696969', 'color': '#fff'});",
          "}"
        ),
        columnDefs = list(list(targets = 1:5, class = "dt-center")),
        # language = list(lengthMenu = "_MENU_"),
        search = list(regex = TRUE, caseInsensitive = TRUE),
        columnDefs = list(list(type = "natural", targets = 2)),
        dom = '<"#js"l>Bfrtip',
        scrollY = TRUE,
        scrollX = TRUE,
        select = list(style = "multi", items = "row"),
        lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")),
        pageLength = 10,
        buttons = list(
          "copy",
          list(extend = "csv", filename = "table"),
          list(extend = "excel", filename = "table", title = NULL)
        )
      )
    )
  }
  #) ##################################################################

  ## GRAPH Panel Logic 
  show_graph_method <- function() {
    #browser()
    #if (is.null()) {
    #  return(NULL)
    #}
    # inFile <- isolate({
    #   input$network_plot
    # })
    # file <- inFile$datapath
    # load(file, envir = .GlobalEnv)

    Graph_output <- reactiveValues(graph = NULL)

    ## Plot the network
    plotInput <<- function() {
      # nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
      # nodes <- strsplit(nodes, ",")[[1]]
      # for (i in 1:length(nodes)) {
      # nodes[i] <- sub("/",".", nodes[i])
      # nodes[i] <- sub(" ",".", nodes[i])
      # nodes[i] <- str_replace(nodes[i], "-",".")
      # }
      nodes <- names(fittedbn)
      # nodes = nodes[11:length(nodes)]
      subgr <<- bnlearn::subgraph(fittedbn, nodes)
      tryit <- try(strength.viewer(
        bayesianNetwork = subgr,
        bayesianNetwork.boot.strength = arc_st_mi,
        bayesianNetwork.arc.strength.label = TRUE,
        bayesianNetwork.arc.strength.tooltip = TRUE,
        # bayesianNetwork.edge.scale.min = 1,
        # bayesianNetwork.edge.scale.max = 1,
        edges.dashes = FALSE,
        bayesianNetwork.layout = "layout_nicely"
      ))
      if (inherits(tryit, "try-error")) {
        gr <- bn.to.igraph(subgr)
        p <- visIgraph(gr)
        # data <- toVisNetworkData(igraph_network)
        # visNetwork(nodes = data$nodes, edges = data$edges)
      } else {
        p <- strength.viewer(
          bayesianNetwork = subgr,
          bayesianNetwork.boot.strength = arc_st_mi,
          bayesianNetwork.arc.strength.label = TRUE,
          bayesianNetwork.arc.strength.tooltip = TRUE,
          # bayesianNetwork.edge.scale.min = 1,
          # bayesianNetwork.edge.scale.max = 1,
          edges.dashes = FALSE,
          bayesianNetwork.layout = "layout_nicely",
        )
      }
      # Graph_output$graph <- p
    }


    Tab_inputs <- reactiveValues(
      nodes = NULL,
      edges = NULL,
      Ord_edg0 = NULL,
      Sel_group = "None",
      refresh = NULL,
      # write = NULL
    )

    # Reset
    
    observeEvent(eventExpr = input$graph_refresh, ignoreNULL = FALSE, {
      shinyjs::reset("Edit_menu")

      updateSidebar("sidebar")

      nodes <<- NULL
      nodes_info <<- NULL
      edges <<- NULL
      edges_info <<- NULL

      # Reseteamos todas las variables filt
      'Filt <<- reactiveValues(sel_tab_n = NULL,
                              sel_tab_e = NULL,
                              sel_menu = NULL,
                              grade = 0,
                              direction = "All")'
      Filt$sel_tab_n <<- NULL
      Filt$sel_tab_e <<- NULL
      Filt$sel_menu <<- NULL
      Filt$sel_by <<- NULL
      Filt$grade <<- 0
      Filt$number <<- 0
      Filt$direction <<- "All"


      Tab_inputs$nodes <<- NULL
      Tab_inputs$edges <<- NULL

      Sel_edg <- NULL
      Sel_nod <- NULL

      # shinyjs::reset("Filter_Tab_N")
      # shinyjs::reset("Filter_Tab_E")

      # shinyjs::reset("Filter_Tab_Menu")
      # Undo()
      updateCheckboxGroupInput(
        inputId = "Filter_Tab_N",
        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
        selected = NULL
      )
      updateCheckboxGroupInput(
        inputId = "Filter_Tab_E",
        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
        selected = NULL
      )
      updateCheckboxGroupInput(
        inputId = "Filter_Menu",
        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
        selected = NULL
      )
      updateCheckboxGroupInput(
        inputId = "Filter_Menu_By",
        choices = list("Selected" = "By_sel", "Group" = "By_group", "Number of interactions" = "By_num"),
        selected = NULL
      )

      DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
        clearSearch()

      DT::dataTableProxy("Tab_edges") %>% # Reordenamos
        clearSearch()


      
    })

    
    observeEvent(input$graph_refresh, ignoreNULL = FALSE, {
      # print("*2")
      Tab_inputs$refresh <- input$graph_refresh
    })

    ## Link button to print image
    p_button <- eventReactive(Tab_inputs$refresh, print(plotInput()))
    # cpt_button <- eventReactive(input$button2, table_cpt())

    # #

    # Net representation
    output$network_proxy <- renderVisNetwork(
      # if(! is.null(Graph_output$graph)){
      p_button() %>%
        # Graph_output$graph %>%
        # Permite seleccionar nodos (file:data)
        visEvents(
          type = "on",
          # Al seleccionar guardamos los ids de nodos y edges
          select = "function(data) {
                Shiny.onInputChange('current_nodes_selection', data.nodes);
                Shiny.onInputChange('current_edges_selection', data.edges);
              }",
          # deselectNode = "function(data) {
          # Shiny.onInputChange('deselect_nodes', data.nodes);
          # }" ,
          doubleClick = "function(data) {
                Shiny.onInputChange('doubleClick_nodes_selection', data.nodes);
                Shiny.onInputChange('doubleClick_edges_selection', data.edges);
               }",
          # input$network_proxy_initialized pasa a true al generar la red completamente, pero solo funciona la primera vez, por tanto al iniciar la red la fijamos en FALSE
          afterDrawing = "function(data) {
                Shiny.onInputChange('network_proxy_initialized', false);
               }",

          # Al deseleccionar, deseleccionamos en la tabla:
        ) %>%
        # Fija los parámetros por defecto de la primara representación
        visNodes(
          color = list(
            background = DT$color_background,
            border = DT$color_border,
            highlight = list(
              background = DT$color_highlight,
              border = DT$color_border
            )
          ),
          physics = DT$bounce,
          shape = DT$shape
        ) %>%
        visEdges(scaling = list(
          min = DT$scaling_min,
          max = DT$scaling_max,
          label = list(
            enabled = DT$scaling_label_enabled,
            min = DT$scaling_label_min,
            max = DT$scaling_label_max
          )
        )) %>%
        visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE)
      # %>%  visPhysics(stabilization = FALSE)
      # }
    )


    observeEvent(eventExpr = input$network_proxy_initialized, ignoreNULL = FALSE, {
      # print("*4")
      # Tab_inputs$write = TRUE
      visNetworkProxy("network_proxy") %>%
        visGetNodes() %>%
        visGetEdges() %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    ## Funciones

    Select_nodes <- function() { # Get selected nodes ids
      # visNetworkProxy("network_proxy") %>%
      # visGetSelectedNodes() # Return input$network_proxy_selectedNodes
      nod <- input$network_proxy_selectedNodes
      if (is.null(nod)) {
        Sel_nodes <- nodes$id
      } else {
        Sel_nodes <- nod
      }
      return(Sel_nodes)
    }


    Get_nodes <- function() { # Get all nodes ids
      visNetworkProxy("network_proxy") %>%
        visGetNodes() # return input$network_proxy_nodes
    }


    Nodes_info <- function() { # Get all nodes info
      # Get_nodes()
      nodes_in <- input$network_proxy_nodes

      nodes_out <- data.frame()
      for (i in c(1:length(nodes_in))) {
        nod <- nodes_in[[i]]
        if (nrow(nodes_out) == 0) {
          nodes_out <- data.frame(nod)
        } else {
          nodes_out <- merge(x = nodes_out, y = nod, all = TRUE)
        }
      }
      return(nodes_out)
    }


    Select_edges <- function() { # Get selected edges ids
      # visNetworkProxy("network_proxy") %>%
      # visGetSelectedEdges() # Return input$network_proxy_selectedEdges
      edg <- input$network_proxy_selectedEdges

      if (is.null(edg)) {
        Sel_edges <- edges$id
      } else {
        Sel_edges <- edg
      }
      return(Sel_edges)
    }

    Get_edges <- function() { # Get all edges ids
      visNetworkProxy("network_proxy") %>%
        visGetEdges() # return input$network_proxy_edges
    }


    Edges_info <- reactive({
      # Get_edges()
      edges_in <- input$network_proxy_edges

      edges_out <- data.frame()
      for (i in c(1:length(edges_in))) {
        nod <- edges_in[[i]]
        if (nrow(edges_out) == 0) {
          edges_out <- data.frame(nod)
        } else {
          edges_out <- merge(x = edges_out, y = nod, all = TRUE)
        }
      }
      return(edges_out)
    })


    Nodes_table <- function() {
      # print(input$network_proxy_nodes[1])
      nodes <<- Nodes_info()

      nodes_info <<- data.frame( # Info nodos
        nodes,
        size = DT$size,
        # Info color
        color.background = DT$color_background,
        color.border = DT$color_border,
        color.highlight.background = DT$color_highlight,
        color.highlight.border = DT$color_border,
        color.hover.background = DT$color_background,
        color.hover.border = DT$color_border,

        # Info labels
        font.color = DT$font_color,
        font.size = DT$font_size,
        font.face = DT$font_face,
        font.strokeWidth = DT$font_strokeWidth,
        font.strokeColor = DT$font_strokeColor,
        font.align = DT$N_font_align

        # Shadow
        # shadow.enabled = DT$shadow_enabled,
        # shadow.color = DT$shadow_color,
        # shadow.size = DT$shadow_size,
        # shadow.x = DT$shadow_x,
        # shadow.y = DT$shadow_y
      )
      Tab_inputs$nodes <- nodes[, c("id", "label")]
      Ord_nod_tab <<- Tab_inputs$nodes

      # Definimos los grupos de nodos a saleccionar posteriormente
      Groups <<- list(All = nodes$id, None = NULL)
    }

    Edges_table <- function() {
      Get_edges()
      edges <<- Edges_info()

      edges_info <<- data.frame(
        edges,

        # Scaling
        scaling.min = DT$scaling_min,
        scaling.max = DT$scaling_max,
        scaling.label.enabled = DT$scaling_label_enabled,
        scaling.label.min = DT$scaling_label_min,
        scaling.label.max = DT$scaling_label_max,

        # Info color
        color.color = DT$color_border,
        color.highlight = DT$color_border,
        color.opacity = 1,

        # Info labels
        font.color = DT$font_color,
        font.size = DT$font_size,
        font.face = DT$font_face,
        font.strokeWidth = DT$font_strokeWidth,
        font.strokeColor = DT$font_strokeColor,
        font.align = DT$E_font_align

        # Shadow
        # shadow.enabled = DT$shadow_enabled,
        # shadow.color = DT$shadow_color,
        # shadow.size = DT$shadow_size,
        # shadow.x = DT$shadow_x,
        # shadow.y = DT$shadow_y
      )

      Tab_inputs$edges <- edges[, c("id", "value", "from", "to")]
      Ord_edg_tab <<- Tab_inputs$edges # Tab_inputs$edges

      # Rep <<- 1
    }


    observeEvent(eventExpr = input$network_proxy_edges, ignoreNULL = TRUE, {
      # print("*5")

      Nodes_table()
      Edges_table()

      if (isFALSE(input$E_label)) {
        S_edges <- edges_info$id
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(id = S_edges, label = " "))
      }
      # Tab_inputs$refresh <<- 0
    })

    ######

    # Tab information
    # Funciones

    ## Ordenar
    OrderNodes <- function() {
      if (isFALSE(input$Reorder_N) | is.null(input$network_proxy_selectedNodes)) {
        Ord_nod_tab <<- nodes[, c("id", "label")] # Tab_inputs$nodes
      } else {
        Ind_nodes <- which(Ord_nod_tab$id %in% input$network_proxy_selectedNodes)

        Sel_tab <- (merge(
          x = data.frame(
            ord = (1:length(input$network_proxy_selectedNodes)),
            node = input$network_proxy_selectedNodes
          ),
          y = data.frame(
            ind = Ind_nodes,
            node = Ord_nod_tab$id[Ind_nodes]
          )
        ))

        Sel_nodes <- (Sel_tab[with(Sel_tab, order(Sel_tab$ord, decreasing = TRUE)), ])$ind
        Non_select <- which(!Ord_nod_tab$id %in% input$network_proxy_selectedNodes)

        Order <- c(Sel_nodes, Non_select)
        Ord_nod_tab <<- Ord_nod_tab[Order, ]
      }
    }


    # Outpur Tab
    observe({
      #  Tab_inputs$refresh
      Tab_inputs$nodes
      input$Data
      Sel_nod0 <- which(Ord_nod_tab$id %in% Sel_nod)

      output$Tab_nodes <- DT::renderDataTable({
        D_T <- DT::datatable(Ord_nod_tab,
          options = list(scrollX = TRUE),
          editable = list(target = "column", disable = list(columns = c(1))), # Permite editar
          escape = FALSE,
          filter = "top",
          selection = list(
            mode = "multiple",
            selected = Sel_nod0,
            target = "row"
          ),
          callback = htmlwidgets::JS( ## https://stackoverflow.com/questions/66129627/create-an-r-shiny-binding-to-a-double-click-event-on-a-dt-datatable
            "table.on('dblclick', 'td',",
            "  function() {",
            "    var row = table.cell(this).index().row;",
            "    var col = table.cell(this).index().column;",
            "    Shiny.setInputValue('dt_dblclick', {dt_row: row, dt_col: col});",
            "  }",
            ");",
            "table.on('click', 'td',",
            "  function() {",
            "    var row = table.cell(this).index().row;",
            "    var col = table.cell(this).index().column;",
            "    Shiny.setInputValue('dt_click', {dt_row: row, dt_col: col});",
            "  }",
            ");"
          )
        )
      })
    })

    '
    observeEvent(input$dt_dblclick, {
      print("DOBLE CLICK")
      print(input$dt_dblclick)
    })
    observeEvent(input$dt_click, {
      print(" CLICK")
      print(input$dt_click)
    })'

    # Selecciona en la red los nodos de la Tab_nodes
    observeEvent(eventExpr = input$Tab_nodes_cell_clicked, ignoreNULL = FALSE, { # Se activa cuando hacemos click en la tabla

      if (length(input$Tab_nodes_cell_clicked) > 0) {
        S_nodes <- {
          nod <- input$Tab_nodes_rows_selected
          tab <- Ord_nod_tab
          if (is.null(nod)) {
            S_nodes <- NULL
          } else {
            sel <- input$Tab_nodes_cell_clicked$row
            n1 <- nod[which(nod == sel)]
            n2 <- nod[which(nod != sel)]
            S_nodes <- append(tab[n1, "id"], tab[n2, "id"])
          }
          S_nodes
        }

        S_edges <- input$network_proxy_selectedEdges
        S_nodes <- rev(S_nodes)

        if (is.null(S_edges) && is.null(S_nodes)) {
          visNetworkProxy("network_proxy") %>%
            visUnselectAll()
        } else {
          visNetworkProxy("network_proxy") %>%
            visSetSelection(
              nodesId = S_nodes,
              edgesId = S_edges,
              unselectAll = TRUE,
              highlightEdges = FALSE
            )
        }
      }

      # Actualizamos
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    # Ordenamos la tabla para posicionar los seleccionados primero
    observe({
      OrderNodes()
    })


    # Seleccionar filas que se corresponden con los nodos seleccionados en la red
    observe({
      Sel_nod <<- input$network_proxy_selectedNodes
      input$Reorder_N

      if (is.null(input$network_proxy_selectedNodes)) {
        DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
          replaceData(data = Ord_nod_tab, clearSelection = FALSE, resetPaging = FALSE) %>%
          # Eliminamos la selección
          selectRows(NULL)
      } else {
        DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
          replaceData(data = Ord_nod_tab, clearSelection = FALSE, resetPaging = FALSE) %>% # Seleccionamos
          selectRows(which(Ord_nod_tab$id %in% input$network_proxy_selectedNodes))
      }
    })


    ### Edicion de las etiquetas de los nodos ###

    # Edición de las Etiquetas de los nodos
    observeEvent(eventExpr = input$Tab_nodes_cell_edit, {
      Nodes_edit <- input$Tab_nodes_cell_edit

      if (colnames(Ord_nod_tab)[Nodes_edit$col] == "label") {
        ID <- Ord_nod_tab[Nodes_edit$row, "id"]
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = ID,
            label = Nodes_edit$value
          ))
        nodes[which(nodes$id == ID), "label"] <<- Nodes_edit$value
        nodes_info[which(nodes_info$id == ID), "label"] <<- Nodes_edit$value
      }
    })


    # Edges

    ## Escogemos el número de decimales
    output$Value_decimals <- renderUI({
      if (!is.null(Tab_inputs$edges$value)) {
        sig_count <- function(n) {
          num <- gsub("^[^(1-9)]*", "", x = n)
          num <- gsub("\\.", "", x = num)
          nchar(num)
        }
        Sig <- max(sapply(X = Tab_inputs$edges$value, FUN = sig_count))

        sliderInput(
          inputId = "Value_decimals", label = "Significant digits",
          step = 1, min = 1, max = Sig, value = Sig
        )
      }
    })


    # Funciones

    # Ordenar
    OrderEdges <- function() {
      if (isFALSE(input$Reorder_E) | is.null(input$network_proxy_selectedEdges)) {
        Ord_edg_tab <<- edges[, c("id", "value", "from", "to")]
      } else {
        Ind_edges <- which(Ord_edg_tab$id %in% input$network_proxy_selectedEdges)

        Sel_tab <- (merge(
          x = data.frame(
            ord = (1:length(input$network_proxy_selectedEdges)),
            edge = input$network_proxy_selectedEdges
          ),
          y = data.frame(
            ind = Ind_edges,
            edge = Ord_edg_tab$id[Ind_edges]
          )
        ))

        Sel_edges <- (Sel_tab[with(Sel_tab, order(Sel_tab$ord, decreasing = TRUE)), ])$ind
        Non_select <- which(!Ord_edg_tab$id %in% input$network_proxy_selectedEdges)
        Order <- c(Sel_edges, Non_select)
        Ord_edg_tab <<- Ord_edg_tab[Order, ]
      }
    }

    # Output de la tabla edges:
    output$Tab_edges <- DT::renderDataTable({
      Sel_edg0 <- which(Ord_edg_tab$id %in% Sel_edg)


      DT_edg <<- DT::datatable( # Tab_inputs$Ord_edg0,
        Ord_edg_tab,
        # Tab_inputs$edges,
        # Ord_edg0,
        options = list(scrollX = TRUE),
        editable = FALSE, # No permite editar por el usuario
        escape = FALSE,
        filter = "top",
        selection = list(
          mode = "multiple",
          selected = Sel_edg0, # Fijamos la selección inicial
          target = "row"
        )
      )
      if (!is.null(input$Value_decimals)) {
        DT_edg %>%
          formatSignif(columns = c("value"), digits = input$Value_decimals)
      }
    })


    # Cambiamos las etiquetas de los edges
    observeEvent(eventExpr = input$Value_decimals, ignoreNULL = TRUE, {
      if (isTRUE(input$E_label)) {
        lab <- sapply(X = edges_info$value, FUN = signif, input$Value_decimals)
        edges_info$label <<- sapply(X = lab, FUN = as.character)
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = edges_info$id,
            label = edges_info$label
          ))
      }
    })


    # Selecciona los edges que une dos nodos seleccionados.
    observeEvent(eventExpr = input$network_proxy_selectedNodes, {
      if (!is.null(input$network_proxy_selectedNodes)) {
        e1 <- edges[edges$from %in% input$network_proxy_selectedNodes, ]
        e2 <- e1[e1$to %in% input$network_proxy_selectedNodes, ]

        if (nrow(e2) > 0) {
          S_edges <- unique(append(input$network_proxy_selectedEdges, e2$id))
          S_nodes <- input$network_proxy_selectedNodes
          visNetworkProxy("network_proxy") %>%
            visSetSelection(
              nodesId = S_nodes,
              edgesId = S_edges,
              unselectAll = FALSE,
              highlightEdges = FALSE
            )
        }

        # Actualizamos
        visNetworkProxy("network_proxy") %>%
          visGetSelectedNodes() %>%
          visGetSelectedEdges()
      }
    })


    # Selecciona en la red los edges de la Tab_edges
    observeEvent(eventExpr = input$Tab_edges_cell_clicked, ignoreNULL = FALSE, {
      S_edges <- {
        edg <- input$Tab_edges_rows_selected
        tab <- Ord_edg_tab
        if (is.null(edg)) {
          S_edges <- NULL
        } else {
          S_edges <- tab[edg, "id"]
        }
        S_edges
      }
      S_nodes <- input$network_proxy_selectedNodes

      if (is.null(S_nodes) && is.null(S_edges)) {
        visNetworkProxy("network_proxy") %>%
          visUnselectAll()
      } else {
        visNetworkProxy("network_proxy") %>%
          visSetSelection(
            nodesId = S_nodes,
            edgesId = S_edges,
            unselectAll = TRUE,
            highlightEdges = FALSE
          )
      }
      # Actualizamos
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    # Ordenamos la tabla para posicionar los seleccionados primero
    observe({
      Tab_inputs$edges
      input$Value_decimals

      OrderEdges()
      visNetworkProxy("network_proxy") %>%
        visGetSelectedEdges()
    })


    # Seleccionar filas que se corresponden con los edges seleccionados en la red
    observe({
      Sel_edg <<- input$network_proxy_selectedEdges
      Tab_inputs$edges
      input$Value_decimals
      input$Reorder_E

      if (is.null(input$network_proxy_selectedEdges)) {
        DT::dataTableProxy("Tab_edges") %>%
          replaceData(data = Ord_edg_tab, clearSelection = TRUE) %>%
          selectRows(NULL)
      } else {
        DT::dataTableProxy("Tab_edges") %>%
          replaceData(data = Ord_edg_tab, clearSelection = FALSE) %>%
          selectRows(which(Ord_edg_tab$id %in% input$network_proxy_selectedEdges))
      }
    })

    # Informacion nodos
    ### Texto a mostrar ###

    # id del nodo tras el doble click
    output$N1 <- renderPrint({
      if (is.null(input$doubleClick_nodes_selection)) {
        NULL
      } else {
        fittedbn[[input$doubleClick_nodes_selection]]
      }
    })


    output$Node_info <- renderUI({
      if (is.null(input$doubleClick_nodes_selection)) {
        NULL
      } else {
        verbatimTextOutput("N1")
      }
    })


    output$E1 <- renderText({
      edge_id <- input$doubleClick_edges_selection

      from <- edges[edges$id == edge_id, "from"]

      to <- edges[edges$id == edge_id, "to"]

      value <- edges[edges$id == edge_id, "value"]

      paste0(
        "Causal Relationships : \n  ",
        from, "  →  ", to,
        "\nEdge Strength : \n  ",
        value
      )
    })


    output$Edge_info <- renderUI({
      if (is.null(input$doubleClick_edges_selection)) {
        NULL
      } else {
        verbatimTextOutput("E1")
      }
    })
    ######

    # Panel de edicion

    ##  Nodos

    observeEvent(eventExpr = input$current_nodes_selection, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    observeEvent(eventExpr = input$Tab_nodes_cell_clicked, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    # Shape
    observeEvent(eventExpr = input$N_shape, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, shape = input$N_shape))
      }
    })


    # Size
    observeEvent(eventExpr = input$N_size, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "size"] <<- input$N_size

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, size = input$N_size))
      }
    })


    Auto_size_all <- function(nod) {
      connections <- length(which(edges["from"] == nod | edges["to"] == nod))
      return(connections)
    }

    Auto_size_from <- function(nod) {
      connections <- length(which(edges["from"] == nod))
      return(connections)
    }

    Auto_size_to <- function(nod) {
      connections <- length(which(edges["to"] == nod))
      return(connections)
    }


    observe({
      if (isTRUE(input$N_autosize)) {
        if (!is.null(nodes)) {
          if (input$N_autosize_type == "E_F") {
            connections <- sapply(nodes$id, Auto_size_from)
          } else {
            if (input$N_autosize_type == "E_T") {
              connections <- sapply(nodes$id, Auto_size_to)
            } else {
              connections <- sapply(nodes$id, Auto_size_all)
            }
          }
          max1 <- max(connections)
          min1 <- min(connections)
          min0 <- input$N_size_range[1]
          max0 <- input$N_size_range[2]

          r <- (max0 - min0) / max1

          sizes <- list()
          for (c in connections) {
            f <- min0 + (c * r)
            sizes <- append(sizes, round(f))
          }
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(id = nodes$id, size = unlist(sizes)))
        }
      } else {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = nodes$id, size = nodes_info$size))
      }
    })


    # Border Width
    observeEvent(eventExpr = input$N_borderWidth, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, borderWidth = input$N_borderWidth))
      }
    })


    ## Selected Border Width
    observeEvent(eventExpr = input$N_borderWidthSelected, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, borderWidthSelected = input$N_borderWidthSelected))
      }
    })


    # Background color
    observeEvent(eventExpr = input$N_color_background, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.background"] <<- input$N_color_background

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Border color
    observeEvent(eventExpr = input$N_color_border, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.border"] <<- input$N_color_border

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Highlight background color
    observeEvent(eventExpr = input$N_color_highlight_background, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.highlight.background"] <<- input$N_color_highlight_background

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Highlight border color
    observeEvent(eventExpr = input$N_color_highlight_border, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.highlight.border"] <<- input$N_color_highlight_border

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Opacity
    observeEvent(eventExpr = input$N_opacity, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, opacity = input$N_opacity))
      }
    })


    # Show labels
    observeEvent(eventExpr = input$N_label, ignoreNULL = FALSE, {
      if (!is.null(nodes)) {
        S_nodes <- nodes$id
        if (isTRUE(input$N_label)) {
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(
              id = S_nodes,
              label = nodes_info$label
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(id = S_nodes, label = " "))
        }
      }
    })


    # Color
    observeEvent(eventExpr = input$N_label_color, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.color"] <<- input$N_label_color

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Tamaño
    observeEvent(eventExpr = input$N_label_size, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.size"] <<- input$N_label_size

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Fuente
    observeEvent(eventExpr = input$N_label_face, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.face"] <<- input$N_label_face

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Fondo
    observeEvent(eventExpr = input$N_label_strokeWidth, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.strokeWidth"] <<- input$N_label_strokeWidth

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[
              N_nodes,
              grep("font.", colnames(nodes_info))
            ]
          ))
      }
    })


    # Fondo color
    observeEvent(eventExpr = input$N_label_strokeColor, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.strokeColor"] <<- input$N_label_strokeColor
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[
              N_nodes,
              grep("font.", colnames(nodes_info))
            ]
          ))
      }
    })


    # Posición
    observeEvent(eventExpr = input$N_label_align, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.align"] <<- input$N_label_align
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Show shadow
    observe({
      if (isTRUE(input$N_show_shadow)) {
        if (!is.null(nodes)) {
          # S_nodes = Select_nodes()
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(
              id = nodes$id,
              shadow = list(
                enabled = input$N_show_shadow,
                size = input$N_size,
                x = input$N_shadow_x,
                y = input$N_shadow_y
              )
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(
              id = nodes$id,
              shadow = input$N_show_shadow
            ))
        }
      }
    })


    # Edges
    observeEvent(eventExpr = input$current_edges_selection, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })

    observeEvent(eventExpr = input$Tab_edges_cell_clicked, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    observeEvent(eventExpr = input$E_hidden, {
      if (!is.null(edges)) {
        # S_edges = select_edges( )
        S_edges <- edges$id
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(id = S_edges, hidden = input$E_hidden))
      }
    })


    # Arrow
    observeEvent(eventExpr = input$E_direction, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            arrows = ifelse(input$E_direction, "to", "NULL")
          ))
      }
    })


    # Dashed
    observeEvent(eventExpr = input$E_dashes, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            dashes = input$E_dashes
          ))
      }
    })


    # Width
    observeEvent(eventExpr = input$E_width, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "scaling.min"] <<- input$E_width
        edges_info[N_edges, "scaling.max"] <<- input$E_width
        edges_info[N_edges, "width"] <<- input$E_width

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("scaling", colnames(edges_info))]
          ))
      }
    })


    # Selected Width
    observeEvent(eventExpr = input$E_selected_width, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            selectionWidth = input$E_selected_width
          ))
      }
    })


    # Color
    observeEvent(eventExpr = input$E_color, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "color.color"] <<- input$E_color

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("color.", colnames(edges_info))]
          ))
      }
    })


    # Highlight
    observeEvent(eventExpr = input$E_color_highlight, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "color.highlight"] <<- input$E_color_highlight

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("color.", colnames(edges_info))]
          ))
      }
    })


    # Opacity
    observeEvent(eventExpr = input$E_opacity, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "color.opacity"] <<- input$E_opacity

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("color.", colnames(edges_info))]
          ))
      }
    })


    # Show labels
    observeEvent(eventExpr = input$E_label, {
      if (!is.null(edges)) {
        S_edges <- edges$id

        if (isTRUE(input$E_label)) {
          if (is.null(input$Value_decimals)) {
            lab <- edges_info$value
          } else {
            lab <- sapply(X = edges_info$value, FUN = signif, input$Value_decimals)
          }

          edges_info$label <<- sapply(X = lab, FUN = as.character)

          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(
              id = edges_info$id,
              label = edges_info$label
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(id = S_edges, label = " "))
        }
      }
    })


    # Color
    observeEvent(eventExpr = input$E_label_color, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.color"] <<- input$E_label_color

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Tamaño
    observeEvent(eventExpr = input$E_label_size, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.size"] <<- input$E_label_size

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Fuente
    observeEvent(eventExpr = input$E_label_face, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.face"] <<- input$E_label_face

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Fondo
    observeEvent(eventExpr = input$E_label_strokeWidth, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.strokeWidth"] <<- input$E_label_strokeWidth

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Fondo color
    observeEvent(eventExpr = input$E_label_strokeColor, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.strokeColor"] <<- input$E_label_strokeColor

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })

    # Posición
    observeEvent(eventExpr = input$E_label_align, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.align"] <<- input$E_label_align

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[
              N_edges,
              grep("font.", colnames(edges_info))
            ]
          ))
      }
    })


    ## Show shadow
    observe({
      if (!is.null(edges)) {
        if (isTRUE(input$E_show_shadow)) {
          S_edges <- Select_edges()
          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(
              id = edges$id,
              shadow = list(
                enabled = input$E_show_shadow,
                x = input$E_shadow_x,
                y = input$E_shadow_y
              )
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(
              id = edges$id,
              shadow = input$E_show_shadow
            ))
        }
      }
    })


    # Rebotar
    observeEvent(eventExpr = input$N_physics, { # Cambia todos
      if (!is.null(edges)) {
        # S_nodes = Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes$id,
            physics = input$N_physics
          ))
      }
    })


    # Fijar eje x
    observeEvent(eventExpr = input$N_fix_x, { # Cambia todos
      if (!is.null(edges)) {
        # S_nodes = Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes$id,
            fixed = list(
              x = input$N_fix_x,
              y = input$N_fix_y
            )
          ))
      }
    })


    # Fijar eje y
    observeEvent(eventExpr = input$N_fix_y, { # Cambia todos
      if (!is.null(edges)) {
        # S_nodes = Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes$id,
            fixed = list(
              x = input$N_fix_x,
              y = input$N_fix_y
            )
          ))
      }
    })


    # Selection functions

    # Selecionamos los nodos en funcion del grado
    observeEvent(eventExpr = input$Set_sel, {
      nod <- input$network_proxy_selectedNodes
      dir <- input$Select_direction
      gra <- input$Select_grade

      # Tomamos los nodos y edges
      S_nodes <- Get_all_connected(nod, dir, gra)
      S_edges <- Get_connected_edg(nod, dir)

      # Los selecciona
      visNetworkProxy("network_proxy") %>%
        visSetSelection(
          nodesId = S_nodes,
          edgesId = S_edges,
          unselectAll = TRUE,
          highlightEdges = FALSE
        )

      # Actualizamos la informaci?n
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })

    # Filter
    # Funciones

    # Enfatizar red en base a los nodos
    Emphasize_N <<- function(S_nodes) {
      H_color <- "rgba(200,200,200,0.5)"

      S_edges <- edges[(which(edges$from %in% S_nodes & edges$to %in% S_nodes)), "id"]
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_nodes) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            color = H_color, label = ""
          )) %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            color = H_color, label = " "
          ))
      }
    }

    # Enfatizar red en base a los edgess
    Emphasize_E <<- function(S_edges) {
      H_color <- "rgba(200,200,200,0.5)"

      Sn <- edges[(which(edges$id %in% S_edges)), c("from", "to")]
      S_nodes <- unique(append(Sn$from, Sn$to))
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_edges) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            color = H_color, label = ""
          )) %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            color = H_color, label = " "
          ))
      }
    }


    ## Funciones para mostrar unicmante los nodos deseados

    Hide_N <<- function(S_nodes) {

      # if ( isTRUE(Tab_inputs$write)){
      S_edges <- edges[(which(edges$from %in% S_nodes | edges$to %in% S_nodes)), "id"]
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_edges) > 0 && length(Hide_nodes) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            hidden = TRUE
          ))
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            hidden = TRUE
          ))
      }
    }


    Hide_E <<- function(S_edges) {
      Sn <- edges[(which(edges$id %in% S_edges)), c("from", "to")]
      S_nodes <- unique(append(Sn$from, Sn$to))
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_edges) > 0 && length(Hide_nodes) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            hidden = TRUE
          ))
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            hidden = TRUE
          ))
      } # }
    }


    Get_connected_edg <- function(nod, dir) { # Tomamos los edges conectados a un nodo en una determinada dirección
      if (dir == "All") {
        edg <- edges[edges$from %in% nod | edges$to %in% nod, "id"] # Toma los los nodos unidos por edges que salen del indicado
      } else {
        if (dir == "From") {
          edg <- edges[edges$from %in% nod, "id"]
        } else {
          if (dir == "To") {
            edg <- edges[edges$to %in% nod, "id"]
          }
        }
      }
      return(unique(edg))
    }


    Get_all_connected <- function(nod, dir, gra) { # Toma todos los edges unidos en una dirección separados a un grado gra
      n <- nod
      connected_nodes <- c(n)

      for  (i in 1:gra) {
        edg <- Get_connected_edg(n, dir)
        n <- append(edges[edges$id %in% edg, "from"], edges[edges$id %in% edg, "to"])
        connected_nodes <- append(connected_nodes, unique(n))
      }
      return(connected_nodes)
    }


    Undo_N <<- function() {
      H_color <- "rgba(200,200,200,0.5)"

      if (isTRUE(input$N_label)) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes_info$id,
            hidden = FALSE,
            # Color
            nodes_info[, grep(
              "color.",
              colnames(nodes_info)
            )],
            # Etiqueta
            label = nodes_info$label
          ))
      } else {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes_info$id,
            hidden = FALSE,
            # Color
            nodes_info[, grep(
              "color.",
              colnames(nodes_info)
            )]
          ))
      }
    }


    Undo_E <<- function() {
      H_color <- "rgba(200,200,200,0.5)"

      if (isTRUE(input$E_label)) {
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = edges_info$id,
            hidden = FALSE,
            # Color
            edges_info[, grep(
              "color.",
              colnames(edges_info)
            )],
            # Etiqueta
            label = edges_info$label
          ))
      } else {
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = edges_info$id,
            hidden = FALSE,
            # Color
            edges_info[, grep(
              "color.",
              colnames(edges_info)
            )]
          ))
      }
    }


    Undo <<- function() {
      if (!is.null(nodes_info)) {
        Undo_N()
        Undo_E()
      }
    }


    Get_conection_num <- reactive({
      edg <- Tab_inputs$edges
      nod <- unlist(Tab_inputs$nodes["id"])
      count <- function(nod, dir) {
        total <- sum(edg[dir] == nod)
        return(total)
      }
      f <- sapply(nod, count, "from")
      t <- sapply(nod, count, "to")
      total <- list()
      for (i in 1:length(nod)) {
        tot <- f[i] + t[i]
        total[[i]] <- tot
      }

      nodes_count <- data.frame("id" = nod, "From" = f, "To" = t, "All" = unlist(total))
      return(nodes_count)
    })


    Emphasize_group <- function(nod, dir, gra) {
      if (!is.null(gra) && gra > 0) {
        nodes <- Get_all_connected(nod, dir, gra)
        Emphasize_N(nodes)
      } else {
        nodes <- nod
        Emphasize_N(nodes)
      }
    }


    Show_group <- function(nod, dir, gra) {
      if (!is.null(gra) && gra > 0) {
        nodes <- Get_all_connected(nod, dir, gra)
        Hide_N(nodes)
      } else {
        nodes <- nod
        Hide_N(nodes)
      }
    }


    # Filtrado por grupos
    Filt <- reactiveValues(
      sel_tab_n = NULL,
      sel_tab_e = NULL,
      sel_menu = NULL,
      sel_by = NULL,
      grade = 0,
      number = 0,
      direction = "All"
    )


    output$Filter_grade <- renderUI(
      if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) {
        if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
          if (input$Filter_Menu_By == "By_sel" || (!is.null(input$Filter_by) && input$Filter_by != "All" && input$Filter_by != "None")) {
            sliderInput(
              inputId = "Filter_grade", label = "Grade",
              min = 0, max = 5, step = 1, value = Filt$grade
            )
          }
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_grade, ignoreNULL = TRUE, {
      Filt$grade <- input$Filter_grade
    })


    output$Filter_direction <- renderUI(
      if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) {
        if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
          if (input$Filter_Menu_By != "By_group" || (!is.null(input$Filter_by) && input$Filter_by != "All" && input$Filter_by != "None")) {
            radioButtons(
              inputId = "Filter_direction", label = "Direction",
              choices = c("All", "From", "To"), selected = Filt$direction,
              inline = TRUE
            )
          }
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_direction, ignoreNULL = TRUE, {
      Filt$direction <- input$Filter_direction
    })


    output$Filter_number <- renderUI(
      if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) {
        if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
          if (input$Filter_Menu_By == "By_num") {
            max <- max(Get_conection_num()[, "All"])
            sliderInput(
              inputId = "Filter_number", label = "Minimum number of conecctions",
              min = 0, max = max, step = 1, value = Filt$number
            )
          }
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_number, ignoreNULL = TRUE, {
      Filt$number <- input$Filter_number
    })


    observeEvent(eventExpr = input$Add_sel_group, {
      G_nodes <<- Select_nodes()
      shinyalert::shinyalert(
        inputId = "Alert_Add_group",
        title = "New group",
        text = "Set group name:",
        type = "input",
        inputType = "text",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })


    observeEvent(eventExpr = input$Add_input_group, {
      nodes <- str_replace_all(input$nodes_input, c("/" = ".", " " = ".", "-" = "."))
      nodes <- strsplit(nodes, ",")[[1]]
      for (i in 1:length(nodes)) {
        nodes[i] <- sub("/", ".", nodes[i])
        nodes[i] <- sub(" ", ".", nodes[i])
        nodes[i] <- str_replace(nodes[i], "-", ".")
      }
      G_nodes <<- nodes

      shinyalert::shinyalert(
        inputId = "Alert_Add_group",
        title = "New group",
        text = "Set group name:",
        type = "input",
        inputType = "text",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })


    observeEvent(eventExpr = input$Alert_Add_group, {
      if (!isFALSE(input$Alert_Add_group)) {
        Groups <<- c(list(G_nodes), Groups)

        names(Groups)[1] <<- input$Alert_Add_group

        shinyalert(
          inputId = "Correct_Add_group",
          title = input$Alert_Add_group,
          text = "Created",
          type = "success",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      } else {
        shinyalert(
          inputId = "Error_Add_group",
          text = "The group has not been created",
          type = "error",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      }
    })


    observeEvent(eventExpr = input$remove_group, ignoreNULL = TRUE, {
      shinyalert::shinyalert(
        inputId = "Alert_Remove_group",
        text = paste("Do you want to remove the group '", input$Filter_by, "' ? "),
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })


    observeEvent(eventExpr = input$Alert_Remove_group, {
      if (isTRUE(input$Alert_Remove_group)) {
        Groups <<- Groups[which(names(Groups) != input$Filter_by)]
        # Alertas
        shinyalert(
          inputId = "Correct_Remove_group",
          title = input$Filter_by,
          text = "Removed",
          type = "success",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      } else {
        shinyalert(
          inputId = "Error_Remove_group",
          text = "The group has not been removed",
          type = "error",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      }
    })


    observe({
      input$Alert_Add_group
      input$Correct_Remove_group
      #
      output$Filter_by <- renderUI(
        if (!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1) {
          if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
            if (input$Filter_Menu_By == "By_group") {
              selectInput(
                inputId = "Filter_by",
                label = "Filter by",
                choices = names(Groups)
              )
            }
          }
        } else {
          NULL
        }
      )
    })


    output$Remove_group <- renderUI(
      if (!is.null(input$Filter_by) && (!is.null(input$Filter_Menu)) && !is.null(input$Filter_Menu_By)) {
        if (input$Filter_Menu_By == "By_group" && input$Filter_by != "None" && input$Filter_by != "All") {
          Filt$groups <- input$Filter_by
          actionButton(
            inputId = "remove_group",
            icon("remove"),
            style = " padding: 4px; border-radius: 2px; color: #fff; background-color: #A80215"
          )
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_Menu, ignoreNULL = FALSE, {
      if (!is.null(input$Filter_Menu)) {
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_N",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_E",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        Filt$sel_tab_e <- NULL
        Filt$sel_tab_n <- NULL
      }

      if (length(input$Filter_Menu) == 2) {
        Sel <- input$Filter_Menu[which(input$Filter_Menu != Filt$sel_menu)]
      } else {
        Sel <- input$Filter_Menu
      }
      updateCheckboxGroupInput(inputId = "Filter_Menu", selected = Sel)
      Filt$sel_menu <<- Sel
    })


    observeEvent(eventExpr = input$Filter_Menu_By, ignoreNULL = FALSE, {
      if (length(input$Filter_Menu_By) == 2) {
        Sel <- input$Filter_Menu_By[which(input$Filter_Menu_By != Filt$sel_by)]
      } else {
        Sel <- input$Filter_Menu_By
      }
      updateCheckboxGroupInput(inputId = "Filter_Menu_By", selected = Sel)
      Filt$sel_by <<- Sel
    })


    observeEvent(eventExpr = input$Filter_Tab_N, {
      if (!is.null(input$Filter_Tab_N)) {
        updateCheckboxGroupInput(
          inputId = "Filter_Menu",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_E",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        Filt$sel_tab_e <- NULL
        Filt$sel_menu <- NULL
      }

      if (length(input$Filter_Tab_N) == 2) {
        Sel <- input$Filter_Tab_N[which(input$Filter_Tab_N != Filt$sel_tab_n)]
      } else {
        Sel <- input$Filter_Tab_N
      }
      updateCheckboxGroupInput(inputId = "Filter_Tab_N", selected = Sel)
      Filt$sel_tab_n <<- Sel
    })


    observeEvent(eventExpr = input$Filter_Tab_E, {
      if (!is.null(input$Filter_Tab_E)) {
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_N",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        updateCheckboxGroupInput(
          inputId = "Filter_Menu",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        Filt$sel_tab_n <- NULL
        Filt$sel_menu <- NULL
      }
      if (length(input$Filter_Tab_E) == 2) {
        Sel <- input$Filter_Tab_E[which(input$Filter_Tab_E != Filt$sel_tab_e)]
      } else {
        Sel <- input$Filter_Tab_E
      }
      updateCheckboxGroupInput(inputId = "Filter_Tab_E", selected = Sel)
      Filt$sel_tab_e <<- Sel
    })


    observe({
      dir <- ifelse(is.null(input$Filter_direction), "All", input$Filter_direction)
      gra <- input$Filter_grade

      if (!is.null(Filt$sel_menu) && !is.null(Filt$sel_by)) {
        Undo()
        if (Filt$sel_by == "By_sel") {
          nod <- input$network_proxy_selectedNodes
        } else if (Filt$sel_by == "By_group") {
          nod <- unlist(Groups[input$Filter_by])
        } else if (Filt$sel_by == "By_num") {
          min <- ifelse(is.null(input$Filter_number), 0, input$Filter_number)
          nodes_count <- Get_conection_num()
          nod <- nodes_count[which(nodes_count[dir] >= min), "id"]
          gra <- 1
        }

        # Escogemos si enfatizamos o escondemos
        if (Filt$sel_menu == "Emph") { # Enfatizaremos
          Emphasize_group(nod, dir, gra)
        } else {
          Show_group(nod, dir, gra)
        }
      }
    })


    observe({
      if (is.null(input$Filter_Tab_E) && is.null(input$Filter_Tab_N) && (is.null(input$Filter_Menu) || is.null(input$Filter_Menu_By))) {
        visNetworkProxy("network_proxy") %>%
          visRedraw()
        Undo()
      }
    })


    observe({
      if (!is.null(Filt$sel_tab_n) && length(Filt$sel_tab_n) == 1) {
        Undo()
        S_nodes <- Ord_nod_tab[input$Tab_nodes_rows_all, "id"]
        if (Filt$sel_tab_n == "Emph") {
          Emphasize_N(S_nodes)
        } else {
          if (Filt$sel_tab_n == "S/H") {
            Hide_N(S_nodes)
          }
        }
      }
    })


    observe({
      if (!is.null(Filt$sel_tab_e) && length(Filt$sel_tab_e == 1)) {
        Undo()
        S_edges <- Ord_edg_tab[input$Tab_edges_rows_all, "id"]
        if (Filt$sel_tab_e == "Emph") {
          Emphasize_E(S_edges)
        } else {
          if (Filt$sel_tab_e == "S/H") {
            Hide_E(S_edges)
          }
        }
      }
    })


    # Captura
    observeEvent(eventExpr = input$Capture, {
      shinyscreenshot::screenshot(
        selector = "#network_proxy",
        filename = "network_out"
      )
    })


    # Export
    observeEvent(eventExpr = input$Save, {
      Get_nodes()
      Get_edges()
    })


    output$network_save <- renderVisNetwork({
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      if (input$Out_type == "HTML") {
        visNetwork(nodes_out, edges_out) %>%
          visNodes(
            color = list(
              background = DT$color_background,
              border = DT$color_border,
              highlight = list(
                background = DT$color_highlight,
                border = DT$color_border
              )
            ),
            physics = DT$bounce,
            shape = DT$shape
          ) %>%
          visEdges(scaling = list(
            min = DT$scaling_min,
            max = DT$scaling_max,
            label = list(
              enabled = DT$scaling_label_enabled,
              min = DT$scaling_label_min,
              max = DT$scaling_label_max
            )
          )) %>%
          visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
          visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE)
      } else {
        visNetwork(nodes_out, edges_out) %>%
          visNodes(
            color = list(
              background = DT$color_background,
              border = DT$color_border,
              highlight = list(
                background = DT$color_highlight,
                border = DT$color_border
              )
            ),
            physics = DT$bounce,
            shape = DT$shape
          ) %>%
          visEdges(scaling = list(
            min = DT$scaling_min,
            max = DT$scaling_max,
            label = list(
              enabled = DT$scaling_label_enabled,
              min = DT$scaling_label_min,
              max = DT$scaling_label_max
            )
          )) %>%
          visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
          visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE) %>%
          # Guardado en png/jpeg/pdf
          visExport(
            type = input$Out_type, name = input$Out_name, float = "right",
            style = DT$st
          )
      }
    })


    observe({
      visNetworkProxy("network_save") %>%
        visSetTitle(
          main = list(text = input$Title_main),
          submain = list(text = input$Title_submain),
          footer = list(text = input$Title_footer)
        )
    })


    output$HTML_Button <- renderUI({
      if (input$Out_type == "HTML") {
        absolutePanel(actionButton(
          inputId = "Save_HTML",
          label = "Export as HTML",
          style = "background-color: white;
                                        color: black;
                                        border: 1px solid #e7e7e7;
                                        border-radius: 6px;
                                        padding: 8px 12px;"
        ),
        right = 12, top = 1,
        fixed = FALSE
        )
      }
    })


    observeEvent(eventExpr = input$Save_HTML, {
      nodes_out <- Nodes_info()
      edges_out <- Edges_info()

      visNetwork(nodes_out, edges_out) %>%
        # Fija los parámetros por defecto de la primara representación
        visNodes(
          color = list(
            background = DT$color_background,
            border = DT$color_border,
            highlight = list(
              background = DT$color_highlight,
              border = DT$color_border
            )
          ),
          physics = DT$bounce,
          shape = DT$shape
        ) %>%
        visEdges(scaling = list(
          min = DT$scaling_min,
          max = DT$scaling_max,
          label = list(
            enabled = DT$scaling_label_enabled,
            min = DT$scaling_label_min,
            max = DT$scaling_label_max
          )
        )) %>%
        visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE) %>%
        saveWidget(file = paste0(input$Out_name, ".html"))
    })


    observeEvent(eventExpr = input$Save_HTML, {
      shinyalert(
        inputId = "Correct_Saved_HTML",
        title = paste0(input$Out_name, ".html"),
        text = "Saved",
        type = "success",
        time = 5000,
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        showConfirmButton = FALSE,
        cancelButtonText = "Close"
      )
    })


    # SVG
    ######

    'observeEvent( eventExpr = input$SVG,{
      Get_nodes()
      Get_edges()
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      gra <- igraph::graph_from_data_frame( d=edges_out, vertices = nodes_out)

      V(gra)$size <- if (is.null(nodes_out$size)) DT$size/10 else nodes_out$size/10
      V(gra)$color <- if (is.null(nodes_out$color.background)) DT$color_background else nodes_out$color.background
      V(gra)$frame.color <- if (is.null(nodes_out$color.border)) DT$color_border else nodes_out$color.border
      V(gra)$frame.width <- if (is.null(nodes_out$borderWidth)) DT$width else nodes_out$borderWidth
      V(gra)$shape <- "circle"
      V(gra)$label <- if (is.null(nodes_out$label)) NA else nodes_out$label
      V(gra)$label.cex <- if (is.null(nodes_out$font.size)) DT$font_size/12 else nodes_out$font.size/12
      V(gra)$label.color <- if (is.null(nodes_out$font.color)) DT$font_color else nodes_out$font.color

      # Edge
      E(gra)$width <- if (is.null(edges_out$scaling.min)) DT$width else edges_out$scaling.min
      E(gra)$color <- if (is.null(edges_out$color.color)) DT$width else edges_out$color.borde
      E(gra)$lty <- if (isTRUE(edges_out$dashes)) 2 else 1
      E(gra)$label <- if (is.null(edges_out$label)) NA else edges_out$label
      E(gra)$label.cex <- if (is.null(edges_out$font.size)) DT$font_size/12 else edges_out$font.size/12
      E(gra)$label.color <- if (is.null(edges_out$font.color)) DT$font_color else edges_out$font.color


      svg(width=20, height=20)
      plot(gra)
      #plot(gra, edge.arrow.size=.2,  vertex.frame.color="#555555",vertex.label.color="black",vertex.label.cex=.7,rescale=F )
      dev.off()
    })    '


    ## Manejar errores: https://es.acervolima.com/manejo-de-errores-en-la-programacion-de-r/
    ## Guardar sin el voton de guardado: https://groups.google.com/g/shiny-discuss/c/YonYRdf7IK8?pli=1
    output$Output_SVG <- downloadHandler(filename = "salida.svg", content = function(filename) {
      # Esto va con una de retraso
      Get_nodes()
      Get_edges()
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      # nodes_svg <- nodes_out[ which(nodes_out$hidden == FALSE), c("id", "label", "x", "y")]
      # edg_from <- edges_out[which(edges_out$from %in% nodes_svg$id),]
      # edges_svg <- edg_from[ which(edg_from$to %in% nodes_svg$id), c( "id","from", "to", "label")]

      # edges_svg <- edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")]

      # nrow(edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")])
      nodes_svg <- nodes_out[, c("id", "label", "x", "y")]
      nodes_out$y <- sapply(nodes_out$y, function(y) {
        0 - y
      })
      # edges_svg <- edg_from[ , c( "id","from", "to", "label")]
      edges_svg <- edges_out[, c("id", "from", "to", "label")]

      gra <- igraph::graph_from_data_frame(d = edges_out, vertices = nodes_out)

      # gra <- igraph::graph_from_data_frame( d=edges_svg, vertices = nodes_svg)

      tryCatch2(
        expr = {
          V(gra)$size <- if (is.null(nodes_out$size)) DT$size / 5 else nodes_out$size / 5
          V(gra)$color <- if (is.null(nodes_out$color.background)) DT$color_background else nodes_out$color.background
          V(gra)$frame.color <- if (is.null(nodes_out$color.border)) DT$color_border else nodes_out$color.border
          V(gra)$frame.width <- if (is.null(nodes_out$borderWidth)) DT$width else nodes_out$borderWidth
          V(gra)$shape <- "circle"
          V(gra)$label <- if (is.null(nodes_out$label)) NA else nodes_out$label
          V(gra)$label.cex <- if (is.null(nodes_out$font.size)) DT$font_size / 12 else nodes_out$font.size / 12
          V(gra)$label.color <- if (is.null(nodes_out$font.color)) DT$font_color else nodes_out$font.color
          # V(gra)$label.font <- if (is.null(nodes_out$font.face)) DT$font_face else nodes_out$font.face
          V(gra)$label.dist <- -1 # hacemos que la etiqueta se coloque debajo del nodo

          # Edge
          E(gra)$width <- if (is.null(edges_out$scaling.min)) DT$width else edges_out$scaling.min
          E(gra)$arrow.size <- if (is.null(edges_out$scaling.min)) DT$width / 2 else edges_out$scaling.min / 2
          E(gra)$color <- if (is.null(edges_out$color.color)) DT$color_border else edges_out$color.color
          E(gra)$lty <- if (isTRUE(edges_out$dashes)) 2 else 1
          E(gra)$label <- if (is.null(edges_out$label)) NA else edges_out$label
          # E(gra)$label.font <- if (is.null(edges_out$font.face)) DT$font_face else edges_out$font.face
          E(gra)$label.cex <- if (is.null(edges_out$font.size)) DT$font_size / 12 else edges_out$font.size / 12
          E(gra)$label.color <- if (is.null(edges_out$font.color)) DT$font_color else edges_out$font.color
        }
      )
      # View(edges_out)
      # View(DT)

      svg(
        filename = filename,
        width = 20, height = 20
      )
      plot(gra)
      # plot(gra, edge.arrow.size=.2,  vertex.frame.color="#555555",vertex.label.color="black",vertex.label.cex=.7,rescale=F )
      dev.off()
    }, contentType = NA, outputArgs = list())

    '    observeEvent( eventExpr = input$SVG,{
      Get_nodes()
      Get_edges()
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      nodes_svg <- nodes_out[ which(nodes_out$hidden == FALSE), c("id", "label", "x", "y")]
      edg_from <- edges_out[which(edges_out$from %in% nodes_svg$id),]
      edges_svg <- edg_from[ which(edg_from$to %in% nodes_svg$id), c( "id","from", "to", "label")]

      edges_svg <- edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")]

      nrow(edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")])
      nodes_svg <- nodes_out[, c("id", "label", "x", "y")]

      #gra <- igraph::graph_from_data_frame( d=edges_out, vertices = nodes_out)

      gra <- igraph::graph_from_data_frame( d=edges_svg, vertices = nodes_svg)

      V(gra)$size <- if (is.null(nodes_out$size)) DT$size/5 else nodes_out$size/5
      V(gra)$color <- if (is.null(nodes_out$color.background)) DT$color_background else nodes_out$color.background
      V(gra)$frame.color <- if (is.null(nodes_out$color.border)) DT$color_border else nodes_out$color.border
      V(gra)$frame.width <- if (is.null(nodes_out$borderWidth)) DT$width else nodes_out$borderWidth
      V(gra)$shape <- "circle"
      V(gra)$label <- if (is.null(nodes_out$label)) NA else nodes_out$label
      V(gra)$label.cex <- if (is.null(nodes_out$font.size)) DT$font_size/12 else nodes_out$font.size/12
      V(gra)$label.color <- if (is.null(nodes_out$font.color)) DT$font_color else nodes_out$font.color
      #V(gra)$label.font <- if (is.null(nodes_out$font.face)) DT$font_face else nodes_out$font.face
      V(gra)$label.dist <- -1 # hacemos que la etiqueta se coloque debajo del nodo

      # Edge
      E(gra)$width <- if (is.null(edges_out$scaling.min)) DT$width else edges_out$scaling.min
      E(gra)$arrow.size <- if (is.null(edges_out$scaling.min)) DT$width/2 else edges_out$scaling.min/2
      E(gra)$color <- if (is.null(edges_out$color.color)) DT$color_border else edges_out$color.color
      E(gra)$lty <- if (isTRUE(edges_out$dashes)) 2 else 1
      E(gra)$label <- if (is.null(edges_out$label)) NA else edges_out$label
      #E(gra)$label.font <- if (is.null(edges_out$font.face)) DT$font_face else edges_out$font.face
      E(gra)$label.cex <- if (is.null(edges_out$font.size)) DT$font_size/12 else edges_out$font.size/12
      E(gra)$label.color <- if (is.null(edges_out$font.color)) DT$font_color else edges_out$font.color
      #View(edges_out)
      #View(DT)

      svg(width=20, height=20)
      plot(gra)
      #plot(gra, edge.arrow.size=.2,  vertex.frame.color="#555555",vertex.label.color="black",vertex.label.cex=.7,rescale=F )
      dev.off()
    })
    '
    ######


    # #


    width <- input$width
    height <- input$height

    output$save_relationships <- downloadHandler(filename = "network_relationships.zip", content = function(fname) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      # browser()
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

      zip::zip(fname, files)
    }, contentType = "application/zip")
  
  ### end of graph
  
  }
  ############ end of graph ##############################

  # Initialization
  ## Creation



  ## Function to select directory
  # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

  # shinyDirChoose(input, 'directory', roots = volumes)

  # observe({
  # cat("\ninput$directory value:\n\n")
  # print(input$directory)
  # })

  ## Function to show selected directory
  # output$directorypath <- renderText({
  # if (is.integer(input$directory)) {
  #  cat("No directory has been selected")
  # } else {
  # parseDirPath(volumes, input$directory)
  # }
  # })

  ## Infer metagenome

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        path <- choose.dir(
          default = readDirectoryInput(session, "directory"),
          caption = "Choose a directory..."
        )
        updateDirectoryInput(session, "directory", value = path)
      }
    }
  )

  output$directory <- renderText({
    readDirectoryInput(session, "directory")
  })


  output$example_counts <- renderUI({
    tags$a(href = "example_raw_counts.txt", target = "blank", "Example raw counts file", download = "example_raw_counts.txt")
  })

  output$example_seqs <- renderUI({
    tags$a(href = "example_sequences.fasta", target = "blank", "Example sequences file", download = "example_sequences.fasta")
  })

  picrust <- function() {
    use_python("/usr/bin/python")
    use_condaenv(condaenv = "picrust2", required = TRUE)
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
    cmd <- paste(path_python_scripts, "place_seqs.py -s ", input$seqs$datapath, " -o ", readDirectoryInput(session, "directory"), "out.tre -p 5 --intermediate ", readDirectoryInput(session, "directory"), "intermediate/place_seqs", sep = "")
    system(cmd)
    message("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.")
    cmd <- paste(path_python_scripts, "hsp.py -i 16S -t ", readDirectoryInput(session, "directory"), "out.tre -o ", readDirectoryInput(session, "directory"), "16S_predicted_and_nsti.tsv.gz -p 5 -n", sep = "")
    system(cmd)
    message("3. Predict the enzymes of gene families present in the predicted genome for each amplicon sequence variant.")
    cmd <- paste(path_python_scripts, "hsp.py -i EC -t ", readDirectoryInput(session, "directory"), "out.tre -o ", readDirectoryInput(session, "directory"), "EC_predicted.tsv.gz -p 5", sep = "")
    system(cmd)
    message("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence.
            The specified sequence abundance table will be normalized by the predicted number of marker gene copies.")
    cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i ", input$counts$datapath, " -m ", readDirectoryInput(session, "directory"), "16S_predicted_and_nsti.tsv.gz -f ", readDirectoryInput(session, "directory"), "EC_predicted.tsv.gz -o ", readDirectoryInput(session, "directory"), "metagenome_out/ --strat_out", sep = "")
    system(cmd)
    message("5. Convert abundance table.")
    cmd <- paste(path_python_scripts, "convert_table.py ", readDirectoryInput(session, "directory"), "metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o ", readDirectoryInput(session, "directory"), "metagenome_out/pred_metagenome_unstrat.tsv.gz", sep = "")
    system(cmd)
    message("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.")
    cmd <- paste(path_python_scripts, "pathway_pipeline.py -i ", readDirectoryInput(session, "directory"), "metagenome_out/pred_metagenome_contrib.tsv.gz -o ", readDirectoryInput(session, "directory"), "pathways_out/", sep = "")
    system(cmd)
    message("7. Add description column to metagenome abundance table.")
    cmd <- paste(path_python_scripts, "add_descriptions.py -i ", readDirectoryInput(session, "directory"), "metagenome_out/pred_metagenome_unstrat.tsv.gz -m EC -o ", readDirectoryInput(session, "directory"), "metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz", sep = "")
    system(cmd)
    message("8. Add description column to pathways abundance table.")
    cmd <- paste(path_python_scripts, "add_descriptions.py -i ", readDirectoryInput(session, "directory"), "pathways_out/path_abun_unstrat.tsv.gz -m METACYC -o ", readDirectoryInput(session, "directory"), "pathways_out/path_abun_unstrat_descrip.tsv.gz", sep = "")
    system(cmd)
    message((h4(HTML("DONE!"))))
  }

  picrust_button <- observeEvent(input$button_picrust, {
    withCallingHandlers(
      {
        shinyjs::html(id = "predicted_metagenome", "")
        picrust()
      },
      message = function(m) {
        shinyjs::html(id = "predicted_metagenome", html = paste0(m$message, "<br>", "<br>"), add = TRUE)
      }
    )
  })

  # output$predicted_metagenome <- renderPrint(picrust_button())

  output$downloadResults <- downloadHandler(
    filename = function() {
      paste(input$down_files, ".zip", sep = "")
    },
    content = function(file) {
      files <- paste(deploy_dir, input$down_files, sep = "")

      zip::zipr(file, files)
    },
    contentType = "application/zip"
  )



  # Anything that calls autoInvalidate will automatically invalidate
  # every 2 seconds.
  autoInvalidate <- reactiveTimer(10000)

  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()

    # Do something each time this is invalidated.
    updateSelectInput(session, "down_files",
      choices = as.list(list.files(deploy_dir, full.names = FALSE))
    )
  })

  updateSelectInput(session, "down_files",
    choices = as.list(list.files(deploy_dir, full.names = FALSE))
  )
})





