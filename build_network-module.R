table.select.in.sytle <- "
.table-select-in .control-label{
  display: none;
}
.table-select-in select {
  padding: 0;
}

"


build_network_ui <- function(id = "build_network_module") {
  ns <- NS(id)

  data_filter_box <- box(
    title = "Processing Options",
    status = "black",
    closable = FALSE,
    width = 3,
    solidHeader = TRUE,
    collapsible = FALSE,
    p("Specify experimental variables to discretize"),
    uiOutput(ns("discretize_variables_selector"), container = div),
    selectInput(ns("dis_method"), label = "Select discretization method", choices = c("quantile", "interval", "hartemink"), selected = "quantile"),
    div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = ns("apply_data_preprocessing"), label = "Apply", style = "float", color = "primary", size = "sm", icon = icon("refresh")))
  )

  data_vars_summary_box <- box(
    title = "Variables Summary",
    status = "navy",
    closable = FALSE,
    width = 9,
    solidHeader = TRUE,
    collapsible = FALSE,
    fluidRow(
      column(8,DT::dataTableOutput(ns("data_variables_summary")) %>% withSpinner(color = "#0dc500")),
      column(4)
    )
  )


  data_summary_box <- box(
    title = "Variable Table",
    status = "navy",
    closable = FALSE,
    width = 12,
    solidHeader = TRUE,
    collapsible = FALSE,
    # actionButton("update", "Toggle card sidebar"),
    # sidebar = boxSidebar(
    #     id = ns("mycardsidebar"),
    #     p("Sidebar Content")

    # ),
    DT::dataTableOutput(ns("data_variables_table")) %>% withSpinner(color = "#0dc5c1")
  )

  count_data_filters_box <- box(
    title = "Count Filters",
    status = "black",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    tags$style("#filter_taxa {height: 35px;}"),
    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;",
      checkboxInput(ns("filter_taxa"), label = HTML('<h5 style="position: relative;">Filter taxa data by its presence in samples</h5>'), value = FALSE, width = NULL),
      conditionalPanel(
        condition = "input.filter_taxa == 1",
        ns = ns,
        pickerInput(ns("before_after_filter"), "Select applying abundance filter before or after building the network", choices = c("Before", "After")),
        pickerInput(ns("filter_option"), "Select Group to filter by a variables or Total to take into account all variables", choices = c("Total", "Group")),
        conditionalPanel(
          condition = "input.filter_option == 'Total'", ns = ns,
          numericInput(ns("filter_countsT"), "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
          conditionalPanel(
            condition = "input.before_after_filter == 'After'", ns = ns,
            div(style = "padding: 50 px 0 px; width: 100 px", textInput(ns("filter_thrT"), "Select global filter threshold", placeholder = "25,50"))
          ),
          conditionalPanel(
            condition = "input.before_after_filter == 'Before'", ns = ns,
            div(style = "padding: 50 px 0 px; width: 100 px", numericInput(ns("filter_thrT"), "Select global filter threshold", value = 50, min = 0, max = 100, step = 0.5))
          )
        ),
        conditionalPanel(
          condition = "input.filter_option == 'Group'", ns = ns,
          # pickerInput(ns("filter_variable"), "Select a variable name to apply this filter" , choices =c()),
          uiOutput(ns("group_variable_filter_selector")),
          numericInput(ns("filter_countsG"), "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
          div(style = "padding: 50 px 0 px; width: 100 px", textInput(ns("filter_thrG"), "Select filter threshold for each variable condition", placeholder = "condition1-50,condition2-30..."))
        ),
        conditionalPanel(
          condition = "input.before_after_filter == 'Before'", ns = ns,
          div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = ns("apply_count_filters"), label = "Apply Filter", style = "float", color = "primary", size = "sm", icon = icon("refresh"))),
        )
      )
    ),
    checkboxInput(ns("remove_zero_sum_taxa"), label = HTML('<h5 style="position: relative;">Remove taxa with total zero sum of normalized count</h5>'), value = TRUE, width = NULL)
  )
  count_data_preprocessing_box <- box(
    title = "Count Data Processing",
    status = "navy",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    checkboxInput(ns("shorten_taxa_name"), label = "Repalce Taxa Names with short name.", value = TRUE),
    uiOutput(ns("shorten_taxa_name_options")),
    DT::dataTableOutput(ns("shorten_taxa_names_table")) %>% withSpinner(color = "#0dc5c1")
  )



  count_data_t1 <- box(
    title = "Raw Count",
    status = "navy",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = FALSE,
    # actionButton("update", "Toggle card sidebar"),
    # sidebar = boxSidebar(
    #     id = ns("mycardsidebar"),
    #     p("Sidebar Content")

    # ),
    DT::dataTableOutput(ns("count_data_table")) %>% withSpinner(color = "#0dc5c1")
  )

  count_data_t2 <- box(
    title = "Normalized/Filtered Count",
    status = "navy",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = FALSE,
    # actionButton("update", "Toggle card sidebar"),
    # sidebar = boxSidebar(
    #     id = ns("mycardsidebar"),
    #     p("Sidebar Content")

    # ),
    DT::dataTableOutput(ns("normalized_count_data_table")) %>% withSpinner(color = "#0dc5c1")
  )

  network_build_opts <- div(
    style = " margin-left: 1em; margin-right: 1em;",
    selectInput(ns("net_score"), label = "Network score", choices = c(BN_SCORE_AIC, BN_SCORE_BIC, BN_SCORE_loglik ), selected = BN_SCORE_BIC),
    selectInput(ns("net_dist"), label = "Taxa Distribution", choices = c(BN_DIST_LOG_NORMAL, BN_DIST_ZINB), selected = BN_DIST_LOG_NORMAL),
    numericInput(ns("mi_thr"), "Link strength: Mutual Information (MI) threshold", value = 0.05, min = 0, max = 50, step = 0.0005),
    numericInput(ns("bic_thr"), "Link strength: Bayesian Information Criterion (BIC) threshold", value = 0, min = -10000000, max = 10000000, step = 0.5),
    # tags$style("#blacklist {height: 35px; width: 50 px;}"),

    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;",
      checkboxInput(ns("add_experimental_variables_bl"), label = "Remove Experimental Variables interactions/relations from the network model structure", value = FALSE, width = NULL),
      checkboxInput(ns("blacklist"), label = HTML('<h5 style="position: relative; top: -50 px">Blacklist: Links not to be included in the network</h5>'), value = FALSE, width = NULL),
      conditionalPanel(
        condition = "input.blacklist == 1", ns = ns,
        fileInput(ns("black_file"), "Upload blacklist file"),
        div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_bl"))),
      )
    ),
    # tags$style("#whitelist {height: 35px; width: 50 px;}"),
    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;", checkboxInput(ns("whitelist"), label = HTML('<h5 style="position: relative; top: -50 px">Whitelist: Links to be included in the network</h5>'), value = FALSE, width = NULL),
      conditionalPanel(
        condition = "input.whitelist == 1", ns = ns,
        fileInput(ns("white_file"), "Upload whitelist file"),
        div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_wl")))
      )
    )
  )

  wl_table_box <- box(
    title = "White List Overview",
    status = "navy",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = FALSE,
    # actionButton("update", "Toggle card sidebar"),
    # sidebar = boxSidebar(
    #     id = ns("mycardsidebar"),
    #     p("Sidebar Content")

    # ),
    DT::dataTableOutput(ns("wl_table")) %>% withSpinner(color = "#0dc5c1")
  )

  bl_table_box <- box(
    title = "Black List Overview",
    status = "navy",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = FALSE,
    # actionButton("update", "Toggle card sidebar"),
    # sidebar = boxSidebar(
    #     id = ns("mycardsidebar"),
    #     p("Sidebar Content")

    # ),
    DT::dataTableOutput(ns("bl_table")) %>% withSpinner(color = "#0dc5c1")
  )

  input_files_box <- box(
    title = "Input Files",
    status = "navy",
    closable = FALSE,
    #width = 6,
    solidHeader = TRUE,
    collapsible = FALSE,
    width = 3,
        tags$label(h3("Upload Input Files :")),
        hr(),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:2em",
          fileInput(ns("data_variables_file"), "CSV file for experimental variables", accept = ".csv")
        ),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:-4em",
          uiOutput(ns("example_data_variables"))
        ),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:2em",
          fileInput(ns("data_taxas_file"), "CSV file for taxa counts", accept = ".csv")
        ),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:-4em",
          uiOutput(ns("example_data_taxas"))
        ),
        checkboxInput(ns("files_include_sample_names"), label = "File includes Sample names.", value = TRUE),
         uiOutput(ns("files_uploads_validations"))
  )

  build_main_panel <- mainPanel(
        width = 9,
        shinybusy::use_busy_spinner(spin = "fading-circle"),
        tabBox(
          # title = "B4",
          # status = "navy",
          width = 12,
          # tabPanel(
          #   "Input Data",
          #   fluidRow(
          #     input_files_box
          #   ),
          #   fluidRow(
          #     data_summary_box,
          #     count_data_t1
          #   )
          # ),
          tabPanel(
            "Experimental Variables Review",
            fluidRow(
              data_filter_box,
              data_vars_summary_box
            ),
            fluidRow(
              data_summary_box
            )
          ),
          tabPanel(
            "Count Data Review",
            fluidRow(
              count_data_preprocessing_box,
              count_data_filters_box
            ),
            fluidRow(
              count_data_t1,
              count_data_t2
            )
          ),
          tabPanel(
            "BN Configs",
            fluidRow(
              box(
                title = "",
                width = 12,
                closable = FALSE,
                solidHeader = TRUE,
                collapsible = FALSE,
                network_build_opts
              )
            ),
            fluidRow(
              wl_table_box,
              bl_table_box
            )
          ),
          tabPanel(
            "Build",
            div(
              style = "font-size: 10px; padding: 0px 0px;",
              textInput(ns("directory_net"),
                "Specify an output name/folder for your result",
                placeholder = "experiment_1"
              )
            ),
            textOutput(ns("did_it_work")),
            div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = ns("start_net"), label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
            div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = ns("stop_net"), label = "Stop process", style = "float", color = "primary", size = "sm", icon = icon("window-close"))),
            div(class = "buttonagency", style = "display:inline-block", actionBttn(inputId = ns("check_net"), label = "Check status", style = "float", color = "primary", size = "sm", icon = icon("check-square")))
          ),
          tabPanel(
            "Jobs",
            DT::dataTableOutput(ns("current_running_jobs")),
            fluidRow(
              verbatimTextOutput(ns("std_output")),
              verbatimTextOutput(ns("err_output"))
            )
          )
        )
      )

  tabPanel(
    HTML("<b>Learning and training</b>"),
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    ")),
    tags$style(HTML(table.select.in.sytle)),
    # build_main_panel
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$label(h3("Upload Input Files :")),
        hr(),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:2em",
          fileInput(ns("data_variables_file"), "CSV file for experimental variables", accept = ".csv")
        ),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:-4em",
          uiOutput(ns("example_data_variables"))
        ),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:2em",
          fileInput(ns("data_taxas_file"), "CSV file for taxa counts", accept = ".csv")
        ),
        div(
          style = "font-size: 10px; padding: 0px 0px; margin-top:-4em",
          uiOutput(ns("example_data_taxas"))
        ),
        checkboxInput(ns("files_include_sample_names"), label = "File includes Sample names.", value = TRUE),
         uiOutput(ns("files_uploads_validations"))
      ),
      build_main_panel
    )
  )
}


build_network_server <- function(session_data, id = "build_network_module") {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    status_file <- tempfile()

    get_status <- function() {
      scan(status_file, what = "character", sep = "\n", quiet = TRUE)
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
    ##################################

    ## all build options should be saved in a list for later access and loading two

    app_data <- reactiveValues(
      apply_count_filters = -1,
      data_errors = list(
        low_taxa_count = NULL
      )
    )

    current_data <- reactiveValues(
    )
    ##########################################
    output$example_data_variables <- renderUI({
      tags$a(href = "example_data_variables.csv", target = "blank", "Example variables file", download = "example_data_variables.csv")
    })

    output$example_data_taxas <- renderUI({
      tags$a(href = "example_data_taxas.csv", target = "blank", "Example taxas file", download = "example_data_taxas.csv")
    })

    output$example_bl <- renderUI({
      tags$a(href = "example_blacklist_whitelist.csv", target = "blank", "Example blacklist/whitelist file", download = "example_blacklist_whitelist.csv")
    })

    output$example_wl <- renderUI({
      tags$a(href = "example_blacklist_whitelist.csv", target = "blank", "Example blacklist/whitelist file", download = "example_blacklist_whitelist.csv")
    })
    ##########################################
    ## Build control
    # token <- uuid::UUIDgenerate()


    # long_run <- eventReactive(input$start_net, {
    #     # # ###
    #     disable("start_net")
    #     f <- future(
    #         {
    #             fire_running("Reading input files")
    #             print("starting exec future")
    #             Sys.sleep(10)
    #         },
    #         seed = TRUE
    #     )
    #     # return(NULL)
    #     f <- catch(f, function(e) {
    #         print(e$message)
    #         showNotification(e$message)
    #     })
    #     f <- finally(
    #         f,
    #         function() {
    #             # # ###
    #             fire_ready()
    #             enable("start_net")
    #         }
    #     )
    #     return(f)
    # })

    # check <- reactive({
    #     invalidateLater(millis = 1000, session = session)
    #     # # # ###
    #     if (!resolved(long_run())) {
    #         x <- "Job running in background"
    #     } else {
    #         x <- "Async job in background completed"
    #     }
    #     return(x)
    # })


    current_bg_process <- eventReactive(input$start_net, {
      
      validate(
        # need(session_data()$fittedbn, "Please Load network first"),
        need(input$directory_net, "Please specify output name for the result."),
        need(current_data$bn_df_variables, "Please Upload and set experimental variables data"),
        need(current_data$bn_df_taxas, "Please Upload and set taxa counts data")
      )
      # # ###
      output_name <- input$directory_net

      ## check if we alrady have this name before
      validate(
        # need(session_data()$fittedbn, "Please Load network first"),
        need(!output_name %in% names(jobs), paste("The output name : ", output_name, " already exist, Please can you specify another output name"))
      )
      ## TODO :: validate rest of inputs
      ##################################
      taxa_count_filters <- NULL
      if (input$filter_taxa == 1) {
        taxa_count_filters <- list(
          filterThrG = input$filter_thrG,
          filterThrT = input$filter_thrT,
          filter_option = input$filter_option,
          filterVariable = input$filter_variable,
          filterCountsT = input$filter_countsT,
          filterCountsG = input$filter_countsG,
          filterBA = input$before_after_filter
        )
      }

      #################################################
      wl <- current_data$final_wl # could be NULL
      if (is.null(wl)) wl <- c()
      bl <- current_data$final_bl
      if (is.null(bl)) bl <- c()
      network_build_option <- list(
        bl = bl,
        wl = wl
      )
      network_build_option$samba_version <- samba_version
      #################################################
      variable_data_options <- list(
        dismethod = input$dis_method,
        discretize_exp_variables = input$discretize_exp_variables
      )

      ############################

      network_build_option$netscore <- input$net_score
      network_build_option$net_dist <- input$net_dist
      ## testing custom build and score
      # network_build_option$netscore <- BN_SCORE_ZINB

      network_build_option$thr_mi <- input$mi_thr
      network_build_option$thr_bic <- input$bic_thr

      ##################
      network_build_option$remove_zero_sum_taxa <- input$remove_zero_sum_taxa
      network_build_option$taxa_count_filters <- taxa_count_filters

      #######
      net_dir <- paste(deploy_dir, output_name, sep = "")
      dir.create(net_dir)
      std_out_file <- file.path(net_dir, "std_out.txt")
      err_out_file <- file.path(net_dir, "err_out.txt")
      network_build_option$net_dir <- net_dir
      ## current_data$bn_df_variables
      result_env <- isolate(reactiveValuesToList(current_data))

      result_env$network_build_option <- network_build_option
      #result_env$variable_data_options <- variable_data_options

      #bn_df_variables <- current_data$bn_df_variables
      #bn_df_taxas <- current_data$bn_df_taxas
      disable("start_net")
      ## source("network_functions.R", local = TRUE)
      ##
      ##build_bn_model(result_env,network_build_option)
      ##enable("start_net")
      build_func <- function(enclose_env) {
        with(enclose_env, {
          source("network_functions.R", local = TRUE)
          # for(i in 1:10) {
          #      fire_running(paste("Test ",i))
          #      print(paste("Test ",i))
          #      Sys.sleep(5)
          # }
          # create_model(
          #   bn_df_variables,
          #   bn_df_taxas,
          #   net_dir,
          #   network_build_option,
          #   variable_data_options,
          #   taxa_count_filters
          # )
          # for(i in 1:100 ) {
          #     print(i)
          #     Sys.sleep(5)
          #   }
          build_bn_model(result_env,network_build_option)
        })
      }

      ## testing ....
      ### build_func(enclose_env = new.env())
      
      bg_process <- callr::r_bg(
        func = build_func,
        args = list(enclose_env = new.env()),
        supervise = TRUE,
        stdout = std_out_file,
        stderr = err_out_file
      )
      pid <- bg_process$get_pid()
      Sys.sleep(5)
      cmd_args <- bg_process$get_cmdline()
      file_id <- basename(cmd_args[6])
      if(is.null(file_id)) {
        Sys.sleep(10)
        cmd_args <- bg_process$get_cmdline()
        file_id <- basename(cmd_args[6])
      }
      jobs[[output_name]] <<- list(
        name = output_name,
        process_id = pid,
        file_id = file_id,
        r_process = bg_process,
        start_time = Sys.time()
        )
      return(bg_process)
    })

    check_current_bg_process <- reactive({
      if (current_bg_process()$is_alive()) {
        invalidateLater(millis = 1000, session = session)

        x <- "Job running in background"
      } else {
        # # # ###
        x <- "Async job in background completed"
        enable("start_net")
      }
      return(x)
    })

    get_jobs <- reactive({
      # # ###
      print("Geting job status")
      invalidateLater(millis = 10000, session = session)
      ###
      job_list <- matrix(data = NA, nrow = length(jobs), ncol = 6)
      job_list <- data.frame(job_list)
      colnames(job_list) <- c("Name", "Start Time", "Status", "MEM", "CPU", "Notes")
      job_index <- 1
      for (job in jobs) {
        job_list[job_index, 1] <- job$name
        job_list[job_index, 2] <- as.character(as.POSIXct(job$start_time))

        if (job$r_process$is_alive()) {
          job_list[job_index, 3] <- job$r_process$get_status()
          mem_info <- job$r_process$get_memory_info()
          job_list[job_index, 4] <- round(mem_info["vms"] / 1024 / 1024)
          job_list[job_index, 5] <- round(job$r_process$get_cpu_times()["user"] * 100)
        } else {
          job_list[job_index, 3] <- "Finished"
        }
        job_list[job_index, 6] <- paste(job$process_id,":",substr(job$file_id,7,str_length(job$file_id)))

        job_index <- job_index + 1
      }
      job_list
    })



    output$current_running_jobs <- DT::renderDataTable(
      DT::datatable({
        # # # ###
        get_jobs()
      },
      selection = "single"
      )
    )
    observe({
      # # # ###
      ids <- input$current_running_jobs_rows_selected
      if (!is.null(ids)) {
        current_name <- names(jobs)[ids]
        current_process <- jobs[[current_name]]$r_process
        # TODO :: implement this to read from file
        # if (!current_process$is_alive()) {
        #   errs_outs <- current_process$read_all_error()
        #   std_outs <- current_process$read_all_output()
        #   output$std_output <- renderPrint({
        #     writeLines(std_outs)
        #   })
        #   output$err_output <- renderPrint({
        #     writeLines(errs_outs)
        #   })
        # }
      }
    })



    # render the background process message to the UI
    output$did_it_work <- renderText({
      check_current_bg_process()
    })

    observeEvent(input$stop_net, {
      # # # ###
      print("Cancel")
      fire_interrupt()

      # enable("button_auto")
    })
    observeEvent(input$check_net, {
      print("Status")
      showNotification(get_status())
    })
    observeEvent(input$whitelist, {
      if (input$whitelist == 0) {
        # shiny::updateTextInput(session = session ,inputId = "white_file", value = "" )
        shinyjs::reset("white_file")
        current_data$wl <- NULL
      }
    })
    observeEvent(input$white_file, {
      current_data$wl <- fread(file = input$white_file$datapath, sep = "auto", dec = ".", header = T)
    })
    observe({
      wl <- c()
      if (!is.null(current_data$wl)) {
        wl <- rbind(wl, current_data$wl)
      }
      ## TODO :: more logic here for wl
      current_data$final_wl <- wl
    })

    observe({
      ## # # ###
      bl <- c()
      if (!is.null(current_data$bl)) {
        bl <- rbind(bl, current_data$bl)
      }
      if (input$add_experimental_variables_bl) {
        if (!is.null(current_data$bn_df_variables)) {
          exp_variables <- colnames(current_data$bn_df_variables)
          bl <- rbind(
            bl,
            bnlearn::tiers2blacklist(exp_variables)
          )
          bl <- rbind(
            bl,
            bnlearn::tiers2blacklist(rev(exp_variables))
          )
        }
      }
      current_data$final_bl <- bl
    })

    observeEvent(input$blacklist, {
      if (input$blacklist == 0) {
        # shiny::updateTextInput(session = session ,inputId = "white_file", value = "" )
        shinyjs::reset("black_file")
        current_data$bl <- NULL
      }
    })
    observeEvent(input$black_file, {
      current_data$bl <- fread(file = input$black_file$datapath, sep = "auto", dec = ".", header = T)
    })
    ################################################################
    output$group_variable_filter_selector <- renderUI({
      exp_variables <- NULL
      if (!is.null(current_data$orginal_bn_df_variables)) {
        exp_variables <- colnames(current_data$orginal_bn_df_variables)
      }
      pickerInput(ns("filter_variable"),
        label = "Select a variable name to apply this filter", choices = exp_variables,
        selected = NULL,
        multiple = FALSE,
        options = pickerOptions(
          "liveSearch" = FALSE,
          actionsBox = FALSE,
          style = "btn-default"
        )
      )
    })

    output$discretize_variables_selector <- renderUI({
      exp_variables <- NULL
      if (!is.null(current_data$orginal_bn_df_variables)) {
        exp_variables <- colnames(current_data$orginal_bn_df_variables)
      }

      pickerInput(ns("discretize_exp_variables"),
        label = NULL, choices = exp_variables,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          "liveSearch" = TRUE,
          actionsBox = TRUE,
          style = "btn-default"
        )
      )
    })

    ##############################################################
    output$shorten_taxa_name_options <- renderUI({
      if (input$shorten_taxa_name) {
        return(textInput(ns("shorten_taxa_name_prefix"), label = "Prefix for short names", value = "T", placeholder = "T"))
      }
      return(NULL)
    })
    output$shorten_taxa_names_table <- DT::renderDataTable(DT::datatable(
      {
        # # ###
        if (is.null(current_data$shorten_taxa_names)) {
          NULL
        }
        else {
          names_df <- data.frame(current_data$shorten_taxa_names, current_data$orginal_taxa_names)
          colnames(names_df) <- c("Shorten Name", "Orginal Taxa Name")
          names_df
        }
      },
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE
      )
    ))

###################################################################################
    ## load new dataframe from data_variables_file
    ## if OK, current_data$bn_df_variables will be set
    observe({
      if (!is.null(input$data_variables_file)) {
        # show the spinner
        shinybusy::show_modal_spinner(
          text = "Please wait, Recalculating Data Tables"
        )
        tryCatch(
          {
            data_variables <- fread(
              file = input$data_variables_file$datapath,
              sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE
            )
            if(input$files_include_sample_names)
              data_variables <- data.frame(data_variables, row.names = 1)
            else 
              data_variables <- data.frame(data_variables)

            for (i in 1:ncol(data_variables)) {
              c <- class(data_variables[, i])
              if (c == "integer") {
                data_variables[, i] <- as.numeric(data_variables[, i])
              }
            }
            ## TODO :: original
            current_data$orginal_bn_df_variables <- data_variables
            ## get variable name
            current_data$bn_df_variables <- data_variables
          },
          error = function(cond) {
              print(cond)
          },
          finally = {
            shinybusy::remove_modal_spinner()
          }
        )
        # current_data$bn_df_variables
      } else {
        current_data$orginal_bn_df_variables <- NULL
        current_data$bn_df_variables <- NULL

        # return(NULL)
      }
    })

    observe({
      df_data <- NULL
      if(!is.null(current_data$bn_df_variables) ) {
          df_data <- generate_variables_summary(current_data$bn_df_variables)
          # for (i in 1:nrow(df_data)) {
          #   df_data$role[i] <- as.character(
          #     div(class="table-select-in",  selectInput( ns(paste0("var_rol_", i)), "", choices = VAR_ROLES)))
          # }
          df_data <- df_data[,c("type","var_values", "role", "scale_transformation", "to_include", "warning")]
          colnames(df_data) <- c("Type","VAR", "Role", "Scale", "Included", "Warning")

      }
      current_data$variables_summary_table <- df_data
    })
    observe({
      # # ###
      str(sapply(1:11, function(i) input[[ns(paste0("var_rol_", i))]]))
    })
    observe({
      print(input$var_rol_1)
    })

    output$data_variables_summary <- DT::renderDataTable(
      current_data$variables_summary_table,
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE
      )
    )


    observeEvent(input$apply_data_preprocessing,
      {
        # # ###
        variable_data_options <- list(
          dismethod = input$dis_method,
          discretize_exp_variables = input$discretize_exp_variables
        )
        shinybusy::show_modal_spinner(
          text = "Please wait, Recalculating Data Tables"
        )
        tryCatch(
          {
            if (!is.null(current_data$orginal_bn_df_variables)) {
              if (length(variable_data_options$discretize_exp_variables) > 0) {
                current_data$bn_df_variables <- discretize_data_variables(
                  current_data$orginal_bn_df_variables,
                  variable_data_options$discretize_exp_variables,
                  variable_data_options$dismethod
                )
              } else {
                current_data$bn_df_variables <- current_data$orginal_bn_df_variables
              }
            } else {
              current_data$bn_df_variables <- NULL
            }
            current_data$variable_data_options <- variable_data_options
          },
          error = function(cond) {
           app_data$data_errors <- append(app_data$data_errors,cond$message)
          },
          finally = {
            shinybusy::remove_modal_spinner()
          }
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
    )


    output$data_variables_table <- DT::renderDataTable(DT::datatable(
      {
        current_data$bn_df_variables
      },
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    ))
################################################################
    ## load new dataframe from data_taxas_file
    ## if OK, the following will be set
    ## current_data$orginal_taxa_names
    ## current_data$orginal_bn_df_taxas
    observe({
      # # ###
      if (!is.null(input$data_taxas_file)) {
        shinybusy::show_modal_spinner(
          text = "Please wait, Recalculating Data Tables"
        )
        tryCatch({
          data_taxas <- fread(file = input$data_taxas_file$datapath, 
              sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE
            )
          data_taxas <- data.frame(data_taxas, row.names = 1)
          for (i in 1:ncol(data_taxas)) {
            c <- class(data_taxas[, i])
            if (c == "integer") {
              data_taxas[, i] <- as.numeric(data_taxas[, i])
            }
          }
          current_data$orginal_taxa_names <- colnames(data_taxas)
          current_data$orginal_bn_df_taxas <- data_taxas
        },
        error = function(cond) {
          print(cond)
        },
        finally = {
          shinybusy::remove_modal_spinner()
        })
        
        
        # current_data$bn_df_taxas
      } else {
        current_data$orginal_taxa_names <- NULL
        current_data$orginal_bn_df_taxas <- NULL
        # return(NULL)
      }
    })


    output$files_uploads_validations <- renderUI({
      
      is_Valid <- TRUE
      validation_msg <- ""
      l_errs <- length(app_data$data_errors)
      if( l_errs > 0 ) {
        is_Valid <- FALSE
        validation_msg <- app_data$data_errors[[l_errs]]
      }
      validate(
        need(is_Valid,validation_msg)
      )
    })

    # observe current_data$orginal_bn_df_taxas and shorten_taxa_name option to set short taxa names 
    ## current_data$bn_df_taxas based/current_data$shorten_taxa_names on shorten_taxa_name check
    observe({
      # # ###
      if (!is.null(current_data$orginal_bn_df_taxas)) {
        shinybusy::show_modal_spinner(
          text = "Please wait, Refreshing Count Tables"
        )
        tryCatch({
          if (input$shorten_taxa_name) {
            old_taxa_names <- colnames(current_data$orginal_bn_df_taxas)
            new_taxa_names <- old_taxa_names
            taxa_prefix <- input$shorten_taxa_name_prefix
            if (is.null(taxa_prefix)) taxa_prefix <- "T"
            for (i in 1:length(new_taxa_names))
            {
              new_taxa_names[i] <- paste0("T", i)
            }
            names_df <- data.frame(new_taxa_names, old_taxa_names)
            colnames(names_df) <- c("Shorten Name", "Orginal Taxa Name")
            current_data$taxa_names_df <- names_df
            current_data$shorten_taxa_names <- new_taxa_names

            # bn_df_taxas <- current_data$orginal_bn_df_taxas
            # colnames(bn_df_taxas) <- new_taxa_names
            # current_data$bn_df_taxas <- bn_df_taxas
          } else {
            current_data$taxa_names_df <- NULL
            current_data$shorten_taxa_names <- NULL
          }


        },
        error = function(cond) {
          print(cond)
        },
        finally = {
          shinybusy::remove_modal_spinner()
        })
        
        
        # current_data$bn_df_taxas
      } else {
        current_data$shorten_taxa_names <- NULL
        current_data$taxa_names_df <- NULL
      }
    })

    observeEvent(input$apply_count_filters, {
      count_values_ld <- isolate(app_data$apply_count_filters)
      if (count_values_ld != input$apply_count_filters) {
        ## just pressed the btn
        app_data$apply_count_filters <- input$apply_count_filters
      }
    })

    ## observe current_data$orginal_bn_df_taxas and apply filter options
    ## if ok 
    observe({
      # 
      ###
      
      if (!is.null(current_data$orginal_bn_df_taxas)) {
        shinybusy::show_modal_spinner(
          text = "Please wait, Filtering and Normaling Count Data"
        )
        tryCatch({
          taxa_count_filters <- NULL
          #count_values_ld <- isolate(app_data$apply_count_filters)
          if (app_data$apply_count_filters > -1) {
            ## just pressed the btn
            #app_data$apply_count_filters <- input$apply_count_filters
            isolate({
              if (input$filter_taxa & input$before_after_filter == "Before") {
                ## then apply filters here after normalization
                taxa_count_filters <- list(
                  filterThrG = input$filter_thrG,
                  filterThrT = input$filter_thrT,
                  filter_option = input$filter_option,
                  filterVariable = input$filter_variable,
                  filterCountsT = input$filter_countsT,
                  filterCountsG = input$filter_countsG,
                  filterBA = input$before_after_filter
                )
              }
            })
          }
          

          result_list <- fitler_norm_count_data(
                current_data$orginal_bn_df_taxas,
                current_data$orginal_bn_df_variables,
                taxa_count_filters,
                input$remove_zero_sum_taxa)
          
          current_data$bn_df_taxas <- result_list$bn_df_taxas
          current_data$bn_df_taxas_norm <- result_list$bn_df_taxas_norm
          current_data$bn_df_taxas_norm_log <- result_list$bn_df_taxas_norm_log
          current_data$to_remove <- result_list$to_remove
          current_data$to_remove_zero_sums <- result_list$to_remove_zero_sums
          data_errors <- NULL
          if(!input$remove_zero_sum_taxa) {
            count_sums <- colSums(round(result_list$bn_df_taxas_norm))
            low_taxa_count <- sum(count_sums == 0)
            if(low_taxa_count > 0 )
              data_errors <- paste("In total " , low_taxa_count , " taxa: have very low count almost zero. Please use filters to remove them" )
            }
            else {
              removed_low_zeros = length(result_list$to_remove_zero_sums )
              if(removed_low_zeros > 0 ) {
                data_errors <- paste("In total" , removed_low_zeros , "taxa: have been removed due to very low total normalized count,almost zero." )
              }
            }
            isolate({
              ## for not get fire observed again
              app_data$data_errors[["low_taxa_count"]] <- data_errors
            })
          #
        },
        error = function(cond) {
          print(cond)
        },
        finally = {
          shinybusy::remove_modal_spinner()
        })
        # current_data$bn_df_taxas
      } else {
          current_data$bn_df_taxas <- NULL
          current_data$bn_df_taxas_norm <- NULL
          current_data$bn_df_taxas_norm_log <- NULL
          current_data$to_remove <- NULL
      }
    })


    output$count_data_table <- DT::renderDataTable(DT::datatable(
      {
        # if (!is.null(input$data_taxas_file)) {
        #     data_taxas <- fread(file = input$data_taxas_file$datapath, sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE)
        #     data_taxas <- data.frame(data_taxas, row.names = 1)
        #     for (i in 1:ncol(data_taxas)) {
        #         c <- class(data_taxas[, i])
        #         if (c == "integer") {
        #             data_taxas[, i] <- as.numeric(data_taxas[, i])
        #         }
        #     }

        #     current_data$bn_df_taxas <- data_taxas
        #     #current_data$bn_df_taxas
        # } else {
        #     current_data$bn_df_taxas <- NULL
        #     #return(NULL)
        # }
        bn_df_taxas  <- current_data$orginal_bn_df_taxas
        if(!is.null(bn_df_taxas) && !is.null(current_data$shorten_taxa_names)) {
          colnames(bn_df_taxas) <- current_data$shorten_taxa_names
        }
        bn_df_taxas
      },
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE
      )
    ))

    output$normalized_count_data_table <- DT::renderDataTable(DT::datatable(
      {
        # # ###
        if (is.null(current_data$bn_df_taxas_norm)) {
          return(NULL)
        }

        bn_df_taxas_norm <- current_data$bn_df_taxas_norm
        if (!is.null(bn_df_taxas_norm)) {
          bn_df_taxas_norm <- round(bn_df_taxas_norm)
          if(!is.null(current_data$shorten_taxa_names)) {
            old_names <- colnames(bn_df_taxas_norm)
            new_names <-  current_data$taxa_names_df[ current_data$taxa_names_df[,2] %in% old_names , 1]
            colnames(bn_df_taxas_norm) <- new_names
          }
        }
        bn_df_taxas_norm
      },
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE
      )
    ))

###########################################################################


    output$wl_table <- DT::renderDataTable(DT::datatable(
      {
        if (is.null(current_data$final_wl)) {
          return(NULL)
        }
        as.data.frame(current_data$final_wl)
      },
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE
      )
    ))
    output$bl_table <- DT::renderDataTable(DT::datatable(
      {
        if (is.null(current_data$final_bl)) {
          return(NULL)
        }
        as.data.frame(current_data$final_bl)
      },
      selection = "single",
      options = list(
        lengthChange = FALSE,
        scrollY = TRUE,
        scrollX = TRUE
      )
    ))
  })
}