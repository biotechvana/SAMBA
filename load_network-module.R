load_network_ui <- function(id = "load_network_module") {
  ns <- NS(id)

  count_data_filters_box <- box(
    title = "Network After Filters",
    status = "black",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    tags$style("#filter_taxa {height: 35px;}"),
    div(
      style = "border-bottom-style: ridge; border-color:#DAD7D6; border-top-style: ridge; margin-top: 1em; margin-bottom: 1em;",
      HTML('<h5 style="position: relative;">Filter taxa data by its presence in samples after network build</h5>'),
      pickerInput(ns("filter_option"), "Select Group to filter by a variables or Total to take into account all variables", choices = c("Total", "Group")),
      conditionalPanel(
        condition = "input.filter_option == 'Total'", ns = ns,
        numericInput(ns("filter_countsT"), "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
        div(style = "padding: 50 px 0 px; width: 100 px", textInput(ns("filter_thrT"), "Select global filter threshold", placeholder = "25,50"))
      ),
      conditionalPanel(
        condition = "input.filter_option == 'Group'", ns = ns,
        # pickerInput(ns("filter_variable"), "Select a variable name to apply this filter" , choices =c()),
        uiOutput(ns("group_variable_filter_selector")),
        numericInput(ns("filter_countsG"), "Specify a minimum number of counts to apply this filter", value = 10, min = 0, step = 0.5),
        # div(style = "padding: 50 px 0 px; width: 100 px", textInput(ns("filter_thrG"), "Select filter threshold for each variable condition", placeholder = "condition1-50,condition2-30...")),
        uiOutput(ns("group_filter_thrG_input"))
      ),
      div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = ns("apply_count_filters"), label = "Preview Filter", style = "float", color = "primary", size = "sm", icon = icon("refresh")))
    ),
    div(
      style = "font-size: 10px; padding: 0px 0px;",
      textInput(ns("directory_net"),
        "Specify an output name/folder for your result",
        placeholder = "experiment_1"
      )
    ),
    uiOutput(ns("did_it_work")),
    div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = ns("start_filter"), label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket")))
  )
  count_data_preprocessing_box <- box(
    title = "Filter Preview",
    status = "navy",
    closable = FALSE,
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    uiOutput(ns("filter_result_validation"))
  )

  tabPanel(
    HTML("<b>Load Network</b>"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$label(h3("Input network")),
        p("Load network from a previous build"),
        # hr(),
        fileInput(ns("input_network_file"), NULL, accept = c(".RData", ".RDATA", ".rdata")),
        uiOutput(ns("more_network_params"))
      ),
      mainPanel(
        width = 9,
        # h3("Network Summary : "),
        # h4("This is a classic simple layout that is often used to keep"),
        shinybusy::use_busy_spinner(spin = "fading-circle"),
        tabBox(
          width = 12,
          tabPanel(
            "Network Summary",
            fluidRow(
              uiOutput(ns("current_network_info")),
            )
          ),
          tabPanel(
            "Metrics Overview",
            fluidRow(
              column(
                6,
                DT::dataTableOutput(ns("all_metrics_table")) %>% withSpinner(color = "#0dc5c1")
              ),
              column(
                6,
                uiOutput(ns("metrics_taxa_view")) %>% withSpinner(color = "#0dc5c1"),
                plotOutput(ns("metrics_plot_view")) %>% withSpinner(color = "#0dc5c1")
              )
            )
          ),
          tabPanel(
            "Build/Filter",
            fluidRow(
              count_data_filters_box,
              count_data_preprocessing_box
            )
          )
        )
      )
    )
  )
}


load_network_server <- function(shared_session_info, id = "load_network_module") {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    current_data <- reactiveValues(
      selected_mt_taxa = NULL,
      info_in_log_scale = TRUE
    )

    set_shared_session_info <- function(res) {
      shared_session_info$fittedbn <- res$fittedbn
      shared_session_info$bn_df_variables <- res$bn_df_variables
      shared_session_info$bn_df_norm <- res$bn_df_norm
      shared_session_info$bn_df_taxas <- res$bn_df_taxas
      shared_session_info$factor_variables <- res$bn_df_variables[
        ,
        sapply(res$bn_df_variables, class) == "factor"
      ]
      shared_session_info$taxa_names <- colnames(res$bn_df_taxas)
      shared_session_info$exposure_variables <- colnames(res$bn_df_variables)
      shared_session_info$outcome_variables <- colnames(res$bn_df_taxas)
      ## for building dagitty we need to see if we already have it or not
      if (is.null(res$dagitty)) {
        ## build it and add all impliedConditionalIndependencies
        res$dagitty <- bn_to_dagitty(res$fittedbn)
        ## for the moment make this init here
        exposure_variables <- colnames(res$bn_df_variables)
        outcome_variables <- colnames(res$bn_df_taxas)

        dagitty::exposures(res$dagitty) <- exposure_variables
        dagitty::outcomes(res$dagitty) <- outcome_variables
      }
      shared_session_info$dagitty <- res$dagitty
      if (!is.null(res$testable_implications_taxa_vars)) {
        shared_session_info$testable_implications_taxa_vars <- res$testable_implications_taxa_vars
      }
      if (!is.null(res$fittedbn_custom)) {
        shared_session_info$fittedbn_custom <- res$fittedbn_custom
      }

      var_list <- list()
      for (i in 1:ncol(shared_session_info$factor_variables)) {
        var_list[[colnames(shared_session_info$factor_variables)[i]]] <- levels(shared_session_info$factor_variables[, i])
      }
      shared_session_info$var_list <- var_list
    }


    load_network_from_env <- function(file) {
      res <- new.env()
      res$fittedbn <- NULL
      load(file, envir = res)
      set_shared_session_info(res)
      shared_session_info$build_env <- res
    }


    output$filter_result_preview <- renderUI({
      if (is.null(shared_session_info$build_env)) {
        NULL
      } else {
        "Loaded"
      }
    })

    output$group_variable_filter_selector <- renderUI({
      exp_variables <- NULL
      if (!is.null(shared_session_info$build_env$orginal_bn_df_variables)) {
        exp_variables <- colnames(shared_session_info$build_env$orginal_bn_df_variables)
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
    output$group_filter_thrG_input <- renderUI({
      if (is.null(input$filter_variable)) {
        ""
      } else {
        data_type <- class(shared_session_info$build_env$orginal_bn_df_variables[[input$filter_variable]])
        if (data_type == "factor") {
          level_lists <- levels(shared_session_info$build_env$orginal_bn_df_variables[[input$filter_variable]])
          tags <- tagList(c())
          for (i in seq_len(length(level_lists))) {
            level_name <- level_lists[i]
            tags <- tagList(
              tags,
              div(
                style = "padding: 50 px 0 px; width: 100 px",
                numericInput(ns(paste("filter_thrG", i, sep = "_")),
                  paste("Select filter threshold for", level_name, "condition"),
                  value = 5,
                  min=0,
                  max = 100
                )
              ),
            )
          }

          tags
        } else {
          "The selected Variable can not be used, It is not factor"
        }
      }
    })

    apply_preview_filter <- eventReactive(input$apply_count_filters,
      {
        ##### Test Filters
        validate(
          need(shared_session_info$build_env, "Please load network first"),
        )
        remove_zero_sum_taxa <- FALSE
        if (!is.null(shared_session_info$build_env$network_build_option)) {
          if (!is.null(shared_session_info$build_env$network_build_option$remove_zero_sum_taxa)) {
            remove_zero_sum_taxa <- shared_session_info$build_env$network_build_option$remove_zero_sum_taxa
          }
        }

        if (input$filter_option == "Group") {
          filterThrG <- ""
          validate(
            # need(input$filter_thrG, "Threshold value is required"),
            need(input$filter_countsG, "Min Count value is required"),
            need(input$filter_variable, "Experiment/variable is required")
          )
          level_lists <- levels(shared_session_info$build_env$orginal_bn_df_variables[[input$filter_variable]])
          tags <- tagList(c())
          for (i in seq_len(length(level_lists))) {
            value <- input[[paste("filter_thrG", i, sep = "_")]]
            if (is.null(value) || value == "") {
              validate(
                need(FALSE, paste("Threshold value is required for ", level_lists[i])),
              )
            }
            if (i == 1) {
              filterThrG <- paste(level_lists[i], value, sep = "-")
            } else {
              filterThrG <- paste(filterThrG, paste(level_lists[i], value, sep = "-"), sep = ",")
            }
          }



          taxa_count_filters <- list(
            filterBA = "After",
            filter_option = input$filter_option,
            filterThrG = filterThrG, # Select filter threshold for each variable condition
            filterVariable = input$filter_variable,
            filterCountsG = input$filter_countsG # "Specify a minimum number of counts to apply this filter"
          )
          # return(taxa_count_filters$filterThrG)
        }

        if (input$filter_option == "Total") {
          validate(
            need(input$filter_thrT, "Threshold value is required"),
            need(input$filter_countsT, "Min Count value is required")
          )
          taxa_count_filters <- list(
            filterBA = "After",
            filter_option = input$filter_option,
            filterThrT = input$filter_thrT, # Select global filter threshold
            filterCountsT = input$filter_countsT # Specify a minimum number of counts to apply this filter
          )
        }
        ##





        shinybusy::show_modal_spinner(
          text = "Please wait, Apply Filter"
        )
        tags <- tagList(c())
        tryCatch(
          {
            if (input$filter_option == "Group") {
              taxa_count_filters_ <- taxa_count_filters
              taxa_count_filters_$filterBA <- "Before"
              result_list <- fitler_norm_count_data(
                shared_session_info$build_env$orginal_bn_df_taxas,
                shared_session_info$build_env$orginal_bn_df_variables,
                taxa_count_filters_,
                remove_zero_sum_taxa
              )
              tags <- htmltools::tagAppendChild(
                tags,
                shiny::fluidRow(
                  shiny::column(
                    5,
                    htmltools::strong(
                      paste("Total to be removed:", length(result_list$to_remove))
                    )
                  )
                )
              )
            }
            if (input$filter_option == "Total") {
              filterThrT_sep <- strsplit(taxa_count_filters$filterThrT, ",")
              for (thr in filterThrT_sep[[1]]) {
                print(thr)
                # TODO  :: validate
                thr <- as.numeric(thr)
                validate(
                  need(thr > 0 && thr <= 100, "Threshold value must be > 0 and <= 100"),
                )
              }
              for (thr in filterThrT_sep[[1]]) {
                thr <- as.numeric(thr)
                min_Count_ <- as.numeric(taxa_count_filters$filterCountsT)
                taxa_count_filters_itr <- taxa_count_filters
                taxa_count_filters_itr$filterBA <- "Before"
                taxa_count_filters_itr$filterThrT <- thr

                result_list <- fitler_norm_count_data(
                  shared_session_info$build_env$orginal_bn_df_taxas,
                  shared_session_info$build_env$orginal_bn_df_variables,
                  taxa_count_filters_itr,
                  remove_zero_sum_taxa
                )

                tags <- htmltools::tagAppendChild(
                  tags,
                  shiny::fluidRow(
                    shiny::column(4, htmltools::span(paste("Threshold=", thr, ":"))),
                    shiny::column(
                      4,
                      htmltools::strong(
                        paste("Totol removed:", length(result_list$to_remove))
                      )
                    )
                  )
                )
              }
            }
          },
          error = function(cond) {
            validate(
              need(FALSE, paste("An Error ocure:", message(cond))),
            )
          },
          finally = {
            shinybusy::remove_modal_spinner()
          }
        )
        tags
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )



    start_build_filter <- eventReactive(input$start_filter,
      {
        ##### Test Filters
        validate(
          need(shared_session_info$build_env, "Please load network first"),
        )
        remove_zero_sum_taxa <- FALSE
        if (!is.null(shared_session_info$build_env$network_build_option)) {
          if (!is.null(shared_session_info$build_env$network_build_option$remove_zero_sum_taxa)) {
            remove_zero_sum_taxa <- shared_session_info$build_env$network_build_option$remove_zero_sum_taxa
          }
        }

        if (input$filter_option == "Group") {
          filterThrG <- ""
          validate(
            # need(input$filter_thrG, "Threshold value is required"),
            need(input$filter_countsG, "Min Count value is required"),
            need(input$filter_variable, "Experiment/variable is required")
          )
          level_lists <- levels(shared_session_info$build_env$orginal_bn_df_variables[[input$filter_variable]])
          tags <- tagList(c())
          for (i in seq_len(length(level_lists))) {
            value <- input[[paste("filter_thrG", i, sep = "_")]]
            if (is.null(value) || value == "") {
              validate(
                need(FALSE, paste("Threshold value is required for ", level_lists[i])),
              )
            }
            if (i == 1) {
              filterThrG <- paste(level_lists[i], value, sep = "-")
            } else {
              filterThrG <- paste(filterThrG, paste(level_lists[i], value, sep = "-"), sep = ",")
            }
          }



          taxa_count_filters <- list(
            filterBA = "After",
            filter_option = input$filter_option,
            filterThrG = filterThrG, # Select filter threshold for each variable condition
            filterVariable = "Experiments",
            filterCountsG = input$filter_countsG # "Specify a minimum number of counts to apply this filter"
          )
          # return(taxa_count_filters$filterThrG)
        }

        if (input$filter_option == "Total") {
          validate(
            need(input$filter_thrT, "Threshold value is required"),
            need(input$filter_countsT, "Min Count value is required")
          )

          taxa_count_filters <- list(
            filterBA = "After",
            filter_option = input$filter_option,
            filterThrT = input$filter_thrT, # Select global filter threshold
            filterCountsT = input$filter_countsT # Specify a minimum number of counts to apply this filter
          )
          filterThrT_sep <- strsplit(taxa_count_filters$filterThrT, ",")
          for (thr in filterThrT_sep[[1]]) {
            print(thr)
            # TODO  :: validate
            thr <- as.numeric(thr)
            validate(
              need(thr > 0 && thr <= 100, "Threshold value must be > 0 and <= 100"),
            )
          }
        }
        validate(
            need(input$directory_net,"output folder is required")
        )
        output_name <- input$directory_net
        net_dir <- paste(deploy_dir, input$directory_net, "/", sep = "")
        validate(
            # need(session_data()$fittedbn, "Please Load network first"),
            need(!dir.exists(net_dir), paste("The output folder : ", input$directory_net, " already exist, Please can you specify another output folder"))
        )
        dir.create(net_dir)
        std_out_file <- file.path(net_dir, "std_out.txt")
        err_out_file <- file.path(net_dir, "err_out.txt")
        result_env <- shared_session_info$build_env
        build_func <- function(enclose_env) {
          with(enclose_env, {
            source("network_functions.R", local = TRUE)
            apply_after_filter(result_env, net_dir, taxa_count_filters)
          })
        }


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

      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    output$filter_result_validation <- renderUI({
      apply_preview_filter()
    })
    output$did_it_work <- renderUI({
      start_build_filter()
      ""
    })

    observeEvent(input$input_network_file,
      {
        shiny::req(input$input_network_file, cancelOutput = TRUE)

        # ###
        if (!is.null(input$input_network_file)) {
          shinybusy::show_modal_spinner(
            text = "Please wait, Loading Network ...."
          )
          tryCatch(
            {
              file <- input$input_network_file$datapath

              ## load new way
              tryCatch(
                {
                  result_env <- readRDS(file)
                  set_shared_session_info(result_env)
                  shared_session_info$build_env <- result_env
                },
                error = function(cond) {
                  ## load old way
                  load_network_from_env(file)
                }
              )
              current_data$info_in_log_scale <- TRUE


              if (!is.null(shared_session_info$build_env)) {
                net_score <- shared_session_info$build_env$network_build_option$netscore
                net_dist <- shared_session_info$build_env$network_build_option$net_dist
                ## account for backward compatiablity of result before this change
                if (is.null(net_dist)) {
                  if (net_score == BN_SCORE_ZINB || net_score == "custom.zinb") {
                    net_score <- BN_SCORE_BIC
                    net_dist <- BN_DIST_ZINB
                  } else {
                    net_dist <- BN_DIST_LOG_NORMAL
                  }
                }
                shared_session_info$build_env$network_build_option$netscore <- net_score
                shared_session_info$build_env$network_build_option$net_dist <- net_dist

                if (is.null(shared_session_info$build_env$taxa_metrics)) {
                  shinybusy::show_modal_spinner(
                    text = "Please wait, Calculating Network Metrics"
                  )
                  targets <- colnames(
                    shared_session_info$build_env$bn_df_taxas
                  )
                  if (net_dist == BN_DIST_ZINB) {
                    current_data$info_in_log_scale <- FALSE
                    taxa_metrics <- do.zinb.bn.cv(
                      shared_session_info$build_env$input_bn_df,
                      shared_session_info$build_env$result_filt,
                      shared_session_info$build_env$fittedbn_custom,
                      targets,
                      colnames(shared_session_info$build_env$bn_df_variables)
                    )
                    taxa_metrics <- zinb.bn.collect.metrics(
                      shared_session_info$build_env$input_bn_df,
                      shared_session_info$build_env$fittedbn_custom,
                      targets,
                      taxa_metrics
                    )
                  } else {
                    # taxa_metrics <- do.bn.cv(
                    #   shared_session_info$build_env$bn_df_norm,
                    #   shared_session_info$build_env$result_filt,
                    #   targets,
                    #   colnames(shared_session_info$build_env$bn_df_variables)
                    # )

                    taxa_metrics <- bn.collect.metrics(
                      shared_session_info$build_env$bn_df_norm,
                      shared_session_info$fittedbn,
                      targets,
                      NULL
                    )
                  }



                  shared_session_info$build_env$taxa_metrics <- taxa_metrics
                }
              }
            },
            error = function(cond) {
              print(cond)
            },
            finally = {
              shinybusy::remove_modal_spinner()
            }
          )
        } else {

        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    output$more_network_params <- renderUI({
      shiny::req(shared_session_info$build_env, cancelOutput = TRUE)
      output <- htmltools::tagList()
      if (shared_session_info$build_env$network_build_option$net_dist == BN_DIST_ZINB) {

      } else {
        output <- htmltools::tagAppendChild(
          output,
          checkboxInput(ns("info_in_log_scale"), label = "View info in Log scale", value = TRUE)
        )
      }

      output
    })


    output$current_network_info <- renderUI({
      # shiny::req(shared_session_info$build_env, cancelOutput = TRUE)

      output <- htmltools::tagList()
      if (is.null(shared_session_info$fittedbn)) {
        output <- htmltools::tagAppendChild(
          output,
          htmltools::span(
            htmltools::strong("No currect active network")
          )
        )
      } else {
        n_nodes <- length(bnlearn::nodes(shared_session_info$fittedbn))
        n_edges <- length(bnlearn::arcs(shared_session_info$fittedbn))

        output <- tagAppendChild(output, h4("Network loaded : OK"))
        output <- htmltools::tagAppendChild(
          output,
          shiny::fluidRow(
            shiny::column(1, htmltools::span("Nodes :")),
            shiny::column(
              3,
              htmltools::strong(
                n_nodes
              )
            )
          )
        )


        output <- htmltools::tagAppendChild(
          output,
          shiny::fluidRow(
            shiny::column(1, htmltools::span("Edges : ")),
            shiny::column(
              3,
              htmltools::strong(
                n_edges
              )
            )
          )
        )

        output <- htmltools::tagAppendChild(
          output,
          htmltools::br()
        )

        if (!is.null(shared_session_info$build_env$taxa_metrics)) {
          output <- htmltools::tagAppendChild(
            output,
            h4("Average Stats :")
          )

          ppc <- shared_session_info$build_env$taxa_metrics[["all"]]$loss
          if (!is.null(ppc)) {
            output <- htmltools::tagAppendChild(
              output,
              shiny::fluidRow(
                shiny::column(
                  3,
                  htmltools::span("Posterior Predictive Correlation : ")
                ),
                shiny::column(
                  3,
                  htmltools::strong(
                    round(ppc, 4)
                  )
                )
              )
            )
          }

          R2 <- shared_session_info$build_env$taxa_metrics[["all"]]$R2
          RMSE <- shared_session_info$build_env$taxa_metrics[["all"]]$RMSE
          dispersion <- shared_session_info$build_env$taxa_metrics[["all"]]$dispersion

          if (current_data$info_in_log_scale) {
            if (!is.null(shared_session_info$build_env$taxa_metrics[["all"]]$R2_lgs)) {
              R2 <- shared_session_info$build_env$taxa_metrics[["all"]]$R2_lgs
              RMSE <- shared_session_info$build_env$taxa_metrics[["all"]]$RMSE_lgs
              dispersion <- shared_session_info$build_env$taxa_metrics[["all"]]$dispersion_lgs
            }
          }



          output <- htmltools::tagAppendChild(
            output,
            shiny::fluidRow(
              shiny::column(2, htmltools::span("R2 : ")),
              shiny::column(
                3,
                htmltools::strong(
                  round(R2, 4)
                )
              )
            )
          )

          output <- htmltools::tagAppendChild(
            output,
            shiny::fluidRow(
              shiny::column(2, htmltools::span("RMSE : ")),
              shiny::column(
                3,
                htmltools::strong(
                  round(RMSE, 4)
                )
              )
            )
          )

          output <- htmltools::tagAppendChild(
            output,
            shiny::fluidRow(
              shiny::column(2, htmltools::span("Dispersion : ")),
              shiny::column(
                3,
                htmltools::strong(
                  round(dispersion, 4)
                )
              )
            )
          )
        }




        net_score <- shared_session_info$build_env$network_build_option$netscore
        net_dist <- shared_session_info$build_env$network_build_option$net_dist
        output <- htmltools::tagAppendChild(
          output,
          htmltools::hr()
        )
        output <- htmltools::tagAppendChild(
          output,
          htmltools::h4("Build Information:")
        )
        output <- htmltools::tagAppendChild(
          output,
          htmltools::span("Score : ")
        )
        output <- htmltools::tagAppendChild(
          output,
          htmltools::strong(net_score)
        )
        output <- htmltools::tagAppendChild(
          output,
          htmltools::br()
        )
        output <- htmltools::tagAppendChild(
          output,
          htmltools::span("Distribution  : ")
        )
        output <- htmltools::tagAppendChild(
          output,
          htmltools::strong(net_dist)
        )
      }

      output
    })

    observeEvent(input$info_in_log_scale, {
      current_data$info_in_log_scale <- input$info_in_log_scale
    })

    get_all_metrics_table <- reactive({
      ## ###
      n_nodes <- 0
      all_metrics <- shared_session_info$build_env$taxa_metrics
      if (is.null(all_metrics)) {
        all_metrics <- list()
      }
      n_nodes <- length(all_metrics)
      metrics_table <- matrix(data = NA, nrow = n_nodes, ncol = 5)
      metrics_table <- data.frame(metrics_table)
      colnames(metrics_table) <- c("Name", "PPC", "RMSE", "R2", "Dispersion")
      node_index <- 1
      for (node_metrics in all_metrics) {
        metrics_table[node_index, 1] <- names(all_metrics)[node_index]
        # node_metrics <- metrics_table[[node_index]]
        if (!is.null(node_metrics$loss)) {
          metrics_table[node_index, 2] <- round(node_metrics$loss, 2)
        }
        metrics_table[node_index, 3] <- round(node_metrics$RMSE, 2)
        metrics_table[node_index, 4] <- round(node_metrics$R2, 2)
        metrics_table[node_index, 5] <- round(node_metrics$dispersion, 2)
        if (current_data$info_in_log_scale) {
          if (!is.null(node_metrics$RMSE_lgs)) {
            metrics_table[node_index, 3] <- round(node_metrics$RMSE_lgs, 2)
            metrics_table[node_index, 4] <- round(node_metrics$R2_lgs, 2)
            metrics_table[node_index, 5] <- round(node_metrics$dispersion_lgs, 2)
          }
        }

        node_index <- node_index + 1
      }
      metrics_table
    })

    observe({
      # # ###
      metrics_table <- get_all_metrics_table()
      if (nrow(metrics_table) > 0) {
        # footer <- as.list(metrics_table[1, ])

        metrics_table[1, 1] <- "Average"
        sketch <- htmltools::withTags(table(
          tableHeader(metrics_table),
          tableFooter(sapply(metrics_table, function(x) x[1]))
        ))
        metrics_table <- metrics_table[2:nrow(metrics_table), ]
      } else {
        sketch <- htmltools::withTags(table(
          tableHeader(metrics_table)
        ))
      }
      current_data$metrics_table <- metrics_table
      current_data$metrics_taxa_names <- metrics_table$Name

      output$all_metrics_table <- DT::renderDataTable(
        DT::datatable(
          {
            metrics_table
          },
          selection = "single",
          container = sketch,
          rownames = FALSE,
          options = list(
            lengthChange = FALSE,
            scrollY = TRUE,
            scrollX = TRUE
          )
        )
      )
    })

    observe({
      # # ###
      # input$all_metrics_table_rows_selected
      if (is.null(input$all_metrics_table_rows_selected)) {
        current_data$selected_mt_taxa <- NULL
      } else {
        ids <- input$all_metrics_table_rows_selected
        current_data$selected_mt_taxa <- current_data$metrics_taxa_names[ids]
      }
    })
    output$metrics_taxa_view <- renderUI({
      if (is.null(current_data$selected_mt_taxa)) {
        htmltools::strong("Average Residuals Plot for all taxa ")
      } else {
        htmltools::strong(paste("Residuals Plot for : ", current_data$selected_mt_taxa))
      }
    })
    output$metrics_plot_view <- renderPlot({
      if (is.null(current_data$selected_mt_taxa)) {
        node_metrics <- shared_session_info$build_env$taxa_metrics[["all"]]
      } else {
        node_metrics <- shared_session_info$build_env$taxa_metrics[[current_data$selected_mt_taxa]]
      }
      # if()
      xlab <- "Taxa"
      residuals <- node_metrics$residuals
      if (current_data$info_in_log_scale) {
        if (!is.null(node_metrics$residuals_lgs)) {
          residuals <- node_metrics$residuals_lgs
        }
      }
      if (!is.null(residuals) && !is.na(residuals)) {
        if (TRUE) {
          hist(
            residuals,
            density = 80,
            col = "red",
            main = paste("Histogram of", current_data$selected_mt_taxa, " Residuals"), xlab = "Residuals"
          )
        } else {
          plot(residuals, ylab = "Residuals", xlab = xlab)
          abline(0, 0)
        }
      }
    })
  })
}
