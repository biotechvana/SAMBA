load_network_ui <- function(id = "load_network_module") {
  ns <- NS(id)
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
            "Network Summry",
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



    observeEvent(input$input_network_file,
      {
        shiny::req(input$input_network_file, cancelOutput = TRUE)

        # browser()
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
      ## browser()
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
        if(!is.null(node_metrics$loss))
          metrics_table[node_index, 2] <- round(node_metrics$loss, 2)
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
      # # browser()
      metrics_table <- get_all_metrics_table()
      if(nrow(metrics_table) > 0) {
        
        #footer <- as.list(metrics_table[1, ])
        
        metrics_table[1,1] <- "Average"
        sketch <- htmltools::withTags(table(
          tableHeader(metrics_table),
          tableFooter(sapply(metrics_table, function(x) x[1]))
        ))
        metrics_table <- metrics_table[2:nrow(metrics_table), ]
      }
      else {
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
      # # browser()
      #input$all_metrics_table_rows_selected
        if ( is.null(input$all_metrics_table_rows_selected) ) {
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
      # browser()

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
      if(!is.null(residuals)) {

        if(TRUE) {
          hist(
            residuals,
            density = 80,
            col = 'red', 
            main = paste("Histogram of" , current_data$selected_mt_taxa , " Residuals") , xlab ="Residuals"
          )
        }
        else {
          plot(residuals,ylab="Residuals", xlab=xlab)
          abline(0, 0) 
        }
      }
       


    })

  })
}