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
        fileInput(ns("input_network_file"), NULL, accept = c(".RData", ".RDATA", ".rdata"))
      ),
      mainPanel(
        width = 9,
        h3("Network Summary : "),
        # h4("This is a classic simple layout that is often used to keep"),
        uiOutput(ns("current_network_info"))
      )
    )
  )
}


load_network_server <- function(shared_session_info, id = "load_network_module") {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

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
        shared_session_info$dagitty <- res$dagitty
      }

      var_list <- list()
      for (i in 1:ncol(shared_session_info$factor_variables)) {
        var_list[[colnames(shared_session_info$factor_variables)[i]]] <- levels(shared_session_info$factor_variables[, i])
      }
      shared_session_info$var_list <- var_list
    }


    load_network_from_env <- function(file){
      res <- new.env()
      res$fittedbn <- NULL
      load(file, envir = res)
      set_shared_session_info(res)
      shared_session_info$build_env <- res
    }



    observeEvent(input$input_network_file,
      {
        browser()
        if (!is.null(input$input_network_file)) {
          shinybusy::show_modal_spinner(
            text = "Please wait, Loading Network"
          )
          tryCatch(
            {
              file <- input$input_network_file$datapath

              ## load new way 
               tryCatch({
                result_env <- readRDS(file)
                set_shared_session_info(result_env)
                shared_session_info$build_env <- result_env
               },
                error = function(cond) {
                  ## load old way 
                  load_network_from_env(file)
                }
              )

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

    output$current_network_info <- renderUI({
      browser()
      if (is.null(shared_session_info$fittedbn)) {
        return(h4("No currect active Network"))
      }
      return(h4("fittedbn loaded OK"))
    })
  })
}