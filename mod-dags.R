nodes_dags_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        HTML("<b>DAG</b>"),
        fluidRow(
            box(
                #title = "B1", 
                status = "black",
                width = 4,
                height = 250,
                solidHeader = TRUE,
                # collapsible = TRUE,
                p("Select a Node to display Info"),
                uiOutput(ns("selector_cpt"), container = div),
                checkboxInput(ns("show_indirect_markov"), label = "Include the indirect Markov Blanket of any Taxa that in the selected Taxa's Markov Blanket", value = FALSE, width = NULL)
            ),
            box(
                #title = "B2", 
                status = "navy",
                width = 8,
                height = 250,
                solidHeader = TRUE,
                # collapsible = TRUE,
                plotOutput(ns("markov_blanket")) %>% withSpinner(color = "#0dc5c1")
            )
        ),
        fluidRow(
            column(
                width = 12,
                tabBox(
                    #title = "B4",
                    # status = "navy",
                    width = 12,
                    # solidHeader = TRUE,
                    # collapsible = TRUE,
                    tabPanel("Conditional Independence",
                        p("Select variables to condition on (control/intervention)"),
                        uiOutput(ns("exposure_variables_placeholder"), container = div),
                        uiOutput(ns("ci_test_info"))),
                    #tabPanel("Debug Info", verbatimTextOutput(ns("debug_output"))),
                    tabPanel("Testable Implications",
                        #checkboxInput(ns("taxa_testable_implications"), label = "Use Taxa as known to drive implications",value = FALSE),
                        uiOutput(ns("testable_implications")))
                )
            )
        )
    )
}






nodes_dags_server <- function(id, session_data) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        current_selection <- reactiveValues()
        observe({
           ## # ###
            fittedbn <- session_data$fittedbn
            if (!is.null(fittedbn)) {
                current_selection$nodes <- session_data$taxa_names
                ## TODO :: move this to build step and do not do it here
                # current_selection$dagitty <- bn_to_dagitty(fittedbn)
                current_selection$markov_blanket <- list()
                if(is.null(session_data$testable_implications_taxa_vars)) {
                    current_selection$testable_implications_taxa_vars <- get_testable_implications_v2(session_data$dagitty, session_data$outcome_variables)
                } else {
                    current_selection$testable_implications_taxa_vars <- session_data$testable_implications_taxa_vars

                }
                #current_selection$testable_implications <- get_testable_implications(session_data$dagitty, session_data$outcome_variables)

            } else {
                current_selection$nodes <- NULL
                current_selection$dagitty <- NULL
                current_selection$markov_blanket <- list()
            }
        })

        observe({
            # # # ###
            if (!is.null(input$nodes_cpt)) {
                active_node <- input$nodes_cpt
                isolate({
                    if (is.null(current_selection$markov_blanket[[active_node]])) {
                        current_selection$markov_blanket[[active_node]] <- markovBlanket(session_data$fittedbn, active_node, current_selection$nodes)
                }
                })
                
                current_selection$active_node <- active_node
            }
        })

        output$selector_cpt <- renderUI({
            fittedbn <- session_data$fittedbn
            nodes <- session_data$taxa_names
            selected_node <- NULL
            if (is.null(fittedbn)) {
                selected_node <- NULL
            } else {
                selected_node <- nodes[1]
            }

            pickerInput(ns("nodes_cpt"),
                label = NULL, choices = nodes,
                selected = selected_node,
                multiple = FALSE,
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
        output$exposure_variables_placeholder <- renderUI({
            pickerInput(ns("selected_exposure_variables"),
                label = NULL, choices = session_data$exposure_variables,
                selected = NULL,
                multiple = TRUE,
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

        observe({
            if (is.null(current_selection$active_node)) {
                return(NULL)
            }
            target_node <- current_selection$active_node
            ms <- current_selection$markov_blanket[[target_node]]
            # # # ###
            fittedbn <- session_data$fittedbn
            if (input$show_indirect_markov) {
                sub_nodes <- union(target_node, ms)
            } else {
                sub_nodes <- union(target_node, attr(ms, "direct"))
            }

            current_selection_nodes <- isolate(current_selection$nodes)
            show_indirect_markov <- isolate(input$show_indirect_markov)
            subgr <- bnlearn::subgraph(fittedbn, sub_nodes)
            current_selection_nodes <- isolate(current_selection$nodes)
            show_indirect_markov <- isolate(input$show_indirect_markov)
            # ##
            ps <- future_promise({
                ##
                print("in future_promise")
                gR <- bnlearn::graphviz.plot(subgr, layout = "dot", shape = "ellipse", highlight = list(nodes = c(target_node), fill = "green", col = "black"), render = FALSE)
                graph::nodeRenderInfo(gR)$fill[attr(ms, "direct")] <- "tomato"
                graph::nodeRenderInfo(gR)$shape[current_selection_nodes] <- "rectangle"
                if (show_indirect_markov) graph::nodeRenderInfo(gR)$fill[attr(ms, "in_direct")] <- "yellow"
                gR
            })
            then(ps, onFulfilled = function(value){
                output$markov_blanket <- renderPlot({
                    ###
                    Rgraphviz::renderGraph(value)
                    })
            })
            # ##
        })

        # output$markov_blanket <- renderPlot({
        #     if (is.null(current_selection$active_node)) {
        #         return(NULL)
        #     }
        #     target_node <- current_selection$active_node
        #     ms <- current_selection$markov_blanket[[target_node]]
        #     # # # ###
        #     fittedbn <- session_data$fittedbn
        #     if (input$show_indirect_markov) {
        #         sub_nodes <- union(target_node, ms)
        #     } else {
        #         sub_nodes <- union(target_node, attr(ms, "direct"))
        #     }

        #     current_selection_nodes <- isolate(current_selection$nodes)
        #     show_indirect_markov <- isolate(input$show_indirect_markov)
        #     future_promise({
        #         subgr <- bnlearn::subgraph(fittedbn, sub_nodes)

        #         gR <- bnlearn::graphviz.plot(subgr, layout = "dot", shape = "ellipse", highlight = list(nodes = c(target_node), fill = "green", col = "black"), render = FALSE)
        #         graph::nodeRenderInfo(gR)$fill[attr(ms, "direct")] <- "tomato"
        #         graph::nodeRenderInfo(gR)$shape[current_selection_nodes] <- "rectangle"
        #         if (show_indirect_markov) graph::nodeRenderInfo(gR)$fill[attr(ms, "in_direct")] <- "yellow"
        #         Rgraphviz::renderGraph(gR)
        #     })


            
        # })

        output$debug_output <- renderPrint({
            print(selected_exposure_variables_ids <- input$selected_exposure_variables)
            current_selection$markov_blanket[[current_selection$active_node]]
        })

        output$ci_test_info <- renderUI({
            active_node <- current_selection$active_node
            if (is.null(active_node)) {
                return(NULL)
            }
            # # # ###
            selected_exposure_variables <- input$selected_exposure_variables
            if (is.null(selected_exposure_variables)) selected_exposure_variables <- c()
            d_separated <- c()
            d_connected <- c()
            not_selected_exposure_variables <- setdiff(session_data$exposure_variables, selected_exposure_variables)
            if (is.null(not_selected_exposure_variables)) {
                # all variable is seleted do something here
            } else {
                for (exp_var in not_selected_exposure_variables) {
                    if (is.null(selected_exposure_variables)) {
                        dsep_result <- bnlearn::dsep(session_data$fittedbn, active_node, exp_var)
                    } else {
                        dsep_result <- bnlearn::dsep(session_data$fittedbn, active_node, exp_var, selected_exposure_variables)
                    }

                    if (dsep_result) {
                        d_separated <- c(d_separated, exp_var)
                    } else {
                        d_connected <- c(d_connected, exp_var)
                    }
                }
            }
            tagList(
                h4(paste("d-Separated  with : ", paste(d_separated, collapse = ","))),
                h4(paste("d-Connected  with : ", paste(d_connected, collapse = ",")))
            )
        })
        output$testable_implications <- renderUI({
            active_node <- current_selection$active_node
            #return(NULL)
            if (is.null(active_node)) {
                return(NULL)
            }
            # # # ###
            # if(input$taxa_testable_implications) 
                 testable_implications <- current_selection$testable_implications_taxa_vars[[active_node]]
            # else
            #    testable_implications <- current_selection$testable_implications[[active_node]]
            tags <- tagList(c())

            for (testable_implication in testable_implications) {
                z <- testable_implication$Z
                #z <- setdiff(z, session_data$outcome_variables)
                y <- testable_implication$Y
                y <- setdiff(y, session_data$outcome_variables)
                if (length(z) > 0 & length(y) > 0) {
                    tags <- tagList(
                        tags,
                        HTML(paste(
                            "Conditional on : ",
                            paste(
                                "<b>",
                                paste(z, collapse = ", "),
                                "</b>"
                            ),
                            "<br/>"
                        ))
                    )
                    tags <- tagList(
                        tags,
                        HTML(paste(active_node, "_||_", "<b>", paste(y, collapse = ", "), "</b>"))
                    )
                    tags <- tagList(tags, hr())
                }
            }
            #tags <- tagList(tags, hr())
            # tagList(unlist(tags))
            tags
        })
    })
}