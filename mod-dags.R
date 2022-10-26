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


markovBlanket_dag <- function(x, v, cond = NULL, c.done = c()) {
    s1 <- dagitty::parents(x, v)
    s2 <- dagitty::children(x, v)
    ms <- union(s1, s2)
    if (length(s2) > 0) {
        for (c in s2) {
            ms <- union(ms, dagitty::parents(x, c))
        }
    }
    direct_ms <- ms
    if (!is.null(cond)) {
        for (c in ms) {
            if (c %in% cond && !c %in% c.done) {
                # ms <- setdiff(ms,c)
                ms <- union(ms, markovBlanket(x, c, cond, c(c.done, c)))
            }
        }
    }
    ms <- setdiff(ms, v)
    attr(ms, "direct") <- setdiff(direct_ms, v)
    attr(ms, "in_direct") <- setdiff(ms, direct_ms)

    ms
}

markovBlanket <- function(fittedbn, v, cond = NULL, c.done = c(v)) {
    #browser()
    mb_set <- mb(fittedbn, v)
    direct_mb_set <- mb_set
    if (!is.null(cond)) {
        for (c in mb_set) {
            if (c %in% cond && !c %in% c.done) {
                # ms <- setdiff(ms,c)
                sub_set <- markovBlanket(fittedbn, c, NULL, c(c.done, c))
                mb_set <- union(mb_set, sub_set)
            }
        }
    }
    mb_set <- setdiff(mb_set, v)
    direct_mb_set <- setdiff(direct_mb_set, v)
    attr(mb_set, "direct") <- direct_mb_set
    attr(mb_set, "in_direct") <- setdiff(mb_set, direct_mb_set)

    mb_set
}

get_testable_implications <- function(dag_obj, outcome_variables) {
    testable_implications <- list()
    for (target_taxa in outcome_variables) {
        local_testable_implications <- get_testable_implications_per_taxa(dag_obj,target_taxa,outcome_variables)
        testable_implications[[target_taxa]] <- local_testable_implications[[target_taxa]]
    }
    testable_implications
}


get_testable_implications_per_taxa <- function(dag_obj, target_taxa, outcome_variables) {
    dag_obj <- dagitty::as.dagitty(dag_obj)
    dagitty::latents(dag_obj) <- setdiff(outcome_variables,target_taxa)
    imls <- dagitty::impliedConditionalIndependencies(dag_obj)
    testable_implications <- list()
    for (iml in imls) {
        ## only process Taxa
        taxa_node <- NULL
        if (iml$X == target_taxa) taxa_node <- iml$X
        if (iml$Y ==  target_taxa ) taxa_node <- iml$Y
        if (!is.null(taxa_node)) {
            if (is.null(testable_implications[[taxa_node]])) testable_implications[[taxa_node]] <- list()
            testable_implications[[taxa_node]] <- append(testable_implications[[taxa_node]], list(iml))
        }
    }
    for (target_node in names(testable_implications)) {
        testable_implication <- testable_implications[[target_node]]
        m <- r2r::hashmap()
        mKeys <- list()
        for (lsd in testable_implication) {
            if (is.null(m[[lsd$Z]])) {
                m[[lsd$Z]] <- c()
                mKeys <- append(mKeys, list(lsd$Z))
            }

            t_node <- lsd$Y
            if (target_node == t_node) {
                t_node <- lsd$X
            }
            m[[lsd$Z]] <- union(m[[lsd$Z]], t_node)
        }
        final_result <- list()
        for (lkey in mKeys) {
            final_result <- append(final_result, list(list(Y = m[[lkey]], Z = lkey)))
        }
        testable_implications[[target_node]] <- final_result
    }
    testable_implications
}

## this version return testable_implications with out setting latent variable
get_testable_implications_v1 <- function(dag_obj, outcome_variables) {
    imls <- dagitty::impliedConditionalIndependencies(dag_obj)
    testable_implications <- list()
    for (iml in imls) {
        ## only process Taxa
        taxa_node <- NULL
        if (iml$X %in% outcome_variables) taxa_node <- iml$X
        if (iml$Y %in% outcome_variables) taxa_node <- iml$Y
        if (!is.null(taxa_node)) {
            if (is.null(testable_implications[[taxa_node]])) testable_implications[[taxa_node]] <- list()
            testable_implications[[taxa_node]] <- append(testable_implications[[taxa_node]], list(iml))
        }
    }
    for (target_node in names(testable_implications)) {
        testable_implication <- testable_implications[[target_node]]
        m <- r2r::hashmap()
        mKeys <- list()
        for (lsd in testable_implication) {
            if (is.null(m[[lsd$Z]])) {
                m[[lsd$Z]] <- c()
                mKeys <- append(mKeys, list(lsd$Z))
            }

            t_node <- lsd$Y
            if (target_node == t_node) {
                t_node <- lsd$X
            }
            m[[lsd$Z]] <- union(m[[lsd$Z]], t_node)
        }
        final_result <- list()
        for (lkey in mKeys) {
            final_result <- append(final_result, list(list(Y = m[[lkey]], Z = lkey)))
        }
        testable_implications[[target_node]] <- final_result
    }
    testable_implications
}

get_testable_implications_v2 <- function(dag_obj, outcome_variables ) {
  imls <- dagitty::impliedConditionalIndependencies(dag_obj,"basis.set" )
  testable_implications <- list()
  for(taxa_node in outcome_variables) {
    for(iml in imls) {
      ## only process Taxa
        if(taxa_node == iml$X  | taxa_node  %in% iml$Y) {
          if(is.null( testable_implications[[taxa_node]]) ) testable_implications[[taxa_node]] <- list()
          testable_implications[[taxa_node]] <- append(testable_implications[[taxa_node]],list(iml))
        } 
      }
    }
  
  
      for (target_node in names(testable_implications)) {
        testable_implication <- testable_implications[[target_node]]
        m <- r2r::hashmap()
        mKeys <- list()
        for (lsd in testable_implication) {
            if (is.null(m[[lsd$Z]])) {
                m[[lsd$Z]] <- c()
                mKeys <- append(mKeys, list(lsd$Z))
            }

            t_node <- lsd$X
            if (target_node == t_node) {
                t_node <- lsd$Y
            }
            m[[lsd$Z]] <- union(m[[lsd$Z]], t_node)
        }
        final_result <- list()
        for (lkey in mKeys) {
            final_result <- append(final_result, list(list(Y = m[[lkey]], Z = lkey)))
        }
        testable_implications[[target_node]] <- final_result
    }
    testable_implications
}



nodes_dags_server <- function(id, session_data) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        current_selection <- reactiveValues()
        observe({
           
            fittedbn <- session_data$fittedbn
            if (!is.null(fittedbn)) {
                current_selection$nodes <- session_data$taxa_names
                ## TODO :: move this to build step and do not do it here
                # current_selection$dagitty <- bn_to_dagitty(fittedbn)
                current_selection$markov_blanket <- list()
                current_selection$testable_implications_taxa_vars <- get_testable_implications_v2(session_data$dagitty, session_data$outcome_variables)
                #current_selection$testable_implications <- get_testable_implications(session_data$dagitty, session_data$outcome_variables)

            } else {
                current_selection$nodes <- NULL
                current_selection$dagitty <- NULL
                current_selection$markov_blanket <- list()
            }
        })

        observe({
            # browser()
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

        output$markov_blanket <- renderPlot({
            if (is.null(current_selection$active_node)) {
                return(NULL)
            }
            target_node <- current_selection$active_node
            ms <- current_selection$markov_blanket[[target_node]]
            # browser()
            fittedbn <- session_data$fittedbn
            if (input$show_indirect_markov) {
                sub_nodes <- union(target_node, ms)
            } else {
                sub_nodes <- union(target_node, attr(ms, "direct"))
            }


            subgr <- bnlearn::subgraph(fittedbn, sub_nodes)

            gR <- bnlearn::graphviz.plot(subgr, layout = "dot", shape = "ellipse", highlight = list(nodes = c(target_node), fill = "green", col = "black"), render = FALSE)
            graph::nodeRenderInfo(gR)$fill[attr(ms, "direct")] <- "tomato"
            graph::nodeRenderInfo(gR)$shape[current_selection$nodes] <- "rectangle"
            if (input$show_indirect_markov) graph::nodeRenderInfo(gR)$fill[attr(ms, "in_direct")] <- "yellow"
            Rgraphviz::renderGraph(gR)
        })

        output$debug_output <- renderPrint({
            print(selected_exposure_variables_ids <- input$selected_exposure_variables)
            current_selection$markov_blanket[[current_selection$active_node]]
        })

        output$ci_test_info <- renderUI({
            active_node <- current_selection$active_node
            if (is.null(active_node)) {
                return(NULL)
            }
            # browser()
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
            # browser()
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