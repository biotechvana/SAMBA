nodes_cpts_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        HTML("<b>CPTs</b>"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                tags$label(h3("Conditional Probability Tables")),
                p("Select a Node to display CPT"),
                uiOutput(ns("selector_cpt"), container = div),
                hr(),
                downloadButton(ns("save_cpt_all"), "Download all tables", class = "butt"),
                tags$head(tags$style(".butt{background-color:#F7F7F7;} .butt{color: black;}")),
                downloadButton(ns("save_cpt"), "Download current table", class = "butt"),
            ),
            mainPanel(
                width = 9,
                tabBox(
                    width = 12,
                    tabPanel(
                        "Conditional probability table",
                        tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                        tags$head(tags$style("#conditional_table{overflow: auto; padding: 20px; text-align:justify; overflow-y:scroll; overflow-x:scroll; max-height: 575px; background: #F8F8F8;}")),
                        verbatimTextOutput(ns("conditional_table"))
                    ),
                    tabPanel(
                        "P1",
                        plotOutput(ns("other_graph_tabs_1"))
                    ),
                    tabPanel(
                        "P2",
                        plotOutput(ns("other_graph_tabs_2"))
                    )
                )
            )
        )
    )
}

nodes_cpts_server <- function(id, session_data) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        output$selector_cpt <- renderUI({
            fittedbn <- session_data$fittedbn
            nodes <- c()
            selected_node <- NULL
            if (is.null(fittedbn)) {
                selected_node <- NULL
            } else {
                nodes <- nodes(fittedbn)
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

        output$conditional_table <- renderPrint({
            validate(
                # need(session_data()$fittedbn, "Please Load network first"),
                need(input$nodes_cpt, "Please select a node.")
            )

            session_data$fittedbn[[input$nodes_cpt]]
        })

        output$other_graph_tabs_1 <- renderPlot({
            validate(
                # need(session_data()$fittedbn, "Please Load network first"),
                need(input$nodes_cpt, "Please select a node.")
            )
            ## # #browser()
            if (input$nodes_cpt %in% session_data$taxa_names) {

                cpt_dist <- session_data$fittedbn[[input$nodes_cpt]]
                bn.fit.histogram(cpt_dist)
                parents <- cpt_dist$parents
                parents <- setdiff(parents,session_data$taxa_names)
                # if(length(parents) > 1) {
                #     require(tidyverse)
                #     tdf <- session_data$bn_df_norm[,parents] %>% 
                #         unite(x, parents, sep = ",", remove = FALSE)
                #     tdf <- unique(cbind( tdf ,c = cpt_dist$configs))
                #     tdf = tdf %>% unite(label, c("c","x"), sep = "=", remove = FALSE)
                #     legend("topleft", tdf$label )

                # }

            } else {
               bn.fit.barchart(session_data$fittedbn[[input$nodes_cpt]])

            }
        })
        output$other_graph_tabs_2 <- renderPlot({
            validate(
                # need(session_data()$fittedbn, "Please Load network first"),
                need(input$nodes_cpt, "Please select a node.")
            )
            ## # #browser()
            if (input$nodes_cpt %in% session_data$taxa_names) {
               bn.fit.qqplot(session_data$fittedbn[[input$nodes_cpt]])
            }
            return(NULL)
        })

        output$save_cpt <- downloadHandler(
            filename = function() {
                if (nchar(input$nodes_cpt) > 200) {
                    name <- substr(input$nodes_cpt, 1, 200)
                } else {
                    name <- input$nodes_cpt
                }
                paste(name, "_CPT.txt", sep = "")
            },
            content = function(fname) {
                # # # #browser()
                fittedbn <- session_data$fittedbn
                nodes <- nodes(fittedbn)
                sink(fname)
                print(fittedbn[[input$nodes_cpt]])
                sink()
            },
            contentType = "text/csv"
        )
        output$save_cpt_all <- downloadHandler(
            filename = "network_CPTs.zip",
            content = function(fname) {
                fittedbn <- session_data$fittedbn
                nodes <- nodes(fittedbn)
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
    })
}