evidence_info_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        HTML("<b>Evidence/Control</b>"),
        # NS(id,"network_evidence_info_ui")
        uiOutput(ns("network_evidence_info_ui"))
        # dashboardPage(
        #     dashboardHeader(),
        #     dashboardSidebar(),
        #     dashboardBody(uiOutput(ns("network_evidence_info_ui")))
        # )
        
    )
}

summary_stats <- function(df.data) {
    require(rethinking)
    df_means <- precis(df.data, prob = .5)
    df_means <- data.frame(df_means)
    df_means <- df_means %>% select(all_of(c("mean", "sd")))
    df_quantiles <- apply(df.data, 2, quantile)
    df_quantiles <- as.data.frame(t(df_quantiles))

    df_means <- round(data.frame(df_means, df_quantiles), 2)
    colnames(df_means) <- c("mean", "sd", "0%", "25%", "50%", "75%", "100%")
    df_means
}


evidence_info_server <- function(id, session_data) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        cTable <- function(df) {

            renderDataTable(df,
                selection = "single",
                options = list(lengthChange = FALSE,
                            scrollY = TRUE,
                            scrollX = TRUE
                ),
                server = TRUE
            )
            # renderDataTable(
            #     df,
            #     plugins = "natural",
            #     server = FALSE,
            #     extensions = c("Buttons"),
            #     options = list(
            #         initComplete = JS(
            #             "function(settings, json) {",
            #             "$(this.api().table().header()).css({'background-color': '#696969', 'color': '#fff'});",
            #             "}"
            #         ),
            #         columnDefs = list(list(targets = 1:5, class = "dt-center")),
            #         # language = list(lengthMenu = "_MENU_"),
            #         search = list(regex = TRUE, caseInsensitive = TRUE),
            #         columnDefs = list(list(type = "natural", targets = 2)),
            #         dom = '<"#js"l>Bfrtip',
            #         scrollY = TRUE,
            #         scrollX = TRUE,
            #         select = list(style = "single", items = "row"),
            #         lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")),
            #         pageLength = 10,
            #         buttons = list(
            #             "copy",
            #             list(extend = "csv", filename = "table"),
            #             list(extend = "excel", filename = "table", title = NULL)
            #         )
            #     )
            # )
        }

        current_selection <- reactiveValues()

        ## # ###
        ## top_level UI
        output$network_evidence_info_ui <- renderUI({
            if (is.null(session_data$fittedbn)) {
                return(h4("No currect active Network"))
            }
            # session_data$ev1 <- 5

            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    tags$label(h4("Setup your evidence/control conditions")),
                    checkboxInput(ns("two_ways_evidence_comparison"), label = "Two Evidences Comparison", value = FALSE, width = NULL),
                    uiOutput(ns("network_evidences_ui_selectors"))#,
                    #checkboxInput(ns("plot_data_hist"), label = "Plot Histogram instead of Density Estimation", value = FALSE, width = NULL),
                ),
                mainPanel(
                    width = 10,
                    uiOutput(ns("comparison_ui_info")),
                    uiOutput(ns("evidence_reference_ui_info")),
                    uiOutput(ns("evidence_target_ui_info"))
                    # h3("Network Summary : "),
                    # fluidRow(box(title = "Histogram", status = "primary", plotOutput("plot21", height = 250))),
                    # fluidRow(
                    #     box(title = "Histogram", status = "primary", plotOutput("plot22", height = 250)),
                    #     box(
                    #         title = "Inputs", status = "warning",
                    #         "Box content here", br(), "More box content",
                    #         sliderInput(ns("slider"), "Slider input:", 1, 100, 50),
                    #         textInput(ns("text"), "Text input:")
                    #     )
                    # )
                )
            )
        })


        output$network_evidences_ui_selectors <- renderUI({
            if (!input$two_ways_evidence_comparison) {
                tags$div(
                    class = "bs-select-all-disable",
                    pickerInput(ns("network_evidence_reference"), "Evidence",
                        choices = session_data$var_list, selected = 1, multiple = TRUE,
                        options = pickerOptions(
                            "liveSearch" = FALSE,
                            # "max-options" = 2,
                            "max-options-group" = 1,
                            "selectOnTab" = TRUE,
                            actionsBox = TRUE #, style = "bs-select-all-disable"
                        )
                    )
                )
            } else {
                list(
                    tags$div(
                        class = "bs-select-all-disable",
                        pickerInput(ns("network_evidence_reference"), "Reference Evidence",
                            choices = session_data$var_list, selected = 1, multiple = TRUE,
                            options = pickerOptions(
                                "liveSearch" = FALSE,
                                # "max-options" = 2,
                                "max-options-group" = 1,
                                "selectOnTab" = TRUE,
                                actionsBox = TRUE#,style = "bs-select-all-disable"
                            )
                        )
                    ),
                    tags$div(
                        class = "bs-select-all-disable",
                        pickerInput(ns("network_evidence_target"), "Target Evidence",
                            choices = session_data$var_list, selected = 1, multiple = TRUE,
                            options = pickerOptions(
                                "liveSearch" = FALSE,
                                # "max-options" = 2,
                                "max-options-group" = 1,
                                "selectOnTab" = TRUE,
                                actionsBox = TRUE#,style = "bs-select-all-disable"
                            )
                        )
                    )
                )
            }
        })

        output$evidence_reference_ui_info <- renderUI({
            if (is.null(input$network_evidence_reference)) {
                session_data$reference_evidence_value <- NULL
                return(NULL)
            }
            # # # ###
            reference_evidence_value <- convert_ui_evidence_selection(session_data$var_list, input$network_evidence_reference)
            session_data$reference_evidence_value <- reference_evidence_value
            bn_df_norm_filtered_REV <- filter_by_evidence(session_data$bn_df_norm, reference_evidence_value)
            if (nrow(bn_df_norm_filtered_REV) == 0) {
                summary_table <- NULL
                current_selection$bn_df_norm_REV_taxas <- NULL
                summary_txt <- "The data does not contain any samples with this evidence."

            } else {
                taxas <- colnames(session_data$bn_df_taxas)
                bn_df_norm_selected_taxas <- bn_df_norm_filtered_REV %>% select(all_of(taxas))
                bn_df_norm_selected_taxas <- expm1(bn_df_norm_selected_taxas)
                current_selection$bn_df_norm_REV_taxas <- bn_df_norm_selected_taxas
                summary_table <- summary_stats(bn_df_norm_selected_taxas)
                current_selection$summary_table_REV <-summary_table
                summary_txt <- paste("Total Number of samples matching evidence :", nrow(bn_df_norm_filtered_REV))
            }

            output$summary_table_REV <- cTable(summary_table)
            fluidRow(
                box(
                    title = "Reference Evidence", status = "primary",
                    width =6,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    summary_txt,
                    br(),
                    DT::dataTableOutput(ns("summary_table_REV"))  %>% withSpinner(color = "#0dc5c1")

                ),
                box(
                    title = "Graph",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status = "info",
                    # sidebar = boxSidebar(
                    #     id = ns("mycardsidebar"),
                    #     width = 25,
                    #     sliderInput(
                    #         ns("obs"),
                    #         "Number of observations:",
                    #         min = 0,
                    #         max = 1000,
                    #         value = 500
                    #     )
                    # ),
                    plotOutput(ns("REV_dist_plot"))
                )
            )
        })

        output$evidence_target_ui_info <- renderUI({
            if (is.null(input$network_evidence_target)) {
                session_data$target_evidence_value <- NULL
                return(NULL)
            }
            # # # ###
            target_evidence_value <- convert_ui_evidence_selection(session_data$var_list, input$network_evidence_target)
            session_data$target_evidence_value <- target_evidence_value
            bn_df_norm_filtered_TEV <- filter_by_evidence(session_data$bn_df_norm, target_evidence_value)
            if (nrow(bn_df_norm_filtered_TEV) == 0) {
                summary_table <- NULL
                current_selection$bn_df_norm_TEV_taxas <- NULL
                summary_txt <- "The data does not contain any samples with this evidence."

            } else {
                taxas <- colnames(session_data$bn_df_taxas)
                bn_df_norm_selected_taxas <- bn_df_norm_filtered_TEV %>% select(all_of(taxas))
                bn_df_norm_selected_taxas <- expm1(bn_df_norm_selected_taxas)
                current_selection$bn_df_norm_TEV_taxas <- bn_df_norm_selected_taxas
                summary_table <- summary_stats(bn_df_norm_selected_taxas)
                summary_txt <- paste("Total Number of samples matching evidence :", nrow(bn_df_norm_filtered_TEV))
            }



            fluidRow(
                box(
                    title = "Target Evidence", status = "primary",
                    width = 6,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    summary_txt,
                    br(),
                    cTable(summary_table)
                ),
                box(
                    title = "Graph",
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    status = "info",
                    # sidebar = boxSidebar(
                    #         id = ns("mycardsidebar"),
                    #         width = 25,
                    #         sliderInput(
                    #             ns("obs"), 
                    #             "Number of observations:",
                    #             min = 0, 
                    #             max = 1000, 
                    #             value = 500
                    #         )),
                    plotOutput(ns("TEV_dist_plot"))
                )
            )
        })

        output$comparison_ui_info <- renderUI({
            if (!input$two_ways_evidence_comparison) {
                return(NULL)
            }
            fluidRow(
                box(
                    title = "Evidence Compare", status = "primary",
                    width = 6,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Evidences Compare",
                    DT::dataTableOutput(ns("compare_table"))  %>% withSpinner(color = "#0dc5c1")
                ),
                box(
                    title = "Graph",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status = "info",
                    plotOutput(ns("diff_dist_plot"))
                )
            )
        })

        get_compare_table <- reactive({
            NULL
            # ###
            if(is.null(current_selection$bn_df_norm_REV_taxas)) return(NULL)
            if(is.null(current_selection$bn_df_norm_TEV_taxas)) return(NULL)
        }) 

        output$compare_table <- DT::renderDataTable(
            DT::datatable(
                {
                get_compare_table()
                },
                selection = "single",
                options = list(
                lengthChange = FALSE,
                scrollY = TRUE,
                scrollX = TRUE
                )
            )
            )

        observe({
            ## # ###
            input$summary_table_REV_rows_selected
            if ( is.null(input$summary_table_REV_rows_selected) ) {
                # current_selection$selected_taxa <- NULL
            } else {
                ids <- input$summary_table_REV_rows_selected
                current_selection$selected_taxa <- rownames(current_selection$summary_table_REV)[ids]
            }
            
        })

        output$diff_dist_plot <- renderPlot({
            if(is.null(current_selection$bn_df_norm_REV_taxas)) return(NULL)
            if(is.null(current_selection$bn_df_norm_TEV_taxas)) return(NULL)
            if(is.null(current_selection$selected_taxa)) return(NULL)
            ## # ###
            selected_taxa <- current_selection$selected_taxa
            count_data_REV <- current_selection$bn_df_norm_REV_taxas[,selected_taxa]
            count_data_TEV <- current_selection$bn_df_norm_TEV_taxas[,selected_taxa]

            # if(input$plot_data_hist) {
                # count_data_REV_max <- max(count_data_REV)
                # count_data_TEV_max <- max(count_data_TEV)
                # n_breaks <- min(length(count_data_REV),length(count_data_TEV))
                # if(count_data_REV_max > count_data_TEV_max) {
                #     hist(count_data_REV, breaks = n_breaks,  density = 80,col = 'red' , main = paste("Histogram of" , selected_taxa ) , xlab ="Normalized Count")
                #     hist(count_data_TEV, breaks = n_breaks,  density = 80,col = 'blue' , add=TRUE)
                # } else {
                #     hist(count_data_TEV, breaks = n_breaks,  density = 80,col = 'blue', main = paste("Histogram of" , selected_taxa ) , xlab ="Normalized Count" )
                #     hist(count_data_REV, breaks = n_breaks,  density = 80,col = 'red' , add=TRUE)


                # }


            # } else {
                require(ggplot2)
                require(plyr)

                df <- data.frame(
                    Evidence = factor(c(
                        rep("Reference", length(count_data_REV)),
                        rep("Target", length(count_data_TEV))
                    )),
                    #Count = c(count_data_REV, count_data_TEV)
                    Count = c(count_data_REV, count_data_TEV)

                )
                mu <- ddply(df, "Evidence", summarise, grp.mean=mean(Count))
                mu$grp.mean <- log1p(mu$grp.mean)
                
                df$Count <- log1p(df$Count)

                mu_log <- ddply(df, "Evidence", summarise, grp.mean=mean(Count))
                p <- ggplot(df, aes(x=Count, fill=Evidence)) + 
                    geom_density(alpha=0.4) +
                    geom_vline(data=mu, aes(xintercept=grp.mean, color=Evidence), linetype="dashed") +
                    geom_vline(data=mu_log, aes(xintercept=grp.mean, color=Evidence), linetype="solid")
                p
            # }
        })


        output$REV_dist_plot <- renderPlot({
            if(is.null(current_selection$bn_df_norm_REV_taxas)) return(NULL)
            if(is.null(current_selection$selected_taxa)) return(NULL)
            selected_taxa <- current_selection$selected_taxa
            count_data <- current_selection$bn_df_norm_REV_taxas[,selected_taxa]
            #if(input$plot_data_hist) {
                hist(count_data,breaks=length(count_data),  density = 80,col = 'red' , main = paste("Histogram of" , selected_taxa , " | Reference Evidence") , xlab ="Normalized Count")
            #} else {

            #}
        })
        output$TEV_dist_plot <- renderPlot({
            if(is.null(current_selection$bn_df_norm_TEV_taxas)) return(NULL)
            if(is.null(current_selection$selected_taxa)) return(NULL)
            # # # ###
            selected_taxa <- current_selection$selected_taxa
            count_data <- current_selection$bn_df_norm_TEV_taxas[,selected_taxa]
            #if(input$plot_data_hist) {
                hist(count_data, breaks=length(count_data), density  = 80,col = 'blue' , main = paste("Histogram of" , selected_taxa , " | Target Evidence") , xlab ="Normalized Count")
            #} else {

            #}
        })



        # observe({
        #     # # ###
        #     input$summary_table_REV_row_selected
        #     if ( is.null(input$summary_table_REV_row_selected) ) return(NULL)
        #     ids <- input$summary_table_REV_row_selected
        #     print(colnames(current_selection$summary_table_REV,ids))
        # })
        # bindEvent(
        #     reactive({
        #         # # ###
        #         ids <- input$summary_table_REV_rows_selected
        #         print(colnames(current_selection$summary_table_REV,ids))
        #     }),
        #     input$summary_table_REV_rows_selected,
        # )

        # return(
        #     list(
        #         xvar = reactive(
        #             input$slider
        #         ),
        #         yvar = reactive(
        #             input$text
        #         )
        #     )
        # )
    })
}