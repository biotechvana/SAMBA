network_prediction_ui <- function(id = "network_prediction_module") {
    ns <- NS(id)

    tabPanel(
        HTML("<b>Prediction</b>"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                tags$label(h3("Predict taxa values")),
                # hr(),
                # ", "Taxas", value = "", placeholder = 'Example: Bacteria_X,Bacteria_Y. Write "all" for all taxas.'),
                uiOutput(ns("selector_predict_taxas"), container = div),
                # textInput("evidence", "Evidence", value= "", placeholder = 'Example: season=S,tissue=AI'),
                # verbatimTextOutput("directorypath"),
                # shinyDirButton('directory', 'Select a folder', 'Please select a folder', FALSE),
                # textInput("outputFolder", "Output folder", value= getwd()),
                # div(style = "display:inline-block; float:right;", actionButton("button1", "Submit")))),
                # numericInput("iterations", "Iterations", value = 1, min = 1, max = 50, step = 0.5),
                # numericInput("error_network", "Conditional probability allowed +- error", value = 0.3, min = 0, max = 1, step = 0.05),
                tags$style(HTML(pickerInput_select)),
                tags$style(HTML(pickerInput_deselect)),
                uiOutput(ns("evidence_selector"), container = div),
                checkboxInput(ns("use_data_as_strong_proir"), label = "Use Input Data as strong proir for network prediction", value = TRUE, width = NULL),
                checkboxInput(ns("show_org_data_dist"), label = "Show Original Data summary", value = TRUE, width = NULL),
                div(class = "buttonagency", actionBttn(inputId = ns("button1"), label = "Submit", style = "float", color = "primary", size = "sm"))
            ),
            mainPanel(
                width = 9,
                # tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; heigth: auto; object-fit: contain;"),
                div(
                    style = "align: center",
                    tabsetPanel(
                        type = "pills",
                        tabPanel(
                            strong("Predicted value"),
                            tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                            dataTableOutput(ns("predicted_value"))
                        ),
                        # tabPanel(
                        #   strong("Conditional probability table"),
                        #   tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                        #   uiOutput("selector_cpt", container = div),
                        #   downloadButton("save_cpt_all", "Download all tables", class = "butt"),
                        #   tags$head(tags$style(".butt{background-color:#F7F7F7;} .butt{color: black;}")),
                        #   downloadButton("save_cpt", "Download current table", class = "butt"),
                        #   tags$head(tags$style("#conditional_table{overflow: auto; padding: 20px; text-align:justify; overflow-y:scroll; overflow-x:scroll; max-height: 575px; background: #F8F8F8;}")),
                        #   verbatimTextOutput("conditional_table")
                        # ),
                        tabPanel(
                            strong("Infer metagenome"),
                            tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                            fileInput(ns("counts"), "Raw counts text file", accept = ".txt"),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_counts"))),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", fileInput(ns("seqs"), "Sequences fasta file", accept = c(".fa", ".fasta"))),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_seqs"))),
                            # shinyDirButton('directory', 'Select an output folder', 'Please select a folder'),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", directoryInput(ns("directory"), label = "Select an output folder")),
                            span(tags$i(h6(HTML("<b>Remember to remove white spaces in fasta headers.</b>"))), style = "color:#52C4DD"),
                            tags$style(".buttonpicrust .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7;}"),
                            div(class = "buttonpicrust", style = "font-size: 10px; padding: 0px 0px; margin-top:2em;", actionBttn(inputId = ns("button_picrust"), label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
                            mainPanel(fluidRow(
                                align = "center", shinyjs::useShinyjs(), style = "background-color:#F8F8F8;",
                                textOutput(ns("predicted_metagenome"))
                            ), width = 12)
                        )
                    )
                )
                # ,
                # style = "width: 100%; height: 1000px"
            )
        )
    )
}

network_prediction_server <- function(session_data, id = "network_prediction_module") {
    ns <- NS(id)


    moduleServer(id, function(input, output, session) {
        local_data <- reactiveValues()
        # if (is.null(input$network)) {
        #   return(NULL)
        # }
        # inFile <- isolate({
        #   input$network
        # })
        # file <- inFile$datapath
        # load(file, envir = .GlobalEnv)

        observe({
            if (!is.null(session_data$bn_df_variables)) {
                variables <- session_data$bn_df_variables[, sapply(session_data$bn_df_variables, class) == "factor"]
                var_list <- list()
                for (i in 1:ncol(variables)) {
                    var_list[[colnames(variables)[i]]] <- levels(variables[, i])
                }
                local_data$var_list <- var_list
            }
            else {
                local_data$var_list <- c()
            }
        })



        output$selector_predict_taxas <- renderUI({
            taxas <- c()
            selected_taxa <- NULL
            if (is.null(session_data$fittedbn)) {
                selected_taxa <- NULL
            } else {
                taxas <- bnlearn::nodes(session_data$fittedbn)
                taxas <- bnlearn::nodes(session_data$fittedbn)
                taxas <- taxas[ncol(session_data$bn_df_variables) + 1:length(taxas)]
                taxas <- na.omit(taxas)
                selected_taxa <- NULL
            }

            pickerInput(ns("predict_selected_taxas"),
                label = NULL, choices = taxas, selected = selected_taxa, multiple = TRUE,
                options = pickerOptions(
                    "liveSearch" = TRUE,
                    # "max-options" = 2,
                    # "max-options-group" = 1,
                    # "selectOnTab" = TRUE,
                    actionsBox = TRUE
                )
            )
        })

        output$evidence_selector <- renderUI({
            tags$div(
                class = "bs-select-all-disable",
                pickerInput(ns("evidence"), "Evidence",
                    choices = local_data$var_list, selected = 1, multiple = TRUE,
                    options = pickerOptions(
                        "liveSearch" = FALSE,
                        # "max-options" = 2,
                        "max-options-group" = 1,
                        "selectOnTab" = TRUE,
                        actionsBox = TRUE # , style = "bs-select-all-disable"
                    )
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
            # browser()
            if (length(input$predict_selected_taxas) == 0) {
                stop("Select some taxa first")
            }
            if (length(input$evidence) == 0) {
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
                res <- lapply(local_data$var_list, function(x) match(i, x))
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
            if (!simple_table) {
                columns_names <- c(
                    columns_names,
                    c(
                        "Data Average",
                        "Data SD",
                        "Data Q-0%",
                        "Data Q-25%",
                        "Data Q-50%",
                        "Data Q-75%",
                        "Data Q-100%"
                    )
                )
            }
            
            df <- matrix(data = NA, nrow = length(taxas), ncol = n_columns)
            colnames(df) <- columns_names



            it <- input$iterations
            error_cp <- input$error_network
            bn_df_norm_filtered_evidence <- filter_by_evidence(session_data$bn_df_norm, input_evidence)
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
                    # browser()
                    HPDI_correct <- FALSE
                    if (data_as_strong_proir) {
                        HPDI_correct <- 0.98
                    } else {
                        HPDI_correct <- 0.98
                    }
                    samples.result <- get.mixed.samples(session_data$fittedbn, session_data$bn_df_norm, taxas[i],
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
                            # browser()
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
                            if (high_range == low_range) high_range <- high_range + 1
                            df[i, 9] <- paste0(round(low_range), "-", round(high_range))
                            data_mask <- posterior_dist$data_value >= log1p(low_range) & posterior_dist$data_value <= log1p(high_range)
                            df[i, 10] <- round(sum(posterior_dist$posterior_p[data_mask]), 2)

                            # hdr <- hdrcde::hdr.den((samples.result$network.samples.raw.values), prob = 50)

                            # df[i, 11] <- paste(round(expm1(hdr$hdr[1])), "-", round(expm1(hdr$hdr[length(hdr$hdr)])))

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
                            # browser()
                            low_range <- mean_value - sd_value
                            if (low_range < 0) low_range <- 0
                            high_range <- mean_value + sd_value
                            df[i, 9] <- paste0(round(low_range), "-", round(high_range))
                            data_mask <- samples.result$network.samples.values >= low_range & samples.result$network.samples.values <= high_range
                            df[i, 10] <- round(sum(data_mask) / length(samples.result$network.samples.values), 2)

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
    })
}