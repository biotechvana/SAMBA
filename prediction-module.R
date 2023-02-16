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
                tags$label(h4("Setup your evidence/control conditions")),
                #checkboxInput(ns("two_ways_evidence_comparison"), label = "Two Evidences Comparison", value = FALSE, width = NULL),
                uiOutput(ns("network_evidences_1_ui_selectors"), container = div),
                uiOutput(ns("network_evidences_2_ui_selectors"), container = div),
                checkboxInput(ns("use_data_as_strong_proir"), label = "Use Input Data as strong proir for network prediction", value = TRUE, width = NULL),
                checkboxInput(ns("show_org_data_dist"), label = "Show Original Data summary", value = TRUE, width = NULL),
                div(class = "buttonagency", actionBttn(inputId = ns("generate_btn"), label = "Submit", style = "float", color = "primary", size = "sm"))
            ),
            mainPanel(
                width = 9,
                # tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; heigth: auto; object-fit: contain;"),
                div(
                    style = "align: center",
                    tabsetPanel(
                        type = "pills",
                        tabPanel(
                            strong("Predict abundances"),
                            tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                            fluidRow(dataTableOutput(ns("predicted_value_e1"))),
                            fluidRow(dataTableOutput(ns("predicted_value_e2"))),
                            fluidRow(uiOutput(ns("network_evidence_info_ui")))#,
                            # fluidRow(
                            #     box(
                            #         title = "Info", status = "info",
                            #         width = 6,
                            #         solidHeader = FALSE,
                            #         collapsible = TRUE,
                            #         collapsed = TRUE,
                            #         "Help Info"
                            #     )
                            # )
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
                            strong("Predict Metagenomes"),
                            tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
                            fileInput(ns("counts"), "Raw counts text file", accept = ".txt"),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_counts"))),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", fileInput(ns("seqs"), "Sequences fasta file", accept = c(".fa", ".fasta"))),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_seqs"))),
                            # shinyDirButton('directory', 'Select an output folder', 'Please select a folder'),
                            # div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", directoryInput(ns("directory"), label = "Select an output folder")),
                            div(style = "font-size: 10px; padding: 0px 0px;", textInput(ns("directory"), "Specify an output folder", placeholder = "experiment_1")),
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
        local_data <- reactiveValues(
            var_list = c(),
            data_errors = list(),
            input_evidence_1_changed = FALSE,
            input_evidence_2_changed = FALSE,
            current_sampling_info1 = NULL,
            current_sampling_info2 = NULL,
            predicted_table_e2 = NULL,
            predicted_table_e1 = NULL
        )
        # if (is.null(input$network)) {
        #   return(NULL)
        # }
        # inFile <- isolate({
        #   input$network
        # })
        # file <- inFile$datapath
        # load(file, envir = .GlobalEnv)


        custom_fit_prediction <- function(input_evidence, target_nodes, simple_table, data_as_strong_proir) {
            df <- NULL
            if (simple_table) n_columns <- 11 else n_columns <- 19
            columns_names <- c(
                "Taxa",
                "Expected Count",
                "Sampling Average",
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

            df <- matrix(data = NA, nrow = length(target_nodes), ncol = n_columns)
            colnames(df) <- columns_names





            withProgress(message = "Generating table", detail = "Wait...", value = 0, {
                incProgress(0 / length(target_nodes), detail = paste("0", "/", length(target_nodes)))

                
                observed_variables <- colnames(session_data$bn_df_variables)
                observed_variables <- intersect(observed_variables, bnlearn::nodes(session_data$fittedbn))
                ## TODO :: introduce some optimzation here
                ## only use target_nodes for samples not the whole network
                sampling.path <- get.sampling.path(session_data$fittedbn,target_nodes)
                average.offset <- 0
                if(!is.null(session_data$build_env$Offset)) {
                    average.offset <- session_data$build_env$Offset
                }
               
                network_samples <- get_samples_all(
                    session_data$fittedbn_custom,
                    session_data$fittedbn,
                    input_evidence,
                    sampling.path, observed_variables,
                    average.offset,
                    incProgress = incProgress,
                )
                ## in log scale
                bn_df_norm_filtered_evidence <- filter_by_evidence(session_data$bn_df_norm, input_evidence)

                for (i in 1:length(target_nodes)) {
                    target_node <- target_nodes[i]
                    df[i, 1] <- target_node
                    target_node <- str_replace_all(target_node, c("/" = ".", " " = ".", "-" = "."))
                    node_samples <- network_samples$all_samples[[target_node]]
                    node_EX_count <- network_samples$exp_counts[[target_node]]

                    data_as_proir <- NULL
                    if (nrow(bn_df_norm_filtered_evidence) > 0) {
                        ## get orginal data
                        data_as_proir <- bn_df_norm_filtered_evidence[[target_node]]
                    }
                    if (is.null(data_as_proir)) {
                        ## can not do data proir here
                        data_as_strong_proir <- FALSE
                    }


                    if (data_as_strong_proir) {
                        ###
                        posterior_dist <- get_posterior_dist(log1p(node_samples), data_as_proir,
                            adjust_samples = 1, adjust_proir = 1
                        )
                        ex_posterior_dist <- get_posterior_dist(log1p(node_EX_count), data_as_proir,
                            adjust_samples = 1, adjust_proir = 1
                        )
                        posterior_dist <- posterior_stats(posterior_dist)
                        ex_posterior_dist <- posterior_stats(ex_posterior_dist)
                        df[i, 2] <- ex_posterior_dist$posterior_mean
                        mean_value <- posterior_dist$posterior_mean
                        sd_value <- posterior_dist$posterior_sd
                        df[i, 3] <- mean_value
                        df[i, 4] <- sd_value
                        q_ranges <- posterior_dist$posterior_quantile
                        df[i, 5] <- q_ranges[1]
                        df[i, 6] <- q_ranges[2]
                        df[i, 7] <- q_ranges[3]
                        df[i, 8] <- q_ranges[4]
                        df[i, 9] <- q_ranges[5]


                        low_range <- mean_value - sd_value
                        if (low_range < 0) low_range <- 0
                        high_range <- mean_value + sd_value
                        if (high_range == low_range) high_range <- high_range + 1
                        df[i, 10] <- paste0(round(low_range), "-", round(high_range))
                        data_mask <- posterior_dist$data_value >= log1p(low_range) & posterior_dist$data_value <= log1p(high_range)
                        df[i, 11] <- round(sum(posterior_dist$posterior_p[data_mask]), 2)
                        hdr_50 <- hdrcde::hdr(den = list(x = posterior_dist$data_value, y = posterior_dist$posterior_w), prob = 50)

                        df[i, 12] <- paste0(round(expm1(hdr_50$hdr[1])), "-", round(expm1(hdr_50$hdr[length(hdr_50$hdr)])))
                    } else {
                        mean_value <- round(mean(node_samples))
                        sd_value <- round(sd(node_samples), 2)
                        df[i, 2] <- round(mean(node_EX_count))
                        df[i, 3] <- mean_value
                        df[i, 4] <- sd_value
                        q_ranges <- quantile(node_samples)
                        df[i, 5] <- q_ranges[1]
                        df[i, 6] <- q_ranges[2]
                        df[i, 7] <- q_ranges[3]
                        df[i, 8] <- q_ranges[4]
                        df[i, 9] <- q_ranges[5]
                        # # # ###
                        low_range <- mean_value - sd_value
                        if (low_range < 0) low_range <- 0
                        high_range <- mean_value + sd_value
                        df[i, 10] <- paste0(round(low_range), "-", round(high_range))
                        data_mask <- node_samples >= low_range & node_samples <= high_range
                        df[i, 11] <- round(sum(data_mask) / length(node_samples), 2)

                        hdr_50 <- hdrcde::hdr(log1p(node_samples), prob = 50)
                        df[i, 12] <- paste0(round(expm1(hdr_50$hdr[1])), "-", round(expm1(hdr_50$hdr[length(hdr_50$hdr)])))
                    }

                    if (!simple_table) {
                        ## add rest of data here
                        if (nrow(bn_df_norm_filtered_evidence) > 0) {
                            ## use orginal data for this
                            data_proir <- expm1(bn_df_norm_filtered_evidence[[target_node]])
                            org_data_average <- mean(data_proir)
                            df[i, 13] <- round(org_data_average)
                            df[i, 14] <- round(sd(data_proir), 2)
                            q_ranges <- round(quantile(data_proir))
                            df[i, 15] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
                            df[i, 16] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
                            df[i, 17] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
                            df[i, 18] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
                            df[i, 19] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])
                        }
                    }

                    incProgress(1 / length(target_nodes), detail = paste(i, "/", length(target_nodes)))
                }
            })
            df
        }
        bnlearn_fit_prediction <- function(input_evidence, target_nodes, simple_table, data_as_strong_proir) {
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

            df <- matrix(data = NA, nrow = length(target_nodes), ncol = n_columns)
            colnames(df) <- columns_names
            bn_df_norm_filtered_evidence <- filter_by_evidence(session_data$bn_df_norm, input_evidence)
            org_data_min_weight <- 0.99
            ## TODO :: we need to check if we can sample from the netowrk with the given evidence if nrow is 0
            if (nrow(bn_df_norm_filtered_evidence) == 0) {
                org_data_min_weight <- n_matchs_to_weight(length(input_evidence), 1)
            }
            # ###
            all.nodes.samples <- NULL
            all.nodes.samples <- bn.network.sampling(
                session_data$fittedbn,
                target_nodes,
                evidence = input_evidence,
                n.samples = 10000,
                min_weight = 0.5
                )

            withProgress(message = "Generating table", detail = "Wait...", value = 0, {
                incProgress(0 / length(target_nodes), detail = paste("0", "/", length(target_nodes)))
                for (i in 1:length(target_nodes)) {
                    print(target_nodes[i])
                    cat(target_nodes[i])
                    # ###
                    df[i, 1] <- target_nodes[i]
                    target_nodes[i] <- str_replace_all(target_nodes[i], c("/" = ".", " " = ".", "-" = "."))
                    org_data_average <- NULL
                    # taxas[i] <- sub("/",".", taxas[i])
                    # taxas[i] <- sub(" ",".", taxas[i])
                    # taxas[i] <- sub("-",".", taxas[i])
                    # predict <- try(cpdist(fittedbn, nodes = taxas[i], evidence = ev, method = "lw", n = 100000))
                    # # # ###
                    HPDI_correct <- FALSE
                    if (data_as_strong_proir) {
                        HPDI_correct <- 0.98
                    } else {
                        HPDI_correct <- 0.95
                    }
                    samples.result <- get.mixed.samples(session_data$fittedbn, session_data$bn_df_norm, target_nodes[i],
                        evidence = input_evidence,
                        n.samples = 100000,
                        org_data_min_weight = org_data_min_weight,
                        HPDI_correct = HPDI_correct
                        ,all.network.samples.values = all.nodes.samples
                    )
                    data_as_proir <- NULL
                    if (nrow(bn_df_norm_filtered_evidence) > 0) {
                        ## get orginal data
                        data_as_proir <- bn_df_norm_filtered_evidence[[target_nodes[i]]]
                    } else if (!is.null(samples.result$org.samples.values)) {
                        data_as_proir <- samples.result$org.samples.raw.values
                    }
                    if (is.null(data_as_proir)) {
                        ## can not do data proir here
                        data_as_strong_proir <- FALSE
                    }

                    if (data_as_strong_proir) {
                        if (!is.null(samples.result$network.samples.values) & !is.null(data_as_proir)) {
                            ## TODO :: bug :: condition with just one sample cuase problem in density estimation 
                            posterior_dist <- get_posterior_dist(samples.result$network.samples.raw.values, data_as_proir,
                                adjust_samples = 1, adjust_proir = 1
                            )
                            posterior_dist <- posterior_stats(posterior_dist)
                            # # # ###
                            mean_value <- posterior_dist$posterior_mean
                            if(!(is.na(mean_value) || is.infinite(mean_value))) {
                                sd_value <- posterior_dist$posterior_sd
                                
                                df[i, 2] <- mean_value
                                df[i, 3] <- sd_value
                                q_ranges <- posterior_dist$posterior_quantile
                                df[i, 4] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
                                df[i, 5] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
                                df[i, 6] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
                                df[i, 7] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
                                df[i, 8] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])

                                if(!( is.nan(sd_value) || is.na(sd_value) || is.infinite(sd_value)) ) {
                                    low_range <- mean_value - sd_value
                                    if (low_range < 0) low_range <- 0
                                    high_range <- mean_value + sd_value
                                    if (high_range == low_range) high_range <- high_range + 1
                                    df[i, 9] <- paste0(round(low_range), "-", round(high_range))
                                    data_mask <- posterior_dist$data_value >= log1p(low_range) & posterior_dist$data_value <= log1p(high_range)
                                    df[i, 10] <- round(sum(posterior_dist$posterior_p[data_mask]), 2)

                                    # hdr <- hdrcde::hdr.den((samples.result$network.samples.raw.values), prob = 50)
                                    # hdrcde::hdr
                                    # df[i, 11] <- paste(round(expm1(hdr$hdr[1])), "-", round(expm1(hdr$hdr[length(hdr$hdr)])))

                                    hdr_50 <- hdrcde::hdr(den = list(x = posterior_dist$data_value, y = posterior_dist$posterior_w), prob = 50)

                                    df[i, 11] <- paste0(round(expm1(hdr_50$hdr[1])), "-", round(expm1(hdr_50$hdr[length(hdr_50$hdr)])))
                                }
                                
                            } else {
                                ## what to do here
                            }
                            
                        }
                        ## end if(data_as_strong_proir)
                    } else {
                        if (!is.null(samples.result$network.samples.values)) {
                            mean_value <- round(samples.result$network.samples.average)
                            df[i, 2] <- mean_value
                            df[i, 3] <- samples.result$network.samples.sd
                            q_ranges <- samples.result$network.samples.quantile
                            df[i, 4] <- q_ranges[1] # paste(q_ranges[1], "-", q_ranges[2])
                            df[i, 5] <- q_ranges[2] # paste(q_ranges[2], "-", q_ranges[3])
                            df[i, 6] <- q_ranges[3] # paste(q_ranges[3], "-", q_ranges[4])
                            df[i, 7] <- q_ranges[4] # paste(q_ranges[4], "-", q_ranges[5])
                            df[i, 8] <- q_ranges[5] # paste(q_ranges[4], "-", q_ranges[5])
                            if(!(is.na(mean_value) || is.infinite(mean_value))) {
                                sd_value <- round(samples.result$network.samples.sd, 2)
                                df[i, 2] <- mean_value
                                df[i, 3] <- sd_value
                                # # # ###
                                if(!( is.nan(sd_value) || is.na(sd_value) || is.infinite(sd_value)) ) {
                                    low_range <- mean_value - sd_value
                                    if (low_range < 0) low_range <- 0
                                    high_range <- mean_value + sd_value
                                    df[i, 9] <- paste0(round(low_range), "-", round(high_range))
                                    data_mask <- samples.result$network.samples.values >= low_range & samples.result$network.samples.values <= high_range
                                    df[i, 10] <- round(sum(data_mask) / length(samples.result$network.samples.values), 2)

                                    hdr_50 <- hdrcde::hdr(samples.result$network.samples.raw.values, prob = 50)
                                    df[i, 11] <- paste0(round(expm1(hdr_50$hdr[1])), "-", round(expm1(hdr_50$hdr[length(hdr_50$hdr)])))
                                }
                                
                            } else {

                            }
                            
                        }
                    }

                    ## end if if(simple_table)
                    if (!simple_table) {
                        ## add rest of data here
                        if (nrow(bn_df_norm_filtered_evidence) > 0) {
                            ## use orginal data for this
                            data_proir <- expm1(bn_df_norm_filtered_evidence[[target_nodes[i]]])
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
                    incProgress(1 / length(target_nodes), detail = paste(i, "/", length(target_nodes)))
                }
            })
            df <- as.data.frame(df)
            for(ci in 2:8)
                df[,ci] <-  as.numeric(df[,ci])
            if (!simple_table)
                for(ci in 12:18)
                    df[,ci] <-  as.numeric(df[,ci])
            df
        }


        observe({
            # # # ###
            if (!is.null(session_data$bn_df_variables)) {
                variables <- session_data$bn_df_variables[, sapply(session_data$bn_df_variables, class) == "factor"]
                var_list <- list()
                for (i in 1:ncol(variables)) {
                    var_list[[colnames(variables)[i]]] <- levels(variables[, i])
                }
                local_data$var_list <- var_list
            } else {
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

        output$network_evidences_1_ui_selectors <- renderUI({
            
            var_list = list()
            if(is.null(local_data$var_list) )
                return(NULL)
            var_list <- local_data$var_list
            tags$div(
                class = "bs-select-all-disable",
                pickerInput(ns("evidence1"), "Evidence",
                    choices = var_list, selected = NULL, multiple = TRUE,
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
        output$network_evidences_2_ui_selectors <- renderUI({
            if (FALSE) { # input$two_ways_evidence_comparison
                tags$div(
                    class = "bs-select-all-disable",
                    pickerInput(ns("evidence2"), "Evidence",
                        choices = local_data$var_list, selected = NULL, multiple = TRUE,
                        options = pickerOptions(
                            "liveSearch" = FALSE,
                            # "max-options" = 2,
                            "max-options-group" = 1,
                            "selectOnTab" = TRUE,
                            actionsBox = TRUE # , style = "bs-select-all-disable"
                        )
                    )
                )
            } 
            else {
                return(NULL)
            }
           
        })

        # observeEvent(input$button2, {
        #   # # ###
        #   nodes <- strsplit(input$nodes, ",")[[1]]
        #   nodes <- str_replace_all(nodes, c("/" = ".", " " = ".", "-" = "."))

        # })

        # outputOptions(output, "selector", suspendWhenHidden = FALSE)

        observe({
            ## # ###
            input_evidence <- list()

            var_list <- isolate(local_data$var_list)

            for (i in input$evidence1) {
                res <- lapply(var_list, function(x) match(i, x))
                for (j in 1:length(res)) {
                    if (!is.na(res[j])) {
                        input_evidence[[names(res[j])]] <- i
                    }
                }
            }

            local_data$input_evidence_1 <- input_evidence
            local_data$input_evidence_1_changed <- TRUE
        })

        # observe({
        #     input_evidence <- list()
        #     # # ###
        #     for (i in input$evidence2) {
        #         res <- lapply(local_data$var_list, function(x) match(i, x))
        #         for (j in 1:length(res)) {
        #             if (!is.na(res[j])) {
        #                 input_evidence[[names(res[j])]] <- i
        #             }
        #         }
        #     }
        #     local_data$input_evidence_2 <- input_evidence
        #     local_data$input_evidence_2_changed <- TRUE
        # })


        generate_prediction_table <- function(input_evidence) {
            #browser()
            ## If statement to create taxa list
            ## if input$taxas == ""
            # # ###

            it <- input$iterations
            error_cp <- input$error_network


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



            if (!is.null(session_data$fittedbn_custom)) {
                ## use custom fit models for predictions
                df <- custom_fit_prediction(input_evidence, local_data$selected_taxas, simple_table, data_as_strong_proir)
            } else {
                df <- bnlearn_fit_prediction(input_evidence, local_data$selected_taxas, simple_table, data_as_strong_proir)
            }

            ## Create output dataframe
            return(df)
        }

        ## Display table when user clicks on button
        observeEvent(input$generate_btn, {
            # # ###
            if (length(input$predict_selected_taxas) == 0) {
                stop("Select some taxa first")
            }
            if (length(input$evidence1) == 0) {
                stop("Select evidence first")
            }

            ## comparre 
            new_selected_taxas <- input$predict_selected_taxas
            
            if (length(setdiff(new_selected_taxas,local_data$selected_taxas)) > 0 ) {
                    #local_data$input_evidence_2_changed <- TRUE
                    local_data$input_evidence_1_changed <- TRUE
            }
            if (length(setdiff(local_data$selected_taxas,new_selected_taxas)) > 0 ) {
                    #local_data$input_evidence_2_changed <- TRUE
                    local_data$input_evidence_1_changed <- TRUE
            }
            local_data$input_evidence_2_changed <- FALSE
            local_data$input_evidence_1_changed <- TRUE
            local_data$selected_taxas <- new_selected_taxas
            
            # input_evidence <- list()

            # for (i in input$evidence1) {
            #     res <- lapply(local_data$var_list, function(x) match(i, x))
            #     for (j in 1:length(res)) {
            #         if (!is.na(res[j])) {
            #             input_evidence[[names(res[j])]] <- i
            #         }
            #     }
            # }
            # local_data$input_evidence <- input_evidence

            shinybusy::show_modal_spinner(
                text = "Please wait, Recalculating Data Tables"
            )
            tryCatch(
                {
                    if(local_data$input_evidence_1_changed)
                        local_data$predicted_table_e1 <- generate_prediction_table(local_data$input_evidence_1)
                    if(local_data$input_evidence_2_changed)
                        local_data$predicted_table_e2 <- generate_prediction_table(local_data$input_evidence_2)

                    #predicted_value_e1_proxy %>% replaceData(local_data$predicted_table_e1)
                    #predicted_value_e2_proxy %>% replaceData(local_data$predicted_table_e2)

                },
                error = function(cond) {
                    print(cond$message)
                    local_data$data_errors <- append(local_data$data_errors, cond$message)
                    local_data$predicted_table_e1  <- NULL
                },
                finally = {
                    shinybusy::remove_modal_spinner()
                    local_data$input_evidence_2_changed <- FALSE
                    local_data$input_evidence_1_changed <- FALSE
                }
            )
        })

        output$predicted_value_e1 <- renderDataTable(
            DT::datatable({
                # # ###
                if(is.null(local_data$predicted_table_e1)) {
                    return(NULL)
                }
                as.data.frame(local_data$predicted_table_e1)
            },
            selection = "single",
            plugins = "natural",
            editable = TRUE,
            #server = TRUE,
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
                ))
            )
        )

        output$predicted_value_e2 <- renderDataTable(
            local_data$predicted_table_e2,
            selection = "single",
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

        # predicted_value_e1_proxy <- dataTableProxy("predicted_value_e1")
        # predicted_value_e2_proxy <- dataTableProxy("predicted_value_e2")

        # observe({
        #     # # ###
        #     if ( is.null(input$predicted_value_e1_rows_selected)) {
        #     } else {
        #         ids <- input$predicted_value_e1_rows_selected
        #         local_data$selected_taxa <- local_data$selected_taxas[ids]
        #         selectRows(predicted_value_e2_proxy,selected = ids)
        #         # DT::selectPage() $$ selectPage(dataId %/% 10 + 1) predicted_value_e1_state
        #     }
        # })

        # observe({
        #     # # ###
        #     if ( is.null(input$predicted_value_e2_rows_selected)) {
        #     } else {
        #         ids <- input$predicted_value_e2_rows_selected
        #         local_data$selected_taxa <- local_data$selected_taxas[ids]
        #         selectRows(predicted_value_e1_proxy,selected = ids)
        #     }
        # })

    picrust <- function(net_dir,raw_count_file) {
        
        path_python_scripts <- deploy_python_scripts
        
        #use_python("/usr/bin/python")
        
        use_condaenv(condaenv = deploy_condaenv_picrust2, conda = deploy_condabin , required = TRUE)
        
        import("picrust2.place_seqs")
        import("picrust2.wrap_hsp")
        import("picrust2.metagenome_pipeline")
        import("picrust2.util")
        import("picrust2.pathway_pipeline")
        import("picrust2.default")
        
        message(div(style = "text-align: center", h4(HTML("<b>Executing metagenome inference</b>")), ))
        Sys.sleep(2)

        message("1. Place study unaligned sequences (i.e. OTUs or ASVs) into a reference tree.")
        cmd <- paste(path_python_scripts, "place_seqs.py -s ", input$seqs$datapath, " -o ", net_dir, "out.tre -p 5 --intermediate ", net_dir, "intermediate/place_seqs", sep = "")
        system(cmd)
        message("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.")
        cmd <- paste(path_python_scripts, "hsp.py -i 16S -t ", net_dir, "out.tre -o ", net_dir, "16S_predicted_and_nsti.tsv.gz -p 5 -n", sep = "")
        system(cmd)
        message("3. Predict the enzymes of gene families present in the predicted genome for each amplicon sequence variant.")
        cmd <- paste(path_python_scripts, "hsp.py -i EC -t ", net_dir, "out.tre -o ", net_dir, "EC_predicted.tsv.gz -p 5", sep = "")
        system(cmd)
        message("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence.
                The specified sequence abundance table will be normalized by the predicted number of marker gene copies.")
        cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i ", raw_count_file, " -m ", net_dir, "16S_predicted_and_nsti.tsv.gz -f ", net_dir, "EC_predicted.tsv.gz -o ", net_dir, "metagenome_out/ --strat_out", sep = "")
        system(cmd)
        message("5. Convert abundance table.")
        cmd <- paste(path_python_scripts, "convert_table.py ", net_dir, "metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o ", net_dir, "metagenome_out/pred_metagenome_unstrat.tsv.gz", sep = "")
        system(cmd)
        message("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.")
        cmd <- paste(path_python_scripts, "pathway_pipeline.py -i ", net_dir, "metagenome_out/pred_metagenome_contrib.tsv.gz -o ", net_dir, "pathways_out/", sep = "")
        system(cmd)
        message("7. Add description column to metagenome abundance table.")
        cmd <- paste(path_python_scripts, "add_descriptions.py -i ", net_dir, "metagenome_out/pred_metagenome_unstrat.tsv.gz -m EC -o ", net_dir, "metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz", sep = "")
        system(cmd)
        message("8. Add description column to pathways abundance table.")
        cmd <- paste(path_python_scripts, "add_descriptions.py -i ", net_dir, "pathways_out/path_abun_unstrat.tsv.gz -m METACYC -o ", net_dir, "pathways_out/path_abun_unstrat_descrip.tsv.gz", sep = "")
        system(cmd)
        message((h4(HTML("DONE!"))))
    }
    prepare_count_file <- function(net_dir){

        if(!is.null(session_data$build_env$taxa_names_df)) {

            data_taxas <- fread(file = input$counts$datapath, 
              sep = "auto", dec = ".", header = T, stringsAsFactors = TRUE
            )
            data_taxas <- data.frame(data_taxas, row.names = 1)
            for (i in 1:ncol(data_taxas)) {
                c <- class(data_taxas[, i])
                if (c == "integer") {
                data_taxas[, i] <- as.numeric(data_taxas[, i])
                }
            }
            mapped_names <- colnames(data_taxas)
            taxa_names_df <- session_data$build_env$taxa_names_df
            colnames(taxa_names_df) <- c('short','org')
            rownames(taxa_names_df) <- taxa_names_df$short
            org_names_mapping <- taxa_names_df[mapped_names,]$org
            not_na <- !is.na(org_names_mapping)
            mapped_names[not_na] <- org_names_mapping[not_na]
            colnames(data_taxas) <- mapped_names
            output_filename <- file.path(net_dir, "taxa_counts.csv")
            samples_names <- rownames(data_taxas)
            data_taxas_s <- data.frame(sample_names = samples_names , data_taxas )
            write.table(data_taxas_s, file = output_filename, dec = ",", sep = ";" , row.names = FALSE)
            # df_clm_names <- colnames(data_taxas)
            # for( i in seq_len(length(df_clm_names))) {
            #     tName <- df_clm_names[i]
            #     if (tName %in% session_data$build_env$shorten_taxa_names) {
            #         which(tName == session_data$build_env$shorten_taxa_names)
            #     }
            # }
            # session_data$build_env$shorten_taxa_names %in% colnames(data_taxas)
            return(output_filename)
        }

        return(input$counts$datapath)
    }

    observeEvent(input$button_picrust, {
        validate(
            need(input$counts,"Raw counts file is required"),
            need(input$seqs,"Sequences fasta file is required"),
        )
        net_dir <- paste(deploy_dir, input$directory, "/", sep = "")
        dir.create(net_dir)
        raw_count_file <- prepare_count_file(net_dir)
        
        withCallingHandlers(
        {
            showLog()
            shinyjs::html(id = "predicted_metagenome", "")
            logjs("start")
            tryCatch(
                {
                    picrust(net_dir,raw_count_file)
                },
                error = function(cond) {
                    logjs(cond$message)
                }
            )
            logjs("end")
        },
        message = function(m) {
            shinyjs::html(id = "predicted_metagenome", html = paste0(m$message, "<br>", "<br>"), add = TRUE)
        }
        )
    })

    })
}
