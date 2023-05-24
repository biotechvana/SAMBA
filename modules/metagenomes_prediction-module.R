


metagenomes_prediction_ui <- function(id = "metagenomes_prediction_module") {
    ns <- NS(id)

    tabPanel(
        HTML("<b>Metagenomes Prediction</b>"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                fileInput(ns("counts"), "Raw counts text file", accept = ".txt"),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_counts"))),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:5em", fileInput(ns("seqs"), "Sequences fasta file", accept = c(".fa", ".fasta"))),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput(ns("example_seqs"))),
                            textOutput(ns("did_it_work")),
                            # shinyDirButton('directory', 'Select an output folder', 'Please select a folder'),
                            div(style = "padding: 0px 0px; margin-top:3em", selectInput(ns("protocol_pcrst"), label = "Select a PICRUSt2 protocol", choices = c("KEGG", "MetaCyc"), selected = "KEGG")),
                            div(style = "font-size: 10px; padding: 0px 0px; margin-top:-1em", textInput(ns("directory"), "Specify an output folder", placeholder = "experiment_1")),
                            span(tags$i(h6(HTML("<b>Remember to remove white spaces in fasta headers.</b>"))), style = "color:#52C4DD"),
                            tags$style(".buttonpicrust .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7;}"),
                            div(class = "buttonpicrust", style = "font-size: 10px; padding: 0px 0px; margin-top:2em;", actionBttn(inputId = ns("button_picrust"), label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket")))
            ),
            mainPanel(
                width = 9,
                # tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; heigth: auto; object-fit: contain;"),
                div(
                    style = "align: center",
                    tabsetPanel(
                        type = "pills",
                        tabPanel(
                            strong("Predict Metagenomes"),
                            tags$hr(style = "margin-left: -1em; max-width: none; margin-top: 5px;margin-bottom: 5px; object-fit: contain;"),
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

metagenomes_prediction_server <- function(session_data, id = "metagenomes_prediction_module") {
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


       


        


       

        
        

       

        


        

        


    picrust_metacyc <- function(net_dir,raw_count_file) {
        
        path_python_scripts <- deploy_python_scripts
        
        #use_python("/usr/bin/python")
        

        # tryCatch(
        #         {
        #            import("picrust2.place_seqs")
        #         },
        #         error = function(cond) {
                    
        #         }
        #     )
        # use_condaenv(condaenv = deploy_condaenv_picrust2, conda = deploy_condabin , required = TRUE)
        # import("picrust2.wrap_hsp")
        # import("picrust2.metagenome_pipeline")
        # import("picrust2.util")
        # import("picrust2.pathway_pipeline")
        # import("picrust2.default")
        
        
        message(div(style = "text-align: center", h4(HTML("<b>Executing metagenome inference</b>")), ))
        Sys.sleep(2)
        run_log <-  paste(net_dir, "runs.log", sep = "")

        stdout_log <-  paste(" 1> ", net_dir, "1.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "1.stderr.log", sep = "")
        message("1. Place study unaligned sequences (i.e. OTUs or ASVs) into a reference tree.")
        cmd <- paste(path_python_scripts, "place_seqs.py -s ", input$seqs$datapath, " -o ", net_dir, "out.tre -p 5 --intermediate ", net_dir, "intermediate/place_seqs",
            stdout_log, 
            stderr_log,
            sep = ""
        )
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "2.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "2.stderr.log", sep = "")
        message("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.")
        
        cmd <- paste(path_python_scripts, "hsp.py -i 16S -t ", net_dir, "out.tre -o ", net_dir, "16S_predicted_and_nsti.tsv.gz -p 5 -n",
            stdout_log, 
            stderr_log,
            sep = ""
            )
        
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "3.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "3.stderr.log", sep = "")
        message("3. Predict the enzymes of gene families present in the predicted genome for each amplicon sequence variant.")
        cmd <- paste(path_python_scripts, "hsp.py -i EC -t ", net_dir, "out.tre -o ", net_dir, "EC_predicted.tsv.gz -p 5",
            stdout_log, 
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "4.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "4.stderr.log", sep = "")
        message("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence.
                The specified sequence abundance table will be normalized by the predicted number of marker gene copies.")
        cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i ", raw_count_file, " -m ", net_dir, "16S_predicted_and_nsti.tsv.gz -f ", net_dir, "EC_predicted.tsv.gz -o ", net_dir, "metagenome_out/ --strat_out",
            stdout_log, 
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "5.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "5.stderr.log", sep = "")
        message("5. Convert abundance table.")
        cmd <- paste(path_python_scripts, "convert_table.py ", net_dir, "metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o ", net_dir, "metagenome_out/pred_metagenome_unstrat.tsv.gz",
            stdout_log,
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "6.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "6.stderr.log", sep = "")
        message("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.")
        cmd <- paste(path_python_scripts, "pathway_pipeline.py -i ", net_dir, "metagenome_out/pred_metagenome_contrib.tsv.gz -o ", net_dir, "pathways_out/",
            stdout_log,
            stderr_log,
            sep = ""
        )
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "7.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "7.stderr.log", sep = "")
        message("7. Add description column to metagenome abundance table.")
        cmd <- paste(path_python_scripts, "add_descriptions.py -i ", net_dir, "metagenome_out/pred_metagenome_unstrat.tsv.gz -m EC -o ", net_dir, "metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz",
            stdout_log,
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "8.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "8.stderr.log", sep = "")
        message("8. Add description column to pathways abundance table.")
        cmd <- paste(path_python_scripts, "add_descriptions.py -i ", net_dir, "pathways_out/path_abun_unstrat.tsv.gz -m METACYC -o ", net_dir, "pathways_out/path_abun_unstrat_descrip.tsv.gz",
            stdout_log,
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")


        message((h4(HTML("DONE!"))))
    }

    picrust_kegg <- function(net_dir,raw_count_file) {
        
        path_python_scripts <- deploy_python_scripts
        picrust_kegg_files <- deploy_picrust_kegg_files
        
        #use_python("/usr/bin/python")
        

        # tryCatch(
        #         {
        #            import("picrust2.place_seqs")
        #         },
        #         error = function(cond) {
                    
        #         }
        #     )
        # use_condaenv(condaenv = deploy_condaenv_picrust2, conda = deploy_condabin , required = TRUE)
        # import("picrust2.wrap_hsp")
        # import("picrust2.metagenome_pipeline")
        # import("picrust2.util")
        # import("picrust2.pathway_pipeline")
        # import("picrust2.default")
        
        
        message(div(style = "text-align: center", h4(HTML("<b>Executing metagenome inference</b>")), ))
        Sys.sleep(2)
        run_log <-  paste(net_dir, "runs.log", sep = "")

        stdout_log <-  paste(" 1> ", net_dir, "1.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "1.stderr.log", sep = "")
        message("1. Place study unaligned sequences (i.e. OTUs or ASVs) into a reference tree.")
        cmd <- paste(path_python_scripts, "place_seqs.py -s ", input$seqs$datapath, " -o ", net_dir, "out.tre -p 5 --intermediate ", net_dir, "intermediate/place_seqs",
            stdout_log, 
            stderr_log,
            sep = ""
        )
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "2.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "2.stderr.log", sep = "")
        message("2. Predict the copy number of gene families present in the predicted genome for each amplicon sequence variant.")
        
        cmd <- paste(path_python_scripts, "hsp.py -i 16S -t ", net_dir, "out.tre -o ", net_dir, "16S_predicted_and_nsti.tsv.gz -p 5 -n",
            stdout_log, 
            stderr_log,
            sep = ""
            )
        
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "3.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "3.stderr.log", sep = "")
        message("3. Predict the KOs of gene families present in the predicted genome for each amplicon sequence variant.")
        cmd <- paste(path_python_scripts, "hsp.py -i KO -t ", net_dir, "out.tre -o ", net_dir, "KO_predicted.tsv.gz -p 5",
            stdout_log, 
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "4.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "4.stderr.log", sep = "")
        message("4. Per-sample metagenome functional profiles are generated based on the predicted functions for each study sequence.
                The specified sequence abundance table will be normalized by the predicted number of marker gene copies.")
        cmd <- paste(path_python_scripts, "metagenome_pipeline.py -i ", raw_count_file, " -m ", net_dir, "16S_predicted_and_nsti.tsv.gz -f ", net_dir, "KO_predicted.tsv.gz -o ", net_dir, "metagenome_out/ --strat_out",
            stdout_log, 
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "5.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "5.stderr.log", sep = "")
        message("5. Convert abundance table.")
        cmd <- paste(path_python_scripts, "convert_table.py ", net_dir, "metagenome_out/pred_metagenome_contrib.tsv.gz -c contrib_to_legacy -o ", net_dir, "metagenome_out/pred_metagenome_unstrat.tsv.gz",
            stdout_log,
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "6.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "6.stderr.log", sep = "")
        message("6. Infer the presence and abundances of pathways based on gene family abundances in a sample.")
        cmd <- paste(path_python_scripts, "pathway_pipeline.py -i ", net_dir, "metagenome_out/pred_metagenome_contrib.tsv.gz -o ", net_dir, "pathways_out/ -p 5 --no_regroup --map ", picrust_kegg_files, "KEGG_pathways_to_KO.tsv",
            stdout_log,
            stderr_log,
            sep = ""
        )
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "7.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "7.stderr.log", sep = "")
        message("7. Add description column to metagenome abundance table.")
        cmd <- paste(path_python_scripts, "add_descriptions.py -i ", net_dir, "metagenome_out/pred_metagenome_unstrat.tsv.gz -m KO -o ", net_dir, "metagenome_out/pred_metagenome_unstrat_descrip.tsv.gz",
            stdout_log,
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")

        stdout_log <-  paste(" 1> ", net_dir, "8.stdout.log", sep = "")
        stderr_log <-  paste(" 2> ", net_dir, "8.stderr.log", sep = "")
        message("8. Add description column to pathways abundance table.")
        cmd <- paste(path_python_scripts, "add_descriptions.py -i ", net_dir, "pathways_out/path_abun_unstrat.tsv.gz --custom_map_table ", picrust_kegg_files, "KEGG_pathways_info.tsv -o ", net_dir, "pathways_out/path_abun_unstrat_descrip.tsv.gz",
            stdout_log,
            stderr_log,
            sep = ""
        )
                
        cat(cmd,file=run_log,sep="\n",append=TRUE)
        ret_code <- system(cmd)
        if(ret_code != 0) {
            message((h4(HTML("Error Please check log file!"))))
            return(NULL)
        }
        message("OK &#x2713;")


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

    current_bg_process <- eventReactive(input$button_picrust, {
        validate(
            need(input$counts,"Raw counts file is required"),
            need(input$seqs,"Sequences fasta file is required"),
            need(input$directory,"output folder is required")
        )
        net_dir <- paste(deploy_dir, input$directory, "/", sep = "")
        validate(
            # need(session_data()$fittedbn, "Please Load network first"),
            need(!dir.exists(net_dir), paste("The output folder : ", input$directory, " already exist, Please can you specify another output folder"))
        )
        dir.create(net_dir)
        raw_count_file <- prepare_count_file(net_dir)
        disable("button_picrust")
        withCallingHandlers(
        {
            showLog()
            shinyjs::html(id = "predicted_metagenome", "")
            logjs("start")
            tryCatch(
                {
                    if (input$protocol_pcrst == "MetaCyc") {
                        picrust_metacyc(net_dir,raw_count_file)
                    } else if (input$protocol_pcrst == "KEGG") {
                        picrust_kegg(net_dir,raw_count_file)
                    }
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
        enable("button_picrust")
    })


    output$did_it_work <- renderText({
      current_bg_process()
    })

    })
}
