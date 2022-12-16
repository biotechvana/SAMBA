
prediction_panel_org <- tabPanel(
  HTML("<b>Prediction and metagenomic inference</b>"),
  selectInput("prediction_mode", "Select mode: ", choices = c("Automatic", "Customized"), selected = "Automatic"),
  conditionalPanel(
    condition = "input.prediction_mode == 'Automatic'",
    sidebarPanel(
      width = 12,
      tags$label(h3("Input files and parameters")),
      hr(),
      tags$style(".buttonagency .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7; margin-top:1em;}"),
      fileInput("network_auto", "Network", accept = ".RData"),
      div(style = "font-size: 10px; padding: 0px 0px; margin-top:-1em", numericInput("iterations_auto", "Iterations", value = 1, min = 1, max = 50, step = 0.5)),
      div(style = "font-size: 10px; padding: 0px 0px; margin-top:3em", fileInput("seqs_auto", "Sequences", accept = c(".fasta", ".fa", ".fas"))),
      div(style = "font-size: 10px; padding: 0px 0px; margin-top:-1em", numericInput("error_network_auto", "Conditional probability allowed +- error", value = 0.3, min = 0, max = 1, step = 0.05)),
      div(style = "font-size: 10px; padding: 0px 0px;", textInput("directory_auto", "Specify an output folder", placeholder = "experiment_1")),
      # div(style = "font-size: 10px; padding: 0px 0px; margin-top:3em",directoryInput('directory_auto', label = 'Select an output folder')),
      div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "button_auto", label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
      div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "stop_button_auto", label = "Stop process", style = "float", color = "primary", size = "sm", icon = icon("window-close"))),
      div(class = "buttonagency", style = "display:inline-block", actionBttn(inputId = "check_button_auto", label = "Check status", style = "float", color = "primary", size = "sm", icon = icon("check-square")))
    ),
    mainPanel(
      shinyjs::useShinyjs(),
      tags$head(tags$style(HTML(".shiny-notification {width: 800px; position:fixed; top: calc(98% - 50px);; left: calc(50% - 400px);;}"))),
      tags$head(tags$style("#automated_pred{text-align:justify; overflow-y:scroll; overflow-x:hidden; max-height: 575px; background: #F8F8F8;}")),
      textOutput("automated_pred")
    )
  ),
  conditionalPanel(
    condition = "input.prediction_mode == 'Customized'",
    fluidRow(
      column(
        3,
        wellPanel(
          tags$label(h3("Input network")),
          # hr(),
          fileInput("network", "Network", accept = ".RData")
        )
      ),
      column(
        6,
        wellPanel(
          tags$label(h3("Predict taxa values")),
          # hr(),
          textInput("taxas", "Taxas", value = "", placeholder = 'Example: Bacteria_X,Bacteria_Y. Write "all" for all taxas.'),
          # textInput("evidence", "Evidence", value= "", placeholder = 'Example: season=S,tissue=AI'),
          # verbatimTextOutput("directorypath"),
          # shinyDirButton('directory', 'Select a folder', 'Please select a folder', FALSE),
          # textInput("outputFolder", "Output folder", value= getwd()),
          # div(style = "display:inline-block; float:right;", actionButton("button1", "Submit")))),
          numericInput("iterations", "Iterations", value = 1, min = 1, max = 50, step = 0.5),
          numericInput("error_network", "Conditional probability allowed +- error", value = 0.3, min = 0, max = 1, step = 0.05),
          tags$style(HTML(pickerInput_select)),
          tags$style(HTML(pickerInput_deselect)),
          uiOutput("selector", container = div),
          div(class = "buttonagency", actionBttn(inputId = "button1", label = "Submit", style = "float", color = "primary", size = "sm"))
        )
      ),
      column(
        3,
        wellPanel(tags$label(h3("Display conditional probability tables")),
          # hr(),
          textInput("nodes", "Nodes", value = "", placeholder = "Example: Bacteria_X,Bacteria_Y,season,tissue"),
          # numericInput("shiny_width", "Width", value= "1000"),
          # numericInput("shiny_heigth", "Heigth", value= "1000"),
          height = 12,
          div(class = "buttonagency", actionBttn(inputId = "button2", label = "Submit", style = "float", color = "primary", size = "sm"))
        )
      )
    ),
    mainPanel(tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
      div(style = "align: center", tabsetPanel(
        type = "pills",
        tabPanel(
          strong("Predicted value"),
          tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
          dataTableOutput("predicted_value")
        ),
        tabPanel(
          strong("Conditional probability table"),
          tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
          uiOutput("selector_cpt", container = div),
          downloadButton("save_cpt_all", "Download all tables", class = "butt"),
          tags$head(tags$style(".butt{background-color:#F7F7F7;} .butt{color: black;}")),
          downloadButton("save_cpt", "Download current table", class = "butt"),
          tags$head(tags$style("#conditional_table{overflow: auto; padding: 20px; text-align:justify; overflow-y:scroll; overflow-x:scroll; max-height: 575px; background: #F8F8F8;}")),
          verbatimTextOutput("conditional_table")
        ),
        tabPanel(
          strong("Infer metagenome"),
          tags$hr(style = "margin-left: -1em; max-width: none; max-heigth: 100vh; width: 100vw; heigth: auto; object-fit: contain;"),
          fileInput("counts", "Raw counts text file", accept = ".txt"),
          div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_counts")),
          div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", fileInput("seqs", "Sequences fasta file", accept = c(".fa", ".fasta"))),
          div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", uiOutput("example_seqs")),
          # shinyDirButton('directory', 'Select an output folder', 'Please select a folder'),
          div(style = "font-size: 10px; padding: 0px 0px; margin-top:2em", directoryInput("directory", label = "Select an output folder")),
          span(tags$i(h6(HTML("<b>Remember to remove white spaces in fasta headers.</b>"))), style = "color:#52C4DD"),
          tags$style(".buttonpicrust .bttn-primary{color: #3B3B3B; border-color: #7D7D7D; background-color: #E7E7E7;}"),
          div(class = "buttonpicrust", style = "font-size: 10px; padding: 0px 0px; margin-top:2em;", actionBttn(inputId = "button_picrust", label = "Launch", style = "float", color = "primary", size = "sm", icon = icon("rocket"))),
          mainPanel(fluidRow(
            align = "center", shinyjs::useShinyjs(), style = "background-color:#F8F8F8;",
            textOutput("predicted_metagenome")
          ), width = 12)
        )
      )),
      style = "width: 1000px; height: 1000px"
    )
  )
)  
  

  ##########################
  
  observeEvent(input$network_plot, {
    if (is.null(input$network_plot)) {
      return(NULL)
    }
    inFile <- isolate({
      input$network_plot
    })
    file <- inFile$datapath
    load(file, envir = .GlobalEnv)

    Graph_output <- reactiveValues(graph = NULL)

    ## Plot the network
    plotInput <<- function() {
      # nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
      # nodes <- strsplit(nodes, ",")[[1]]
      # for (i in 1:length(nodes)) {
      # nodes[i] <- sub("/",".", nodes[i])
      # nodes[i] <- sub(" ",".", nodes[i])
      # nodes[i] <- str_replace(nodes[i], "-",".")
      # }
      nodes <- names(fittedbn)
      # nodes = nodes[11:length(nodes)]
      subgr <<- bnlearn::subgraph(fittedbn, nodes)
      tryit <- try(strength.viewer(
        bayesianNetwork = subgr,
        bayesianNetwork.boot.strength = arc_st_mi,
        bayesianNetwork.arc.strength.label = TRUE,
        bayesianNetwork.arc.strength.tooltip = TRUE,
        # bayesianNetwork.edge.scale.min = 1,
        # bayesianNetwork.edge.scale.max = 1,
        edges.dashes = FALSE,
        bayesianNetwork.layout = "layout_nicely"
      ))
      if (inherits(tryit, "try-error")) {
        gr <- bn.to.igraph(subgr)
        p <- visIgraph(gr)
        # data <- toVisNetworkData(igraph_network)
        # visNetwork(nodes = data$nodes, edges = data$edges)
      } else {
        p <- strength.viewer(
          bayesianNetwork = subgr,
          bayesianNetwork.boot.strength = arc_st_mi,
          bayesianNetwork.arc.strength.label = TRUE,
          bayesianNetwork.arc.strength.tooltip = TRUE,
          # bayesianNetwork.edge.scale.min = 1,
          # bayesianNetwork.edge.scale.max = 1,
          edges.dashes = FALSE,
          bayesianNetwork.layout = "layout_nicely",
        )
      }
      # Graph_output$graph <- p
    }


    Tab_inputs <- reactiveValues(
      nodes = NULL,
      edges = NULL,
      Ord_edg0 = NULL,
      Sel_group = "None",
      refresh = NULL,
      # write = NULL
    )

    # Reset
    
    observeEvent(eventExpr = input$button_plot, ignoreNULL = FALSE, {
      shinyjs::reset("Edit_menu")

      updateSidebar("sidebar")

      nodes <<- NULL
      nodes_info <<- NULL
      edges <<- NULL
      edges_info <<- NULL

      # Reseteamos todas las variables filt
      'Filt <<- reactiveValues(sel_tab_n = NULL,
                              sel_tab_e = NULL,
                              sel_menu = NULL,
                              grade = 0,
                              direction = "All")'
      Filt$sel_tab_n <<- NULL
      Filt$sel_tab_e <<- NULL
      Filt$sel_menu <<- NULL
      Filt$sel_by <<- NULL
      Filt$grade <<- 0
      Filt$number <<- 0
      Filt$direction <<- "All"


      Tab_inputs$nodes <<- NULL
      Tab_inputs$edges <<- NULL

      Sel_edg <- NULL
      Sel_nod <- NULL

      # shinyjs::reset("Filter_Tab_N")
      # shinyjs::reset("Filter_Tab_E")

      # shinyjs::reset("Filter_Tab_Menu")
      # Undo()
      updateCheckboxGroupInput(
        inputId = "Filter_Tab_N",
        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
        selected = NULL
      )
      updateCheckboxGroupInput(
        inputId = "Filter_Tab_E",
        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
        selected = NULL
      )
      updateCheckboxGroupInput(
        inputId = "Filter_Menu",
        choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
        selected = NULL
      )
      updateCheckboxGroupInput(
        inputId = "Filter_Menu_By",
        choices = list("Selected" = "By_sel", "Group" = "By_group", "Number of interactions" = "By_num"),
        selected = NULL
      )

      DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
        clearSearch()

      DT::dataTableProxy("Tab_edges") %>% # Reordenamos
        clearSearch()


      
    })


    observeEvent(input$button_plot, ignoreNULL = FALSE, {
      # print("*2")
      Tab_inputs$refresh <- input$button_plot
    })

    ## Link button to print image
    p_button <- eventReactive(Tab_inputs$refresh, print(plotInput()))
    # cpt_button <- eventReactive(input$button2, table_cpt())

    # #

    # Net representation
    output$network_proxy <- renderVisNetwork(
      # if(! is.null(Graph_output$graph)){
      p_button() %>%
        # Graph_output$graph %>%
        # Permite seleccionar nodos (file:data)
        visEvents(
          type = "on",
          # Al seleccionar guardamos los ids de nodos y edges
          select = "function(data) {
                Shiny.onInputChange('current_nodes_selection', data.nodes);
                Shiny.onInputChange('current_edges_selection', data.edges);
              }",
          # deselectNode = "function(data) {
          # Shiny.onInputChange('deselect_nodes', data.nodes);
          # }" ,
          doubleClick = "function(data) {
                Shiny.onInputChange('doubleClick_nodes_selection', data.nodes);
                Shiny.onInputChange('doubleClick_edges_selection', data.edges);
               }",
          # input$network_proxy_initialized pasa a true al generar la red completamente, pero solo funciona la primera vez, por tanto al iniciar la red la fijamos en FALSE
          afterDrawing = "function(data) {
                Shiny.onInputChange('network_proxy_initialized', false);
               }",

          # Al deseleccionar, deseleccionamos en la tabla:
        ) %>%
        # Fija los parámetros por defecto de la primara representación
        visNodes(
          color = list(
            background = DT$color_background,
            border = DT$color_border,
            highlight = list(
              background = DT$color_highlight,
              border = DT$color_border
            )
          ),
          physics = DT$bounce,
          shape = DT$shape
        ) %>%
        visEdges(scaling = list(
          min = DT$scaling_min,
          max = DT$scaling_max,
          label = list(
            enabled = DT$scaling_label_enabled,
            min = DT$scaling_label_min,
            max = DT$scaling_label_max
          )
        )) %>%
        visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE)
      # %>%  visPhysics(stabilization = FALSE)
      # }
    )


    observeEvent(eventExpr = input$network_proxy_initialized, ignoreNULL = FALSE, {
      # print("*4")
      # Tab_inputs$write = TRUE
      visNetworkProxy("network_proxy") %>%
        visGetNodes() %>%
        visGetEdges() %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    ## Funciones

    Select_nodes <- function() { # Get selected nodes ids
      # visNetworkProxy("network_proxy") %>%
      # visGetSelectedNodes() # Return input$network_proxy_selectedNodes
      nod <- input$network_proxy_selectedNodes
      if (is.null(nod)) {
        Sel_nodes <- nodes$id
      } else {
        Sel_nodes <- nod
      }
      return(Sel_nodes)
    }


    Get_nodes <- function() { # Get all nodes ids
      visNetworkProxy("network_proxy") %>%
        visGetNodes() # return input$network_proxy_nodes
    }


    Nodes_info <- function() { # Get all nodes info
      # Get_nodes()
      nodes_in <- input$network_proxy_nodes

      nodes_out <- data.frame()
      for (i in c(1:length(nodes_in))) {
        nod <- nodes_in[[i]]
        if (nrow(nodes_out) == 0) {
          nodes_out <- data.frame(nod)
        } else {
          nodes_out <- merge(x = nodes_out, y = nod, all = TRUE)
        }
      }
      return(nodes_out)
    }


    Select_edges <- function() { # Get selected edges ids
      # visNetworkProxy("network_proxy") %>%
      # visGetSelectedEdges() # Return input$network_proxy_selectedEdges
      edg <- input$network_proxy_selectedEdges

      if (is.null(edg)) {
        Sel_edges <- edges$id
      } else {
        Sel_edges <- edg
      }
      return(Sel_edges)
    }

    Get_edges <- function() { # Get all edges ids
      visNetworkProxy("network_proxy") %>%
        visGetEdges() # return input$network_proxy_edges
    }


    Edges_info <- reactive({
      # Get_edges()
      edges_in <- input$network_proxy_edges

      edges_out <- data.frame()
      for (i in c(1:length(edges_in))) {
        nod <- edges_in[[i]]
        if (nrow(edges_out) == 0) {
          edges_out <- data.frame(nod)
        } else {
          edges_out <- merge(x = edges_out, y = nod, all = TRUE)
        }
      }
      return(edges_out)
    })


    Nodes_table <- function() {
      # print(input$network_proxy_nodes[1])
      nodes <<- Nodes_info()

      nodes_info <<- data.frame( # Info nodos
        nodes,
        size = DT$size,
        # Info color
        color.background = DT$color_background,
        color.border = DT$color_border,
        color.highlight.background = DT$color_highlight,
        color.highlight.border = DT$color_border,
        color.hover.background = DT$color_background,
        color.hover.border = DT$color_border,

        # Info labels
        font.color = DT$font_color,
        font.size = DT$font_size,
        font.face = DT$font_face,
        font.strokeWidth = DT$font_strokeWidth,
        font.strokeColor = DT$font_strokeColor,
        font.align = DT$N_font_align

        # Shadow
        # shadow.enabled = DT$shadow_enabled,
        # shadow.color = DT$shadow_color,
        # shadow.size = DT$shadow_size,
        # shadow.x = DT$shadow_x,
        # shadow.y = DT$shadow_y
      )
      Tab_inputs$nodes <- nodes[, c("id", "label")]
      Ord_nod_tab <<- Tab_inputs$nodes

      # Definimos los grupos de nodos a saleccionar posteriormente
      Groups <<- list(All = nodes$id, None = NULL)
    }

    Edges_table <- function() {
      Get_edges()
      edges <<- Edges_info()

      edges_info <<- data.frame(
        edges,

        # Scaling
        scaling.min = DT$scaling_min,
        scaling.max = DT$scaling_max,
        scaling.label.enabled = DT$scaling_label_enabled,
        scaling.label.min = DT$scaling_label_min,
        scaling.label.max = DT$scaling_label_max,

        # Info color
        color.color = DT$color_border,
        color.highlight = DT$color_border,
        color.opacity = 1,

        # Info labels
        font.color = DT$font_color,
        font.size = DT$font_size,
        font.face = DT$font_face,
        font.strokeWidth = DT$font_strokeWidth,
        font.strokeColor = DT$font_strokeColor,
        font.align = DT$E_font_align

        # Shadow
        # shadow.enabled = DT$shadow_enabled,
        # shadow.color = DT$shadow_color,
        # shadow.size = DT$shadow_size,
        # shadow.x = DT$shadow_x,
        # shadow.y = DT$shadow_y
      )

      Tab_inputs$edges <- edges[, c("id", "value", "from", "to")]
      Ord_edg_tab <<- Tab_inputs$edges # Tab_inputs$edges

      # Rep <<- 1
    }


    observeEvent(eventExpr = input$network_proxy_edges, ignoreNULL = TRUE, {
      # print("*5")

      Nodes_table()
      Edges_table()

      if (isFALSE(input$E_label)) {
        S_edges <- edges_info$id
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(id = S_edges, label = " "))
      }
      # Tab_inputs$refresh <<- 0
    })

    ######

    # Tab information
    # Funciones

    ## Ordenar
    OrderNodes <- function() {
      if (isFALSE(input$Reorder_N) | is.null(input$network_proxy_selectedNodes)) {
        Ord_nod_tab <<- nodes[, c("id", "label")] # Tab_inputs$nodes
      } else {
        Ind_nodes <- which(Ord_nod_tab$id %in% input$network_proxy_selectedNodes)

        Sel_tab <- (merge(
          x = data.frame(
            ord = (1:length(input$network_proxy_selectedNodes)),
            node = input$network_proxy_selectedNodes
          ),
          y = data.frame(
            ind = Ind_nodes,
            node = Ord_nod_tab$id[Ind_nodes]
          )
        ))

        Sel_nodes <- (Sel_tab[with(Sel_tab, order(Sel_tab$ord, decreasing = TRUE)), ])$ind
        Non_select <- which(!Ord_nod_tab$id %in% input$network_proxy_selectedNodes)

        Order <- c(Sel_nodes, Non_select)
        Ord_nod_tab <<- Ord_nod_tab[Order, ]
      }
    }


    # Outpur Tab
    observe({
      #  Tab_inputs$refresh
      Tab_inputs$nodes
      input$Data
      Sel_nod0 <- which(Ord_nod_tab$id %in% Sel_nod)

      output$Tab_nodes <- DT::renderDataTable({
        D_T <- DT::datatable(Ord_nod_tab,
          options = list(scrollX = TRUE),
          editable = list(target = "column", disable = list(columns = c(1))), # Permite editar
          escape = FALSE,
          filter = "top",
          selection = list(
            mode = "multiple",
            selected = Sel_nod0,
            target = "row"
          ),
          callback = htmlwidgets::JS( ## https://stackoverflow.com/questions/66129627/create-an-r-shiny-binding-to-a-double-click-event-on-a-dt-datatable
            "table.on('dblclick', 'td',",
            "  function() {",
            "    var row = table.cell(this).index().row;",
            "    var col = table.cell(this).index().column;",
            "    Shiny.setInputValue('dt_dblclick', {dt_row: row, dt_col: col});",
            "  }",
            ");",
            "table.on('click', 'td',",
            "  function() {",
            "    var row = table.cell(this).index().row;",
            "    var col = table.cell(this).index().column;",
            "    Shiny.setInputValue('dt_click', {dt_row: row, dt_col: col});",
            "  }",
            ");"
          )
        )
      })
    })

    '
    observeEvent(input$dt_dblclick, {
      print("DOBLE CLICK")
      print(input$dt_dblclick)
    })
    observeEvent(input$dt_click, {
      print(" CLICK")
      print(input$dt_click)
    })'

    # Selecciona en la red los nodos de la Tab_nodes
    observeEvent(eventExpr = input$Tab_nodes_cell_clicked, ignoreNULL = FALSE, { # Se activa cuando hacemos click en la tabla

      if (length(input$Tab_nodes_cell_clicked) > 0) {
        S_nodes <- {
          nod <- input$Tab_nodes_rows_selected
          tab <- Ord_nod_tab
          if (is.null(nod)) {
            S_nodes <- NULL
          } else {
            sel <- input$Tab_nodes_cell_clicked$row
            n1 <- nod[which(nod == sel)]
            n2 <- nod[which(nod != sel)]
            S_nodes <- append(tab[n1, "id"], tab[n2, "id"])
          }
          S_nodes
        }

        S_edges <- input$network_proxy_selectedEdges
        S_nodes <- rev(S_nodes)

        if (is.null(S_edges) && is.null(S_nodes)) {
          visNetworkProxy("network_proxy") %>%
            visUnselectAll()
        } else {
          visNetworkProxy("network_proxy") %>%
            visSetSelection(
              nodesId = S_nodes,
              edgesId = S_edges,
              unselectAll = TRUE,
              highlightEdges = FALSE
            )
        }
      }

      # Actualizamos
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    # Ordenamos la tabla para posicionar los seleccionados primero
    observe({
      OrderNodes()
    })


    # Seleccionar filas que se corresponden con los nodos seleccionados en la red
    observe({
      Sel_nod <<- input$network_proxy_selectedNodes
      input$Reorder_N

      if (is.null(input$network_proxy_selectedNodes)) {
        DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
          replaceData(data = Ord_nod_tab, clearSelection = FALSE, resetPaging = FALSE) %>%
          # Eliminamos la selección
          selectRows(NULL)
      } else {
        DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
          replaceData(data = Ord_nod_tab, clearSelection = FALSE, resetPaging = FALSE) %>% # Seleccionamos
          selectRows(which(Ord_nod_tab$id %in% input$network_proxy_selectedNodes))
      }
    })


    ### Edicion de las etiquetas de los nodos ###

    # Edición de las Etiquetas de los nodos
    observeEvent(eventExpr = input$Tab_nodes_cell_edit, {
      Nodes_edit <- input$Tab_nodes_cell_edit

      if (colnames(Ord_nod_tab)[Nodes_edit$col] == "label") {
        ID <- Ord_nod_tab[Nodes_edit$row, "id"]
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = ID,
            label = Nodes_edit$value
          ))
        nodes[which(nodes$id == ID), "label"] <<- Nodes_edit$value
        nodes_info[which(nodes_info$id == ID), "label"] <<- Nodes_edit$value
      }
    })


    # Edges

    ## Escogemos el número de decimales
    output$Value_decimals <- renderUI({
      if (!is.null(Tab_inputs$edges$value)) {
        sig_count <- function(n) {
          num <- gsub("^[^(1-9)]*", "", x = n)
          num <- gsub("\\.", "", x = num)
          nchar(num)
        }
        Sig <- max(sapply(X = Tab_inputs$edges$value, FUN = sig_count))

        sliderInput(
          inputId = "Value_decimals", label = "Significant digits",
          step = 1, min = 1, max = Sig, value = Sig
        )
      }
    })


    # Funciones

    # Ordenar
    OrderEdges <- function() {
      if (isFALSE(input$Reorder_E) | is.null(input$network_proxy_selectedEdges)) {
        Ord_edg_tab <<- edges[, c("id", "value", "from", "to")]
      } else {
        Ind_edges <- which(Ord_edg_tab$id %in% input$network_proxy_selectedEdges)

        Sel_tab <- (merge(
          x = data.frame(
            ord = (1:length(input$network_proxy_selectedEdges)),
            edge = input$network_proxy_selectedEdges
          ),
          y = data.frame(
            ind = Ind_edges,
            edge = Ord_edg_tab$id[Ind_edges]
          )
        ))

        Sel_edges <- (Sel_tab[with(Sel_tab, order(Sel_tab$ord, decreasing = TRUE)), ])$ind
        Non_select <- which(!Ord_edg_tab$id %in% input$network_proxy_selectedEdges)
        Order <- c(Sel_edges, Non_select)
        Ord_edg_tab <<- Ord_edg_tab[Order, ]
      }
    }

    # Output de la tabla edges:
    output$Tab_edges <- DT::renderDataTable({
      Sel_edg0 <- which(Ord_edg_tab$id %in% Sel_edg)


      DT_edg <<- DT::datatable( # Tab_inputs$Ord_edg0,
        Ord_edg_tab,
        # Tab_inputs$edges,
        # Ord_edg0,
        options = list(scrollX = TRUE),
        editable = FALSE, # No permite editar por el usuario
        escape = FALSE,
        filter = "top",
        selection = list(
          mode = "multiple",
          selected = Sel_edg0, # Fijamos la selección inicial
          target = "row"
        )
      )
      if (!is.null(input$Value_decimals)) {
        DT_edg %>%
          formatSignif(columns = c("value"), digits = input$Value_decimals)
      }
    })


    # Cambiamos las etiquetas de los edges
    observeEvent(eventExpr = input$Value_decimals, ignoreNULL = TRUE, {
      if (isTRUE(input$E_label)) {
        lab <- sapply(X = edges_info$value, FUN = signif, input$Value_decimals)
        edges_info$label <<- sapply(X = lab, FUN = as.character)
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = edges_info$id,
            label = edges_info$label
          ))
      }
    })


    # Selecciona los edges que une dos nodos seleccionados.
    observeEvent(eventExpr = input$network_proxy_selectedNodes, {
      if (!is.null(input$network_proxy_selectedNodes)) {
        e1 <- edges[edges$from %in% input$network_proxy_selectedNodes, ]
        e2 <- e1[e1$to %in% input$network_proxy_selectedNodes, ]

        if (nrow(e2) > 0) {
          S_edges <- unique(append(input$network_proxy_selectedEdges, e2$id))
          S_nodes <- input$network_proxy_selectedNodes
          visNetworkProxy("network_proxy") %>%
            visSetSelection(
              nodesId = S_nodes,
              edgesId = S_edges,
              unselectAll = FALSE,
              highlightEdges = FALSE
            )
        }

        # Actualizamos
        visNetworkProxy("network_proxy") %>%
          visGetSelectedNodes() %>%
          visGetSelectedEdges()
      }
    })


    # Selecciona en la red los edges de la Tab_edges
    observeEvent(eventExpr = input$Tab_edges_cell_clicked, ignoreNULL = FALSE, {
      S_edges <- {
        edg <- input$Tab_edges_rows_selected
        tab <- Ord_edg_tab
        if (is.null(edg)) {
          S_edges <- NULL
        } else {
          S_edges <- tab[edg, "id"]
        }
        S_edges
      }
      S_nodes <- input$network_proxy_selectedNodes

      if (is.null(S_nodes) && is.null(S_edges)) {
        visNetworkProxy("network_proxy") %>%
          visUnselectAll()
      } else {
        visNetworkProxy("network_proxy") %>%
          visSetSelection(
            nodesId = S_nodes,
            edgesId = S_edges,
            unselectAll = TRUE,
            highlightEdges = FALSE
          )
      }
      # Actualizamos
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    # Ordenamos la tabla para posicionar los seleccionados primero
    observe({
      Tab_inputs$edges
      input$Value_decimals

      OrderEdges()
      visNetworkProxy("network_proxy") %>%
        visGetSelectedEdges()
    })


    # Seleccionar filas que se corresponden con los edges seleccionados en la red
    observe({
      Sel_edg <<- input$network_proxy_selectedEdges
      Tab_inputs$edges
      input$Value_decimals
      input$Reorder_E

      if (is.null(input$network_proxy_selectedEdges)) {
        DT::dataTableProxy("Tab_edges") %>%
          replaceData(data = Ord_edg_tab, clearSelection = TRUE) %>%
          selectRows(NULL)
      } else {
        DT::dataTableProxy("Tab_edges") %>%
          replaceData(data = Ord_edg_tab, clearSelection = FALSE) %>%
          selectRows(which(Ord_edg_tab$id %in% input$network_proxy_selectedEdges))
      }
    })

    # Informacion nodos
    ### Texto a mostrar ###

    # id del nodo tras el doble click
    output$N1 <- renderPrint({
      if (is.null(input$doubleClick_nodes_selection)) {
        NULL
      } else {
        fittedbn[[input$doubleClick_nodes_selection]]
      }
    })


    output$Node_info <- renderUI({
      if (is.null(input$doubleClick_nodes_selection)) {
        NULL
      } else {
        verbatimTextOutput("N1")
      }
    })


    output$E1 <- renderText({
      edge_id <- input$doubleClick_edges_selection

      from <- edges[edges$id == edge_id, "from"]

      to <- edges[edges$id == edge_id, "to"]

      value <- edges[edges$id == edge_id, "value"]

      paste0(
        "Causal Relationships : \n  ",
        from, "  →  ", to,
        "\nEdge Strength : \n  ",
        value
      )
    })


    output$Edge_info <- renderUI({
      if (is.null(input$doubleClick_edges_selection)) {
        NULL
      } else {
        verbatimTextOutput("E1")
      }
    })
    ######

    # Panel de edicion

    ##  Nodos

    observeEvent(eventExpr = input$current_nodes_selection, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    observeEvent(eventExpr = input$Tab_nodes_cell_clicked, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    # Shape
    observeEvent(eventExpr = input$N_shape, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, shape = input$N_shape))
      }
    })


    # Size
    observeEvent(eventExpr = input$N_size, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "size"] <<- input$N_size

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, size = input$N_size))
      }
    })


    Auto_size_all <- function(nod) {
      connections <- length(which(edges["from"] == nod | edges["to"] == nod))
      return(connections)
    }

    Auto_size_from <- function(nod) {
      connections <- length(which(edges["from"] == nod))
      return(connections)
    }

    Auto_size_to <- function(nod) {
      connections <- length(which(edges["to"] == nod))
      return(connections)
    }


    observe({
      if (isTRUE(input$N_autosize)) {
        if (!is.null(nodes)) {
          if (input$N_autosize_type == "E_F") {
            connections <- sapply(nodes$id, Auto_size_from)
          } else {
            if (input$N_autosize_type == "E_T") {
              connections <- sapply(nodes$id, Auto_size_to)
            } else {
              connections <- sapply(nodes$id, Auto_size_all)
            }
          }
          max1 <- max(connections)
          min1 <- min(connections)
          min0 <- input$N_size_range[1]
          max0 <- input$N_size_range[2]

          r <- (max0 - min0) / max1

          sizes <- list()
          for (c in connections) {
            f <- min0 + (c * r)
            sizes <- append(sizes, round(f))
          }
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(id = nodes$id, size = unlist(sizes)))
        }
      } else {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = nodes$id, size = nodes_info$size))
      }
    })


    # Border Width
    observeEvent(eventExpr = input$N_borderWidth, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, borderWidth = input$N_borderWidth))
      }
    })


    ## Selected Border Width
    observeEvent(eventExpr = input$N_borderWidthSelected, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, borderWidthSelected = input$N_borderWidthSelected))
      }
    })


    # Background color
    observeEvent(eventExpr = input$N_color_background, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.background"] <<- input$N_color_background

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Border color
    observeEvent(eventExpr = input$N_color_border, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.border"] <<- input$N_color_border

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Highlight background color
    observeEvent(eventExpr = input$N_color_highlight_background, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.highlight.background"] <<- input$N_color_highlight_background

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Highlight border color
    observeEvent(eventExpr = input$N_color_highlight_border, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "color.highlight.border"] <<- input$N_color_highlight_border

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
          ))
      }
    })


    # Opacity
    observeEvent(eventExpr = input$N_opacity, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(id = S_nodes, opacity = input$N_opacity))
      }
    })


    # Show labels
    observeEvent(eventExpr = input$N_label, ignoreNULL = FALSE, {
      if (!is.null(nodes)) {
        S_nodes <- nodes$id
        if (isTRUE(input$N_label)) {
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(
              id = S_nodes,
              label = nodes_info$label
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(id = S_nodes, label = " "))
        }
      }
    })


    # Color
    observeEvent(eventExpr = input$N_label_color, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.color"] <<- input$N_label_color

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Tamaño
    observeEvent(eventExpr = input$N_label_size, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.size"] <<- input$N_label_size

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Fuente
    observeEvent(eventExpr = input$N_label_face, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.face"] <<- input$N_label_face

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Fondo
    observeEvent(eventExpr = input$N_label_strokeWidth, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.strokeWidth"] <<- input$N_label_strokeWidth

        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[
              N_nodes,
              grep("font.", colnames(nodes_info))
            ]
          ))
      }
    })


    # Fondo color
    observeEvent(eventExpr = input$N_label_strokeColor, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.strokeColor"] <<- input$N_label_strokeColor
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[
              N_nodes,
              grep("font.", colnames(nodes_info))
            ]
          ))
      }
    })


    # Posición
    observeEvent(eventExpr = input$N_label_align, {
      if (!is.null(nodes)) {
        S_nodes <- Select_nodes()
        N_nodes <- which(nodes_info$id %in% S_nodes)
        nodes_info[N_nodes, "font.align"] <<- input$N_label_align
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = S_nodes,
            nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
          ))
      }
    })


    # Show shadow
    observe({
      if (isTRUE(input$N_show_shadow)) {
        if (!is.null(nodes)) {
          # S_nodes = Select_nodes()
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(
              id = nodes$id,
              shadow = list(
                enabled = input$N_show_shadow,
                size = input$N_size,
                x = input$N_shadow_x,
                y = input$N_shadow_y
              )
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateNodes(nodes = data.frame(
              id = nodes$id,
              shadow = input$N_show_shadow
            ))
        }
      }
    })


    # Edges
    observeEvent(eventExpr = input$current_edges_selection, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })

    observeEvent(eventExpr = input$Tab_edges_cell_clicked, ignoreNULL = FALSE, {
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })


    observeEvent(eventExpr = input$E_hidden, {
      if (!is.null(edges)) {
        # S_edges = select_edges( )
        S_edges <- edges$id
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(id = S_edges, hidden = input$E_hidden))
      }
    })


    # Arrow
    observeEvent(eventExpr = input$E_direction, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            arrows = ifelse(input$E_direction, "to", "NULL")
          ))
      }
    })


    # Dashed
    observeEvent(eventExpr = input$E_dashes, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            dashes = input$E_dashes
          ))
      }
    })


    # Width
    observeEvent(eventExpr = input$E_width, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "scaling.min"] <<- input$E_width
        edges_info[N_edges, "scaling.max"] <<- input$E_width
        edges_info[N_edges, "width"] <<- input$E_width

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("scaling", colnames(edges_info))]
          ))
      }
    })


    # Selected Width
    observeEvent(eventExpr = input$E_selected_width, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            selectionWidth = input$E_selected_width
          ))
      }
    })


    # Color
    observeEvent(eventExpr = input$E_color, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "color.color"] <<- input$E_color

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("color.", colnames(edges_info))]
          ))
      }
    })


    # Highlight
    observeEvent(eventExpr = input$E_color_highlight, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "color.highlight"] <<- input$E_color_highlight

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("color.", colnames(edges_info))]
          ))
      }
    })


    # Opacity
    observeEvent(eventExpr = input$E_opacity, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "color.opacity"] <<- input$E_opacity

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("color.", colnames(edges_info))]
          ))
      }
    })


    # Show labels
    observeEvent(eventExpr = input$E_label, {
      if (!is.null(edges)) {
        S_edges <- edges$id

        if (isTRUE(input$E_label)) {
          if (is.null(input$Value_decimals)) {
            lab <- edges_info$value
          } else {
            lab <- sapply(X = edges_info$value, FUN = signif, input$Value_decimals)
          }

          edges_info$label <<- sapply(X = lab, FUN = as.character)

          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(
              id = edges_info$id,
              label = edges_info$label
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(id = S_edges, label = " "))
        }
      }
    })


    # Color
    observeEvent(eventExpr = input$E_label_color, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.color"] <<- input$E_label_color

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Tamaño
    observeEvent(eventExpr = input$E_label_size, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.size"] <<- input$E_label_size

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Fuente
    observeEvent(eventExpr = input$E_label_face, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.face"] <<- input$E_label_face

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Fondo
    observeEvent(eventExpr = input$E_label_strokeWidth, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.strokeWidth"] <<- input$E_label_strokeWidth

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })


    # Fondo color
    observeEvent(eventExpr = input$E_label_strokeColor, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.strokeColor"] <<- input$E_label_strokeColor

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[N_edges, grep("font.", colnames(edges_info))]
          ))
      }
    })

    # Posición
    observeEvent(eventExpr = input$E_label_align, {
      if (!is.null(edges)) {
        S_edges <- Select_edges()
        N_edges <- which(edges_info$id %in% S_edges)
        edges_info[N_edges, "font.align"] <<- input$E_label_align

        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = S_edges,
            edges_info[
              N_edges,
              grep("font.", colnames(edges_info))
            ]
          ))
      }
    })


    ## Show shadow
    observe({
      if (!is.null(edges)) {
        if (isTRUE(input$E_show_shadow)) {
          S_edges <- Select_edges()
          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(
              id = edges$id,
              shadow = list(
                enabled = input$E_show_shadow,
                x = input$E_shadow_x,
                y = input$E_shadow_y
              )
            ))
        } else {
          visNetworkProxy("network_proxy") %>%
            visUpdateEdges(edges = data.frame(
              id = edges$id,
              shadow = input$E_show_shadow
            ))
        }
      }
    })


    # Rebotar
    observeEvent(eventExpr = input$N_physics, { # Cambia todos
      if (!is.null(edges)) {
        # S_nodes = Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes$id,
            physics = input$N_physics
          ))
      }
    })


    # Fijar eje x
    observeEvent(eventExpr = input$N_fix_x, { # Cambia todos
      if (!is.null(edges)) {
        # S_nodes = Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes$id,
            fixed = list(
              x = input$N_fix_x,
              y = input$N_fix_y
            )
          ))
      }
    })


    # Fijar eje y
    observeEvent(eventExpr = input$N_fix_y, { # Cambia todos
      if (!is.null(edges)) {
        # S_nodes = Select_nodes()
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes$id,
            fixed = list(
              x = input$N_fix_x,
              y = input$N_fix_y
            )
          ))
      }
    })


    # Selection functions

    # Selecionamos los nodos en funcion del grado
    observeEvent(eventExpr = input$Set_sel, {
      nod <- input$network_proxy_selectedNodes
      dir <- input$Select_direction
      gra <- input$Select_grade

      # Tomamos los nodos y edges
      S_nodes <- Get_all_connected(nod, dir, gra)
      S_edges <- Get_connected_edg(nod, dir)

      # Los selecciona
      visNetworkProxy("network_proxy") %>%
        visSetSelection(
          nodesId = S_nodes,
          edgesId = S_edges,
          unselectAll = TRUE,
          highlightEdges = FALSE
        )

      # Actualizamos la informaci?n
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    })

    # Filter
    # Funciones

    # Enfatizar red en base a los nodos
    Emphasize_N <<- function(S_nodes) {
      H_color <- "rgba(200,200,200,0.5)"

      S_edges <- edges[(which(edges$from %in% S_nodes & edges$to %in% S_nodes)), "id"]
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_nodes) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            color = H_color, label = ""
          )) %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            color = H_color, label = " "
          ))
      }
    }

    # Enfatizar red en base a los edgess
    Emphasize_E <<- function(S_edges) {
      H_color <- "rgba(200,200,200,0.5)"

      Sn <- edges[(which(edges$id %in% S_edges)), c("from", "to")]
      S_nodes <- unique(append(Sn$from, Sn$to))
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_edges) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            color = H_color, label = ""
          )) %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            color = H_color, label = " "
          ))
      }
    }


    ## Funciones para mostrar unicmante los nodos deseados

    Hide_N <<- function(S_nodes) {

      # if ( isTRUE(Tab_inputs$write)){
      S_edges <- edges[(which(edges$from %in% S_nodes | edges$to %in% S_nodes)), "id"]
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_edges) > 0 && length(Hide_nodes) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            hidden = TRUE
          ))
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            hidden = TRUE
          ))
      }
    }


    Hide_E <<- function(S_edges) {
      Sn <- edges[(which(edges$id %in% S_edges)), c("from", "to")]
      S_nodes <- unique(append(Sn$from, Sn$to))
      Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"]
      Hide_edges <- edges[which(!edges$id %in% S_edges), "id"]

      if (length(Hide_edges) > 0 && length(Hide_nodes) > 0) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = Hide_nodes,
            hidden = TRUE
          ))
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = Hide_edges,
            hidden = TRUE
          ))
      } # }
    }


    Get_connected_edg <- function(nod, dir) { # Tomamos los edges conectados a un nodo en una determinada dirección
      if (dir == "All") {
        edg <- edges[edges$from %in% nod | edges$to %in% nod, "id"] # Toma los los nodos unidos por edges que salen del indicado
      } else {
        if (dir == "From") {
          edg <- edges[edges$from %in% nod, "id"]
        } else {
          if (dir == "To") {
            edg <- edges[edges$to %in% nod, "id"]
          }
        }
      }
      return(unique(edg))
    }


    Get_all_connected <- function(nod, dir, gra) { # Toma todos los edges unidos en una dirección separados a un grado gra
      n <- nod
      connected_nodes <- c(n)

      for  (i in 1:gra) {
        edg <- Get_connected_edg(n, dir)
        n <- append(edges[edges$id %in% edg, "from"], edges[edges$id %in% edg, "to"])
        connected_nodes <- append(connected_nodes, unique(n))
      }
      return(connected_nodes)
    }


    Undo_N <<- function() {
      H_color <- "rgba(200,200,200,0.5)"

      if (isTRUE(input$N_label)) {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes_info$id,
            hidden = FALSE,
            # Color
            nodes_info[, grep(
              "color.",
              colnames(nodes_info)
            )],
            # Etiqueta
            label = nodes_info$label
          ))
      } else {
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes = data.frame(
            id = nodes_info$id,
            hidden = FALSE,
            # Color
            nodes_info[, grep(
              "color.",
              colnames(nodes_info)
            )]
          ))
      }
    }


    Undo_E <<- function() {
      H_color <- "rgba(200,200,200,0.5)"

      if (isTRUE(input$E_label)) {
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = edges_info$id,
            hidden = FALSE,
            # Color
            edges_info[, grep(
              "color.",
              colnames(edges_info)
            )],
            # Etiqueta
            label = edges_info$label
          ))
      } else {
        visNetworkProxy("network_proxy") %>%
          visUpdateEdges(edges = data.frame(
            id = edges_info$id,
            hidden = FALSE,
            # Color
            edges_info[, grep(
              "color.",
              colnames(edges_info)
            )]
          ))
      }
    }


    Undo <<- function() {
      if (!is.null(nodes_info)) {
        Undo_N()
        Undo_E()
      }
    }


    Get_conection_num <- reactive({
      edg <- Tab_inputs$edges
      nod <- unlist(Tab_inputs$nodes["id"])
      count <- function(nod, dir) {
        total <- sum(edg[dir] == nod)
        return(total)
      }
      f <- sapply(nod, count, "from")
      t <- sapply(nod, count, "to")
      total <- list()
      for (i in 1:length(nod)) {
        tot <- f[i] + t[i]
        total[[i]] <- tot
      }

      nodes_count <- data.frame("id" = nod, "From" = f, "To" = t, "All" = unlist(total))
      return(nodes_count)
    })


    Emphasize_group <- function(nod, dir, gra) {
      if (!is.null(gra) && gra > 0) {
        nodes <- Get_all_connected(nod, dir, gra)
        Emphasize_N(nodes)
      } else {
        nodes <- nod
        Emphasize_N(nodes)
      }
    }


    Show_group <- function(nod, dir, gra) {
      if (!is.null(gra) && gra > 0) {
        nodes <- Get_all_connected(nod, dir, gra)
        Hide_N(nodes)
      } else {
        nodes <- nod
        Hide_N(nodes)
      }
    }


    # Filtrado por grupos
    Filt <- reactiveValues(
      sel_tab_n = NULL,
      sel_tab_e = NULL,
      sel_menu = NULL,
      sel_by = NULL,
      grade = 0,
      number = 0,
      direction = "All"
    )


    output$Filter_grade <- renderUI(
      if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) {
        if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
          if (input$Filter_Menu_By == "By_sel" || (!is.null(input$Filter_by) && input$Filter_by != "All" && input$Filter_by != "None")) {
            sliderInput(
              inputId = "Filter_grade", label = "Grade",
              min = 0, max = 5, step = 1, value = Filt$grade
            )
          }
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_grade, ignoreNULL = TRUE, {
      Filt$grade <- input$Filter_grade
    })


    output$Filter_direction <- renderUI(
      if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) {
        if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
          if (input$Filter_Menu_By != "By_group" || (!is.null(input$Filter_by) && input$Filter_by != "All" && input$Filter_by != "None")) {
            radioButtons(
              inputId = "Filter_direction", label = "Direction",
              choices = c("All", "From", "To"), selected = Filt$direction,
              inline = TRUE
            )
          }
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_direction, ignoreNULL = TRUE, {
      Filt$direction <- input$Filter_direction
    })


    output$Filter_number <- renderUI(
      if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) {
        if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
          if (input$Filter_Menu_By == "By_num") {
            max <- max(Get_conection_num()[, "All"])
            sliderInput(
              inputId = "Filter_number", label = "Minimum number of conecctions",
              min = 0, max = max, step = 1, value = Filt$number
            )
          }
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_number, ignoreNULL = TRUE, {
      Filt$number <- input$Filter_number
    })


    observeEvent(eventExpr = input$Add_sel_group, {
      G_nodes <<- Select_nodes()
      shinyalert::shinyalert(
        inputId = "Alert_Add_group",
        title = "New group",
        text = "Set group name:",
        type = "input",
        inputType = "text",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })


    observeEvent(eventExpr = input$Add_input_group, {
      nodes <- str_replace_all(input$nodes_input, c("/" = ".", " " = ".", "-" = "."))
      nodes <- strsplit(nodes, ",")[[1]]
      for (i in 1:length(nodes)) {
        nodes[i] <- sub("/", ".", nodes[i])
        nodes[i] <- sub(" ", ".", nodes[i])
        nodes[i] <- str_replace(nodes[i], "-", ".")
      }
      G_nodes <<- nodes

      shinyalert::shinyalert(
        inputId = "Alert_Add_group",
        title = "New group",
        text = "Set group name:",
        type = "input",
        inputType = "text",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })


    observeEvent(eventExpr = input$Alert_Add_group, {
      if (!isFALSE(input$Alert_Add_group)) {
        Groups <<- c(list(G_nodes), Groups)

        names(Groups)[1] <<- input$Alert_Add_group

        shinyalert(
          inputId = "Correct_Add_group",
          title = input$Alert_Add_group,
          text = "Created",
          type = "success",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      } else {
        shinyalert(
          inputId = "Error_Add_group",
          text = "The group has not been created",
          type = "error",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      }
    })


    observeEvent(eventExpr = input$remove_group, ignoreNULL = TRUE, {
      shinyalert::shinyalert(
        inputId = "Alert_Remove_group",
        text = paste("Do you want to remove the group '", input$Filter_by, "' ? "),
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })


    observeEvent(eventExpr = input$Alert_Remove_group, {
      if (isTRUE(input$Alert_Remove_group)) {
        Groups <<- Groups[which(names(Groups) != input$Filter_by)]
        # Alertas
        shinyalert(
          inputId = "Correct_Remove_group",
          title = input$Filter_by,
          text = "Removed",
          type = "success",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      } else {
        shinyalert(
          inputId = "Error_Remove_group",
          text = "The group has not been removed",
          type = "error",
          closeOnClickOutside = TRUE,
          showCancelButton = TRUE,
          showConfirmButton = FALSE,
          cancelButtonText = "Close"
        )
      }
    })


    observe({
      input$Alert_Add_group
      input$Correct_Remove_group
      #
      output$Filter_by <- renderUI(
        if (!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1) {
          if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
            if (input$Filter_Menu_By == "By_group") {
              selectInput(
                inputId = "Filter_by",
                label = "Filter by",
                choices = names(Groups)
              )
            }
          }
        } else {
          NULL
        }
      )
    })


    output$Remove_group <- renderUI(
      if (!is.null(input$Filter_by) && (!is.null(input$Filter_Menu)) && !is.null(input$Filter_Menu_By)) {
        if (input$Filter_Menu_By == "By_group" && input$Filter_by != "None" && input$Filter_by != "All") {
          Filt$groups <- input$Filter_by
          actionButton(
            inputId = "remove_group",
            icon("remove"),
            style = " padding: 4px; border-radius: 2px; color: #fff; background-color: #A80215"
          )
        }
      } else {
        NULL
      }
    )


    observeEvent(eventExpr = input$Filter_Menu, ignoreNULL = FALSE, {
      if (!is.null(input$Filter_Menu)) {
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_N",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_E",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        Filt$sel_tab_e <- NULL
        Filt$sel_tab_n <- NULL
      }

      if (length(input$Filter_Menu) == 2) {
        Sel <- input$Filter_Menu[which(input$Filter_Menu != Filt$sel_menu)]
      } else {
        Sel <- input$Filter_Menu
      }
      updateCheckboxGroupInput(inputId = "Filter_Menu", selected = Sel)
      Filt$sel_menu <<- Sel
    })


    observeEvent(eventExpr = input$Filter_Menu_By, ignoreNULL = FALSE, {
      if (length(input$Filter_Menu_By) == 2) {
        Sel <- input$Filter_Menu_By[which(input$Filter_Menu_By != Filt$sel_by)]
      } else {
        Sel <- input$Filter_Menu_By
      }
      updateCheckboxGroupInput(inputId = "Filter_Menu_By", selected = Sel)
      Filt$sel_by <<- Sel
    })


    observeEvent(eventExpr = input$Filter_Tab_N, {
      if (!is.null(input$Filter_Tab_N)) {
        updateCheckboxGroupInput(
          inputId = "Filter_Menu",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_E",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        Filt$sel_tab_e <- NULL
        Filt$sel_menu <- NULL
      }

      if (length(input$Filter_Tab_N) == 2) {
        Sel <- input$Filter_Tab_N[which(input$Filter_Tab_N != Filt$sel_tab_n)]
      } else {
        Sel <- input$Filter_Tab_N
      }
      updateCheckboxGroupInput(inputId = "Filter_Tab_N", selected = Sel)
      Filt$sel_tab_n <<- Sel
    })


    observeEvent(eventExpr = input$Filter_Tab_E, {
      if (!is.null(input$Filter_Tab_E)) {
        updateCheckboxGroupInput(
          inputId = "Filter_Tab_N",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        updateCheckboxGroupInput(
          inputId = "Filter_Menu",
          choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
          selected = NULL
        )
        Filt$sel_tab_n <- NULL
        Filt$sel_menu <- NULL
      }
      if (length(input$Filter_Tab_E) == 2) {
        Sel <- input$Filter_Tab_E[which(input$Filter_Tab_E != Filt$sel_tab_e)]
      } else {
        Sel <- input$Filter_Tab_E
      }
      updateCheckboxGroupInput(inputId = "Filter_Tab_E", selected = Sel)
      Filt$sel_tab_e <<- Sel
    })


    observe({
      dir <- ifelse(is.null(input$Filter_direction), "All", input$Filter_direction)
      gra <- input$Filter_grade

      if (!is.null(Filt$sel_menu) && !is.null(Filt$sel_by)) {
        Undo()
        if (Filt$sel_by == "By_sel") {
          nod <- input$network_proxy_selectedNodes
        } else if (Filt$sel_by == "By_group") {
          nod <- unlist(Groups[input$Filter_by])
        } else if (Filt$sel_by == "By_num") {
          min <- ifelse(is.null(input$Filter_number), 0, input$Filter_number)
          nodes_count <- Get_conection_num()
          nod <- nodes_count[which(nodes_count[dir] >= min), "id"]
          gra <- 1
        }

        # Escogemos si enfatizamos o escondemos
        if (Filt$sel_menu == "Emph") { # Enfatizaremos
          Emphasize_group(nod, dir, gra)
        } else {
          Show_group(nod, dir, gra)
        }
      }
    })


    observe({
      if (is.null(input$Filter_Tab_E) && is.null(input$Filter_Tab_N) && (is.null(input$Filter_Menu) || is.null(input$Filter_Menu_By))) {
        visNetworkProxy("network_proxy") %>%
          visRedraw()
        Undo()
      }
    })


    observe({
      if (!is.null(Filt$sel_tab_n) && length(Filt$sel_tab_n) == 1) {
        Undo()
        S_nodes <- Ord_nod_tab[input$Tab_nodes_rows_all, "id"]
        if (Filt$sel_tab_n == "Emph") {
          Emphasize_N(S_nodes)
        } else {
          if (Filt$sel_tab_n == "S/H") {
            Hide_N(S_nodes)
          }
        }
      }
    })


    observe({
      if (!is.null(Filt$sel_tab_e) && length(Filt$sel_tab_e == 1)) {
        Undo()
        S_edges <- Ord_edg_tab[input$Tab_edges_rows_all, "id"]
        if (Filt$sel_tab_e == "Emph") {
          Emphasize_E(S_edges)
        } else {
          if (Filt$sel_tab_e == "S/H") {
            Hide_E(S_edges)
          }
        }
      }
    })


    # Captura
    observeEvent(eventExpr = input$Capture, {
      shinyscreenshot::screenshot(
        selector = "#network_proxy",
        filename = "network_out"
      )
    })


    # Export
    observeEvent(eventExpr = input$Save, {
      Get_nodes()
      Get_edges()
    })


    output$network_save <- renderVisNetwork({
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      if (input$Out_type == "HTML") {
        visNetwork(nodes_out, edges_out) %>%
          visNodes(
            color = list(
              background = DT$color_background,
              border = DT$color_border,
              highlight = list(
                background = DT$color_highlight,
                border = DT$color_border
              )
            ),
            physics = DT$bounce,
            shape = DT$shape
          ) %>%
          visEdges(scaling = list(
            min = DT$scaling_min,
            max = DT$scaling_max,
            label = list(
              enabled = DT$scaling_label_enabled,
              min = DT$scaling_label_min,
              max = DT$scaling_label_max
            )
          )) %>%
          visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
          visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE)
      } else {
        visNetwork(nodes_out, edges_out) %>%
          visNodes(
            color = list(
              background = DT$color_background,
              border = DT$color_border,
              highlight = list(
                background = DT$color_highlight,
                border = DT$color_border
              )
            ),
            physics = DT$bounce,
            shape = DT$shape
          ) %>%
          visEdges(scaling = list(
            min = DT$scaling_min,
            max = DT$scaling_max,
            label = list(
              enabled = DT$scaling_label_enabled,
              min = DT$scaling_label_min,
              max = DT$scaling_label_max
            )
          )) %>%
          visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
          visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE) %>%
          # Guardado en png/jpeg/pdf
          visExport(
            type = input$Out_type, name = input$Out_name, float = "right",
            style = DT$st
          )
      }
    })


    observe({
      visNetworkProxy("network_save") %>%
        visSetTitle(
          main = list(text = input$Title_main),
          submain = list(text = input$Title_submain),
          footer = list(text = input$Title_footer)
        )
    })


    output$HTML_Button <- renderUI({
      if (input$Out_type == "HTML") {
        absolutePanel(actionButton(
          inputId = "Save_HTML",
          label = "Export as HTML",
          style = "background-color: white;
                                        color: black;
                                        border: 1px solid #e7e7e7;
                                        border-radius: 6px;
                                        padding: 8px 12px;"
        ),
        right = 12, top = 1,
        fixed = FALSE
        )
      }
    })


    observeEvent(eventExpr = input$Save_HTML, {
      nodes_out <- Nodes_info()
      edges_out <- Edges_info()

      visNetwork(nodes_out, edges_out) %>%
        # Fija los parámetros por defecto de la primara representación
        visNodes(
          color = list(
            background = DT$color_background,
            border = DT$color_border,
            highlight = list(
              background = DT$color_highlight,
              border = DT$color_border
            )
          ),
          physics = DT$bounce,
          shape = DT$shape
        ) %>%
        visEdges(scaling = list(
          min = DT$scaling_min,
          max = DT$scaling_max,
          label = list(
            enabled = DT$scaling_label_enabled,
            min = DT$scaling_label_min,
            max = DT$scaling_label_max
          )
        )) %>%
        visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE) %>%
        saveWidget(file = paste0(input$Out_name, ".html"))
    })


    observeEvent(eventExpr = input$Save_HTML, {
      shinyalert(
        inputId = "Correct_Saved_HTML",
        title = paste0(input$Out_name, ".html"),
        text = "Saved",
        type = "success",
        time = 5000,
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        showConfirmButton = FALSE,
        cancelButtonText = "Close"
      )
    })


    # SVG
    ######

    'observeEvent( eventExpr = input$SVG,{
      Get_nodes()
      Get_edges()
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      gra <- igraph::graph_from_data_frame( d=edges_out, vertices = nodes_out)

      V(gra)$size <- if (is.null(nodes_out$size)) DT$size/10 else nodes_out$size/10
      V(gra)$color <- if (is.null(nodes_out$color.background)) DT$color_background else nodes_out$color.background
      V(gra)$frame.color <- if (is.null(nodes_out$color.border)) DT$color_border else nodes_out$color.border
      V(gra)$frame.width <- if (is.null(nodes_out$borderWidth)) DT$width else nodes_out$borderWidth
      V(gra)$shape <- "circle"
      V(gra)$label <- if (is.null(nodes_out$label)) NA else nodes_out$label
      V(gra)$label.cex <- if (is.null(nodes_out$font.size)) DT$font_size/12 else nodes_out$font.size/12
      V(gra)$label.color <- if (is.null(nodes_out$font.color)) DT$font_color else nodes_out$font.color

      # Edge
      E(gra)$width <- if (is.null(edges_out$scaling.min)) DT$width else edges_out$scaling.min
      E(gra)$color <- if (is.null(edges_out$color.color)) DT$width else edges_out$color.borde
      E(gra)$lty <- if (isTRUE(edges_out$dashes)) 2 else 1
      E(gra)$label <- if (is.null(edges_out$label)) NA else edges_out$label
      E(gra)$label.cex <- if (is.null(edges_out$font.size)) DT$font_size/12 else edges_out$font.size/12
      E(gra)$label.color <- if (is.null(edges_out$font.color)) DT$font_color else edges_out$font.color


      svg(width=20, height=20)
      plot(gra)
      #plot(gra, edge.arrow.size=.2,  vertex.frame.color="#555555",vertex.label.color="black",vertex.label.cex=.7,rescale=F )
      dev.off()
    })    '


    ## Manejar errores: https://es.acervolima.com/manejo-de-errores-en-la-programacion-de-r/
    ## Guardar sin el voton de guardado: https://groups.google.com/g/shiny-discuss/c/YonYRdf7IK8?pli=1
    output$Output_SVG <- downloadHandler(filename = "salida.svg", content = function(filename) {
      # Esto va con una de retraso
      Get_nodes()
      Get_edges()
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      # nodes_svg <- nodes_out[ which(nodes_out$hidden == FALSE), c("id", "label", "x", "y")]
      # edg_from <- edges_out[which(edges_out$from %in% nodes_svg$id),]
      # edges_svg <- edg_from[ which(edg_from$to %in% nodes_svg$id), c( "id","from", "to", "label")]

      # edges_svg <- edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")]

      # nrow(edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")])
      nodes_svg <- nodes_out[, c("id", "label", "x", "y")]
      nodes_out$y <- sapply(nodes_out$y, function(y) {
        0 - y
      })
      # edges_svg <- edg_from[ , c( "id","from", "to", "label")]
      edges_svg <- edges_out[, c("id", "from", "to", "label")]

      gra <- igraph::graph_from_data_frame(d = edges_out, vertices = nodes_out)

      # gra <- igraph::graph_from_data_frame( d=edges_svg, vertices = nodes_svg)

      tryCatch2(
        expr = {
          V(gra)$size <- if (is.null(nodes_out$size)) DT$size / 5 else nodes_out$size / 5
          V(gra)$color <- if (is.null(nodes_out$color.background)) DT$color_background else nodes_out$color.background
          V(gra)$frame.color <- if (is.null(nodes_out$color.border)) DT$color_border else nodes_out$color.border
          V(gra)$frame.width <- if (is.null(nodes_out$borderWidth)) DT$width else nodes_out$borderWidth
          V(gra)$shape <- "circle"
          V(gra)$label <- if (is.null(nodes_out$label)) NA else nodes_out$label
          V(gra)$label.cex <- if (is.null(nodes_out$font.size)) DT$font_size / 12 else nodes_out$font.size / 12
          V(gra)$label.color <- if (is.null(nodes_out$font.color)) DT$font_color else nodes_out$font.color
          # V(gra)$label.font <- if (is.null(nodes_out$font.face)) DT$font_face else nodes_out$font.face
          V(gra)$label.dist <- -1 # hacemos que la etiqueta se coloque debajo del nodo

          # Edge
          E(gra)$width <- if (is.null(edges_out$scaling.min)) DT$width else edges_out$scaling.min
          E(gra)$arrow.size <- if (is.null(edges_out$scaling.min)) DT$width / 2 else edges_out$scaling.min / 2
          E(gra)$color <- if (is.null(edges_out$color.color)) DT$color_border else edges_out$color.color
          E(gra)$lty <- if (isTRUE(edges_out$dashes)) 2 else 1
          E(gra)$label <- if (is.null(edges_out$label)) NA else edges_out$label
          # E(gra)$label.font <- if (is.null(edges_out$font.face)) DT$font_face else edges_out$font.face
          E(gra)$label.cex <- if (is.null(edges_out$font.size)) DT$font_size / 12 else edges_out$font.size / 12
          E(gra)$label.color <- if (is.null(edges_out$font.color)) DT$font_color else edges_out$font.color
        }
      )
      # View(edges_out)
      # View(DT)

      svg(
        filename = filename,
        width = 20, height = 20
      )
      plot(gra)
      # plot(gra, edge.arrow.size=.2,  vertex.frame.color="#555555",vertex.label.color="black",vertex.label.cex=.7,rescale=F )
      dev.off()
    }, contentType = NA, outputArgs = list())

    '    observeEvent( eventExpr = input$SVG,{
      Get_nodes()
      Get_edges()
      nodes_out <<- Nodes_info()
      edges_out <<- Edges_info()

      nodes_svg <- nodes_out[ which(nodes_out$hidden == FALSE), c("id", "label", "x", "y")]
      edg_from <- edges_out[which(edges_out$from %in% nodes_svg$id),]
      edges_svg <- edg_from[ which(edg_from$to %in% nodes_svg$id), c( "id","from", "to", "label")]

      edges_svg <- edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")]

      nrow(edges_out[ which(edges_out$hidden == FALSE), c( "id","from", "to", "label")])
      nodes_svg <- nodes_out[, c("id", "label", "x", "y")]

      #gra <- igraph::graph_from_data_frame( d=edges_out, vertices = nodes_out)

      gra <- igraph::graph_from_data_frame( d=edges_svg, vertices = nodes_svg)

      V(gra)$size <- if (is.null(nodes_out$size)) DT$size/5 else nodes_out$size/5
      V(gra)$color <- if (is.null(nodes_out$color.background)) DT$color_background else nodes_out$color.background
      V(gra)$frame.color <- if (is.null(nodes_out$color.border)) DT$color_border else nodes_out$color.border
      V(gra)$frame.width <- if (is.null(nodes_out$borderWidth)) DT$width else nodes_out$borderWidth
      V(gra)$shape <- "circle"
      V(gra)$label <- if (is.null(nodes_out$label)) NA else nodes_out$label
      V(gra)$label.cex <- if (is.null(nodes_out$font.size)) DT$font_size/12 else nodes_out$font.size/12
      V(gra)$label.color <- if (is.null(nodes_out$font.color)) DT$font_color else nodes_out$font.color
      #V(gra)$label.font <- if (is.null(nodes_out$font.face)) DT$font_face else nodes_out$font.face
      V(gra)$label.dist <- -1 # hacemos que la etiqueta se coloque debajo del nodo

      # Edge
      E(gra)$width <- if (is.null(edges_out$scaling.min)) DT$width else edges_out$scaling.min
      E(gra)$arrow.size <- if (is.null(edges_out$scaling.min)) DT$width/2 else edges_out$scaling.min/2
      E(gra)$color <- if (is.null(edges_out$color.color)) DT$color_border else edges_out$color.color
      E(gra)$lty <- if (isTRUE(edges_out$dashes)) 2 else 1
      E(gra)$label <- if (is.null(edges_out$label)) NA else edges_out$label
      #E(gra)$label.font <- if (is.null(edges_out$font.face)) DT$font_face else edges_out$font.face
      E(gra)$label.cex <- if (is.null(edges_out$font.size)) DT$font_size/12 else edges_out$font.size/12
      E(gra)$label.color <- if (is.null(edges_out$font.color)) DT$font_color else edges_out$font.color
      #View(edges_out)
      #View(DT)

      svg(width=20, height=20)
      plot(gra)
      #plot(gra, edge.arrow.size=.2,  vertex.frame.color="#555555",vertex.label.color="black",vertex.label.cex=.7,rescale=F )
      dev.off()
    })
    '
    ######


    # #


    width <- input$width
    height <- input$height

    output$save_relationships <- downloadHandler(filename = "network_relationships.zip", content = function(fname) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      # # ###
      nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
      nodes <- strsplit(nodes, ",")[[1]]
      subgr <- bnlearn::subgraph(fittedbn, nodes)
      fileName <- "network_relationships.txt"
      sink(fileName)
      print(arcs(subgr), quote = FALSE)
      sink()

      subgr.igraph <- as.igraph(subgr)
      fileName_igraph <- "network_relationships_igraph.dot"
      write_graph(subgr.igraph, fileName_igraph, format = "dot")

      files <- c(fileName, fileName_igraph)

      zip::zip(fname, files)
    }, contentType = "application/zip")
  })














# if (class(predict)[1] == "try-error") {
            #   df[i,2] <- as.character("Cannot be calculated")
            # } else {
            #   no_na <- !is.na(predict)
            #   pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
            #   if (!is.nan(pred_val)) {
            #     vector <- c(as.numeric(pred_val))
            #     if (it != 1) {
            #       for (v in 2:it) {
            #         predict <- cpdist(fittedbn, nodes = taxas[i], evidence = ev, method = "lw", n = 100000)
            #         no_na <- !is.na(predict)
            #         pred_val <- sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na])
            #         if (!is.nan(pred_val)) {
            #           vector <- c(vector, as.numeric(pred_val))
            #         }
            #       }
            #       pred_value_log <- median(vector)
            #       if (expm1(pred_value_log) >= 0) {
            #         df[i,2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
            #       } else {
            #         df[i,2] <- 0
            #       }

            #     } else {
            #       pred_value_log <- pred_val
            #       if (expm1(pred_value_log) >= 0) {
            #         df[i,2] <- round(as.numeric(expm1(pred_value_log)), digits = 2)
            #       } else {
            #         df[i,2] <- 0
            #       }
            #     }

            #   } else {
            #     df[i,2] <- as.character("NaN")
            #   }
            # }

            # if (class(total_raw_counts) == "numeric") {
            #   media <- mean(bn_df_raw_filt_taxas[[taxas[i]]])
            #   df[i,3] <- round(media, digits = 0)
            # } else {
            #   df[i,3] <- as.character("Cannot be calculated")
            # }

            # if (class(total_raw_counts) == "numeric") {
            #   desves <- sd(bn_df_raw_filt_taxas[[taxas[i]]])
            #   df[i,4] <- round(desves, digits = 2)
            # } else {
            #   df[i,4] <- as.character("Cannot be calculated")
            # }

            # df[i,2] <- as.numeric(sum(predict[no_na, 1] * attr(predict, "weights")[no_na]) / sum(attr(predict, "weights")[no_na]))
            # filt_data <- bn_df_norm
            # for (v in 1:length(ev)) {
            #   filt_data <- filt_data[filt_data[[names(ev[v])]] == ev[[v]][1], ]
            # }

            # average_norm <- round(mean(expm1(filt_data[[taxas[i]]])), digits = 2)
            # desvest_norm <- round(sd(expm1(filt_data[[taxas[i]]])), digits = 2)

            # if ((average_norm - desvest_norm) < 0) {
            #   low_range <- 0
            # } else {
            #   low_range <- average_norm - desvest_norm
            # }

            # high_range <- average_norm + desvest_norm

            # df[i,5] <- paste(low_range, "-", high_range)

            # if ((as.numeric(df[i,3]) == 0) & (as.numeric(df[i,4]) == 0)) {
            #   df[i,2] <- as.character(0)
            # }

            # if ((df[i,2] != "Cannot be calculated") & (df[i,2] != "NaN")) {
            #   event_cpquery = paste("((", taxas[i], ">= ", (as.numeric(pred_value_log)-error_cp*as.numeric(pred_value_log)), ") & (", taxas[i], "<= ", (as.numeric(pred_value_log)+error_cp*as.numeric(pred_value_log)), "))", sep = "")
            #   result_cpquery <- try(eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', evidence_cpquery, ', method = "ls")'))))
            #   #result_cpquery <- eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', list(ev), ', method = "lw")'))) ## lw method
            #   if (!is.nan(result_cpquery)) {
            #     vector2 <- c(as.numeric(result_cpquery))
            #     if (it != 1) {
            #       for (v in 2:it) {
            #         result_cpquery <- try(eval(parse(text=paste('cpquery(fittedbn, event = ', event_cpquery, ', evidence = ', evidence_cpquery, ', method = "ls")'))))
            #         if (!is.nan(result_cpquery)) {
            #           vector2 <- c(vector2, as.numeric(result_cpquery))
            #         }
            #       }
            #       df[i,6] <- round(as.numeric(median(vector2)), digits = 2)
            #     } else {
            #       df[i,6] <- round(as.numeric(result_cpquery), digits = 2)
            #     }

            #   } else {
            #     df[i,6] <- "NaN"
            #   }
            # } else {
            #   if (df[i,2] == "Cannot be calculated") {
            #     df[i,6] <- "Cannot be calculated"
            #   } else {
            #     df[i,6] <- "NaN"
            #   }
            # }