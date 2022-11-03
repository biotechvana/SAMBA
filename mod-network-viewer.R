dropdownBlock_Rox <- function (..., id, icon = NULL, title = NULL, badgeStatus = "danger") 
{
  if (!is.null(badgeStatus)) 
    #validateStatus(badgeStatus)
  items <- c(list(...))
  dropdownClass <- paste0("dropdown")
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- dashboardLabel(status = badgeStatus)
  }
  shiny::tags$li(shiny::singleton(shiny::tags$head(shiny::tags$script(shiny::HTML(paste0("$(document).ready(function(){\n                $('#", 
                                                                                         id, "').find('ul').click(function(e){\n                  e.stopPropagation();\n                });\n              });\n              "))))), 
                 class = dropdownClass, id = id, shiny::tags$a(href = "#", 
                                                               class = "dropdown-toggle", `data-toggle` = "dropdown", 
                                                               icon, title, badge), shiny::tags$ul(class = "dropdown-menu", 
                                                                                                   style = "left: 0; right: auto;", shiny::tags$li(shiny::tags$ul(class = "menu", 
                                                                                                                                                                  shiny::tags$div(style = "margin-left: auto; margin-right: auto; width: 80%;", 
                                                                                                                                                                                  items)))))
}

js_fs <- "
function openFullscreen(elem) {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}

function closeFullscreen(elem) {
  if (document.exitFullscreen) {
    document.exitFullscreen();
  } else if (elem.mozExitFullscreen) { /* Firefox */
    document.mozExitFullScreen();
  } else if (elem.webkitExitFullscreen) { /* Chrome, Safari and Opera */
    document.webkitExitFullscreen();
  } else if (elem.msExitFullscreen) { /* IE/Edge */
    document.msExitFullscreen();
  }
}
"

css_fs <- "
#network_viewer_mod-network_proxy:-webkit-full-screen {
  height: 100%;
  margin: 0;
}
#network_viewer_mod-network_proxy:-ms-fullscreen {
  height: 100%;
}
#network_viewer_mod-network_proxy:fullscreen {
  height: 100%;
}

#mynetwork {
  width: 600px;
  height: 200px;
} 

/* The Modal (background) */
.modal {
  display: none; /* Hidden by default */
  position: fixed; /* Stay in place */
  justify-content: center;
  z-index: 1; /* Sit on top */
  padding-top: 300px; /* Location of the box */  
  left: 0;
  top: 0;
  width: 100%; /* Full width */
  height: 100%; /* Full height */
  overflow: auto; /* Enable scroll if needed */
  background-color: rgb(0,0,0); /* Fallback color */
  background-color: rgba(0,0,0,0.4); /* Black w/ opacity */
  
}

/* Modal Content */
.modal-content {
  margin: auto;
  width: 500px;
  display: flex;
  flex-direction: column;
  align-items: center;
  background-color: #fff;
  color: #000;
  text-align: center;
  border-radius: 20px;
  padding: 30px 30px 70px;

}

/* The Close Button */
.close { 
    width: 30px;
    font-size: 20px;
    color: #c0c5cb;
    align-self: flex-end;
    background-color: transparent;
    border: none;
    margin-bottom: 10px; 
    
    
}

.close:hover,
.close:focus {
  color: #ed6755;
  text-decoration: none;
  cursor: pointer;
}
" 

css_modal = 
  'body {font-family: Arial, Helvetica, sans-serif;}

/* The Modal (background) */
.modal {
  display: none; /* Hidden by default */
  position: fixed; /* Stay in place */
  justify-content: center;
  z-index: 1; /* Sit on top */
  padding-top: 300px; /* Location of the box */  
  left: 0;
  top: 0;
  width: 100%; /* Full width */
  height: 100%; /* Full height */
  overflow: auto; /* Enable scroll if needed */
  background-color: rgb(0,0,0); /* Fallback color */
  background-color: rgba(0,0,0,0.4); /* Black w/ opacity */
  
}

/* Modal Content */
.modal-content {
  margin: auto;
  width: 500px;
  display: flex;
  flex-direction: column;
  align-items: center;
  background-color: #fff;
  color: #000;
  text-align: center;
  border-radius: 20px;
  padding: 30px 30px 70px;

}

/* The Close Button */
.close { 
    width: 30px;
    font-size: 20px;
    color: #c0c5cb;
    align-self: flex-end;
    background-color: transparent;
    border: none;
    margin-bottom: 10px; 
    
    
}

.close:hover,
.close:focus {
  color: #ed6755;
  text-decoration: none;
  cursor: pointer;
}

/* The Acept Button */
.accept {
    background-color: #ed6755;
    border: none;
    border-radius: 5px;
    width: 100px;
    padding: 14px;
    font-size: 16px;
    color: white;
    box-shadow: 0px 6px 18px -5px rgba(237, 103, 85, 1);
  }
  
.accept:hover{ 
  background: #27ae62;
}

.accept:active {
  transform: translateY(-1px);
    box-shadow: 0 5px 10px rgba(0, 0, 0, 0.2);
} 

 
input[type=text], select {
  width: 100%;
  padding: 12px 20px;
  margin: 8px 0;
  display: inline-block;
  border: 1px solid #ccc;
  border-radius: 4px;
  box-sizing: border-box;
}'

html_modal='<!-- The Label edit Modal -->
<div id="LabelModal" class="modal">

  <!-- Modal content -->
  <div class="modal-content">
    <span id="close1" class="close">&times;</span>
    <h2 id="title1">TITLE</h2>
    <div id="content1" >CONTENT</div>
    <br>
    <input type="text" id="name1" name="name" class="form__input" value="Nodo"  > 
    <br>
    <button id="aceptModel1" class="accept">Save</button>
  </div>

</div>

<!-- The Group edit Modal -->
<div id="GroupModal" class="modal">

  <!-- Modal content -->
  <div class="modal-content">
    <span id="close2" class="close">&times;</span>
    <h2 id="title2">TITLE</h2>
    <div id="content2" >CONTENT</div>
    <br>
    <input type="text" id="name2" name="name" class="form__input" value="Nodo"  > 
    <br>
    <button id="aceptModel2" class="accept">Save</button>
  </div>

</div>

<!-- The Group edit Modal -->
<div id="RemoveModal" class="modal">

  <!-- Modal content -->
  <div class="modal-content">
    <span id="close3" class="close">&times;</span>
    <h2 id="title3">TITLE</h2> 
    <br>
    <button id="aceptModel3" class="accept">Remove</button>
  </div>

</div>


'



# #
DT <- list( 
  size = 25, 
  width = 1,
  sel_width = 2,
  dashes = FALSE,
  direction = TRUE,
  bounce = FALSE,
  E_hidden = FALSE,
  opacity = 1,
  
  fix_x = FALSE,
  fix_y = FALSE,
  # Scaling
  scaling_min = 1,
  scaling_max = 1,
  scaling_label_enabled = FALSE,
  scaling_label_min = 14,
  scaling_label_max = 14,
  
  # Color
  color_background = "#84B8BD",
  color_border = "#616161",
  color_highlight = "#177782",
  shape = "dot", 
  
  # Font
  show_Nlabel = TRUE,
  show_Elabel = FALSE,
  font_color = "#343434",
  font_size = 14,
  font_face = "arial",
  font_background = NULL,
  font_strokeWidth = 1,
  font_strokeColor = "#ffffff",
  N_font_align = "center",
  E_font_align = "horizontal",
  
  # Shadow
  shadow_enabled = FALSE,
  shadow_color = "#FFFFFF",
  shadow_size = 10,
  shadow_x = 5,
  shadow_y = 5,
  st = " position: absolute; 
  top: 25px;
  text-align: center;
  background-color: white;
  color: black;
  border: 1px solid #e7e7e7;
  border-radius: 6px;
  padding: 8px 12px;
  ",
  
  # Otros
  # Diseño UI
  slider_color = "#95979A",
  digits = 3,
  # Grado para la seleccion
  grade = 0)

Ord_nod_tab <- NULL
Ord_edg_tab <- NULL

nodes <- NULL
edges <- NULL

#Rep <- 0
Sel_nod <- NULL
nodes_info <- NULL
edges_info <- NULL


network_viewer_ui <- function(id = "network_viewer_mod") {
    ns <- NS(id)

    graph_page <- shinydashboardPlus::dashboardPage(
        skin = "black",
        dashboardHeader(
            title = " ",
            leftUi = tagList(
                dropdownBlock_Rox(
                    id = ns("Edit_dropdown"),
                    title = "Label editing",
                    icon = icon(
                        name = "tags",
                        lib = "glyphicon"
                    ),
                    badgeStatus = "primary",
                    div(
                        br(),
                        h4("Label editing"),
                        hr(),
                        prettySwitch(
                            inputId = ns("Enable_edition"),
                            label = "Enable label editing.",
                            fill = TRUE,
                            value = FALSE
                        ),
                        shiny::conditionalPanel(
                            condition = "input.Enable_edition==true",
                            ns = ns,
                            p("Double click on a node to edit the label.")
                        )
                    )
                )
            ),
            # disable = TRUE,
            # Save:
            # tags$li(class = "dropdown", downloadLink(outputId = "Output_SVG", label = " ",
            #                                         style =  "background-color: transparent; border-color: transparent; font-size: 24px;")),

            tags$li(class = "dropdown", 
                actionButton(
                    inputId = ns("graph_refresh"), label = "",
                    icon = icon("refresh", lib = "glyphicon"),
                    style = "background-color: transparent;border-color: transparent;font-size: 24px;"
            )),
            tags$li(class = "dropdown", 
                actionButton(
                    inputId = ns("Capture"), label = "",
                    icon = icon("camera", lib = "glyphicon"),
                    style = "background-color:transparent;border-color: transparent;font-size: 24px;"
            )),
            tags$li(class = "dropdown", 
                actionButton(
                    inputId = ns("Save"), label = "",
                    icon = icon("save", lib = "glyphicon"),
                    style = "background-color:transparent;border-color: transparent;font-size: 24px;"
            )),
            tags$li(class = "dropdown", 
                actionButton(
                    inputId = ns("Fullscreen"), label = "",
                    icon = icon("fullscreen", lib = "glyphicon"),
                    style ="background-color: transparent; border-color: transparent;font-size: 24px;",
                # Tomamos la informacion de HTML para poner pantalla completa https://stackoverflow.com/questions/61128930/button-to-view-in-full-screen
                # onclick = "openFullscreen(document.getElementById('graphContainer'));" # Abrimos en pantalla completa
            ))
        ),
        dashboardSidebar(
            id = ns("sidebar"),
            collapsed = TRUE,
            minified = TRUE,

            # Scrollbar
            # https://stackoverflow.com/questions/7347532/how-to-position-a-div-scrollbar-on-the-left-hand-side
            # tags$style(
            #  "#sidebarItemExpanded {
            #    direction: rtl;
            #    overflow-y: auto;
            #    overflow-x: hidden !important;
            #    height: calc(100vh - 50px) !important;
            #  }
            # sidebarItemExpanded div{
            #    direction:ltr;
            #    }"
            #  ),
            # Edit menue

            # ·························································#
            #                                                         #
            #              Panel de edición de la red                 #
            #                                                         #
            # ·························································#
            div(
                id = ns("Edit_menu"),
                # Pestaña Edit: edición de nodos y links
                sidebarMenu(
                    id = ns("Edit"),
                    fluidRow(
                        column(10,
                            offset = 2,
                            tags$h3(icon("edit"), HTML("&nbsp;"), "Edit:")
                        )
                    ),


                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                    #                                        #
                    #                Nodos                   #
                    #                                        #
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


                    menuItem("Nodes",
                        tabName = "Nodes",
                        icon = icon("chevron-right"),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #        Tamaño y forma        #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        menuItem("Shape and size",
                            icon = icon("resize-full", lib = "glyphicon"),
                            ## Forma :
                            fixedRow(
                                column(3, br(), tags$p("Shape")),
                                column(9, selectInput(
                                    inputId = ns("N_shape"),
                                    # label = "Nodes shape",
                                    label = NULL,
                                    selected = DT$shape,
                                    choices = c(
                                        "box", "database", "ellipse", "diamond", "dot", "hexagon",
                                        "square", "star", "text", "triangle", "triangleDown"
                                    )
                                ))
                            ),
                            # HTML("<hr>"),
                            ## Autosize
                            checkboxInput(
                                inputId = ns("N_autosize"),
                                label = "Auto size",
                                value = FALSE
                            ),
                            conditionalPanel(
                                condition = "input.N_autosize == true",
                                ns = ns,
                                radioButtons(
                                    inputId = ns("N_autosize_type"), label = "Set nodes size acording to the number of:",
                                    choices = list("All connected edges" = "C_E", "Edges from the node" = "E_F", "Edges to the node" = "E_T"),
                                    selected = "C_E"
                                ),
                                sliderInput(
                                    inputId = ns("N_size_range"), "Size range:",
                                    min = 0, max = 100, value = c(10, 50)
                                )
                            ),
                            # HTML("<hr>"),
                            ## Tamaño
                            conditionalPanel(
                                condition = "input.N_autosize == false",
                                ns = ns,
                                sliderInput(
                                    inputId = ns("N_size"), label = "Size",
                                    min = 10, max = 100, value = DT$size
                                )
                            ),
                            sliderInput(
                                inputId = ns("N_borderWidth"), label = "Border Width",
                                min = 0, max = 10, value = DT$width
                            ),
                            sliderInput(
                                inputId = ns("N_borderWidthSelected"), label = "Selected Border Width",
                                min = 0, max = 10, value = DT$sel_width
                            ),
                            br()
                        ),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #            Color             #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        ## Color de los nodos
                        menuItem("Color",
                            tabName = "Nodes_color",
                            icon = icon("tint"),
                            tags$h5("Nodes color"),
                            fixedRow(
                                column(4, br(), tags$p("Fill")),
                                column(8, colourInput(
                                    inputId = ns("N_color_background"),
                                    label = NULL, value = DT$color_background
                                ))
                            ),
                            fixedRow(
                                column(4, br(), tags$p("Border")),
                                column(8, colourInput(
                                    inputId = ns("N_color_border"),
                                    label = NULL, value = DT$color_border
                                ))
                            ),
                            # HTML("<hr>"),

                            ## Color selección
                            tags$h5("Selected nodes color"),
                            fixedRow(
                                column(4, br(), tags$p("Fill")),
                                column(8, colourInput(
                                    inputId = ns("N_color_highlight_background"),
                                    label = NULL, value = DT$color_highlight
                                ))
                            ),
                            fixedRow(
                                column(4, br(), tags$p("Border")),
                                column(8, colourInput(
                                    inputId = ns("N_color_highlight_border"),
                                    label = NULL, value = DT$color_border
                                ))
                            ),
                            # HTML("<hr>"),
                            ## Opacidad
                            sliderInput(
                                inputId = ns("N_opacity"), label = "Opacity",
                                min = 0, max = 1, value = DT$opacity
                            ),
                            br()
                        ),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #           Etiquetas          #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        menuItem("Labels",
                            tabName = "N_labels",
                            icon = icon("tags"),
                            # Show/Hide labels
                            checkboxInput(
                                inputId = ns("N_label"),
                                label = "Show labels",
                                value = DT$show_Nlabel
                            ),
                            # Color
                            fixedRow(
                                column(4, br(), tags$p("Color")),
                                column(
                                    8,
                                    colourInput(
                                        inputId = ns("N_label_color"),
                                        label = NULL, value = DT$font_color
                                    )
                                )
                            ),
                            # Tamaño
                            sliderInput(
                                inputId = ns("N_label_size"),
                                label = "Size",
                                min = 1, max = 100,
                                value = DT$font_size
                            ),
                            # Fuente
                            fixedRow(
                                column(4, br(), tags$p("Font")),
                                column(
                                    8,
                                    selectInput(
                                        inputId = ns("N_label_face"),
                                        # label = "Font",
                                        label = NULL,
                                        selected = DT$font_face,
                                        choices = c(
                                            "Arial", "Arial Black",
                                            "Calibri", "Century Gothic",
                                            "Comic Sanz", "Courier New",
                                            "Impact", "Lucida Sans",
                                            "Times New Roman"
                                        )
                                    )
                                )
                            ),
                            # background

                            # Fondo
                            sliderInput(
                                inputId = ns("N_label_strokeWidth"), label = "Background width",
                                min = 0, max = 100, value = DT$font_strokeWidth
                            ),
                            # Fondo color
                            fixedRow(
                                column(4, tags$p("Background"), tags$p("  color")),
                                column(
                                    8,
                                    colourInput(
                                        inputId = ns("N_label_strokeColor"),
                                        label = NULL, value = DT$font_strokeColor
                                    )
                                )
                            ),

                            # Align
                            fixedRow(
                                column(4, br(), tags$p("Position")),
                                column(
                                    8,
                                    selectInput(
                                        inputId = ns("N_label_align"),
                                        label = NULL,
                                        # label = "Position",
                                        selected = DT$N_font_align,
                                        choices = c("center", "left")
                                    )
                                )
                            ),
                            br()
                        ),
                        menuItem("Shadow",
                            tabName = "N_shadow",
                            icon = icon("adjust"),
                            fluidRow(
                                checkboxInput(
                                    inputId = ns("N_show_shadow"),
                                    label = "Show shadow",
                                    value = DT$shadow_enabled
                                ),
                                shiny::conditionalPanel(
                                    condition = "input.N_show_shadow==true",
                                    ns = ns,
                                    sliderInput(
                                        inputId = ns("N_shadow_x"), label = "X",
                                        min = -25, max = 25, value = DT$shadow_x
                                    ),
                                    sliderInput(
                                        inputId = ns("N_shadow_y"), label = "Y",
                                        min = -25, max = 25, value = DT$shadow_y
                                    )
                                )
                            ),
                            br()
                        )
                    ),


                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                    #                                        #
                    #                Edges                   #
                    #                                        #
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


                    menuItem("Edges",
                        tabName = "Edges",
                        icon = icon("chevron-right"),
                        checkboxInput(
                            inputId = ns("E_hidden"),
                            label = "Hide  all links",
                            value = DT$E_hidden
                        ),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #        Tamaño y forma        #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        menuItem("Shape and size",
                            icon = icon("resize-full", lib = "glyphicon"),
                            checkboxInput(
                                inputId = ns("E_direction"),
                                label = "Directional",
                                value = DT$direction
                            ),
                            checkboxInput(
                                inputId = ns("E_dashes"),
                                label = "Dashed line",
                                value = DT$dashes
                            ),
                            sliderInput(
                                inputId = ns("E_width"),
                                label = "Width",
                                min = 0,
                                max = 25,
                                value = DT$width
                            ),
                            sliderInput(
                                inputId = ns("E_selected_width"),
                                label = "Selected width",
                                min = 0,
                                max = 25,
                                value = DT$sel_width
                            ),
                            br()
                        ),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #            Color             #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        menuItem("Color",
                            tabName = "Edges_color",
                            icon = icon("tint"),
                            tags$h5("Edges color"),
                            # Color
                            fixedRow(
                                column(4, br(), tags$p("Color")),
                                column(8, colourInput(
                                    inputId = ns("E_color"),
                                    label = NULL, value = DT$color_border
                                ))
                            ),
                            fixedRow(
                                column(4, br(), tags$p("Selected color")),
                                column(8, colourInput(
                                    inputId = ns("E_color_highlight"),
                                    label = NULL, value = DT$color_border
                                ))
                            ),
                            ## Opacidad
                            sliderInput(
                                inputId = ns("E_opacity"),
                                label = "Opacity",
                                min = 0,
                                max = 1,
                                value = DT$opacity
                            ),
                            br()
                        ),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #           Etiquetas          #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        # Labels
                        menuItem("Labels",
                            tabName = ns("E_labels"),
                            icon = icon("tags"),
                            # Show/Hide labels
                            checkboxInput(
                                inputId = ns("E_label"),
                                label = "Show labels",
                                value = DT$show_Elabel
                            ),
                            # Color
                            fixedRow(
                                column(4, br(), tags$p("Color")),
                                column(
                                    8,
                                    colourInput(
                                        inputId = ns("E_label_color"),
                                        label = NULL,
                                        value = DT$font_color
                                    )
                                )
                            ),
                            # Tamaño
                            sliderInput(
                                inputId = ns("E_label_size"), label = "Size",
                                min = 1, max = 30, value = DT$font_size,
                            ),
                            # Fuente
                            fixedRow(
                                column(4, br(), tags$p("Font")),
                                column(
                                    8,
                                    selectInput(
                                        inputId = ns("E_label_face"),
                                        # label = "Font",
                                        label = NULL,
                                        selected = DT$font_face,
                                        choices = c(
                                            "Arial", "Arial Black", "Calibri",
                                            "Century Gothic", "Comic Sanz",
                                            "Courier New", "Impact",
                                            "Lucida Sans", "Times New Roman"
                                        )
                                    )
                                )
                            ),
                            # background

                            # Fondo
                            sliderInput(
                                inputId = ns("E_label_strokeWidth"), label = "Background",
                                min = 0, max = 100, value = DT$font_strokeWidth
                            ),
                            # Fondo color
                            fixedRow(
                                column(4, br(), tags$p("Background"), tags$p("  color")),
                                column(
                                    8,
                                    colourInput(
                                        inputId = ns("E_label_strokeColor"),
                                        label = NULL,
                                        value = DT$font_strokeColor
                                    )
                                )
                            ),

                            # Align
                            fixedRow(
                                column(4, br(), tags$p("Position")),
                                column(
                                    8,
                                    selectInput(
                                        inputId = ns("E_label_align"),
                                        # label = "Position",
                                        label = NULL,
                                        selected = DT$E_font_align,
                                        choices = c(
                                            "horizontal", "top",
                                            "middle", "bottom"
                                        )
                                    )
                                )
                            ),
                            br()
                        ),


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                        #            Sombra            #
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

                        menuItem("Shadow",
                            tabName = "E_shadow",
                            icon = icon("adjust"),
                            fluidRow(
                                checkboxInput(
                                    inputId = ns("E_show_shadow"),
                                    label = "Show shadow",
                                    value = DT$shadow_enabled
                                ),
                                shiny::conditionalPanel(
                                    condition = "input.E_show_shadow==true",
                                    ns = ns,
                                    sliderInput(
                                        inputId = ns("E_shadow_x"), label = "X",
                                        min = -25, max = 25, value = DT$shadow_x
                                    ),
                                    sliderInput(
                                        inputId = ns("E_shadow_y"), label = "Y",
                                        min = -25, max = 25, value = DT$shadow_y
                                    )
                                )
                            )
                        )
                    ),


                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                    #                                        #
                    #         Parámetros físicos             #
                    #                                        #
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


                    menuItem("Physics",
                        tabName = "Physics",
                        icon = icon("chevron-right"),
                        # Estatico
                        checkboxInput(
                            inputId = ns("N_physics"),
                            label = "Bounce", ### Cambiar nombre
                            value = DT$bounce
                        ),
                        # Fijar eje X
                        checkboxInput(
                            inputId = ns("N_fix_x"),
                            label = "Fix X", ### Cambiar nombre
                            value = DT$fix_x
                        ),
                        # Fijar eje y
                        checkboxInput(
                            inputId = ns("N_fix_y"),
                            label = "Fix Y", ### Cambiar nombre
                            value = DT$fix_y
                        ),
                        br()
                    )
                ),

                ######
                # Cambio de seleccion

                # pestania de seleccion : filtrado de la red:
                sidebarMenu(
                    id = ns("Menu_selecion"),
                    fluidRow(
                        column(10,
                            offset = 2,
                            tags$h3(icon("pushpin", lib = "glyphicon"), HTML("&nbsp;"), "Select:")
                        )
                    ),
                    menuItem("Select menu",
                        tabName = "Select",
                        icon = icon("chevron-right"),
                        radioButtons(
                            inputId = ns("Select_direction"), label = "Direction",
                            choices = c("All" = "All", "Parents" = "to", "Children" = "from"),
                            selected = "All",
                            inline = TRUE
                        ),
                        sliderInput(
                            inputId = ns("Select_grade"), label = "Increase selection by grade",
                            min = 1, max = 5, value = DT$grade
                        ),
                        fluidRow(column(4,
                            offset = 1, br(),
                            tags$body(" Select the grade and the "), br(),
                            tags$body("direction and click 'Set selection'"), br(),
                            tags$body(" to fix the selection."), style = "font-size:12px;"
                        )), fluidRow(column(12,
                            align = "center",
                            div(
                                class = "buttonagency", style = "vertical-align: middle;",
                                actionBttn(
                                    inputId = ns("Set_sel"), label = "Set selection",
                                    style = "float", color = "primary", size = "sm"
                                )
                            )
                        )),
                        br()
                        # Selecionamos todos los nodos filtrados de la tabla
                        # actionButton(inputId = "Tab_nod_sel", label = "Select all table nodes"),
                        # Seleccionamos todos los nodos conectados por los edges visibles en la tabla
                        # actionButton(inputId = "Tab_edg_sel", label = "No se que poner")
                    ),
                    br()
                ),
                ######

                # ·························································#
                #                                                         #
                #        Filtrado en base a criterios de usuario          #
                #                                                         #
                # ·························································#


                # Pestaña Filter: filtrado de la red:
                sidebarMenu(
                    id = ns("Menu_filtrado"),
                    fluidRow(
                        column(10,
                            offset = 2,
                            tags$h3(icon("tasks"), HTML("&nbsp;"), "Filter:")
                        )
                    ),
                    menuItem("Filter menu",
                        tabName = "Filter",
                        icon = icon("chevron-right"),
                        fluidRow(
                            column(width = 5, checkboxGroupInput(
                                inputId = ns("Filter_Menu"), label = "Filter",
                                choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H")
                            )),
                            column(width = 6, checkboxGroupInput(
                                inputId = ns("Filter_Menu_By"), label = "By",
                                choices = list("Selected" = "By_sel", "Group" = "By_group", "Number of interactions" = "By_num")
                            ))
                        ),
                        fixedRow(
                            column(width = 9, uiOutput(outputId = ns("Filter_by"))),
                            column(width = 1, absolutePanel(uiOutput(ns("Remove_group")), right = 8, top = 32))
                        ),
                        # fluidRow(column(4, offset = 1,
                        uiOutput(outputId = ns("Filter_grade")),
                        uiOutput(outputId = ns("Filter_number")),
                        uiOutput(
                            outputId = ns("Filter_direction") # ))
                        ),
                        fluidRow(column(4,
                            align = "center",
                            offset = 1, br(),
                            tags$body(" Select a group of nodes"), br(),
                            tags$body(" and click 'Add group'"), br(),
                            tags$body(" to define a new group."), style = "font-size:12px;"
                        )),
                        fluidRow(column(12,
                            align = "center",
                            div(
                                class = "buttonagency", style = "vertical-align: middle;",
                                actionBttn(inputId = ns("Add_sel_group"), label = "Add group", icon = icon("plus"), style = "float", color = "primary", size = "sm")
                            )
                        )),
                        # actionButton(inputId = "Undo_", label = "undo", icon = icon("minus")) ,
                        textInput(ns("nodes_input"), "Input a list of nodes", value = "", placeholder = "Example: Bacteria_X,Bacteria_Y,season,tissue"),
                        fluidRow(column(12,
                            align = "center",
                            div(
                                class = "buttonagency", style = "vertical-align: middle;",
                                actionBttn(inputId = ns("Add_input_group"), label = "Add input group", icon = icon("plus"), style = "float", color = "primary", size = "sm")
                            )
                        )),
                        br()
                    ),
                    menuItem("Filter by subgraphs",
                        tabName = "Filter",
                        icon = icon("chevron-right"),
                        checkboxGroupInput(
                            inputId = ns("Filter_Subgraph"), label = "Filter by components.",
                            choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H")
                        ),
                        uiOutput(ns("Filter_by_subgraphs")),
                        uiOutput(ns("Filter_subgraph_by_num")),
                        br()
                    )
                )
            )
            ######
        ),
        dashboardBody( 
            tags$script(HTML(
                "document.querySelector('body > div.wrapper > header > nav > div > ul > li > a > span').style.visibility = 'hidden';"
            )), # Eliminamos los numeritos de del desplegable de edicion de etiquetas
            tags$head(
              tags$style(css_modal)
            ),
            tags$body(HTML(html_modal),
tags$script(HTML(paste0('
// Get the modal
var modal1 = document.getElementById("LabelModal"); 

// Get the <span> element that closes the modal
var span1 = document.getElementById("close1"); 

var modal2 = document.getElementById("GroupModal"); 

// Get the <span> element that closes the modal
var span2 = document.getElementById("close2"); 

var modal3 = document.getElementById("RemoveModal"); 

// Get the <span> element that closes the modal
var span3 = document.getElementById("close3");


// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
  if (event.target == modal1) { 
    Shiny.onInputChange("', ns('Change_label'),'", false);
    modal1.style.display = "none";
  } 
  if (event.target == modal2) {
    Shiny.onInputChange("', ns('Alert_Add_group'),'", false);
    modal2.style.display = "none";
  } 
  if (event.target == modal3) { 
  Shiny.onInputChange("', ns('Alert_Remove_group'),'", false);
    modal3.style.display = "none"; 
    
  } 
}

// When the user clicks on <span> (x), close the modal
span1.onclick = function() {
  Shiny.onInputChange("', ns('Change_label'),'", false);
  modal1.style.display = "none";
}

// Whan click acept close and save de value
document.getElementById("aceptModel1").onclick = function() {
  Shiny.onInputChange("', ns('Change_label'),'", document.getElementById("name1").value);
  modal1.style.display = "none";
} 

// When the user clicks on <span> (x), close the modal
span2.onclick = function() {
  Shiny.onInputChange("', ns('Alert_Add_group'),'", false);
  modal2.style.display = "none";
}

// Whan click acept close and save de value
document.getElementById("aceptModel2").onclick = function() {
  Shiny.onInputChange("', ns('Alert_Add_group'),'", document.getElementById("name2").value);
  modal2.style.display = "none";
}

// When the user clicks on <span> (x), close the modal
span3.onclick = function() { 
  Shiny.onInputChange("', ns('Alert_Remove_group'),'", false);
  modal3.style.display = "none";
}

// Whan click acept close and save de value
document.getElementById("aceptModel3").onclick = function() { 
  Shiny.onInputChange("', ns('Alert_Remove_group'),'", true);
  modal3.style.display = "none";
} 
')))),
            
            fluidRow(
                column(
                    width = 12, # shinyjqui::jqui_draggable(
                    tags$head(
                        tags$script(HTML(js_fs)),
                        tags$style(HTML(css_fs)),
                        tags$style("div.vis-network{background-color: white;}"), # Color blanco de fondo
                    ),
                    # shinydashboardPlus :: box( id = "Graph_output",
                    # shinyjqui::jqui_resizable(
                    # options = list( alsoResize  = "Graph_output", minWidth ='100px', maxWidth ='850px'),

                    # shinyjqui::jqui_resizable(
                    visNetworkOutput(ns("network_proxy"), width = "100%", height = "700px") # ,

                    # absolutePanel(id = "edit_labels_panel",
                    #             class = "panel panel-default",
                    #            draggable = TRUE,
                    #           top = 10, left = "auto", right = 20, bottom = "auto",
                    #          width = 200,
                    #         height = "auto",
                    #        h4("Labels edition"),
                    #       checkboxInput(inputId = "Enable_edition",
                    #               label ="",
                    #             value= FALSE)
                    #   )
                    # ), width = NULL, height = NULL
                    # )
                ) # )
            ),
            ####### Lo que estaba
            # downloadButton("save_plot", "Save network plot"),
            # downloadButton("save_relationships", "Save network relationships"),
            # actionBttn(inputId = "SVG", label = "SVG", style = "float", color = "primary", size = "sm"),
            # br(),
            #########
            fluidRow(
                column(
                    4,
                    fluidRow(
                        # Informacion del nodo tras hacer doble click
                        # shinyjqui::jqui_resizable(
                        # jqui_draggable(
                        shinydashboardPlus::box(
                            id = ns("Info_nodes"),
                            title = "Node info",
                            footer = "Double click on a node to display the information.",
                            icon = icon("bookmark"),
                            width = 12,
                            status = "black",
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            # Ponemos la información:
                            uiOutput(ns("Node_info")),
                            # column(6, h4(textOutput("N1"))),
                            # column(6, h4(textOutput("N2")))
                            uiOutput(outputId = ns("Info_graph_nodes")) # ))
                        )
                    ),
                    fluidRow(
                        # shinyjqui::jqui_resizable(
                        # jqui_draggable(
                        shinydashboardPlus::box(
                            id = ns("Info_edges"),
                            title = "Edge info",
                            footer = "Double click on an edge to display the information.",
                            icon = icon("bookmark"),
                            width = 12,
                            status = "black",
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            uiOutput(ns("Edge_info"))
                            # conditionalPanel(condition = "(  input.doubleClick_edges_selection.length > 0)" ,
                            # Ponemos la información:

                            # h4("Causal Relationships:"),
                            # h5(textOutput("E1"))'
                        ) # ))
                    )
                ),

                # Informacion del nodo tras hacer doble click
                # Tabla de nodos y edges.
                # shinyjqui::jqui_resizable(
                # jqui_draggable(
                tabBox(
                    id = ns("Data"),
                    selected = "Nodes",
                    width = 8,
                    tabPanel(
                        title = "Nodes",
                        fluidRow(
                            column(
                                width = 8,
                                checkboxGroupInput(
                                    inputId = ns("Filter_Tab_N"), label = "Select an option to emphasize or show only selected nodes",
                                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"), selected = NULL,
                                    width = "100%"
                                )
                            ),
                            column(
                                width = 4,
                                checkboxInput(
                                    inputId = ns("Reorder_N"),
                                    label = "Reorder (selected first)",
                                    value = TRUE
                                ),
                                # checkboxInput(inputId = "Select_Tab_N", label = "Select/ deselect all table visible nodes.",
                                #            value = FALSE)
                            )
                        ),
                        # fluidRow( column(1),
                        #         column(2,offset = 8,
                        #                 switchInput(inputId = "Emphasize",
                        #                             label = "Emphasize",
                        #                             value = FALSE,
                        #                             onLabel = "ON",
                        #                             offLabel = "OFF",
                        #                             onStatus = "success",
                        #                             offStatus = "danger")
                        #          )
                        # ),
                        # Tabla con la información de los nodos
                        dataTableOutput(ns("Tab_nodes"))
                    ),
                    tabPanel(
                        title = "Edges",
                        column(
                            width = 8,
                            checkboxGroupInput(
                                inputId = ns("Filter_Tab_E"), label = "Select an option to emphasize or show only selected nodes",
                                choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"), selected = NULL,
                                width = "100%"
                            )
                        ),
                        column(
                            width = 4,
                            checkboxInput(
                                inputId = ns("Reorder_E"),
                                label = "Reorder (selected first)",
                                value = TRUE
                            ),
                            uiOutput(outputId = ns("Value_decimals"))
                        ),
                        # Tabla con la información de los nodos
                        dataTableOutput(ns("Tab_edges")),
                        tags$style(
                            type = "text/css",
                            ".noUi-connect {background: #95979A;}"
                        )
                    )
                ) # ))
            ),

            # ·························································#
            #                                                         #
            #                 Exportación de la red                   #
            #                                                         #
            # ·························································#



            # Pop up de previsualización del guardado
            bsModal(
                id = ns("Pop_up"), title = "Preview",
                trigger = "Save", size = "large",
                fluidRow(conditionalPanel(
                    condition = "input.Out_type!='HTML'",
                    ns = ns,
                    fluidRow(
                        column(
                            4,
                            textInput(
                                inputId = ns("Title_main"),
                                label = "Main title: ",
                                value = ""
                            )
                        ),
                        column(
                            4,
                            textInput(
                                inputId = ns("Title_submain"),
                                label = "Submain: ",
                                value = ""
                            )
                        ),
                        column(
                            4,
                            textInput(
                                inputId = ns("Title_footer"),
                                label = "Footer: ",
                                value = ""
                            )
                        )
                    )
                )),
                fluidRow(column(
                    12,
                    visNetworkOutput(ns("network_save"), width = "100%", height = "100vh")
                )),
                fluidRow(
                    column(
                        4,
                        textInput(
                            inputId = ns("Out_name"), label = "File name",
                            value = "Output"
                        )
                    ),
                    column(4, selectInput(
                        inputId = ns("Out_type"),
                        label = "Format",
                        selected = "png",
                        choices = c(
                            "HTML", "png",
                            "jpeg", "pdf"
                        )
                    ), ),
                    column(
                        4,
                        uiOutput(outputId = ns("HTML_Button"))
                    )
                )
            ),


            ######



            ######
            style = "width: 100%; height: 100%"
        )
    )



    tabPanel(
        HTML("<b>Network Viewer</b>"),
        #tags$label(h3("Network Viewer")),
        #conditionalPanel(condition = "output.loaded_data!=null", ns = ns),
        # sidebarPanel(
        #   width = 12,
        #   fileInput("network_plot", "Network", accept = ".RData"),
        #   # textInput("nodes_plot", "Nodes", value= "", placeholder = 'Example: Bacteria_X,Bacteria_Y,season,tissue'),
        #   div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", actionBttn(inputId = "button_plot", label = "Submit", style = "float", color = "primary", size = "sm")),
        #   div(class = "buttonagency", style = "display:inline-block; margin-right:10px;", downloadBttn(outputId = "save_relationships", label = "Save network relationships", style = "float", color = "primary", size = "sm", icon = icon("download"))),
        # ),
        #hr(),
        mainPanel(
            width = 12, 
            chooseSliderSkin("Shiny", color = DT$slider_color), # Slider imput style
            fluidRow(
                column(
                    6,
                    h4("Network: "),
                    uiOutput(ns("loaded_data"))
                ),
                column(2,
                    offset = 4,
                    div(
                        class = "buttonagency", style = "display:inline-block; margin-right:10px;",
                        actionBttn(inputId = ns("button_plot"), label = "Refresh", style = "simple", icon = icon("refresh", lib = "glyphicon"), color = "primary", size = "sm")
                    ),
                    div(
                        class = "buttonagency", style = "display:inline-block; margin-right:1px;",
                        actionBttn(inputId = ns("close_graph_panel"), label = "Close", style = "simple", color = "primary", size = "sm", icon = icon("remove"))
                    )
                )
            ),
            hr(),
            conditionalPanel(
                condition = "output.display_graph_panel==1",
                ns = ns,
                div(
                    id = "graphContainer", style = "overflow-x: scroll",
                    graph_page
                )
            )
        )
    )
}

network_viewer_server <- function(session_data , id = "network_viewer_mod") {
    ns <- NS(id)

    moduleServer(id, function(input, output, session) {
        
        state <- reactiveValues(entry = NULL)
        observe({ 
            ## Activate of the module is here by setting entry in state 
            if (!is.null(session_data$fittedbn)) {
                state$entry <- session_data$fittedbn
                state$arc_st_mi <- session_data$build_env$arc_st_mi

            } else {
                if (!is.null(session_data$dyn.fitted)) {
                    state$entry <- session_data$dyn.fitted
                   
                } else {
                    state$entry <- NULL
                   
                }
            }
        })
       

        output$display_graph_panel <- reactive({ # Cuando se cargan datos nuevos se pone en 0
            0
        })
        outputOptions(output, "display_graph_panel", suspendWhenHidden = FALSE)
        
        

        # Asignamos el valor a Tab_inputs$refresh que usaremos para regenerar la red
        observeEvent(input$button_plot, {
            validate(
                need(state$entry,"To view the graph panel, load or create a network first")
            )
            output$display_graph_panel <- reactive({TRUE})
            Reset_fun()
            Tab_inputs$refresh <- input$button_plot
        })
        
        
        output$loaded_data <- renderUI({
            validate(
                need(state$entry,"To view the graph panel, load or create a network first")
            )
            return(h5("Network loaded successfully, Refresh to view the it."))
        })

        observeEvent( eventExpr = input$close_graph_panel,  ignoreInit = TRUE,{
            #print("Close2")
            output$display_graph_panel <- reactive({2}) 
        }) 
        
       
        
        #### Fullscreen
        observeEvent( eventExpr = input$Fullscreen, ignoreInit = TRUE,{
            if (isTRUE( state$fullscreen)){ # Estamos en pantalla completa
            print("cerrar")
            runjs("closeFullscreen(document.getElementById('graphContainer'))")
            state$fullscreen <- FALSE
            } else{
            print("pantalla completa")  #Cambiamos a pantalla completa
            runjs("openFullscreen(document.getElementById('graphContainer'))")
            state$fullscreen <- TRUE
            } 
        }) 
        
        # browser()
        # if (is.null()) {
        #  return(NULL)
        # }
        # inFile <- isolate({
        #   input$network_plot
        # })
        # file <- inFile$datapath
        # load(file, envir = .GlobalEnv)

        Graph_output <- reactiveValues(graph = NULL)

        ## Plot subgraph of the network
        plotInput <- function() {
            # nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
            # nodes <- strsplit(nodes, ",")[[1]]
            # for (i in 1:length(nodes)) {
            # nodes[i] <- sub("/",".", nodes[i])
            # nodes[i] <- sub(" ",".", nodes[i])
            # nodes[i] <- str_replace(nodes[i], "-",".")
            # }
            # Adaptamos la entrada encfuncion de lso datos
            # Aya
            # Ides ! usar ls para obtener los objetos de la clase bn.fit y tomar el adecuado
            # if (exists("fittedbn")) {
            # browser()
            if (!is.null(session_data$fittedbn)) {
                entry <- session_data$fittedbn
            } else {
                if (!is.null(session_data$dyn.fitted)) {
                    entry <- session_data$dyn.fitted
                } else {
                    return(NULL)
                }
            }
            
            nodes <- names(entry)
            # nodes = nodes[11:length(nodes)]
            subgr <<- bnlearn::subgraph(entry, nodes)
            tryit <- try(strength.viewer(
                bayesianNetwork = subgr,
                bayesianNetwork.boot.strength = state$arc_st_mi,
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
                    bayesianNetwork.boot.strength = state$arc_st_mi,
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
        Reset_fun <- function() { # Actualizamos los inputs
            # Tab_inputs$write = FALSE
            # browser()
            shinyjs::reset(ns("Edit_menu"))
            # Actualizamos la barra lateral
            # updateSidebar(ns("sidebar"))
            ## FIX :: updateSidebar does not work with ns 
            ## this is the only way to do the the update
            message <- list(value = !session$input[["sidebar"]])
            session$sendInputMessage(ns("sidebar"), message)
            #####################################################
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
            Filt$subgraph_edg <<- NULL
            Filt$subgraph_nod <<- NULL
            Filt$sel_subgraph <<- NULL
            Filt$show_sugraph <<- NULL
            Filt$sel_by <<- NULL
            Filt$grade <<- 0
            Filt$number <<- 0
            Filt$direction <<- "All"

            # Debemos resetear las variables, reset no va y si lo pongo como abjo tampoco, asi que individual:
            Tab_inputs$nodes <<- NULL
            Tab_inputs$edges <<- NULL


            '# Regeneramos las variables reactivas de los que dependen nuestros objetos( Sin la doble flecha no va)
            Tab_inputs <<- reactiveValues(nodes = NULL,
                                        edges = NULL,
                                        Ord_edg0 = NULL,
                                        Sel_group = "None",
                                        refresh = NULL
            ) '

            # Fijamos en nulo los nodos y edges seleccionados
            Sel_edg <- NULL
            Sel_nod <- NULL

            # Regeneramos las variables reactivas de los que dependen nuestros objetos
            'Tab_inputs <- reactiveValues(nodes = NULL,
                                   edges = NULL,
                                   Ord_edg0 = NULL,
                                   Sel_group = "None",
                                   refresh = NULL ,
                                   #write = NULL
            ) '

            "Filt <- reactiveValues(sel_tab_n = NULL,
                             sel_tab_e = NULL,
                             sel_menu = NULL
                             )"

            #

            # Fijamos en nulo los nodos seleccionado
            # Sel_nod <<- NULL

            'updateCheckboxInput(inputId = "Select_Tab_N", value = FALSE )'


            Filt$sel_tab_n
            Filt$sel_tab_e


            # shinyjs::reset("Filter_Tab_N")
            # shinyjs::reset("Filter_Tab_E")

            # shinyjs::reset("Filter_Tab_Menu")
            # Undo()
            updateCheckboxGroupInput(
                inputId = ns("Filter_Tab_N"),
                choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                selected = NULL
            )
            updateCheckboxGroupInput(
                inputId = ns("Filter_Tab_E"),
                choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                selected = NULL
            )
            updateCheckboxGroupInput(
                inputId = ns("Filter_Menu"),
                choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                selected = NULL
            )
            updateCheckboxGroupInput(
                inputId = ns("Filter_Menu_By"),
                choices = list("Selected" = "By_sel", "Group" = "By_group", "Number of interactions" = "By_num"),
                selected = NULL
            )
            updateCheckboxGroupInput(
                inputId = ns("Filter_Subgraph"),
                choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                selected = NULL
            )

            input$Filter_Tab_N
            input$Filter_Tab_E
            # Tab_inputs$refresh <- input$button_plot


            # Eliminamos los filtros de la tabla
            DT::dataTableProxy("Tab_nodes") %>% # Reordenamos
                clearSearch()
            # Eliminamos los filtros de la tabla
            DT::dataTableProxy("Tab_edges") %>% # Reordenamos
                clearSearch() 
        }

        # Reiniciamos los inputs (al pulsar el boton de submit reiniciamos el panel de edicion y las variables)
        observeEvent(eventExpr = input$graph_refresh, ignoreNULL = FALSE, {
            Reset_fun()
            Tab_inputs$refresh <- runif(n = 1, min = -100.00, max = -1.00)
        })


       


        'observeEvent( input$button_plot , {
      print("Iniciado")
      print(input$network_proxy_initialized)
      visNetworkProxy("network_proxy") %>%
      visRedraw()

      shinyjs::reset("Graph_output")
      print("Reiniciado:")
      print(input$network_proxy_initialized)
    })

    observe({
      input$network_proxy_initialized
      print("activado")}
    )'

        'observeEvent(input$button_plot, {
      plotInput()
      print("Es nulo")
    })'


        ## Link button to print image
        p_button <- eventReactive(Tab_inputs$refresh, print(plotInput()))
        # cpt_button <- eventReactive(input$button2, table_cpt())







        # ·························································#
        #                                                         #
        #               Representación de la red                  #
        #                                                         #
        # ·························································#
        output$network_proxy <- renderVisNetwork(
            # if(! is.null(Graph_output$graph)){
            p_button() %>%
                # Graph_output$graph %>%
                # Permite seleccionar nodos (file:data)
                visEvents(
                    type = "on", # https://visjs.github.io/vis-network/docs/network/
                    # Al seleccionar guardamos los ids de nodos y edges
                    # select = "function(data) {
                    # Shiny.onInputChange('current_nodes_selection', data.nodes);
                    # Shiny.onInputChange('current_edges_selection', data.edges);
                    # }" ,
                    # Detectamos cualquier cmabio de seleccion
                    select = paste0("function(data) {
                            Shiny.onInputChange('",ns("change_selection"),"', Math.random());
                        }"),
                    # deselectNode = "function(data) {
                    # Shiny.onInputChange('deselect_nodes', data.nodes);
                    # }" ,
                    doubleClick = paste0("function(data) {
                        console.log(data);
                  Shiny.onInputChange('",ns("doubleClick_nodes_trigger"),"', Math.random());
                  Shiny.onInputChange('",ns("doubleClick_nodes_selection"),"', data.nodes);
                  Shiny.onInputChange('",ns("doubleClick_edges_selection"),"', data.edges);}"),
                    # input$network_proxy_initialized pasa a true al generar la red completamente, pero solo funciona la primera vez, por tanto al iniciar la red la fijamos en FALSE
                    afterDrawing = paste0("function(data) {
                Shiny.onInputChange('",ns("network_proxy_initialized"),"', false); }"),

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
                visInteraction(
                    multiselect = TRUE, selectConnectedEdges = FALSE,
                    navigationButtons = TRUE, zoomView = FALSE,
                    keyboard = TRUE
                ) %>%
                visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE)
            # %>%  visPhysics(stabilization = FALSE)
            # }
        )

        # Al iniciarse la red tomamos el valor de los nodos y los edges actuales.
        # Además tomamos tambein el valor de los seleccionados que deben ser nulos
        observeEvent(eventExpr = input$network_proxy_initialized, ignoreNULL = FALSE, {
            visNetworkProxy(ns("network_proxy")) %>%
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
            visNetworkProxy(ns("network_proxy")) %>%
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
            visNetworkProxy(ns("network_proxy")) %>%
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

            # Aya

            if (!is.null(edges_out$weight)) {
                colnames(edges_out)[which(colnames(edges_out) == "weight")] <- "value"
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


        # Si detectamos cambios en los datos reconstruimos la tabla
        observeEvent(eventExpr = input$network_proxy_edges, ignoreNULL = TRUE, {
            Nodes_table()
            Edges_table()

            # Eliminamos las etiquetas al generar la red
            if (isFALSE(input$E_label)) {
                S_edges <- edges_info$id
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(id = S_edges, label = " "))
            }

            '# comprobamos si se ha introducido algun NA
        Null_edg = edges[which(is.na(edges$value)), "id" ]
        Null_nod = nodes[which(is.na(nodes[,2])), "id" ]
        print(Null_edg)
      print(Null_nod)

        if (length(Null_edg > 0)){
          visNetworkProxy("network_proxy") %>%
            visRemoveEdges( id = Null_edg)
          print("CAMBIOOOE")
        }
      if (length(Null_nod > 0)){
        visNetworkProxy("network_proxy") %>%
          visRemoveNodes(id = Null_nod)
        print("CAMBIOOON")
      }'

            # Tab_inputs$refresh <<- 0
        })

        'observe({
      if( ! is.null(input$network_proxy_edges)){
        Nodes_table()
        Edges_table()
      }
      print(".......")
      #Tab_inputs$refresh <<- 0
    }) '



        ######



        # ·························································#
        #                                                         #
        #        Tabla e información de los nodos y edges         #
        #                                                         #
        # ·························································#



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Nodos                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        ### Funciones básicas ###

        # Ordenar
        OrderNodes <- function() {
            # Si la reordenación esta activada:
            if (isFALSE(input$Reorder_N) | is.null(input$network_proxy_selectedNodes)) {
                Ord_nod_tab <<- nodes[, c("id", "label")] # Tab_inputs$nodes
                # print("DD1")
            } else {
                # print("DD2")
                # Indices nodos seleccionados
                Ind_nodes <- which(Ord_nod_tab$id %in% input$network_proxy_selectedNodes)

                # print(input$network_proxy_selectedNodes)

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
                # print(Sel_tab)

                Sel_nodes <- (Sel_tab[with(Sel_tab, order(Sel_tab$ord, decreasing = TRUE)), ])$ind

                # print(Sel_nodes)
                # Indices nodos no seleccionados
                Non_select <- which(!Ord_nod_tab$id %in% input$network_proxy_selectedNodes)

                # Indices ordenados
                Order <- c(Sel_nodes, Non_select)

                # Asignamos el valor de la variable, también se puede usar assign()
                Ord_nod_tab <<- Ord_nod_tab[Order, ]
            }
        }


        ### Selección y ordenamiento de los nodos en la red y tabla ###

        # Output de la Tabla de nodos:
        Generate_nodestable <- function() {
            output$Tab_nodes <- DT::renderDataTable({
                Sel_nod0 <- which(Ord_nod_tab$id %in% Sel_nod) # Tomamos los nodos seleccionados al generar la tabla, no es reactivo, si lo fuese regeneraria la tabla al resetear.

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
                          paste0("    Shiny.setInputValue('",ns("dt_dblclick"),"', {dt_row: row, dt_col: col});"),
                        "  }",
                        ");",
                        "table.on('click', 'td',",
                        "  function() {",
                        "    var row = table.cell(this).index().row;",
                        "    var col = table.cell(this).index().column;",
                        paste0("    Shiny.setInputValue('",ns("dt_click"),"', {dt_row: row, dt_col: col});"),
                        "  }",
                        ");"
                    )
                )
            })
        }

        observe({
            #  Tab_inputs$refresh
            # Al regenerar los datos
            Tab_inputs$nodes
            # Al cambiar los datos los nodos se cambian al cambiar de pestaña
            # input$Data
            Generate_nodestable()
        })

        observeEvent(eventExpr = input$Data, {
            print(input$Data)
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
            # print(input$Tab_nodes_cell_clicked)

            if (length(input$Tab_nodes_cell_clicked) > 0) { # Al crear la tabla si al hacer submit estamos en la pestaña de edges se crea la variable como una lista vacía named list(), esta condición impide que en dicho caso se ejecute el resto y se borre la selección inicial
                S_nodes <- {
                    nod <- input$Tab_nodes_rows_selected # https://yihui.shinyapps.io/DT-rows/ Tomamos las filas seleccionadas
                    tab <- Ord_nod_tab # Tabla ordenada

                    if (is.null(nod)) {
                        S_nodes <- NULL
                    } else {
                        sel <- input$Tab_nodes_cell_clicked$row # Tomamos la última seleccionda y la colocamos delante (solo si esta en seleccionado, si es una deselección su valor será null)

                        n1 <- nod[which(nod == sel)] # Tomamos la ultima seleccionada
                        n2 <- nod[which(nod != sel)] # Tomamos el resto
                        S_nodes <- append(tab[n1, "id"], tab[n2, "id"]) # nod = input$Tab_nodes_rows_selected, son numero asi que lo paso a ids
                        # print(S_nodes)
                    }
                    S_nodes
                }

                # visNetworkProxy("network_proxy") %>%
                # visSelectNodes(S_nodes, highlightEdges = FALSE)

                # Tomamos los edges que habia previmente seleccionados
                S_edges <- input$network_proxy_selectedEdges
                # Tomamos los nodos a seleccionar, es importante tener en cuenta el orden, ya que en caso contrario se invertirá en la tabla
                S_nodes <- rev(S_nodes)

                # Modificamos la selección
                if (is.null(S_edges) && is.null(S_nodes)) {
                    # Si no hay nodos y edges seleccionados deseleccionamos todo
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUnselectAll()
                } else {
                    visNetworkProxy(ns("network_proxy")) %>%
                        visSetSelection(
                            nodesId = S_nodes,
                            edgesId = S_edges,
                            unselectAll = TRUE,
                            highlightEdges = FALSE
                        )
                }
            }

            # Actualizamos
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedNodes() %>%
                visGetSelectedEdges()
        })


        # Ordenamos la tabla para posicionar los seleccionados primero
        observe({
            OrderNodes()
        })


        # Seleccionar filas que se corresponden con los nodos seleccionados en la red
        observe({
          print("change")
            input$Correct_Change_label # Si se ha cambiado el nombre de alguna etiqueta

            # Guardamos en un variable los nodos seleccionados para usarlos al generar la tabla
            Sel_nod <<- input$network_proxy_selectedNodes

            # Detectamos si queremos reordenar o no
            input$Reorder_N

            # Guardamos los nodos seleccionado en una variable
            # Sel_nod <<- input$network_proxy_selectedNodes


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



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Edges                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        ## Escogemos el número de decimales
        output$Value_decimals <- renderUI({
            if (!is.null(Tab_inputs$edges$value)) {
                # Tomamos contamos la cifras significativas.
                sig_count <- function(n) {
                    num <- gsub("^[^(1-9)]*", "", x = n)
                    num <- gsub("\\.", "", x = num)
                    nchar(num)
                }

                Sig <- max(sapply(X = Tab_inputs$edges$value, FUN = sig_count))

                sliderInput(
                    inputId = ns("Value_decimals"), label = "Significant digits",
                    step = 1, min = 1, max = Sig, value = Sig
                )
            }
        })


        ### Funciones básicas ###

        # Ordenar
        OrderEdges <- function() {
            if (isFALSE(input$Reorder_E) | is.null(input$network_proxy_selectedEdges)) {
                Ord_edg_tab <<- edges[, c("id", "value", "from", "to")] # Tab_inputs$edges
            } else {
                # Indices edges seleccionados
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

                # Sel_edges = which(Ord_edg_tab$id %in% input$network_proxy_selectedEdges)

                # Indices edges no seleccionados
                Non_select <- which(!Ord_edg_tab$id %in% input$network_proxy_selectedEdges)

                # Indices ordenados
                # if( isTRUE(revers)){
                Order <- c(Sel_edges, Non_select)
                # } else {
                #  Order = c(Sel_edges, Non_select)
                # }

                # Asignamos el valor de la variable, también se puede usar assign()
                Ord_edg_tab <<- Ord_edg_tab[Order, ]
            }
        }

        # Output de la tabla edges:
        output$Tab_edges <- DT::renderDataTable({
            # Tomamos los edges previmente seleccionados en las red
            # reactivate this if Tab_inputs$edges changes
            

            ##
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
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = edges_info$id,
                        label = edges_info$label
                    ))
            }
        })



        # Edit_server línea 42 se actualizan los nodos y edges seleccionados.
        # Selecciona los edges que une dos nodos seleccionados.
        observeEvent(eventExpr = input$network_proxy_selectedNodes, {
            if (!is.null(input$network_proxy_selectedNodes)) {
                # Tomamos los edges que unen dos nodos seleccionados
                e1 <- edges[edges$from %in% input$network_proxy_selectedNodes, ]
                e2 <- e1[e1$to %in% input$network_proxy_selectedNodes, ]

                if (nrow(e2) > 0) {
                    # Unimos los edges anteriores con los previamente seleccionados
                    S_edges <- unique(append(input$network_proxy_selectedEdges, e2$id))

                    # Modificamos la selección
                    S_nodes <- input$network_proxy_selectedNodes
                    visNetworkProxy(ns("network_proxy")) %>%
                        visSetSelection(
                            nodesId = S_nodes,
                            edgesId = S_edges,
                            unselectAll = FALSE,
                            highlightEdges = FALSE
                        )
                }

                # Actualizamos la información:
                visNetworkProxy(ns("network_proxy")) %>%
                    visGetSelectedNodes() %>%
                    visGetSelectedEdges()
            }
        })


        # Selecciona en la red los edges de la Tab_edges
        observeEvent(eventExpr = input$Tab_edges_cell_clicked, ignoreNULL = FALSE, { # Se activa cuando hacemos click en la tabla
            S_edges <- {
                edg <- input$Tab_edges_rows_selected # https://yihui.shinyapps.io/DT-rows/
                tab <- Ord_edg_tab
                if (is.null(edg)) {
                    S_edges <- NULL
                } else {
                    S_edges <- tab[edg, "id"] # edg = input$Tab_edges_rows_selected, son numero asi que lo paso a ids
                }
                S_edges
            }
            S_nodes <- input$network_proxy_selectedNodes # Tomamos los nodos previamente seleccionados

            if (is.null(S_nodes) && is.null(S_edges)) {
                visNetworkProxy(ns("network_proxy")) %>%
                    visUnselectAll()
            } else {
                visNetworkProxy(ns("network_proxy")) %>%
                    visSetSelection(
                        nodesId = S_nodes,
                        edgesId = S_edges,
                        unselectAll = TRUE, # Deseleccionamos todos, sino al eliminar uno no cambia la selección
                        highlightEdges = FALSE
                    )
            }
            # Actualizamos
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedNodes() %>%
                visGetSelectedEdges()
        })


        # Ordenamos la tabla para posicionar los seleccionados primero
        observe({
            # Reordenamos al resetear la red
            Tab_inputs$edges
            # Al cambiar los decimales
            input$Value_decimals

            OrderEdges()
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedEdges()
        })
        ' observeEvent( eventExpr = input$network_proxy_selectedEdges,
                  ignoreNULL = FALSE, {
                    print("*36")
                    OrderEdges(revers = FALSE)
                  }) '
        'observeEvent(eventExpr = input$Reorder_E,
                 ignoreNULL = FALSE, {
                   print("*36")
                   OrderEdges(revers = FALSE)
                 })'


        # Seleccionar filas que se corresponden con los edges seleccionados en la red
        observe({
            # Guardamos en un variable los edges seleccionados para usarlos al regenerar la tabla
            Sel_edg <<- input$network_proxy_selectedEdges

            # Esto hará que se reinicia al reiniciar la red
            Tab_inputs$edges
            # Esto hará que se reinicia al cambiar de pestaña
            input$Value_decimals
            # Indicamos si queremos ordenarlo o no:
            input$Reorder_E

            if (is.null(input$network_proxy_selectedEdges)) {
                DT::dataTableProxy("Tab_edges") %>% # Reordenamos
                    replaceData(data = Ord_edg_tab, clearSelection = TRUE) %>%
                    # Eliminamos la selección
                    selectRows(NULL)
            } else {
                DT::dataTableProxy("Tab_edges") %>% # Reordenamos
                    replaceData(data = Ord_edg_tab, clearSelection = FALSE) %>% # Seleccionamos
                    selectRows(which(Ord_edg_tab$id %in% input$network_proxy_selectedEdges))
            }
        })

        #
        #
        #     Cambiamos las etiquetas de los nodos
        #
        #

        # Pop up para modificar la etiqueta

        observeEvent(eventExpr = input$doubleClick_nodes_trigger, ignoreNULL = TRUE, {
            # Detectamos que se ha hecho doble click sobre el panel, debemos ver si es sobre un nodo
            # Comprobamos que lo que se ha clickado es un nodo
            #browser()
            if (isTRUE(input$Enable_edition)) {
                if (!is.null(input$doubleClick_nodes_selection)) {
                    # Tomamos la informacion del nodo
                    id <- input$doubleClick_nodes_selection
                    label <- nodes[which(nodes$id == id), "label"]
                    print("modal")  
                    
                    runjs(sprintf("document.getElementById('title1').innerHTML = '%s';
                                   document.getElementById('content1').innerHTML = '%s';
                                   document.getElementById('name1').value = '%s' 
                                   document.getElementById('LabelModal').style.display = 'block';",
                                  "Rename node:", id, label))
                    #runjs(sprintf("document.getElementById('N1').innerHTML = '%s'; 
                     #     document.getElementById('myModal').style.display = 'block';", id)) 
          
                }
            }
        })

        'observeEvent( eventExpr = input$doubleClick_nodes_selection, ignoreNULL = TRUE, {

      shinyalert::shinyalert(
        inputId = "Change_label",
        #title = "Change node label",
        text = "Set node label",
        type = "input",
        inputType = "text",
        inputValue = input$doubleClick_nodes_selection,
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        size = "xs"
      )
    })' 

        # Cambiamos el nombre de la etiqueta
        observeEvent(eventExpr = input$Change_label, {
          if (! isFALSE(input$Change_label)){
            runjs(paste0('Shiny.onInputChange("', ns('Correct_Change_label'),'", true);'))
            print(input$Change_label)
            id <- input$doubleClick_nodes_selection
            label <- nodes[which(nodes$id == id), "label"]
            
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = id,
                        label = input$Change_label
                    ))
                # Cambiamos los datos del original:
                nodes[which(nodes$id == id), "label"] <<- input$Change_label
                nodes_info[which(nodes_info$id == id), "label"] <<- input$Change_label
                Ord_nod_tab[which(Ord_nod_tab$id == id), "label"] <<- input$Change_label
                # print(Ord_nod_tab[which(nodes_info$id == id), "label"])
                # Tab_inputs$nodes[which(Tab_inputs$nodes == id), "label"] <<- input$Change_label 
            }  
        })



        # ·························································#
        #                                                         #
        #   Mostrar info individualizada al hacer doble click     #
        #                                                         #
        # ·························································#



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Nodos                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        ### Texto a mostrar ###

        # Información del id del nodo tras el doble click
        output$N1 <- renderPrint({
            if (is.null(input$doubleClick_nodes_selection)) {
                NULL
            } else {
                state$entry[[input$doubleClick_nodes_selection]]
            }
        })


        output$Node_info <- renderUI({
            if (is.null(input$doubleClick_nodes_selection)) {
                NULL
            } else {
                verbatimTextOutput(ns("N1"))
            }
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Edges                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



        ### Texto a mostrar ###

        # Información del id del nodo tras el doble click
        output$E1 <- renderText({
            edge_id <- input$doubleClick_edges_selection

            from <- edges[edges$id == edge_id, "from"]

            to <- edges[edges$id == edge_id, "to"]

            value <- edges[edges$id == edge_id, "value"]

            paste0(
                "Causal Relationships : \n  ",
                from, "  ->  ", to,
                "\nEdge Strength : \n  ",
                value
            )
        })


        output$Edge_info <- renderUI({
            if (is.null(input$doubleClick_edges_selection)) {
                NULL
            } else {
                verbatimTextOutput(ns("E1"))
            }
        })

        '    # Información del id del edge tras el doble click
    output$E1 = renderText({
      edge_id = input$doubleClick_edges_selection
      if (is.null(edge_id) | ! is.null(input$doubleClick_nodes_selection)){
        NULL
      }else{
        from = edges[edges$id == edge_id, "from"]
        paste(from)
      }
    })

    # Información de la etiqueta del edge tras el doble click
    output$E2 = renderText({
      edge_id = input$doubleClick_edges_selection
      if (is.null(edge_id) | ! is.null(input$doubleClick_nodes_selection)){
        NULL
      }else{
        to = edges[edges$id == edge_id, "to"]

        paste(to)
      }
    })

    # Información de la etiqueta del edge tras el doble click
    output$E3 = renderText({
      edge_id = input$doubleClick_edges_selection
      if (is.null(edge_id) | ! is.null(input$doubleClick_nodes_selection)){
        NULL
      }else{
        value = edges[edges$id == edge_id, "value"]

        paste(value)
      }
    }) '

        ######



        # ·························································#
        #                                                         #
        #              Panel de edición de la red                 #
        #                                                         #
        # ·························································#


        ### Funciones básicas ###
        "
    # Obtención de los IDs de todos o de los nodos seleccionados
    Select_nodes <- reactive({ # Función reactiva que toma los ids seleccionado, si no hay seleccionados toma todos
      if (is.null(input$network_proxy_selectedNodes)){
        Sel_nodes = nodes$id
      } else{
        Sel_nodes = input$network_proxy_selectedNodes
      }
      return(Sel_nodes)
    })


    # Obtención de los IDs de todos o de los edges seleccionados
    Select_edges <- reactive({
      if (is.null(input$network_proxy_selectedEdges)){
        Sel_edges = edges$id
      } else{
        Sel_edges = input$network_proxy_selectedEdges
      }
      return(Sel_edges)
    })"



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Nodos                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        ### Obtención de los nodos y edges seleccionados ante cambios en el entorno ###

        # Si detectamos cambios en la red tomamos los nodos y edges seleccionados
        observeEvent(eventExpr = input$change_selection, ignoreNULL = FALSE, {
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedNodes() %>%
                visGetSelectedEdges()
        })

        # Si detectamos algún cambio en la tabla tomamos los nodos y edges seleccionados
        observeEvent(eventExpr = input$Tab_nodes_cell_clicked, ignoreNULL = FALSE, {
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedNodes() %>%
                visGetSelectedEdges()
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #        Tamaño y forma        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Shape
        observeEvent(eventExpr = input$N_shape, {
            if (!is.null(nodes)) {
                S_nodes <- Select_nodes()
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(id = S_nodes, shape = input$N_shape))
            }
        })


        # Size
        observeEvent(eventExpr = input$N_size, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()
                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "size"] <<- input$N_size

                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(id = S_nodes, size = input$N_size))
            }
        })

        # Funciones que determinana el tamaño
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
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateNodes(nodes = data.frame(id = nodes$id, size = unlist(sizes)))
                }
            } else {
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(id = nodes$id, size = nodes_info$size))
            }
        })


        # Border Width
        observeEvent(eventExpr = input$N_borderWidth, {
            if (!is.null(nodes)) {
                S_nodes <- Select_nodes()
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(id = S_nodes, borderWidth = input$N_borderWidth))
            }
        })


        ## Selected Border Width
        observeEvent(eventExpr = input$N_borderWidthSelected, {
            if (!is.null(nodes)) {
                S_nodes <- Select_nodes()
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(id = S_nodes, borderWidthSelected = input$N_borderWidthSelected))
            }
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #            Color             #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Background color
        observeEvent(eventExpr = input$N_color_background, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()
                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "color.background"] <<- input$N_color_background

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
                    ))
            }
        })


        # Border color
        observeEvent(eventExpr = input$N_color_border, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "color.border"] <<- input$N_color_border

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
                    ))
            }
        })


        # Highlight background color
        observeEvent(eventExpr = input$N_color_highlight_background, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "color.highlight.background"] <<- input$N_color_highlight_background

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("color.", colnames(nodes_info))]
                    ))
            }
        })


        # Highlight border color
        observeEvent(eventExpr = input$N_color_highlight_border, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "color.highlight.border"] <<- input$N_color_highlight_border

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
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
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(id = S_nodes, opacity = input$N_opacity))
            }
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #           Etiquetas          #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Show labels
        observeEvent(eventExpr = input$N_label, ignoreNULL = FALSE, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- nodes$id

                if (isTRUE(input$N_label)) {
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateNodes(nodes = data.frame(
                            id = S_nodes,
                            label = nodes_info$label
                        ))
                } else {
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateNodes(nodes = data.frame(id = S_nodes, label = " "))
                }
            }
        })

        # Color
        observeEvent(eventExpr = input$N_label_color, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "font.color"] <<- input$N_label_color

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
                    ))
            }
        })


        # Tamaño
        observeEvent(eventExpr = input$N_label_size, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "font.size"] <<- input$N_label_size

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
                    ))
            }
        })


        # Fuente
        observeEvent(eventExpr = input$N_label_face, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "font.face"] <<- input$N_label_face
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
                    ))
            }
        })


        # Fondo
        observeEvent(eventExpr = input$N_label_strokeWidth, {
            if (!is.null(nodes)) {
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "font.strokeWidth"] <<- input$N_label_strokeWidth

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
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
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()
                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "font.strokeColor"] <<- input$N_label_strokeColor
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
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
                # Tomamos los IDs de los nodos
                S_nodes <- Select_nodes()

                # Tomamos su posición en el data frame
                N_nodes <- which(nodes_info$id %in% S_nodes)

                # Cambiamos el color por defecto
                nodes_info[N_nodes, "font.align"] <<- input$N_label_align

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = S_nodes,
                        nodes_info[N_nodes, grep("font.", colnames(nodes_info))]
                    ))
            }
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #            Sombra            #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Show shadow
        observe({
            if (isTRUE(input$N_show_shadow)) {
                if (!is.null(nodes)) {
                    # S_nodes = Select_nodes()
                    visNetworkProxy(ns("network_proxy")) %>%
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
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateNodes(nodes = data.frame(
                            id = nodes$id,
                            shadow = input$N_show_shadow
                        ))
                }
            }
        })

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Edges                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        ### Obtención de los nodos y edges seleccionados ante cambios en el entorno ###

        '# Si detectamos cambios en la red tomamos los edges seleccionados
    observeEvent( eventExpr = input$change_selection,ignoreNULL = FALSE,  {
      print("*13")
      visNetworkProxy("network_proxy") %>%
        visGetSelectedNodes() %>%
        visGetSelectedEdges()
    }) # Ya lo hemos hecho antes
    '

        # Si detectamos algún cambio en la tabla tomamos los nodos seleccionados
        observeEvent(eventExpr = input$Tab_edges_cell_clicked, ignoreNULL = FALSE, {
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedNodes() %>%
                visGetSelectedEdges()
        })


        # Esconder/ mostrar
        observeEvent(eventExpr = input$E_hidden, {
            if (!is.null(edges)) {
                # S_edges = select_edges( )
                S_edges <- edges$id
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(id = S_edges, hidden = input$E_hidden))
            }
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #        Tamaño y forma        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



        # Arrow
        observeEvent(eventExpr = input$E_direction, {
            if (!is.null(edges)) {
                S_edges <- Select_edges()
                visNetworkProxy(ns("network_proxy")) %>%
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
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        dashes = input$E_dashes
                    ))
            }
        })


        # Width
        observeEvent(eventExpr = input$E_width, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el prametro por defecto
                edges_info[N_edges, "scaling.min"] <<- input$E_width
                edges_info[N_edges, "scaling.max"] <<- input$E_width
                edges_info[N_edges, "width"] <<- input$E_width


                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("scaling", colnames(edges_info))]
                    ))
            }
        })


        # Selected Width
        observeEvent(eventExpr = input$E_selected_width, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        selectionWidth = input$E_selected_width
                    ))
            }
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #            Color             #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



        # Color
        observeEvent(eventExpr = input$E_color, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "color.color"] <<- input$E_color

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("color.", colnames(edges_info))]
                    ))
            }
        })


        # Highlight
        observeEvent(eventExpr = input$E_color_highlight, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los edges
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "color.highlight"] <<- input$E_color_highlight

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("color.", colnames(edges_info))]
                    ))
            }
        })


        # Opacity
        observeEvent(eventExpr = input$E_opacity, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los edges
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "color.opacity"] <<- input$E_opacity

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("color.", colnames(edges_info))]
                    ))
            }
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #           Labels             #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Show labels
        observeEvent(eventExpr = input$E_label, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los edges
                S_edges <- edges$id

                '# Cambiamos el valor en la tabla
        nodes_info[N_nodes, "label"] <<- nodes_info[N_nodes, "value"]

        # Cambiamos los valores en la red
        visNetworkProxy("network_proxy") %>%
          visUpdateNodes(nodes= data.frame(id = S_nodes,
                                           nodes_info[N_nodes, grep("color.", colnames(nodes_info)) ] ))
        '
                if (isTRUE(input$E_label)) {
                    if (is.null(input$Value_decimals)) {
                        lab <- edges_info$value
                    } else {
                        lab <- sapply(X = edges_info$value, FUN = signif, input$Value_decimals)
                    }

                    edges_info$label <<- sapply(X = lab, FUN = as.character)

                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateEdges(edges = data.frame(
                            id = edges_info$id,
                            label = edges_info$label
                        ))
                } else {
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateEdges(edges = data.frame(id = S_edges, label = " "))
                }
            }
        })


        # Color
        observeEvent(eventExpr = input$E_label_color, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "font.color"] <<- input$E_label_color

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("font.", colnames(edges_info))]
                    ))
            }
        })


        # Tamaño
        observeEvent(eventExpr = input$E_label_size, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el tamaño por defecto
                edges_info[N_edges, "font.size"] <<- input$E_label_size

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("font.", colnames(edges_info))]
                    ))
            }
        })


        # Fuente
        observeEvent(eventExpr = input$E_label_face, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()

                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos la fuente por defecto
                edges_info[N_edges, "font.face"] <<- input$E_label_face

                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("font.", colnames(edges_info))]
                    ))
            }
        })


        # Fondo
        observeEvent(eventExpr = input$E_label_strokeWidth, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()
                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "font.strokeWidth"] <<- input$E_label_strokeWidth
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("font.", colnames(edges_info))]
                    ))
            }
        })


        # Fondo color
        observeEvent(eventExpr = input$E_label_strokeColor, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()
                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "font.strokeColor"] <<- input$E_label_strokeColor
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[N_edges, grep("font.", colnames(edges_info))]
                    ))
            }
        })

        # Posición
        observeEvent(eventExpr = input$E_label_align, {
            if (!is.null(edges)) {
                # Tomamos los IDs de los nodos
                S_edges <- Select_edges()
                # Tomamos su posición en el data frame
                N_edges <- which(edges_info$id %in% S_edges)

                # Cambiamos el color por defecto
                edges_info[N_edges, "font.align"] <<- input$E_label_align
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = S_edges,
                        edges_info[
                            N_edges,
                            grep("font.", colnames(edges_info))
                        ]
                    ))
            }
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #            Sombra            #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



        ## Show shadow
        observe({
            if (!is.null(edges)) {
                if (isTRUE(input$E_show_shadow)) {
                    S_edges <- Select_edges()
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateEdges(edges = data.frame(
                            id = edges$id,
                            shadow = list(
                                enabled = input$E_show_shadow,
                                x = input$E_shadow_x,
                                y = input$E_shadow_y
                            )
                        ))
                } else {
                    visNetworkProxy(ns("network_proxy")) %>%
                        visUpdateEdges(edges = data.frame(
                            id = edges$id,
                            shadow = input$E_show_shadow
                        ))
                }
            }
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #         Parámetros físicos           #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Rebotar
        observeEvent(eventExpr = input$N_physics, { # Cambia todos
            if (!is.null(edges)) {
                # S_nodes = Select_nodes()
                visNetworkProxy(ns("network_proxy")) %>%
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
                visNetworkProxy(ns("network_proxy")) %>%
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
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = nodes$id,
                        fixed = list(
                            x = input$N_fix_x,
                            y = input$N_fix_y
                        )
                    ))
            }
        })


        ######
        # ·························································#
        #                                                         #
        #          Introducimos funciones de selecci�n            #
        #                                                         #
        # ·························································#


        # Selecionamos los nodos en funcion del grado
        observeEvent(eventExpr = input$Set_sel, {
            nod <- input$network_proxy_selectedNodes
            dir <- input$Select_direction
            gra <- input$Select_grade

            # Tomamos los nodos y edges
            S_nodes <- Get_all_connected(nod, dir, gra)
            S_edges <- Get_connected_edg(nod, dir)

            # Los selecciona
            visNetworkProxy(ns("network_proxy")) %>%
                visSetSelection(
                    nodesId = S_nodes,
                    edgesId = S_edges,
                    unselectAll = TRUE,
                    highlightEdges = FALSE
                )

            # Actualizamos la informaci�n
            visNetworkProxy(ns("network_proxy")) %>%
                visGetSelectedNodes() %>%
                visGetSelectedEdges()
        })


        ####


        # ·························································#
        #                                                         #
        #        Filtrado en base a criterios de usuario          #
        #                                                         #
        # ·························································#


        ### Funciones básicas ###

        ## Funciones de enfatizado

        # Enfatizar red en base a los nodos
        Emphasize_N <<- function(S_nodes) {

            # if ( isTRUE(Tab_inputs$write)){

            H_color <- "rgba(200,200,200,0.5)"

            S_edges <- edges[(which(edges$from %in% S_nodes & edges$to %in% S_nodes)), "id"] # Edges a resaltar
            Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"] # Nodos a enmascarar
            Hide_edges <- edges[which(!edges$id %in% S_edges), "id"] # Edges a enmascarar

            if (length(Hide_nodes) > 0) {
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = Hide_nodes,
                        color = H_color, label = ""
                    )) %>%
                    visUpdateEdges(edges = data.frame(
                        id = Hide_edges,
                        color = H_color, label = " "
                    ))
            } # }
        }

        # Enfatizar red en base a los edgess
        Emphasize_E <<- function(S_edges) {

            # if ( isTRUE(Tab_inputs$write)){

            H_color <- "rgba(200,200,200,0.5)"

            Sn <- edges[(which(edges$id %in% S_edges)), c("from", "to")] # Edges a resaltar
            S_nodes <- unique(append(Sn$from, Sn$to))
            Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"] # Nodos a enmascarar
            Hide_edges <- edges[which(!edges$id %in% S_edges), "id"] # Edges a enmascarar
            if (length(Hide_edges) > 0) {
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = Hide_nodes,
                        color = H_color, label = ""
                    )) %>%
                    visUpdateEdges(edges = data.frame(
                        id = Hide_edges,
                        color = H_color, label = " "
                    ))
            }
            # }
        }


        ## Funciones para mostrar unicmante los nodos deseados

        Hide_N <<- function(S_nodes) {

            # if ( isTRUE(Tab_inputs$write)){
            S_edges <- edges[(which(edges$from %in% S_nodes | edges$to %in% S_nodes)), "id"] # Edges a resaltar
            Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"] # Nodos a enmascarar
            Hide_edges <- edges[which(!edges$id %in% S_edges), "id"] # Edges a enmascarar

            if (length(Hide_edges) > 0 && length(Hide_nodes) > 0) {
                # Cambiamos los valores en la red
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = Hide_nodes,
                        hidden = TRUE
                    ))
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateEdges(edges = data.frame(
                        id = Hide_edges,
                        hidden = TRUE
                    ))
            }
            # }
        }


        Hide_E <<- function(S_edges) {

            # if ( isTRUE(Tab_inputs$write)){
            Sn <- edges[(which(edges$id %in% S_edges)), c("from", "to")] # Edges a resaltar
            S_nodes <- unique(append(Sn$from, Sn$to))
            Hide_nodes <- nodes[which(!nodes$id %in% S_nodes), "id"] # Nodos a enmascarar
            Hide_edges <- edges[which(!edges$id %in% S_edges), "id"] # Edges a enmascarar

            if (length(Hide_edges) > 0 && length(Hide_nodes) > 0) {
                # Cambiamos los valores en la red
                visNetworkProxy(ns("network_proxy")) %>%
                    visUpdateNodes(nodes = data.frame(
                        id = Hide_nodes,
                        hidden = TRUE
                    ))
                visNetworkProxy(ns("network_proxy")) %>%
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
                edg <- edges[edges[, dir] %in% nod, "id"]
                'if (dir == "From") {
          edg <- edges[edges$from %in% nod, "id"]
        } else {
          if (dir == "To") {
            edg <- edges[edges$to %in% nod, "id"]
          }
        }'
            }
            return(unique(edg))
        }

        Get_all_connected <- function(nod, dir, gra) { # Toma todos los edges unidos en una dirección separados a un grado gra
            n <- nod # Tomamos los nodos introducidos
            connected_nodes <- c(n) # Tomamos los nodos seleccionados inicialmente, en caso contrario, si es resto es null no saldrian
            # connected_edges = c()

            for  (i in 1:gra) {
                edg <- Get_connected_edg(n, dir)
                n <- append(edges[edges$id %in% edg, "from"], edges[edges$id %in% edg, "to"])
                # connected_edges = append(connected_edges, unique(edg))
                connected_nodes <- append(connected_nodes, unique(n))
            }
            # conections = list( nod = connected_nodes, edg = connected_edges)
            # return(conections)
            return(connected_nodes)
        }


        ## Anular el enfatizado

        # Deseleccionar nodos
        Undo_N <<- function() {
            H_color <- "rgba(200,200,200,0.5)"

            if (isTRUE(input$N_label)) {
                visNetworkProxy(ns("network_proxy")) %>%
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
                visNetworkProxy(ns("network_proxy")) %>%
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


        # Deseleccionar edges
        Undo_E <<- function() {
            H_color <- "rgba(200,200,200,0.5)"

            if (isTRUE(input$E_label)) {
                visNetworkProxy(ns("network_proxy")) %>%
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
                visNetworkProxy(ns("network_proxy")) %>%
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

        # Deshacer
        Undo <<- function() {
            # if ( isTRUE(Tab_inputs$write)){
            if (!is.null(nodes_info)) { # Evitamos que se ejecute durente la creación de la red
                Undo_N()
                Undo_E()
                # }
            }
        }


        # Tomamos el numero de conexiones de cada nodo, lo hacemos una funcion reactiva, es suficnete que lo calcule una vez por datos
        Get_conection_num <- reactive({
            edg <- Tab_inputs$edges
            nod <- unlist(Tab_inputs$nodes["id"])
            # Cuenta el numero de interacciones
            count <- function(nod, dir) {
                total <- sum(edg[dir] == nod)
                return(total)
            }
            # Calculamos el numero de conexiones
            f <- sapply(nod, count, "from")
            t <- sapply(nod, count, "to")
            total <- list()
            for (i in 1:length(nod)) {
                tot <- f[i] + t[i]
                total[[i]] <- tot
            }

            # aniadimos la informacion a un DF
            nodes_count <- data.frame("id" = nod, "from" = f, "to" = t, "All" = unlist(total))
            return(nodes_count)
        })

        # Obtiene los componenetes o subgrafos de la red
        # Obtiene los componenetes o subgrafos de la red (subgraph)
        Get_components <- reactive({ # Como contar los componentes https://stackoverflow.com/questions/72780103/r-count-number-of-subgraphs-components-in-a-graph
            # Introduciendo la informaci�n de los edges y nodos con igraph obtenemos los componentes
            edg <<- Tab_inputs$edges[, c("from", "to", "id", "value")]
            nod <<- Tab_inputs$nodes[, c("id", "label")]
            comp <- igraph::decompose(graph_from_data_frame(d = edg, vertices = nod))

            # comp <- igraph::decompose(graph_from_data_frame( d= edges_out, vertices = nodes_out))

            # Comp es una lista de cada uno de los componenetes (subgrafos)
            get_data <- function(graph, info) {
                df <- igraph::as_data_frame(graph, what = info)
                df
                # df_edg = data.frame(from = df$from_name, to = df$to_name,
                #                   value = df$value, id = df$id,
                #                  label = df$label)
            }
            subgraph_edg <- (lapply(comp, get_data, "edges"))
            subgraph_nod <- (lapply(comp, get_data, "vertices"))
            # Obtenemos numerados todos los subgrafos
            names(subgraph_edg) <- as.character(1:length(subgraph_edg))
            names(subgraph_nod) <- as.character(1:length(subgraph_nod))
            Filt$subgraph_edg <- subgraph_edg
            Filt$subgraph_nod <- subgraph_nod
        })

        "Get_components <- reactive({ # Como contar los componentes https://stackoverflow.com/questions/72780103/r-count-number-of-subgraphs-components-in-a-graph
      # Creamos un algoritmo que recorra la red e introduzca en grupos

      comp <- igraph::decompose(graph_from_data_frame( d= Tab_inputs$edges, vertices = Tab_inputs$nodes))

      comp <- igraph::decompose(graph_from_data_frame( d= edges_out, vertices = nodes_out))

      # Comp es una lista de cada uno de los componenetes (subgrafos)
      comp[[1]]
      class(comp[[1]])
      plot(comp[[1]])
      names(comp[[1]]$name)
      View(comp)
      View(V(comp[[1]]))
      View(comp[[2]])

      ig <- graph_from_data_frame(edges_out)
      igraph::count_components(ig) View(count_components)
      #data <- toVisNetworkData(igra
    })"


        # Enfatixado y mostrado

        Emphasize_group <- function(nod, dir, gra) { # Enfatiza los nodos conectados a un grupo en una dirección y grado
            if (!is.null(gra) && gra > 0) {
                nodes <- Get_all_connected(nod, dir, gra)
                Emphasize_N(nodes)
            } else {
                nodes <- nod
                Emphasize_N(nodes)
            }
        }

        Show_group <- function(nod, dir, gra) { # Muestra solo los nodos conectados a un grupo en una dirección y grado
            if (!is.null(gra) && gra > 0) {
                nodes <- Get_all_connected(nod, dir, gra)
                Hide_N(nodes)
            } else {
                nodes <- nod
                Hide_N(nodes)
            }
        }


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #   Filtrado por grupos predefinidos     #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        ##################################################

        # En el menú de filtado izquierdo permite definir
        # grupos de nodos y posteriormente enfatizarlos

        ##################################################

        Filt <- reactiveValues(
            sel_tab_n = NULL,
            sel_tab_e = NULL,
            sel_menu = NULL,
            sel_by = NULL,
            grade = 0,
            number = 0,
            direction = "All",
            sel_subgraph = NULL,
            show_sugraph = NULL,
            subgraph_edg = NULL,
            subgraph_nod = NULL
        )


        # Opciones del filtrado: Grado
        # observeEvent( eventExpr = input$Filter_by, ignoreNULL = FALSE, {
        # print("*455")
        output$Filter_grade <- renderUI(
            # Mostramos el grado unicmante si esta selecionado unos de lso valores de filt_menu y si filt_menu_by es group
            if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) { # Condiciones del menu
                if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) { # Ciondiciones del filtrado by
                    if (input$Filter_Menu_By == "By_sel" || (!is.null(input$Filter_by) && input$Filter_by != "All" && input$Filter_by != "None")) {
                        # Escogemos el grado de enfasis
                        sliderInput(
                            inputId = ns("Filter_grade"), label = "Grade",
                            min = 0, max = 5, step = 1, value = Filt$grade
                        )
                    }
                }
            } else {
                NULL
            }
        )
        # })

        # Al cambiar el grado por el usuario lo almacenamos en la variable
        observeEvent(eventExpr = input$Filter_grade, ignoreNULL = TRUE, { # Cuando no aparece su valor es null, no nos interesa cambiar esta variable a Null
            Filt$grade <- input$Filter_grade
        })


        # Opciones del filtrado: Direccion
        # observeEvent( eventExpr = input$Filter_grade, ignoreNULL = FALSE, {
        output$Filter_direction <- renderUI(
            if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) { # Condiciones del menu
                if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) { # Ciondiciones del filtrado by
                    if (input$Filter_Menu_By != "By_group" || (!is.null(input$Filter_by) && input$Filter_by != "All" && input$Filter_by != "None")) {
                        # if ( input$Filter_grade > 0){
                        # Escogemos la dirección
                        radioButtons(
                            inputId = ns("Filter_direction"), label = "Direction",
                            choices = c("All" = "All", "Parents" = "to", "Children" = "from"), selected = Filt$direction,
                            inline = TRUE
                        )
                    }
                }
            } else {
                NULL
            }
            # } else{
            #  NULL
            # }
        )
        # })

        # Al cambiar la ditrecci?n por el usuario lo almacenamos en la variable
        observeEvent(eventExpr = input$Filter_direction, ignoreNULL = TRUE, { # Cuando no aparece su valor es null, no nos interesa cambiar esta variable a Null
            Filt$direction <- input$Filter_direction
        })

        # Opciones del filtrado: Numero de nodos
        # observeEvent( eventExpr = input$Filter_grade, ignoreNULL = FALSE, {
        output$Filter_number <- renderUI(
            if ((!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1)) { # Condiciones del menu
                if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) { # Ciondiciones del filtrado by
                    if (input$Filter_Menu_By == "By_num") {
                        # if ( input$Filter_grade > 0){
                        # Determinamos el numero maximo en funcion del numero de conexiones
                        max <- max(Get_conection_num()[, "All"])
                        sliderInput(
                            inputId = ns("Filter_number"), label = "Minimum number of conecctions",
                            min = 0, max = 20, step = 1, value = Filt$number
                        )
                    }
                }
            } else {
                NULL
            }
        )

        # Al cambiar el hnumero minimo de conexones por el usuario lo almacenamos en la variable
        observeEvent(eventExpr = input$Filter_number, ignoreNULL = TRUE, { # Cuando no aparece su valor es null, no nos interesa cambiar esta variable a Null
            Filt$number <- input$Filter_number
        })


        # Pop-up que permite definir un grupo de nodos seleccionados al pulsar Add group
        observeEvent(eventExpr = input$Add_sel_group, {
            G_nodes <<- Select_nodes()
            runjs(sprintf("document.getElementById('title2').innerHTML = '%s';
                                   document.getElementById('content2').innerHTML = '%s'; 
                                   document.getElementById('name2').value = '%s';
                                   document.getElementById('GroupModal').style.display = 'block';",
                          "New group:", "Set group name:", ""))
            'shinyalert::shinyalert(
                inputId = ns("Alert_Add_group"),
                title = "New group",
                text = "Set group name:",
                type = "input",
                inputType = "text",
                showConfirmButton = TRUE,
                showCancelButton = TRUE
            )'
        })

        # Adición de grupo a partir de una lista de nodos
        observeEvent(eventExpr = input$Add_input_group, {
            # Tomamos los nodos introducidos
            nodes <- str_replace_all(input$nodes_input, c("/" = ".", " " = ".", "-" = "."))
            nodes <- strsplit(nodes, ",")[[1]]
            for (i in 1:length(nodes)) {
                nodes[i] <- sub("/", ".", nodes[i])
                nodes[i] <- sub(" ", ".", nodes[i])
                nodes[i] <- str_replace(nodes[i], "-", ".")
            }
            G_nodes <<- nodes

            # Alerta que añade el título
            runjs(sprintf("document.getElementById('title2').innerHTML = '%s';
                                   document.getElementById('content2').innerHTML = '%s'; 
                                   document.getElementById('name2').value = '%s';
                                   document.getElementById('GroupModal').style.display = 'block';",
                          "New group:", "Set group name:", ""))
            'shinyalert::shinyalert(
                inputId = ns("Alert_Add_group"),
                title = "New group",
                text = "Set group name:",
                type = "input",
                inputType = "text",
                showConfirmButton = TRUE,
                showCancelButton = TRUE
            )'
        })


        # Pop-up creacion grupo
        observeEvent(eventExpr = input$Alert_Add_group, {
            if (!isFALSE(input$Alert_Add_group)) {
                # S_nodes = Select_nodes()
                Groups <<- c(list(G_nodes), Groups)

                names(Groups)[1] <<- input$Alert_Add_group }
                
                'shinyalert(
                    inputId = ns("Correct_Add_group"),
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
                    inputId = ns("Error_Add_group"),
                    text = "The group has not been created",
                    type = "error",
                    closeOnClickOutside = TRUE,
                    showCancelButton = TRUE,
                    showConfirmButton = FALSE,
                    cancelButtonText = "Close"
                )
            }'
        })


        # Pop-up de eliminación de grupos
        observeEvent(eventExpr = input$remove_group, ignoreNULL = TRUE, {
          # Alerta que añade el título
          print("removed") 
          runjs(sprintf(paste0("document.getElementById('title3').innerHTML = 'Do you want to remove the group %s ?'; 
                                  Shiny.onInputChange('", ns("Alert_Remove_group"),"', false);
                                   document.getElementById('RemoveModal').style.display = 'block';"), input$Filter_by))
          #runjs(sprintf("document.getElementById('title3').innerHTML = '%s';
           #                        document.getElementById('content3').innerHTML = '%s'; 
            #                       document.getElementById('RemoveModal').style.display = 'block';",
             #           "?", ask))
            # Alerta de eliminación de nodos
            'shinyalert::shinyalert(
                inputId = ns("Alert_Remove_group"),
                text = ask
                # text = paste("¿Quiere eliminar el grupo:", input$Filter_by),
                type = "warning",
                # inputType = "text",
                showConfirmButton = TRUE,
                showCancelButton = TRUE
            )'
        })
        
        observe({ 
          print("Alert:")
          print(input$Alert_Remove_group)
        })


        # Eliminación de grupo
        observeEvent(eventExpr = input$Alert_Remove_group, {
            if (isTRUE(input$Alert_Remove_group)) {
              print("eliminado")
                # Eliminamos el grupo
                Groups <<- Groups[which(names(Groups) != input$Filter_by)]
                output$Filter_by <- renderUI(
                  # if(! is.null(input$Filter_Menu)){
                  if (!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1) { # Al generarse es null, mientras que al cambiar de opcion debemos recordar que se toman varias
                    if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
                      if (input$Filter_Menu_By == "By_group") {
                        # if (length(names(Groups)) > 0){
                        # if(input$Filter_Menu != "Emph_sel"){
                        selectInput(
                          inputId = ns("Filter_by"),
                          label = "Filter by",
                          # selected = "All", # All muestra todos los nodos y edges
                          choices = names(Groups)
                        )
                      }
                    }
                  } else {
                    NULL
                  }
                  # }
                )

                '# Alertas
                shinyalert(
                    inputId = ns("Correct_Remove_group"),
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
                    inputId = ns("Error_Remove_group"),
                    text = "The group has not been removed",
                    type = "error",
                    closeOnClickOutside = TRUE,
                    showCancelButton = TRUE,
                    showConfirmButton = FALSE,
                    cancelButtonText = "Close"
                )'
            }
        })


        # Panel de selección de grupo predefinido
        observe({
            input$Alert_Add_group
            #input$Correct_Remove_group
            #
            output$Filter_by <- renderUI(
                # if(! is.null(input$Filter_Menu)){
                if (!is.null(input$Filter_Menu) && length(input$Filter_Menu) == 1) { # Al generarse es null, mientras que al cambiar de opcion debemos recordar que se toman varias
                    if (!is.null(input$Filter_Menu_By) && length(input$Filter_Menu_By) == 1) {
                        if (input$Filter_Menu_By == "By_group") {
                            # if (length(names(Groups)) > 0){
                            # if(input$Filter_Menu != "Emph_sel"){
                            selectInput(
                                inputId = ns("Filter_by"),
                                label = "Filter by",
                                # selected = "All", # All muestra todos los nodos y edges
                                choices = names(Groups)
                            )
                        }
                    }
                } else {
                    NULL
                }
                # }
            )
        })



        "observeEvent(eventExpr = input$Filter_by, {
      Filt$groups <- input$Filter_by
    })"

        # Panel de eliminación de grupo
        # observe({
        # input$Filter_by
        # print(input$Filter_by)
        # print("borrado")
        #
        output$Remove_group <- renderUI(
            if (!is.null(input$Filter_by) && (!is.null(input$Filter_Menu)) && !is.null(input$Filter_Menu_By)) {
                if (input$Filter_Menu_By == "By_group" && input$Filter_by != "None" && input$Filter_by != "All") {
                    Filt$groups <- input$Filter_by
                    actionButton(
                        inputId = ns("remove_group"),
                        icon("remove"),
                        style = " padding: 4px; border-radius: 2px; color: #fff; background-color: #A80215"
                    )
                }
            } else {
                NULL
            }
            # }
        )
        # })


        # Menu
        # Al seleccionar este filtro eliminamos el resto
        observeEvent(eventExpr = input$Filter_Subgraph, ignoreNULL = FALSE, {
          print("deselect")
            if (!is.null(input$Filter_Subgraph)) {
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Tab_N"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Tab_E"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Menu"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                # Fijamos los valores de Filt$ tambien en nulo
                Filt$sel_tab_e <- NULL
                Filt$sel_tab_n <- NULL
                Filt$sel_menu <- NULL
            }

            if (length(input$Filter_Subgraph) == 2) {
                Sel <- input$Filter_Subgraph[which(input$Filter_Subgraph != Filt$sel_subgraph)]
            } else {
                Sel <- input$Filter_Subgraph
            }
            updateCheckboxGroupInput(inputId = ns("Filter_Subgraph"), selected = Sel)
            Filt$sel_subgraph <<- Sel
        })

        # Criterio
        observeEvent(eventExpr = input$Filter_Menu_By, ignoreNULL = FALSE, { # en este caso no modificamos el resto de casillas, pero no dejamos que este tenga m�s de un valor

            if (length(input$Filter_Menu_By) == 2) {
                Sel <- input$Filter_Menu_By[which(input$Filter_Menu_By != Filt$sel_by)]
            } else {
                Sel <- input$Filter_Menu_By
            }
            updateCheckboxGroupInput(inputId = ns("Filter_Menu_By"), selected = Sel)
            Filt$sel_by <<- Sel
        })

        # Subgraph
        # Al seleccionar este filtro eliminamos el resto
        observeEvent(eventExpr = input$Filter_Menu, ignoreNULL = FALSE, {
            if (!is.null(input$Filter_Menu)) {
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Tab_N"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Tab_E"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Subgraph"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                # Fijamos los valores de Filt$ tambien en nulo
                Filt$sel_tab_e <- NULL
                Filt$sel_tab_n <- NULL
                Filt$sel_subgraph <- NULL
            }

            if (length(input$Filter_Menu) == 2) {
                Sel <- input$Filter_Menu[which(input$Filter_Menu != Filt$sel_menu)]
            } else {
                Sel <- input$Filter_Menu
            }
            updateCheckboxGroupInput(inputId = ns("Filter_Menu"), selected = Sel)
            Filt$sel_menu <<- Sel
        })

        # Nodes
        observeEvent(eventExpr = input$Filter_Tab_N, { # No queremos que entren nulos
            if (!is.null(input$Filter_Tab_N)) {
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Menu"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Tab_E"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Subgraph"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )

                # Fijamos los valores de Filt$ tambien en nulo
                Filt$sel_tab_e <- NULL
                Filt$sel_menu <- NULL
                Filt$sel_subgraph <- NULL
            }

            if (length(input$Filter_Tab_N) == 2) {
                Sel <- input$Filter_Tab_N[which(input$Filter_Tab_N != Filt$sel_tab_n)]
            } else {
                Sel <- input$Filter_Tab_N
            }
            updateCheckboxGroupInput(inputId = ns("Filter_Tab_N"), selected = Sel)
            # print(Filt$sel_tab_n)
            Filt$sel_tab_n <<- Sel
            # print(Filt$sel_tab_n)
        })

        # Edges
        observeEvent(eventExpr = input$Filter_Tab_E, {
            if (!is.null(input$Filter_Tab_E)) {
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Tab_N"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = "Filter_Menu",
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                updateCheckboxGroupInput(
                    inputId = ns("Filter_Subgraph"),
                    choices = list("Emphasize" = "Emph", "Show/Hide" = "S/H"),
                    selected = NULL
                )
                # Fijamos los valores de Filt$ tambien en nulo
                Filt$sel_tab_n <- NULL
                Filt$sel_menu <- NULL
                Filt$sel_subgraph <- NULL
            }
            if (length(input$Filter_Tab_E) == 2) {
                Sel <- input$Filter_Tab_E[which(input$Filter_Tab_E != Filt$sel_tab_e)]
            } else {
                Sel <- input$Filter_Tab_E
            }
            updateCheckboxGroupInput(inputId = ns("Filter_Tab_E"), selected = Sel)
            Filt$sel_tab_e <<- Sel
        })

        'observe({
      print("*20")
      #print(Filt$sel_menu)
      #print("Cambiando la seleccion")
      if (! is.null(Filt$sel_menu)){
        Undo()
        if( Filt$sel_menu == "Emph_sel"){
          S_nodes = input$network_proxy_selectedNodes
          Emphasize_N(S_nodes)
        } else{
          # Tomamos los IDs correpondientes al grupo escogido
          S_nodes = unlist(Groups[input$Filter_by])
          if( Filt$sel_menu == "Emph"){
            Emphasize_N(S_nodes)
          } else{
            if( Filt$sel_menu == "S/H"){
              Hide_N(S_nodes)
            }
          }}
      }
    })'

        # MENU
        # En funcion de los filtros escogemos si enfatizamos o escondemos y bajo que criterios
        observe({
            # print(Filt$sel_menu)
            # print("Cambiando la seleccion")
            # Definimos la dirección y el grado:
            dir <- ifelse(is.null(input$Filter_direction), "All", input$Filter_direction)
            gra <- input$Filter_grade

            if (!is.null(Filt$sel_menu) && !is.null(Filt$sel_by)) {
                Undo()
                # En funcion de filter_Menu_By escogemos el criterio por el que se crean lso grupos
                if (Filt$sel_by == "By_sel") {
                    nod <- input$network_proxy_selectedNodes
                } else if (Filt$sel_by == "By_group") {
                    nod <- unlist(Groups[input$Filter_by])
                } else if (Filt$sel_by == "By_num") {
                    min <- ifelse(is.null(input$Filter_number), 0, input$Filter_number)
                    nodes_count <- Get_conection_num()
                    # nod0 = nodes_count[c("id",dir)]
                    # print(nod0)
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


        'observeEvent(input$network_proxy_selectedNodes,{
      input$Filter_grade
      S_nodes = input$network_proxy_selectedNodes
      Get_nodes_by_grade(S_nodes, 1, "All")
    })'

        # SUBGRAPH

        output$Filter_by_subgraphs <- renderUI({
            if (!is.null(input$Filter_Subgraph) && length(input$Filter_Subgraph) == 1) {
                Get_components()
                if (!is.null(Filt$subgraph_edg) || !is.null(Filt$subgraph_nod)) {
                    selectInput(
                        inputId = ns("Filter_by_subgraphs"),
                        label = "Select subgraph",
                        multiple = TRUE, # All muestra todos los nodos y edges
                        choices = names(Filt$subgraph_nod),
                        selected = Filt$show_sugraph
                    )
                }
            }
        })


        observeEvent(eventExpr = input$Filter_Subgraph, ignoreNULL = FALSE, {
            Filt$show_sugraph <<- input$Filter_by_subgraphs
        })


        Get_components_size <- reactive({
            num_subn <<- sapply(Filt$subgraph_nod, nrow)
            min_subgh_size <<- min(num_subn)
            max_subgh_size <<- max(num_subn)
            # num_sube <<- sapply(Filt$subgraph_edg, nrow)
        })

        'observeEvent( eventExpr =Filt$subgraph_nod, ignoreNULL = FALSE, ignoreInit = TRUE,{ # Cuando cmabiamos los subgrafos se calcula el tama�o
      print("Reclaculando el tama�o")
      Get_components_size()
      # Al crear el objeto determinamos un m�nimo y maximo
      min_subgh_size <<- min(num_subn)
      max_subgh_size <<- max(num_subn)
      print(min_subgh_size)
      print(max_subgh_size)
    } )'

        output$Filter_subgraph_by_num <- renderUI({ # Slider que detenima el numero minimo y m�ximo de subgrafos
            if (!is.null(input$Filter_Subgraph) && length(input$Filter_Subgraph) == 1) {
                Get_components()
                if (!is.null(Filt$subgraph_edg) || !is.null(Filt$subgraph_nod)) {
                    Get_components_size()
                    min <- min_subgh_size
                    max <- max_subgh_size
                    sliderInput(
                        inputId = ns("Filter_subgraph_by_num"),
                        label = "Discard subraphs by size",
                        min = min(num_subn),
                        max = max(num_subn),
                        value = c(min, max),
                        step = 1
                    )
                }
            }
        })

        observeEvent(eventExpr = input$Filter_subgraph_by_num, ignoreNULL = FALSE, ignoreInit = TRUE, {
            sel <- list()
            min_subgh_size <<- input$Filter_subgraph_by_num[1] # Guardamos los cambios del usuario en las variables globales
            max_subgh_size <<- input$Filter_subgraph_by_num[2]
            # Comprobamos el tama�o de las redes
            for (i in 1:length(Filt$subgraph_nod)) {
                len_subgp <- nrow(Filt$subgraph_nod[[i]])
                if (len_subgp >= min_subgh_size && len_subgp <= max_subgh_size) {
                    sel <- append(sel, as.character(i))
                }
            }
            Filt$show_sugraph <- sel
        })

        # En funcion de los filtros escogemos si enfatizamos o escondemos unos subgrafos
        observe({ # En funcion de los que esta seleccionado en el nput$Filter_by_subgraphs seleccionamos unos subgrafos

            if (!is.null(Filt$sel_subgraph)) {
                Undo()

                if (length(input$Filter_by_subgraphs) != length(Filt$subgraph_nod)) {
                    nod <- c()
                    edg <- c()
                    for (i in input$Filter_by_subgraphs) {
                        nod <- append(nod, Filt$subgraph_nod[[i]][, 1])
                        edg <- append(edg, Filt$subgraph_edg[[i]]$id)
                    }

                    if (Filt$sel_subgraph == "Emph") {
                        Emphasize_N(nod)
                        Emphasize_E(edg)
                    } else {
                        Hide_N(nod)
                        Hide_E(edg)
                    }
                }
            }
        })


        # Al hacer click sobre alguna función de filtrado se activa, si todas son null, se deshace el filtrado
        observe({
            if (is.null(input$Filter_Tab_E) && is.null(input$Filter_Tab_N) && (is.null(input$Filter_Menu) && is.null(input$Filter_Subgraph) || (!is.null(input$Filter_Menu) && is.null(input$Filter_Menu_By)))) {
                # print("3 UNDO")
                # if(isTRUE(Tab_inputs$write)){
                visNetworkProxy(ns("network_proxy")) %>%
                    visRedraw()
                Undo() # }
            }
        })


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #     Filtrado en base a la tabla        #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        ##################################################

        # Enfatiza los nodos y edges a partir del filtrado
        # de la tabla Nodes

        ##################################################

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Nodos                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        observe({
            if (!is.null(Filt$sel_tab_n) && length(Filt$sel_tab_n) == 1) {
                Undo()
                S_nodes <- Ord_nod_tab[input$Tab_nodes_rows_all, "id"] # Nodos a resaltar
                if (Filt$sel_tab_n == "Emph") {
                    # Enfatizar:
                    Emphasize_N(S_nodes)
                } else {
                    if (Filt$sel_tab_n == "S/H") {
                        Hide_N(S_nodes)
                    }
                }
            }
        })


        "observeEvent( eventExpr = Filt$sel_tab_n, ignoreNULL = FALSE,{
      if(! is.null(nodes_info)){
        Undo()
      }
    })"

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #                Edges                   #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        observe({
            # print("filt_tab_e")
            if (!is.null(Filt$sel_tab_e) && length(Filt$sel_tab_e == 1)) {
                Undo()
                S_edges <- Ord_edg_tab[input$Tab_edges_rows_all, "id"] # Nodos a resaltar
                if (Filt$sel_tab_e == "Emph") {
                    # Enfatizar:
                    Emphasize_E(S_edges)
                } else {
                    if (Filt$sel_tab_e == "S/H") {
                        Hide_E(S_edges)
                    }
                }
            }
        })


        "observeEvent( eventExpr = Filt$sel_tab_e, ignoreNULL = FALSE,{
      if(! is.null(edges_info)){
        Undo()
      }
    })"


        # ·························································#
        #                                                         #
        #                 Exportación de la red                   #
        #                                                         #
        # ·························································#



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #         Captura de pantalla            #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



        # Captura de pantalla de la red actual con shinyscreenshot.
        observeEvent(eventExpr = input$Capture, {
            shinyscreenshot::screenshot(
                selector = "#network_proxy",
                filename = "network_out"
            )
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #         Guardado de la red             #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Obtención de la información de la red tras pulsar el botón de guardado
        observeEvent(eventExpr = input$Save, {
            Get_nodes()
            Get_edges()
        })

        # Previsualización de la red para el guardado en HTML/PNG/JPG/PDF
        output$network_save <- renderVisNetwork({
            nodes_out <<- Nodes_info()
            edges_out <<- Edges_info()

            if (input$Out_type == "HTML") {
                # Previsualizacion formato HTML
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
                    visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE, zoomSpeed = 0.1) %>%
                    visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE)
            } else {
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
                    visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE, zoomSpeed = 0.1) %>%
                    visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE, autoResize = TRUE) %>%
                    # Guardado en png/jpeg/pdf
                    visExport(
                        type = input$Out_type, name = input$Out_name, float = "right",
                        style = DT$st
                    )
            }
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #         PNG/JPEG/PDF         #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Añadir titulo
        observe({
            visNetworkProxy(ns("network_save")) %>%
                visSetTitle(
                    main = list(text = input$Title_main),
                    submain = list(text = input$Title_submain),
                    footer = list(text = input$Title_footer)
                )
        })



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #             HTML             #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # Aparición del botón de guardado en HTML
        output$HTML_Button <- renderUI({
            if (input$Out_type == "HTML") {
                absolutePanel(actionButton(
                    inputId = ns("Save_HTML"),
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


        # Guardado en HTML:
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


        # Pop-up guardado
        observeEvent(eventExpr = input$Save_HTML, {
            shinyalert(
                inputId = ns("Correct_Saved_HTML"),
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


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #                                        #
        #         Guardado en SVG                #
        #                                        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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
            input$network_proxy_nodes
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





        #width <- input$width
        #height <- input$height

        output$save_relationships <- downloadHandler(filename = "network_relationships.zip", content = function(fname) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            files <- NULL

            nodes <- str_replace_all(input$nodes_plot, c("/" = ".", " " = ".", "-" = "."))
            nodes <- strsplit(nodes, ",")[[1]]
            subgr <- bnlearn::subgraph(session_data$fittedbn, nodes)
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

        # Representación de la red:
        ### end of graph
    })
}