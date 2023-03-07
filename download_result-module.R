    get_result_list <- function(){
      all_folders <- list.dirs(deploy_dir, full.names = FALSE, recursive = FALSE)
      lsfiles <- file.info(paste0(deploy_dir, all_folders), extra_cols = FALSE)
      # lsfiles <- lsfiles[order(lsfiles$mtime,decreasing = TRUE),]
      all_folders <- all_folders[order(lsfiles$mtime, decreasing = TRUE)]
      all_folders
    }


download_result_ui <- function(id = "download_result_module") {
  ns <- NS(id)
  tabPanel(
    HTML("<b>Downloads</b>"),
    shinybusy::use_busy_spinner(spin = "fading-circle"),
    tags$label(h3("Experiment results")),
    # radioButtons("down_files","Download Files", choiceNames = as.list(list.files('/srv/shiny-server/samba/files/', full.names = FALSE)), choiceValues = as.list(list.files('/srv/shiny-server/samba/files/', full.names = FALSE)), selected = "")
    selectInput(ns("down_files"), "Download Files", as.list(get_result_list()), selected = ""),
    downloadButton(ns("downloadResults"), "Download"),
    # Button(ns("deleteResults"), "Delete")
    actionButton(inputId = ns("deleteResults"), label = "Delete", color = "primary", icon = icon("trash"))
  )
}

download_result_server <- function(shared_session_info, id = "download_result_module") {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    output$downloadResults <- downloadHandler(
      filename = function() {
        paste(input$down_files, ".zip", sep = "")
      },
      content = function(file) {
        files <- paste(deploy_dir, input$down_files, sep = "")

        zip::zipr(file, files)
      },
      contentType = "application/zip"
    )



    # Anything that calls autoInvalidate will automatically invalidate
    # every 2 seconds.
    autoInvalidate <- reactiveTimer(10000)

    observe({
      # Invalidate and re-execute this reactive expression every time the
      # timer fires.
      # browser()
      autoInvalidate()
      debug_msg("autoInvalidate")
      # Do something each time this is invalidated.
      pre_selection <- input$down_files
      updateSelectInput(session, "down_files",
        choices = as.list(get_result_list()),
        selected = pre_selection
      )
    })

    # updateSelectInput(session, "down_files",
    #   choices = as.list(get_result_list())
    # )
    observeEvent(input$deleteResults,{
    # show the spinner
        shinybusy::show_modal_spinner(
          text = "Please wait, Deleting Result"
        )
        tryCatch(
          {
            browser()
            selected_path <-  file.path(deploy_dir, input$down_files)
            unlink(selected_path, recursive = TRUE,force=TRUE)
            updateSelectInput(session, "down_files",
              choices = as.list(get_result_list())
            )
          },
          error = function(cond) {
              print(cond)
          },
          finally = {
            shinybusy::remove_modal_spinner()
          }
        )
    })

  })
}