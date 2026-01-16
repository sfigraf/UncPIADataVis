downloadData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("downloadActionButton"), label = "Save Data"), 
    hr(),
  )
}

downloadData_Server <- function(id, data, fileName = "UncDataDownload") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$downloadActionButton, {

        showModal(modalDialog(
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadCSV"), "Download as CSV")
            )
          ), 
          br(), 
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadRDS"), "Download as RDS")
            )
          ),
          br(),
          
          footer = tagList(
            fluidRow(
              column(
                width = 12,
                align = "center", 
                modalButton("Cancel")
              )
            )
          ),
          
          easyClose = TRUE, 
          size = "s"
          
        ))
        #prevents modal from popping up again on re-render bc once it's been clicked once, input$downloadActionButton > 0 so it will re-initialize and show unless this is set to false
      }, ignoreInit = TRUE)
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          paste(filenameReactive, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          #grabs the current version of that data with this call using ()
          data <- if(is.reactive(data)) data() else data
          write_csv(data, file, progress = TRUE)
        }
      )
      
      output$downloadRDS <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          paste(filenameReactive, "_", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          #grabs the current version of that data with this call using ()
          data <- if(is.reactive(data)) data() else data
          saveRDS(data, file = file)
        }
      )
      
    }
  )
}