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
        print("download button clicked")
        
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
      }, ignoreInit = TRUE)
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          paste(fileName, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          write_csv(data, file, progress = TRUE)
          
        }
      )
      
      
      output$downloadRDS <- downloadHandler(
        filename = function() {
          paste(fileName, "_", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          saveRDS(data, file = file)
        }
      )
      
    }
  )
}