QAQC_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Released Tags with >1 dec_tag Detection", 
               DTOutput(ns("excessReleaseTable"))
               ), 
      tabPanel("Tags with >1 Entry in Release File", 
               DTOutput(ns("morethan1ReleaseTable"))
      )
    )
  
  )
}

QAQC_Server <- function(id, qaqcData) {
  moduleServer(
    id,
    function(input, output, session) {
      output$excessReleaseTable <- renderDT({
        datatable(qaqcData$morethan1dec_tag,
                  rownames = FALSE,
                  caption = c("Tags in Release File that have more than 1 unique tag detection of dec_tag from Biomark data. In other words, 
                  most urgent to address tags that are missing digits since they have detections.")
                  # extensions = c('Buttons'),
                  # #for slider filter instead of text input
                  # filter = 'top',
                  # options = list(
                  #   pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  #   dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                  #   language = list(emptyTable = "Enter inputs and press Render Table")
                  # )
        )
      })
      
      output$morethan1ReleaseTable <- renderDT({
        datatable(qaqcData$moreThan1ReleaseEntry,
                  rownames = FALSE,
                  caption = c("Tags in release file that have more than 1 entry in the release file.")
        )
      })
    }
  )
}