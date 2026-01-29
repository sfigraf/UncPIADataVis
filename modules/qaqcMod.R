QAQC_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      # tabPanel("Released Tags with >1 dec_tag Detection", 
      #          DTOutput(ns("excessReleaseTable"))
      #          ), 
      tabPanel("Tags with >1 Entry in Release File", 
               DTOutput(ns("morethan1ReleaseTable"))
      ), 
      tabPanel("Unknown Tags", 
               sidebarLayout(
                 sidebarPanel(
                   DTOutput(ns("unknownTags"))
                 ), 
                 mainPanel(
                   DTOutput(ns("unknownDetections"))
                   )
               )
             )
    )
  
  )
}

QAQC_Server <- function(id, qaqcData) {
  moduleServer(
    id,
    function(input, output, session) {
      # output$excessReleaseTable <- renderDT({
      #   datatable(qaqcData$morethan1dec_tag,
      #             rownames = FALSE,
      #             caption = c("Tags in Release File that have more than 1 unique tag detection of dec_tag from Biomark data. In other words, 
      #             most urgent to address tags that are missing digits since they have detections.")
      #   )
      # })
      
      output$morethan1ReleaseTable <- renderDT({
        datatable(qaqcData$moreThan1ReleaseEntry,
                  rownames = FALSE,
                  caption = c("Tags in release file that have more than 1 entry in the release file.")
        )
      })
      
      output$unknownTags <- renderDT({
        uniqueUnknowntags <- qaqcData$detectionsWithoutReleaseData %>%
          distinct(dec_tag)
        
        datatable(uniqueUnknowntags,
                  rownames = FALSE,
                  caption = c("Uniqe tags in detection file without release data.")
        )
      })
      
      output$unknownDetections <- renderDT({
        datatable(qaqcData$detectionsWithoutReleaseData,
                  rownames = FALSE,
                  caption = c("All detections in detection file without release data.
                              Should mostly add up to missing rows in raw detection file vs what is displayed in filtered detection/usgs table.")
        )
      })
    }
  )
}