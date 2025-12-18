
environmentalData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(radioButtons(ns("YaxisSelect"), 
                                "Primary Y Axis Data",
                                choices = c("Detections", 
                                            "Discharge"),
                                selected = "Detections")
      ), 
      mainPanel(shinydashboard::box(title = "Detections and Discharge",
                                    width = 12, 
                                    plotlyOutput(ns("OverlayPlot")), 
                                    withSpinner(DT::DTOutput(ns("detectionDataTable"))),
                                    
      )
      )
    )
    
  )
}

environmentalData_Server <- function(id, USGSData, detectionData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$OverlayPlot <- renderPlotly({
        
        # if(!input$variableSelect2 %in% c("USGSDischarge", "USGSWatertemp")){
        #   line_color = ~Site
        #   nameOfLine = ~Site
        # } else{
        #   line_color = I("#87CEEB")
        #   nameOfLine = case_when(input$variableSelect2 == "USGSDischarge" ~ "USGS Discharge", 
        #                          input$variableSelect2 == "USGSWatertemp" ~ "USGS Water Temp (F)")
        # }
        
        line_color = I("#87CEEB")
        nameOfLine = "USGS Discharge"
      
        if(input$YaxisSelect == "Detections"){
          movYaxis = "y1"
          envYaxis = "y2"
          primaryYaxisName = "Detection Data (Daily Counts)"
          SecondaryYaxisName = "Discharge"
          
        } else{
          movYaxis = "y2"
          envYaxis = "y1"
          primaryYaxisName = "Discharge"
          SecondaryYaxisName = "Detection Data (Daily Counts)"
        }
        
        plot_ly() %>%
          add_trace(data = USGSData, x = ~Date,
                    y = ~Flow,
                    name = nameOfLine,
                    color = line_color, 
                    type = "scatter",
                    yaxis = envYaxis,
                    connectgaps = TRUE,
                    mode = "lines"
                    #colors = allColors
          ) %>%
          add_trace(data = detectionData, x = ~Date, y = ~n,
                    yaxis = movYaxis,
                    color = ~antennaName,
                    #colors = allColors,
                    hoverinfo = "text",
                    text = ~paste('Date: ', as.character(Date), '<br>Number of Detections: ', n),
                    type = 'bar') %>%
          layout(legend = list(x = 1.05, y = 1),
                 barmode = "overlay",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = primaryYaxisName, side = "left", showgrid = FALSE),
                 yaxis2 = list(title = SecondaryYaxisName, side = "right", overlaying = "y",
                               showgrid = FALSE))
      })
      
      output$detectionDataTable <- renderDT({
        datatable(detectionData,
                  rownames = FALSE,
                  extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        )
      })
    }
  )
}