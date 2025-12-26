
environmentalData_UI <- function(id, detectionData) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      tabsetPanel(
        tabPanel(
          "Data Filters",
          sidebarPanel(
            textInput(ns("textinput3"), label = "Filter by Tag"),
            sliderInput(ns("slider2"), "Date",
                        min = min(detectionData$DetectionDate -1),
                        max = max(detectionData$DetectionDate +1),  
                        value = c(min(detectionData$DetectionDate -1),max(detectionData$DetectionDate +1)),
                        step = 1,
                        timeFormat = "%d %b %y",
                        #animate = animationOptions(interval = 500, loop = FALSE)
            ),
            pickerInput(ns("picker10"),
                        label = "Select Species Type",
                        choices = sort(unique(detectionData$SPP)),
                        selected = unique(detectionData$SPP),
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE #this makes the "select/deselect all" option
                        )
            ), #end of picker 10 
            sliderInput(ns("slider10"), "Fish Release Length",
                        min = min(detectionData$`TL 1st Enc. (mm)`, na.rm = TRUE),
                        max = max(detectionData$`TL 1st Enc. (mm)`, na.rm = TRUE),  
                        value = c(min(detectionData$`TL 1st Enc. (mm)`, na.rm = TRUE), max(detectionData$`TL 1st Enc. (mm)`, na.rm = TRUE)),
                        step = 1
            ),
            pickerInput(ns("arrayPicker"),
                        label = "Select Array",
                        choices = sort(unique(detectionData$antennaName)),
                        selected = unique(detectionData$antennaName),
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE #this makes the "select/deselect all" option
                        )
            ), #end of picker 7 
            pickerInput(ns("picker7"),
                        label = "Select Specific Antenna",
                        choices = sort(unique(detectionData$antenna)),
                        selected = unique(detectionData$antenna),
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE #this makes the "select/deselect all" option
                        )
            ), #end of picker 7 
            
            
            actionButton(ns("renderButton"), label = "Render Data", width = "100%"), 
            h6("Note: entries with NA values in any of the filter fields are excluded from the results")
            
          )
          
        ), 
        tabPanel("Display Options", 
                 sidebarPanel(
                   radioButtons(ns("DetectionSelect"), 
                                "Detection Display",
                                choices = c("Total Detections", 
                                            "Status"),
                                selected = "Total Detections"),
                   h6("'Status' refers to the last detected array of the day for an individual tag"),
                   
                   radioButtons(ns("YaxisSelect"), 
                                "Primary Y Axis Data",
                                choices = c("Detections", 
                                            "Discharge"),
                                selected = "Detections"),
                   radioButtons(ns("BarDisplay"), 
                                "Bar Display",
                                choices = c("group", 
                                            "stack", 
                                            "overlay"),
                                selected = "stack")
                   
                 ) 
                 )
      ),
      mainPanel(
        uiOutput(ns("mainPanelUI")),
      )
    )
    
  )
}

environmentalData_Server <- function(id, USGSData, detectionData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      plotTitle <- reactiveVal("")
      # filter the data
      allDataFiltered <- eventReactive(input$renderButton,ignoreNULL = FALSE,{
        
        #validate(need(isTruthy(input$sliderDischarge)))
        
        if(input$textinput3 != ''){
          
          validate(
            need(input$textinput3 %in% detectionData$newTag, "Tag value not found in newTag column.Try removing last 2 digits of tag.")
          )
          
          detectionDatafiltered <- detectionData %>%
            filter(newTag %in% trimws(input$textinput3),
                   DetectionDate >= input$slider2[1] & DetectionDate <= input$slider2[2],
                   antennaName %in% c(input$arrayPicker),
                   antenna %in% c(input$picker7),
                   SPP %in% c(input$picker10),
                   `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
                   
            ) %>%
            arrange(detected)
          
        } else {
          
          detectionDatafiltered <- detectionData  %>% 
            filter(
              DetectionDate >= input$slider2[1] & DetectionDate <= input$slider2[2],
              antennaName %in% c(input$arrayPicker),
              antenna %in% c(input$picker7),
              SPP %in% c(input$picker10),
              `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
              
            ) %>%
            arrange(detected)
          
        }
        
        
        #if raw counts button presed, display counts
        if(input$DetectionSelect == "Total Detections"){
          detectionCountDataToDisplay <- detectionDatafiltered %>%
            count(DetectionDate, `Antenna or Status` = antennaName)
          
          allDataToDisplay <- detectionDatafiltered
          
        } else{
          dailyStatus <- getMovementsFunction(detectionDatafiltered)
          detectionCountDataToDisplay <- dailyStatus %>%
            count(DetectionDate, `Antenna or Status` = `Study Area Status`)
          
          allDataToDisplay <- dailyStatus
        }
        
        #otherwise, display movements
        #getMovementsFunction
        # input = list(slider2 = c("2025-11-10", "2025-11-11"))
        # x <- USGSFlows$USGSDataDaily %>%
        #   dplyr::filter(
        #   Date >= input$slider2[1] & Date <= input$slider2[2]
        # )
          
        
        USGSFiltered <- USGSData %>%
          dplyr::filter(
            Date >= (input$slider2[1]) & Date <= input$slider2[2]
          )
        
        
        dataList <- list("detectionCountDataToDisplay"= detectionCountDataToDisplay,
                         "allDataToDisplay" = allDataToDisplay, 
                         "USGSFiltered" = USGSFiltered)
        return(dataList)
      })
      
      newTitle <- paste("Daily", input$DetectionSelect, "and Discharge") 
      
      plotTitle(newTitle)
      output$mainPanelUI <- renderUI({
        tagList(
          box(title = plotTitle(), 
              width = 12, 
              plotlyOutput(ns("OverlayPlot")) 
              
          ), 
          tabsetPanel(
            tabPanel("Count Data (graphed)",
              withSpinner(DT::DTOutput(ns("countsDataTable"))), 
              downloadData_UI(ns("downloadCountsDataTable"))
            ),
            tabPanel("All Data",
                     withSpinner(DT::DTOutput(ns("allDataTable"))), 
                     downloadData_UI(ns("downloadAllDataTable"))
            )
          )
          
        )
      })
      
      output$OverlayPlot <- renderPlotly({
        
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
        
        
        
        # colorCol <- switch(input$DetectionSelect,
        #                      "Total Detections" = "antennaName",
        #                      "Movements" = "movement") # Default
        
        plot_ly() %>%
          add_trace(data = allDataFiltered()$USGSFiltered, x = ~Date,
                    y = ~Flow,
                    name = nameOfLine,
                    color = line_color, 
                    type = "scatter",
                    yaxis = envYaxis,
                    connectgaps = TRUE,
                    mode = "lines+markers", 
                    inherit = FALSE
                    #colors = allColors
          ) %>%
          add_trace(data = allDataFiltered()$detectionCountDataToDisplay, x = ~DetectionDate, y = ~n,
                    inherit = FALSE,
                    yaxis = movYaxis,
                    color = ~`Antenna or Status`,
                    #colors = allColors,
                    hoverinfo = "text",
                    text = ~paste('Date: ', as.character(DetectionDate), '<br>N: ', n),
                    type = 'bar') %>%
          layout(legend = list(x = 1.05, y = 1),
                 #xaxis = list(type = 'date', range = c(input$slider2[1] - .5,input$slider2[2] + .5)),
                 barmode = input$BarDisplay,
                 xaxis = list(title = "Date"),
                 yaxis = list(title = primaryYaxisName, side = "left", showgrid = FALSE),
                 yaxis2 = list(title = SecondaryYaxisName, side = "right", overlaying = "y",
                               showgrid = FALSE))
      })
      
      output$countsDataTable <- renderDT({
        # detectionDataNoSF <- allDataFiltered()$detectionCountDataToDisplay #%>%
        #   #st_drop_geometry()
        datatable(allDataFiltered()$detectionCountDataToDisplay,
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
      
      downloadData_Server("downloadCountsDataTable", allDataFiltered()$detectionCountDataToDisplay, "countsData")
      
      
      output$allDataTable <- renderDT({
        # detectionDataNoSF <- allDataFiltered()$detectionCountDataToDisplay #%>%
        #   #st_drop_geometry()
        datatable(allDataFiltered()$allDataToDisplay,
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
      
      downloadData_Server("downloadAllDataTable", allDataFiltered()$allDataToDisplay, "AllData")
      
    }
  )
}