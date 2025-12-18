
environmentalData_UI <- function(id, detectionData) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(ns("textinput3"), label = "Filter by Tag"),
        sliderInput(ns("slider2"), "Date",
                    min = min(detectionData$detected -1),
                    max = max(detectionData$detected +1),  
                    value = c(min(detectionData$detected -1),max(detectionData$detected +1)),
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
        radioButtons(ns("YaxisSelect"), 
                                "Primary Y Axis Data",
                                choices = c("Detections", 
                                            "Discharge"),
                                selected = "Detections"), 
        actionButton(ns("renderButton"), label = "Render Data", width = "100%")
        
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
      
      # filter the data
      allDataFiltered <- eventReactive(input$renderButton,ignoreNULL = FALSE,{
        
        if(input$textinput3 != ''){
          detectionDatafiltered <- detectionData %>%
            filter(newTag %in% trimws(input$textinput3),
                   detected >= input$slider2[1] & detected <= input$slider2[2],
                   antennaName %in% c(input$arrayPicker),
                   antenna %in% c(input$picker7),
                   SPP %in% c(input$picker10),
                   `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
                   
            ) %>%
            arrange(detected)
          
        } else {
          print("no tag selected")
          print(paste("DAte 1: ", input$slider2[1], "and date 2:", input$slider2[2]))
          print(paste("antennaName ", input$arrayPicker))
          print(paste("antenna ", input$picker7))
          print(paste("species ", input$picker10))
          print(paste("length 1: ", input$slider10[1], "and length 2:", input$slider10[2]))
          
          detectionDatafiltered <- detectionData  %>% 
            filter(
              detected >= input$slider2[1] & detected <= input$slider2[2],
              antennaName %in% c(input$arrayPicker),
              antenna %in% c(input$picker7),
              SPP %in% c(input$picker10),
              `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
              
            ) %>%
            arrange(detected)
          
        }
        
        
        #if raw counts button presed, display counts
        detectionDatafilteredCounts <- detectionDatafiltered %>%
          count(Date = date(detected), antennaName)
        #otherwise, display movements
        
        USGSFiltered <- USGSData %>%
          filter(
            Date >= input$slider2[1] & Date <= input$slider2[2]
          )
        
        
        dataList <- list("detectionDatafiltered"= detectionDatafilteredCounts, 
                         "USGSFiltered" = USGSFiltered)
      })
      
      
      
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
          add_trace(data = allDataFiltered()$USGSFiltered, x = ~Date,
                    y = ~Flow,
                    name = nameOfLine,
                    color = line_color, 
                    type = "scatter",
                    yaxis = envYaxis,
                    connectgaps = TRUE,
                    mode = "lines"
                    #colors = allColors
          ) %>%
          add_trace(data = allDataFiltered()$detectionDatafiltered, x = ~Date, y = ~n,
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
        detectionDataNoSF <- allDataFiltered()$detectionDatafiltered %>%
          st_drop_geometry()
        datatable(detectionDataNoSF,
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