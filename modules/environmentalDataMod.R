
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
            uiOutput(ns("arrayAndAntennaPickerUI")), 
            actionButton(ns("renderButton"), label = "Render Data", width = "100%"), 
            h6("Note: entries with NA values in any of the filter fields are excluded from the results")
            
          )
          
        ), 
        tabPanel("Display Options", 
                 sidebarPanel(
                   radioButtons(ns("DetectionSelect"), 
                                "Data Display",
                                choices = c("Total Detections", 
                                            "Status"),
                                selected = "Total Detections"),
                   h6("'Status' refers to the last detected array of the day for an individual tag"),
                   radioButtons(ns("statusDisplayOption"), 
                                "Display Type",
                                choices = c("Bar" = "bar", 
                                            "Line" = "scatter"),
                                selected = "bar"),
                    
                   
                   radioButtons(ns("YaxisSelect"), 
                                "Primary Y Axis Data",
                                choices = c("Detections", 
                                            "Discharge"),
                                selected = "Detections"),
                   uiOutput(ns("barDisplayOptionUI"))
                   
                   
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

# data filtering ----------------------------------------------------------

      
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
          dailyStatus <- getStatusFunction(detectionDatafiltered)
          detectionCountDataToDisplay <- dailyStatus %>%
            count(DetectionDate = StatusDate, `Antenna or Status` = `Study Area Status`) 
          
          allDataToDisplay <- dailyStatus
        }
        
        USGSFiltered <- USGSData %>%
          dplyr::filter(
            Date >= (input$slider2[1]) & Date <= input$slider2[2]
          )
        
        
        dataList <- list("detectionCountDataToDisplay"= detectionCountDataToDisplay,
                         "allDataToDisplay" = allDataToDisplay, 
                         "USGSFiltered" = USGSFiltered)
        return(dataList)
      })

# UI components -----------------------------------------------------------

      
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
      
      output$barDisplayOptionUI <- renderUI({
        if(input$statusDisplayOption == "bar"){
          radioButtons(ns("BarDisplay"), 
                       "Bar Display",
                       choices = c("group", 
                                   "stack", 
                                   "overlay"),
                       selected = "group")
        }
      })
        output$arrayAndAntennaPickerUI <- renderUI({
          if(input$DetectionSelect != "Status"){
            tagList(
              pickerInput(ns("arrayPicker"),
                          label = "Select Array",
                          choices = sort(unique(detectionData$antennaName)),
                          selected = unique(detectionData$antennaName),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE #this makes the "select/deselect all" option
                          )
              ),
              pickerInput(ns("picker7"),
                          label = "Select Specific Antenna",
                          choices = sort(unique(detectionData$antenna)),
                          selected = unique(detectionData$antenna),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE #this makes the "select/deselect all" option
                          )
              )
            )
            
          }
        })
        
      
      

# PLOT OUTPUT -------------------------------------------------------------

      
      output$OverlayPlot <- renderPlotly({
        #define plot object 
        p <- plot_ly()
        #define display parameters
        line_color = I("#87CEEB")
        nameOfLine = "USGS Discharge"
      
        if(input$YaxisSelect == "Detections"){
          movYaxis = "y1"
          envYaxis = "y2"
          primaryYaxisName = "Detection Data (Daily Counts)"
          SecondaryYaxisName = "Discharge (cfs)"
          
        } else{
          movYaxis = "y2"
          envYaxis = "y1"
          primaryYaxisName = "Discharge (cfs)"
          SecondaryYaxisName = "Detection Data (Daily Counts)"
        }
        
        
        ##detection data args
        #base args that won't change
        detectionDataArgs <- list(data = allDataFiltered()$detectionCountDataToDisplay, x = ~DetectionDate, y = ~n,
                                  inherit = FALSE,
                                  yaxis = movYaxis,
                                  color = ~`Antenna or Status`,
                                  #colors = allColors,
                                  hoverinfo = "text",
                                  text = ~paste('Date: ', as.character(DetectionDate), '<br>N: ', n), 
                                  type = input$statusDisplayOption
                                  )
        #additional args if status diplsay option is available
        if(input$statusDisplayOption == "scatter"){
          detectionDataArgs$connectgaps = TRUE
          detectionDataArgs$mode = "lines+markers"
        }
        # if (!isTruthy(input$statusDisplayOption) | input$DetectionSelect != "Status") {
        #   detectionDataArgs$type = "bar"
        # } else {
        #   detectionDataArgs$type = as.character(input$statusDisplayOption)
        #   #add desired line plot args if the type is scatter
        #   if(input$statusDisplayOption == "scatter"){
        #     detectionDataArgs$connectgaps = TRUE
        #     detectionDataArgs$mode = "lines+markers"
        #   }
        # }
        #print(paste("is tructhy status diplsy option: ", isTruthy(input$statusDisplayOption)))

          p <- p %>%
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
            layout(legend = list(x = 1.05, y = 1),
                   #xaxis = list(type = 'date', range = c(input$slider2[1] - .5,input$slider2[2] + .5)),
                   barmode = input$BarDisplay,
                   xaxis = list(title = "Date"),
                   yaxis = list(title = primaryYaxisName, side = "left", showgrid = FALSE),
                   yaxis2 = list(title = SecondaryYaxisName, side = "right", overlaying = "y",
                                 showgrid = FALSE))
          #unwrap args defined above 
          #do.call is like saying "apply this function ("Add_trace()") using these arguments
          #helpful when sometimes you need to add or change arguments. detectionDataArgs doesn't stay the same
          #adding the plot p as an argument that needs to be passed as well with list(p = p)
          p <- do.call(add_trace, c(list(p = p), detectionDataArgs))
          #display layered plot
          p
        
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