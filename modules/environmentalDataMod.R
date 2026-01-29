
environmentalData_UI <- function(id, combinedDetectionAndStatusData) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
             tabsetPanel(
               tabPanel(
                 "Data Filters",
                 wellPanel(
                   textInput(ns("textinput3"), label = "Filter by Tag"),
                   #filters by maxing what is in the detection file
                   sliderInput(ns("slider2"), "Date",
                               min = min(combinedDetectionAndStatusData$detectionsAttributesFlows$DetectionDate -1),
                               max = max(combinedDetectionAndStatusData$detectionsAttributesFlows$DetectionDate +1),  
                               value = c(min(combinedDetectionAndStatusData$detectionsAttributesFlows$DetectionDate -1),
                                         max(combinedDetectionAndStatusData$detectionsAttributesFlows$DetectionDate +1)),
                               step = 1,
                               timeFormat = "%d %b %y",
                               #animate = animationOptions(interval = 500, loop = FALSE)
                   ),
                   pickerInput(ns("picker10"),
                               label = "Species Type",
                               choices = sort(unique(combinedDetectionAndStatusData$dailyStatus$Species)),
                               selected = unique(combinedDetectionAndStatusData$dailyStatus$Species),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ), #end of picker 10 
                   sliderInput(ns("slider10"), "Fish Release Length",
                               min = min(combinedDetectionAndStatusData$dailyStatus$`TL 1st Enc. (mm)`, na.rm = TRUE),
                               max = max(combinedDetectionAndStatusData$dailyStatus$`TL 1st Enc. (mm)`, na.rm = TRUE),  
                               value = c(min(combinedDetectionAndStatusData$dailyStatus$`TL 1st Enc. (mm)`, na.rm = TRUE), max(combinedDetectionAndStatusData$dailyStatus$`TL 1st Enc. (mm)`, na.rm = TRUE)),
                               step = 1
                   ),
                   pickerInput(ns("releaseDatePicker"),
                               label = "Release Dates",
                               choices = sort(unique(combinedDetectionAndStatusData$dailyStatus$`Release Date`)),
                               selected = as.character(unique(combinedDetectionAndStatusData$dailyStatus$`Release Date`)),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE #this makes the "select/deselect all" option
                               )
                   ),
                   uiOutput(ns("arrayAndAntennaPickerUI")), 
                   actionButton(ns("renderButton"), label = "Render Data", width = "100%"), 
                   h6("Note: entries with NA values in any of the filter fields are excluded from the results")
                 )
               ),
               tabPanel("Display Options", 
                        wellPanel(
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
             uiOutput(ns("statusSummaryText")),
      ),
      column(width = 8, 
             
             wellPanel(
               uiOutput(ns("mainPanelUI")),
             )
      )
    )
    
  )
}

environmentalData_Server <- function(id, USGSData, combinedDetectionAndStatusData, allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      plotTitle <- reactiveVal("")

# data filtering ----------------------------------------------------------
      #decide which data to use
      if (input$DetectionSelect == "Total Detections") {
        detectionData <- combinedDetectionAndStatusData$detectionsAttributesFlows
        dateColumnToFilter <- "DetectionDate"
      } else {
        detectionData <- combinedDetectionAndStatusData$dailyStatus
        dateColumnToFilter <- "StatusDate"
        
      }
      
      # filter the data
      allDataFiltered <- eventReactive(input$renderButton,ignoreNULL = FALSE,{
        
        if(input$textinput3 != ''){
          
          validate(
            need(input$textinput3 %in% detectionData$dec_tag, "Tag value not found in dec_tag column.")
          )
          
          detectionDatafiltered <- detectionData %>%
            filter(dec_tag %in% trimws(input$textinput3)
            ) %>%
            arrange(detected)
          
        } else {
          detectionDatafiltered <- detectionData
         }
        
        if(input$DetectionSelect == "Total Detections"){
          detectionDatafiltered <- detectionDatafiltered %>%
            filter(antennaName %in% c(input$arrayPicker),
                   antenna %in% c(input$picker7)
                   )
        }
        
        #filters that apply to all data tables
        detectionDatafiltered <- detectionDatafiltered %>%
          filter(
            .data[[dateColumnToFilter]] >= input$slider2[1] & .data[[dateColumnToFilter]] <= input$slider2[2], 
            Species %in% c(input$picker10),
            `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2], 
            #have to use as.character for the picker. use slider maybe otherwise idk
            as.character(`Release Date`) %in% input$releaseDatePicker
          ) %>%
          arrange(detected)

        #if raw counts button presed, display counts
        if(input$DetectionSelect == "Total Detections"){
          detectionCountDataToDisplay <- detectionDatafiltered %>%
            count(DetectionDate, `Antenna or Status` = antennaName)
          
          
        } else{
          detectionCountDataToDisplay <- detectionDatafiltered %>%
            count(DetectionDate = StatusDate, `Antenna or Status` = `Study Area Status`) 
            
        }
        
        detectionCountDataToDisplay <- detectionCountDataToDisplay %>%
          group_by(DetectionDate) %>%
          mutate(dailyPercent = round(n / sum(n) * 100, 2)) %>%
          ungroup()
        
        USGSFiltered <- USGSData %>%
          dplyr::filter(
            Date >= (input$slider2[1]) & Date <= input$slider2[2]
          )
        
        
        dataList <- list("detectionCountDataToDisplay"= detectionCountDataToDisplay,
                         "allDataToDisplay" = detectionDatafiltered, 
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
                          label = "Array",
                          choices = sort(unique(detectionData$antennaName)),
                          selected = unique(detectionData$antennaName),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE #this makes the "select/deselect all" option
                          )
              ),
              pickerInput(ns("picker7"),
                          label = "Specific Antenna",
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
        
        output$statusSummaryText <- renderUI({
          if(input$DetectionSelect == "Status"){
            
            displayText <- paste0("On ", min(allDataFiltered()$detectionCountDataToDisplay$DetectionDate), ", there were an estimated ", 
                                  allDataFiltered()$detectionCountDataToDisplay$n[allDataFiltered()$detectionCountDataToDisplay$DetectionDate == min(allDataFiltered()$detectionCountDataToDisplay$DetectionDate)
                                                                & allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"], " fish outside the study area for the selected filters, representing ", 
                                  allDataFiltered()$detectionCountDataToDisplay$dailyPercent[allDataFiltered()$detectionCountDataToDisplay$DetectionDate == min(allDataFiltered()$detectionCountDataToDisplay$DetectionDate)
                                                                                  & allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"], "% of selected/filtered fish. 
                                  
                                  On ", max(allDataFiltered()$detectionCountDataToDisplay$DetectionDate), ", there were an estimated ", 
                                  allDataFiltered()$detectionCountDataToDisplay$n[allDataFiltered()$detectionCountDataToDisplay$DetectionDate == max(allDataFiltered()$detectionCountDataToDisplay$DetectionDate)
                                                                                  & allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"], " fish outside the study area for the selected filters, representing ", 
                                  allDataFiltered()$detectionCountDataToDisplay$dailyPercent[allDataFiltered()$detectionCountDataToDisplay$DetectionDate == max(allDataFiltered()$detectionCountDataToDisplay$DetectionDate)
                                                                                             & allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"], "% of selected/filtered fish.
                                  
                                  This represents a change of ", round(allDataFiltered()$detectionCountDataToDisplay$dailyPercent[allDataFiltered()$detectionCountDataToDisplay$DetectionDate == max(allDataFiltered()$detectionCountDataToDisplay$DetectionDate)
                                                                                                                            & allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"] - 
                                    allDataFiltered()$detectionCountDataToDisplay$dailyPercent[allDataFiltered()$detectionCountDataToDisplay$DetectionDate == min(allDataFiltered()$detectionCountDataToDisplay$DetectionDate)
                                                                                                                            & allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"], 2), 
                                  "% across ", difftime(max(allDataFiltered()$detectionCountDataToDisplay$DetectionDate), min(allDataFiltered()$detectionCountDataToDisplay$DetectionDate), units = "days"), " days."
                                  )
            h5(displayText)
          }
        })

# PLOT OUTPUT -------------------------------------------------------------

      
      output$OverlayPlot <- renderPlotly({
        #define plot object 
        p <- plot_ly()
        
        nameOfLine = "USGS Discharge"
      
        if(input$YaxisSelect == "Detections"){
          movYaxis = "y1"
          envYaxis = "y2"
          primaryYaxisName = paste0(as.character(input$DetectionSelect), " (Daily Counts)")
          SecondaryYaxisName = "Discharge (cfs)"
          
        } else{
          movYaxis = "y2"
          envYaxis = "y1"
          primaryYaxisName = "Discharge (cfs)"
          SecondaryYaxisName = paste0(c(as.character(input$DetectionSelect), " (Daily Counts)"))
        }
        
        ##detection data args
        #base args that won't change
        detectionDataArgs <- list(data = allDataFiltered()$detectionCountDataToDisplay, x = ~DetectionDate, y = ~n,
                                  inherit = FALSE,
                                  yaxis = movYaxis,
                                  color = ~`Antenna or Status`,
                                  colors = allColors,
                                  hoverinfo = "text",
                                  text = ~paste0('Date: ', as.character(DetectionDate),
                                                '<br>N: ', n, 
                                                '<br>Daily Percentage of total: ', dailyPercent, "%"), 
                                  type = input$statusDisplayOption
                                  )
        #additional args if status diplsay option is available
        if(input$statusDisplayOption == "scatter"){
          detectionDataArgs$connectgaps = TRUE
          detectionDataArgs$mode = "lines+markers"
        }
        #unwrap args defined above 
        #do.call is like saying "apply this function ("Add_trace()") using these arguments
        #helpful when sometimes you need to add or change arguments. detectionDataArgs doesn't stay the same
        #adding the plot p as an argument that needs to be passed as well with list(p = p)
        #for assigning color purposes with allCOlor, this part has to be the first trace
        p <- do.call(add_trace, c(list(p = p), detectionDataArgs))

        p <- p %>%
            add_trace(data = allDataFiltered()$USGSFiltered, x = ~Date,
                      y = ~Flow,
                      name = nameOfLine,
                      color = I(allColors[["USGSLineColor"]]), 
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
          
          #display layered plot
          p
        
      })
      
      output$countsDataTable <- renderDT({
        
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
      
      downloadData_Server("downloadCountsDataTable", reactive({allDataFiltered()$detectionCountDataToDisplay}), "countsData")
      
      output$allDataTable <- renderDT({
        
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
      
      downloadData_Server("downloadAllDataTable", reactive({allDataFiltered()$allDataToDisplay}), "AllData")
      
    }
  )
}