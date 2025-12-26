#get USGS data
# codeID = "09147025"
# startDate = "2025-08-01"
getDailyand15MinUSGSData <- function(codeID, startDate = "2020-08-06", endDate = Sys.Date(), waterTemp = TRUE) {
  ##windy gap/hitching post 
  #reading in USGS data with upt to date data
  USGSDataDaily <- read_waterdata_daily(monitoring_location_id = paste0("USGS-", as.character(codeID)),
                                        parameter_code = c("00060", "00010"), #this is parameter codes for discharge and celsius water temp; more can be added if needed. https://help.waterdata.usgs.gov/codes-and-parameters/parameters
                                        time = c(startDate, endDate)
  )
  #removes geometry so no longer sf object
  #gets to desired wide format while applying "mean" function to the values
  USGSDataDaily <- USGSDataDaily %>%
    st_drop_geometry() %>%
    pivot_wider(id_cols = time, names_from = parameter_code, values_from = value, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
    rename(Flow = `00060`, 
           Date = time)
  
  if(waterTemp){
    USGSDataDaily <- USGSDataDaily %>%
      rename(tempC = `00010`)
    USGSDataDaily <- USGSDataDaily %>%
      mutate(WtempF = (tempC * 9/5) + 32)
  }
  
  #sometimes this can fail if USGS is having issues on their end
  #maybe this function readNWISuv should be replaced with read_waterdata_latest_continuous but no error on that yet. monitor
  USGSData <- readNWISuv(siteNumbers = codeID, #code for windy gap
                         parameterCd = c("00060", "00010"), #this is parameter code for discharge; more can be added if needed
                         startDate = startDate, #if you want to do times it is this format: "2014-10-10T00:00Z",
                         endDate = endDate,
                         tz = "America/Denver")
  
  USGSData <- renameNWISColumns(USGSData) 
  
  if(waterTemp){
    USGSData <- USGSData %>%
      mutate(USGSWatertemp = (Wtemp_Inst * 9/5) + 32) 
  }
  USGSData <- USGSData %>%
    rename(USGSDischarge = Flow_Inst)
  #this is to make attaching these readings to detections later
  USGSData$dateTime <- lubridate::force_tz(USGSData$dateTime, tzone = "UTC") 
  
  return(list("USGSData" = USGSData, 
              "USGSDataDaily" = USGSDataDaily))
}
