
require(dplyr)
require(lubridate)

# parFiles <- parFiles
# fluxDF <- fluxDFRaw
# parTZ <- "America/Mexico_City"
# fluxTZ = "Africa/Johannesburg"

# based on a funciton from Michael Mustri

readAndAddPAR <- function(parFiles = NULL, 
                          fluxDF = NULL,
                          parTZ = "UTC",
                          fluxTZ = "UTC",
                          fluxDFTimeCol = "DateTime",
                          parSkip = 7){
  
  
  if(is.null(parFiles)){
    print("Please provide PAR file names")
  }
  
  if(is.null(fluxDF)){
    print("Please provide flux data")
  }
  
  parDT <- list()
  
  if(!is.null(parFiles)){
    parDT[[1]] <- suppressMessages(suppressWarnings(read_delim(parFiles[1], delim="\t", skip = parSkip)))
    if(length(parFiles) > 1){
      for(f in c(2:length(parFiles))){
        parDT[[f]] <-  suppressMessages(suppressWarnings(read_delim(parFiles[f], delim="\t", skip = parSkip)))
      }
      parComb <- bind_rows(parDT)
    }
    else{
      parComb <- parDT[[1]]
    }
  }
  else{
    parComb = NULL
  }
  
  
  if(!is.null(parComb)){
    fluxDF$POSIXct <- as.POSIXct(fluxDF[[fluxDFTimeCol]], 
                                format="%Y-%m-%d %H:%M:%S", 
                                tz = fluxTZ)
    parComb$POSIXct_uc <- as.POSIXct(paste(parComb$Date, parComb$Time),
                                 format="%Y-%m-%d %H:%M:%S", 
                                 tz = parTZ)
    commonTZ = fluxTZ
    
    parComb$POSIXct <- with_tz(parComb$POSIXct_uc, tzone = commonTZ)
    parFluxDTRaw <- inner_join(parComb, fluxDF, by="POSIXct") %>% 
      rename(PAR = INPUT1) %>% 
      dplyr::select(c(PAR, Filename, DateTime))
    
    print(paste0("Succesfully joined PAR and fluxes, returning fluxDF with PAR column"))
  }
  else{
    
    fluxDF$POSIXct <- as.POSIXct(fluxDF[[fluxDFTimeCol]], 
                                 format="%Y-%m-%d %H:%M:%S", 
                                 tz = fluxTZ)
    parFluxDTRaw <- fluxDF %>% 
      dplyr::select(c(Filename, DateTime)) %>% mutate(PAR = NA)
    
    print(paste0("Something went wrong with the PAR files - returning fluxDF without PAR column"))
  }
  
  parFluxDT <- suppressMessages(fluxDF %>% left_join(parFluxDTRaw))
  
  if(sum(is.na(parFluxDT$PAR)) > 0 &  sum(is.na(parFluxDT$PAR)) < nrow(parFluxDT)){
    print(paste0("Uhoh, some PAR values are NAs (", round(sum(is.na(parFluxDT$PAR))/nrow(parFluxDT)*100, 1), "%)"))
  }
  return(parFluxDT)
}
