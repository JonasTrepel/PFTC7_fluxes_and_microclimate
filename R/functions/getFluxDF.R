# install.packages("devtools")
# devtools::install_github("bgctw/RespChamberProc")

require(RespChamberProc)

getFluxDF <- function(files, skip = 3, device = "LI7500", toi = NULL){  
  # Function to read multiple flux data files and process them into a combined data table.
  # Arguments:
  # fluxfiles: A list of file paths to flux data files.
  # skip: The number of lines to skip at the start of each file (default is 3 which makes at least sense for the Licor7500).
  # device: The device used to record the flux data (default is "Licor7500"), it could be useful to include more devices later on.
  
  if(device == "LI7500"){ 
    
    # Convert the list of flux files to a vector and remove names
    fluxFileV <- unlist(fluxFiles) %>% unname()  
    
    # Create an empty data table to store the combined flux data
    fluxDT <- data.table::data.table()  
    
    # Loop through each unique file in the fluxFileV vector
    for(file in unique(fluxFileV)){  
      
      # Read the flux data file, skipping the first few lines as specified by `skip`.
      # The delimiter is tab-separated ("\t").
      rawFlux <- suppressMessages(readr::read_delim(file, skip = skip, delim = "\t"))  
      
      # Process the raw flux data and select specific columns for analysis
      tempFlux <- rawFlux %>%
        dplyr::select(Time, Date, `CO2 (umol/mol)`, `H2O (mmol/mol)`, `Temperature (C)`, `Pressure (kPa)`, `CO2 Signal Strength`) %>%
        
        # Rename the columns for easier use later
        rename(
          ConcCO2 = `CO2 (umol/mol)`,
          ConcH2O = `H2O (mmol/mol)`, 
          AirTemperature = `Temperature (C)`, 
          PressureKPa = `Pressure (kPa)`,  
          SignalStrength = `CO2 Signal Strength`,  
          LicorTime = Time, 
          LicorDate = Date   
        ) %>% 
        
        # Perform data transformations
        mutate(
          LicorTime = gsub("\\:000", "", LicorTime),  # Clean time column by removing trailing ":000" - no idea why that's there 
          LicorDate = lubridate::ymd(LicorDate),
          LicorTime = lubridate::hms(LicorTime),
          DateTime = lubridate::ymd_hms(paste0(LicorDate, " ", LicorTime)),  
          StartTime = min(DateTime), 
          Filename = file, 
          ConcCO2 = as.numeric(ConcCO2),
          ConcH2O = as.numeric(ConcH2O)
        ) %>% 
        dplyr::select(-c(LicorDate, LicorTime))  
      
      # Append the processed data (tempFlux) to the combined data table (fluxDT)
      fluxDT <- rbind(fluxDT, tempFlux)  
      
      # Print a message indicating that the file has been processed
      print(paste0("File done: ", file))  
    }  
    
  }else if(device == "LI8100"){

    e <- tryCatch(
      {
        library(RespChamberProc)
      },
      error = function(e) {
        message("Please install the RespChamberProc package: devtools::install_github('bgctw/RespChamberProc')")
      }
    )
    library(tidyverse)
    
    if(is.null(toi)){print("Please provide timeframe of interest (toi)")}
    
    # Read in data
    tempSR <- map_df(set_names(files), function(file) {
      file %>%
        set_names() %>%
        map_df(~ RespChamberProc::read81xVar(fName = file)) #important! reading in American format
    }, .id = "File")
    
    
    fluxDT <- tempSR %>% 
      mutate(File = basename(File),
             File = str_remove(File, ".81x")
      ) |>
      # Separate into relevant info
      separate(File, into = c("flux", "siteID", "elevation", "aspect"), remove = FALSE) |>
      # Rename site and plot so they behave
      mutate(site = paste0(siteID),
             plot = paste0(iChunk),
             aspect = str_to_lower(aspect), 
             plotID = paste0(elevation, aspect, plot)
      ) %>% 
      #select only measurements from 120 seconds onwards (which is the most representative window)
      filter(Etime %in% c(toi)) %>% 
      rename(
        ConcCO2 = `CO2`,
        ConcH2O = `H2O`, 
        ChamberTemperature = `Tcham`, 
        PressureKPa = `Pressure`,  
        DateTime = Date
    )
    
  }else{
    print(paste0("Sorry, can do only Licor7500 and Licor8100 at this point :("))
    }  
  
  return(fluxDT)  
}
