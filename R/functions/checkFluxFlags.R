require(cpop)
require(ggplot2)
require(dplyr)
require(lubridate)
require(tidyverse)
require(reshape2)
require(cowplot)
require(sarima)


checkFluxFlags <- function( 
    vol = NA,  # Volume in cubic meters, default is 2.197
    area = NA,  # Area in square meters, default is 1.69
    sigStrengthThresh = 95.0,  # Threshold for valid signal strength
    fluxIDs = NA, #Ids of the fluxes to check 
    parThresh = 650,  # Photosynthetically Active Radiation (PAR) threshold
    fluxDF = NULL,  # Data frame containing flux data, 
    fittedFluxes = NULL, 
    param = "co2",  # Parameter to analyze, default is CO2
    parCol = "PAR",  # Column name for PAR in fluxDF
    dateTimeCol = "DateTime",  # Column name for datetime in fluxDF
    co2Col = "ConcCO2",  # Column name for CO2 concentration in fluxDF
    h2oCol = "ConcH2O",  # Column name for H2O concentration in fluxDF
    signalStrengthCol = "SignalStrength",  # Column name for signal strength in fluxDF
    tempCol = "AirTemperature",  # Column name for air temperature in fluxDF
    pressureCol = "PressureKPa",  # Column name for pressure in kPa in fluxDF
    fluxTypeCol = "measurement",  # Column name for flux type in fluxDF
    plotIDCol = "plotID",  # Column name for plot ID in fluxDF
    redoCol = NA, 
    dayNightCol = NA,
    skip = 7 #number of rows to skip from each flux measurement
){ 
  
  
  if(is.na(vol)){print("Please provide the tent volume (in m^3)")}
  
  if(is.na(area)){print("Please provide the tent area (in m^2)")}
  
  if(is.null(fluxDF)){print("Please provide a dataframe with gas concentrations")}

  
  # Loop through each unique flux measurement filename in the flux data frame
  
  # flux <- "data/rawData/LI7500/LI7500_Site 4/4_2600_west_1_day_photo.txt" 
  # flux <- "data/rawData/LI7500/LI7500_Site 4/4_2600_west_3_day_photo.txt"     
  # flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_east_2_day_photo.txt"   
  # flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_east_3_day_photo.txt"  
  
  for(flux in unique(fluxIDs)){
    
    if(is.na(flux)){next}  # Skip if the flux filename is NA
    
    # Subset the flux data frame for the current filename and keep distinct rows
    dtSub <- fluxDF %>% filter(Filename == {{flux}}) %>% unique()#%>% distinct(across(-f_fluxID), .keep_all = TRUE)
    
    if(nrow(dtSub)< 60+skip){next}  # Skip if n is too small 
    
    
    # Get the unique flux type from the subset data
    fluxType <- unique(dtSub[[fluxTypeCol]]) #%>% pull()
    
    # If more than one flux type is found, calculate the mode
    if(length(fluxType) > 1){
      table(dtSub[fluxTypeCol])  
      Mode <- function(x, na.rm = TRUE) {
        if(na.rm){x = x[!is.na(x)]}
        ux <- unique(x)  
        return(ux[which.max(tabulate(match(x, ux)))])  # Return the most frequent value (mode)
      }
      fluxType <- Mode(dtSub[fluxTypeCol])  # Assign mode to fluxType
      # this should usually not be necessary 
    }
    
    # Skip if the flux type is ambient measurements
    if(fluxType == "a"){next} 
    
    # Define ambient measurement for comparison
    fluxDF$fluxType <- fluxDF[[fluxTypeCol]]  
    # Select ambient measurements within 15 minutes of the current measurement
    dtAmbient <- suppressWarnings(fluxDF %>%  
                                    mutate(timeDiffAmb = difftime(StartTime, dtSub$StartTime, units = "mins")) %>%  
                                    filter(
                                      plotID == dtSub$plotID & abs(timeDiffAmb) < 15 & fluxType == "a"  # Filter for matching plot ID and time difference
                                    ))
    
    # If no ambient data is found, take the first 5 rows of the current measurement
    if(nrow(dtAmbient) < 1){ 
      dtAmbient <- dtSub[1:5,] 
    }
    
    # Check if the measurement is a night measurement or respiration
    check_night_resp <- FALSE
    if(grepl("resp", flux, fixed = TRUE)){ 
      check_night_resp <- TRUE 
    }
    
    ## remove first rows of the flux 
    
    dtSub <- dtSub[!1:skip, ]
    
    
    ## Define parameters for calculations
    R <- 8.314472  # Universal gas constant in J/(K·mol)
    
    # Prepare variables for calculations
    time <- as.numeric(dtSub[[dateTimeCol]]) - min(as.numeric(dtSub[[dateTimeCol]]))  # Time since the start of the measurement
    co2 <- as.numeric(dtSub[[co2Col]])  # CO2 concentration
    h2o <- as.numeric(dtSub[[h2oCol]])  # H2O concentration
    
    # Determine PAR values; if no PAR data is available or if it's a respiration measurement, set PAR to threshold + 1
    if(check_night_resp == TRUE | sum(is.na(dtSub$PAR)) == nrow(dtSub)){ 
      par <- rep(parThresh + 1, nrow(dtSub))  # Default PAR value
    }else{ 
      par <- as.numeric(dtSub$PAR)  # Use actual PAR data
    }
    
    # Prepare other parameters for calculations
    press <- as.numeric(dtSub[[pressureCol]])  # Pressure
    temp <- as.numeric(dtSub[[tempCol]])  # Air temperature
    signalStrength <- as.numeric(dtSub[[signalStrengthCol]])  # Signal strength
    
    # Calculate c' and w' with respect to the H2O concentration
    cPrime <- co2 / (1 - (h2o / 1000))  
    wPrime <- h2o / (1 - (h2o / 1000))  
    wavDil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
    co2Amb <- mean(as.numeric(as.character(dtAmbient[[co2Col]])) / (1 - (as.numeric(as.character(dtAmbient[[h2oCol]])) / 1000)))  # Ambient CO2 concentration
    h2oAmb <- mean(as.numeric(as.character(dtAmbient[[h2oCol]])) / (1 - (as.numeric(as.character(dtAmbient[[h2oCol]])) / 1000)))  # Ambient H2O concentration
    
    # Identify change points in the time series of c'
    res <- suppressMessages(cpop(cPrime, minseglen = 30))  # Identify change points with a minimum segment length of 30 seconds
    changepoints(res)  # Extract change points from the result
    cPrime_seg <- fitted(res)  # Get the fitted values from the change point analysis
    
    # Create a ggplot for visualizing CO2 concentration over time
    p2 <- ggplot(data = dtSub, aes(y = .data[[co2Col]],  
                                   x = as.numeric(.data[[dateTimeCol]]) - min(as.numeric(.data[[dateTimeCol]])), 
                                   color = as.numeric(.data[[signalStrengthCol]]))) +  
      geom_point() +  # Add points for CO2 data
      scale_color_gradient(high = "blue", low = "red", limits = c(79, 101)) +  # Color gradient for signal strength
      geom_line() +  # Add lines connecting the points
      theme(legend.position = "bottom") +  # Remove legend
      #ylim(min(dtSub[[co2Col]], na.rm = TRUE), max(dtSub[[co2Col]], na.rm = TRUE)) +  # Set y-axis limits for CO2
      ylab("CO2") +  # Label y-axis
      xlab("Time") +  # Label x-axis
      geom_vline(xintercept = cPrime_seg$x0)  # Add vertical lines at change points
    
    # Create a ggplot for visualizing PAR over time
    p1 <- ggplot(data = dtSub,  
                 aes(y = .data[[parCol]],  
                     x = as.numeric(.data[[dateTimeCol]]) - min(as.numeric(.data[[dateTimeCol]])))) +  
      geom_point(color = "red") +  # Add points for PAR data
      geom_line(color = "red") +  # Add lines connecting the points
      #  ylim(min(dtSub[[parCol]], na.rm = TRUE), max(dtSub[[parCol]], na.rm = TRUE)) +  # Set y-axis limits for PAR
      ylab("PAR") +  # Label y-axis
      xlab("Time") +  # Label x-axis
      labs(title = paste0(flux)) +  # Set title based on flux filename
      geom_vline(xintercept = cPrime_seg$x0)  # Add vertical lines at change points
    
    # Combine the two plots vertically
    p_c <- plot_grid(p1, p2, ncol=1, align="v", axis=1) 
    plot(p_c)  # Display the combined plot
    
    # Create a sequence of segment indices based on the number of rows in cPrime_seg data frame
    segs <- c(1:nrow(cPrime_seg))  
    
    # Initialize vectors to store various calculated metrics
    aicLinearFit <- c()  # Stores AIC values for linear fits
    inter <- c()         # Stores intercept values from linear fits
    dcw_dt <- c()       # Stores the derivative (slope) of the linear fit
    r2LinearFit <- c()  # Stores R-squared values for linear fits
    param_lm <- c()     # Stores parameters from linear model
    param_exp <- c()    # Stores parameters from exponential model
    avgTemperatureC <- c()  # Stores average temperature in Celsius for each segment
    avgPressureKPa <- c()   # Stores average pressure in KPa for each segment
    cav <- c()            # Stores average CO2 concentrations for each segment
    wav <- c()            # Stores average H2O concentrations for each segment
    lmSlope <- c()
    
    r2LinearFitAll <- c()
    eff_sample_all <- 0
    # Initialize index for storing results and counters for effective sample sizes
    i = 1  
    eff_sample <- 0          # Counter for effective sample size based on linear fit criteria
    eff_sample_nls <- 0      # Counter for effective sample size based on nonlinear fit criteria
    
    predPlot <- data.table()
    
    # Loop over each segment to perform calculations
    for(s in segs){  
      end <- length(par)  # Get the total number of PAR values
      
      s1 <- cPrime_seg$x0[s]  # Starting index for current segment
      
      # Determine the ending index for the current segment
      if(s == nrow(cPrime_seg)){  
        s2 <- end  # If it's the last segment, use the last index
      } else{
        s2 <- cPrime_seg$x0[s+1]  # Otherwise, use the starting index of the next segment
      }
      
      # Calculate the mean signal strength and PAR for the current segment
      meanSigStrength <- mean(signalStrength[s1:s2])  
      meanPAR <- mean(par[s1:s2])  
      
      # Select the concentration variable based on the parameter specified
      if ("co2" == param) {  
        cwPrime <- cPrime  # Use cPrime for CO2
      } else if ("h2o" == param) {  
        cwPrime <- wPrime  # Use wPrime for H2O
      }
      
      # Tag to identify the parameter being processed
      if ("co2" == param) {  
        tag <- "cPrime"  
      } else if ("h2o" == param) {  
        tag <- "wPrime"  
      }
      
      # Proceed only if both signal strength and PAR exceed the respective thresholds
      if((meanSigStrength > sigStrengthThresh) && (meanPAR > parThresh)){  
        
        # Fit a linear model to the current segment
        linear.fit <- stats::lm(cwPrime[s1:s2] ~ (time[s1:s2]))  
        
        lmSlope[i] <- linear.fit$coeff[2]
        
        # Extract R-squared value from the linear model
        rsq <- as.numeric(summary(linear.fit)$r.sq)  
        
        pred <- data.frame(time = time[s1:s2])
        pred$pred <- predict(linear.fit)*(1 - (mean(h2o[s1:s2]) / 1000))  
        pred$rsq <- rsq
        
        
        predPlot <- rbind(pred, predPlot)
        
        p3 <- p2 + 
          geom_line(data = predPlot, aes(x = time, y = pred, alpha = rsq), color = "black", linewidth = 1.1) +
          labs(title = paste0(flux))
        print(p3)
        
        print(s)  
        i <- i+1  # Increment the index for storing results
      }  
    }
    
    
    flagChange <- readline("Do you want to change the flag to 'keep'? Please type 'keep' or 'k': ")
    
    # Check if the input is 'keep' or 'k'
    if (flagChange == "k" || flagChange == "keep") {
      fittedFluxes[filename == flux, ]$fluxFlag <- "keep"
    }
  
  }
  # Return the combined data table with calculated fluxes
  return(fittedFluxes)  
}
