
require(cpop)
require(ggplot2)
require(dplyr)
require(lubridate)
require(tidyverse)
require(reshape2)
require(cowplot)
require(sarima)

#' Calculate Tent Fluxes
#'
#' This function calculates the fluxes of a specified parameter (e.g., CO2 or H2O) 
#' from a provided dataset of flux measurements.
#' 
#' Specifically, the function devides the flux into different segments (based on brakpoints) and 
#' calculates the fluxes for each segment separately. Results are weighted by the length of the segment. 
#' We fit a linear and a non-linear function separately. For the linear one, segments with an rsq of < are discared. 
#' 
#' 
#' @param vol Volume of the measurement chamber (default = 2.197).
#' @param area Area of the measurement chamber (default = 1.69).
#' @param sigStrengthThresh Signal strength threshold for measurements (default = 90.0).
#' @param parThresh Photosynthetically Active Radiation threshold (default = 650).
#' @param fluxDF Data frame containing matched flux measurements (default = matchedFluxes).
#' @param param Parameter to calculate flux for: "co2" or "h2o" (default = "co2").
#' @param parCol Column name for PAR data (default = "PAR").
#' @param dateTimeCol Column name for date and time (default = "f_datetime").
#' @param co2Col Column name for CO2 concentration (default = "f_conc").
#' @param h2oCol Column name for H2O concentration (default = "ConcH2O").
#' @param signalStrengthCol Column name for signal strength measurements (default = "SignalStrength").
#' @param tempCol Column name for air temperature (default = "AirTemperature").
#' @param pressureCol Column name for pressure in kPa (default = "PressureKPa").
#' @param fluxTypeCol Column name for flux type measurements (default = "measurement").
#' @param plotIDCol Column name for plot identifiers (default = "plotID").
#'
#' @return A data.table containing calculated flux values, average temperature, pressure, 
#'         and other relevant statistics for each unique flux measurement file.
#'         

#flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_west_3_night_resp.txt"

calcTentFluxes <- function( 
    vol = NA,  # Volume in cubic meters, default is 2.197
    area = NA,  # Area in square meters, default is 1.69
    sigStrengthThresh = 95.0,  # Threshold for valid signal strength
    parThresh = 650,  # Photosynthetically Active Radiation (PAR) threshold
    fluxDF = NULL,  # Data frame containing flux data
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
  
  
  # Initialize an empty data table to store calculated fluxes
  calculatedFluxes <- data.table::data.table() 
  
  # Loop through each unique flux measurement filename in the flux data frame
  
  # flux <- "data/rawData/LI7500/LI7500_Site 4/4_2600_west_1_day_photo.txt" 
  # flux <- "data/rawData/LI7500/LI7500_Site 4/4_2600_west_3_day_photo.txt"     
  # flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_east_2_day_photo.txt"   
  # flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_east_3_day_photo.txt"  
####flux <- t[3]
  for(flux in unique(fluxDF$Filename)){
    
    if(is.na(flux)){next}  # Skip if the flux filename is NA
    
    # Subset the flux data frame for the current filename and keep distinct rows
    dtSub <- fluxDF %>% filter(Filename == {{flux}}) %>% unique()#%>% distinct(across(-f_fluxID), .keep_all = TRUE)
    
    if(nrow(dtSub) < 60 + skip){next}  # Skip if n is too small 
    
    
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
    
    # Discard fluxes with a suspiciously high range 
    if(param == "co2"){
      if( (max(co2, na.rm = T) - min(co2, na.rm = T)) > 50){next}
    } else if(param == "h2o"){
      if( (max(h2o, na.rm = T) - min(h2o, na.rm = T)) > 50){next} ### gotta decide on a better water fluxes threshold 
    }

    
    
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
    
    
    if ("co2" == param) {  
      cwPrime <- cPrime  # Use cPrime for CO2
      tag <- "cPrime"  
    } else if ("h2o" == param) {  
      cwPrime <- wPrime  # Use wPrime for H2O
      tag <- "wPrime"  
    }
    
    # Identify change points in the time series of c'
    res <- suppressMessages(cpop(cwPrime, minseglen = 30))  # Identify change points with a minimum segment length of 30 seconds
    changepoints(res)  # Extract change points from the result
    cwPrime_seg <- fitted(res)  # Get the fitted values from the change point analysis
    
    
    if(param == "co2"){
      concCol <- co2Col
    }else if(param == "h2o"){
      concCol <- h2oCol
      
    }
    # Create a ggplot for visualizing CO2 concentration over time
    p2 <- ggplot(data = dtSub, aes(y = .data[[concCol]],  
                                   x = as.numeric(.data[[dateTimeCol]]) - min(as.numeric(.data[[dateTimeCol]])), 
                                   color = as.numeric(.data[[signalStrengthCol]]))) +  
      geom_point() +  # Add points for CO2 data
      scale_color_gradient(high = "blue", low = "red", limits = c(79, 101)) +  # Color gradient for signal strength
      geom_line() +  # Add lines connecting the points
      theme(legend.position = "none") +  # Remove legend
      #ylim(min(dtSub[[co2Col]], na.rm = TRUE), max(dtSub[[co2Col]], na.rm = TRUE)) +  # Set y-axis limits for CO2
      ylab(paste0(param, " concentration")) +  # Label y-axis
      xlab("Time") +  # Label x-axis
      geom_vline(xintercept = cwPrime_seg$x0)  # Add vertical lines at change points
    
    # Create a ggplot for visualizing PAR over time
    p1 <- ggplot(data = dtSub,  
                 aes(y = .data[[parCol]],  
                     x = as.numeric(.data[[dateTimeCol]]) - min(as.numeric(.data[[dateTimeCol]])))) +  
      geom_point(color = "forestgreen") +  # Add points for PAR data
      geom_line(color = "forestgreen") +  # Add lines connecting the points
    #  ylim(min(dtSub[[parCol]], na.rm = TRUE), max(dtSub[[parCol]], na.rm = TRUE)) +  # Set y-axis limits for PAR
      ylab("PAR") +  # Label y-axis
      xlab("Time") +  # Label x-axis
      labs(title = paste0(flux)) +  # Set title based on flux filename
      geom_vline(xintercept = cwPrime_seg$x0)  # Add vertical lines at change points
    
    # Combine the two plots vertically
    p_c <- plot_grid(p1, p2, ncol=1, align="v", axis=1) 
    plot(p_c)  # Display the combined plot
    
    # Create a sequence of segment indices based on the number of rows in cwPrime_seg data frame
    segs <- c(1:nrow(cwPrime_seg))  
    
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
      
      s1 <- cwPrime_seg$x0[s]  # Starting index for current segment
      
      # Determine the ending index for the current segment
      if(s == nrow(cwPrime_seg)){  
        s2 <- end  # If it's the last segment, use the last index
      } else{
        s2 <- cwPrime_seg$x0[s+1]  # Otherwise, use the starting index of the next segment
      }
      
      # Calculate the mean signal strength and PAR for the current segment
      meanSigStrength <- mean(signalStrength[s1:s2])  
      meanPAR <- mean(par[s1:s2])  
      sdPAR <- sd(par[s1:s2])
      cvPAR <- sdPAR/meanPAR
      
      # Select the concentration variable based on the parameter specified
      # Tag to identify the parameter being processed
      
      if ("co2" == param) {  
        cwPrime <- cPrime  # Use cPrime for CO2
        tag <- "cPrime"  
      } else if ("h2o" == param) {  
        cwPrime <- wPrime  # Use wPrime for H2O
        tag <- "wPrime"  
      }
      
      
      # Proceed only if both signal strength and PAR exceed the respective thresholds
      if((meanSigStrength > sigStrengthThresh) && (meanPAR > parThresh) && (cvPAR < .25)){  
        
        # Fit a linear model to the current segment
        linear.fit <- stats::lm(cwPrime[s1:s2] ~ (time[s1:s2]))  
        
        lmSlope[i] <- linear.fit$coeff[2]
        
        # Extract R-squared value from the linear model
        rsq <- as.numeric(summary(linear.fit)$r.sq)  
        
        pred <- data.frame(time = time[s1:s2])
        pred$pred <- predict(linear.fit)*(1 - (mean(h2o[s1:s2]) / 1000))  
        
        predPlot <- rbind(pred, predPlot)
        
        p3 <- p2 + 
          geom_line(data = predPlot, aes(x = time, y = pred), color = "black", linewidth = 1.1) +
          labs(title = paste0(flux))
        print(p3)
        # Calculate average temperature, pressure, and H2O concentration for the segment
        t_av <- mean(temp[s1:s2])  
        p_av <- mean(press[s1:s2])  
        w_av <- mean(h2o[s1:s2])  
        
        
        
        ### save R2 for all segments
        frac_sample_all <- (s2-s1)  # Calculate the fraction of samples in the current segment
        eff_sample_all <- eff_sample_all + (s2-s1)  # Update the effective sample size for linear fits
        
        r2LinearFitAll[i] <- rsq
        
        # Only consider the segment if the R-squared value meets the threshold (0.6)
        if(rsq > 0.7){  
          frac_sample <- (s2-s1)  # Calculate the fraction of samples in the current segment
          eff_sample <- eff_sample + (s2-s1)  # Update the effective sample size for linear fits
          
          # Store metrics for linear model results
          aicLinearFit[i] <- as.numeric(stats::AIC(linear.fit))*frac_sample  # Store AIC multiplied by sample fraction
          r2LinearFit[i] <- as.numeric(summary(linear.fit)$r.sq)*frac_sample  # Store R-squared multiplied by sample fraction
          inter[i] <- as.numeric(linear.fit$coeff[1])  # Store intercept from linear fit
          dcw_dt[i] <- as.numeric(linear.fit$coeff[2])  # Store slope from linear fit
          avgTemperatureC[i] <- mean(temp[s1:s2])*frac_sample  # Store average temperature multiplied by sample fraction
          avgPressureKPa[i] <- mean(press[s1:s2])*frac_sample  # Store average pressure multiplied by sample fraction
          cav[i] <- mean(co2[s1:s2])*frac_sample  # Store average CO2 concentration multiplied by sample fraction
          wav[i] <- mean(h2o[s1:s2])*frac_sample  # Store average H2O concentration multiplied by sample fraction
          
          # Calculate the linear model parameter based on the parameter type
          if ("co2" == param) {  
            param_lm[i] <- (vol * p_av * (1000) * dcw_dt[i])/(R * area * (t_av + 273.15))*frac_sample  
          } else if ("h2o" == param) {  
            param_lm[i] <- (vol * p_av * (1000) * dcw_dt[i])/(R * area * (t_av + 273.15))*frac_sample  
          }  
        }  
        
        # Print the current segment index
        print(s)  
        i <- i+1  # Increment the index for storing results
      }  
    }
    
    # Calculate average metrics based on effective sample sizes
    aic_linear_avg <- sum(aicLinearFit, na.rm = TRUE)/eff_sample  # Average AIC for linear fits
    inter_avg <- sum(inter, na.rm = TRUE)/eff_sample  # Average intercept for linear fits
    dcw_dt_avg <- sum(dcw_dt, na.rm = TRUE)/eff_sample  # Average slope for linear fits
    r2LinearFit_avg <- sum(r2LinearFit, na.rm = TRUE)/eff_sample  # Average R-squared for linear fits
    param_lm_avg <- sum(param_lm, na.rm = TRUE)/eff_sample  # Average parameters for linear model
    avgTemperatureC_avg <- if(eff_sample == 0){mean(temp)}else{sum(avgTemperatureC, na.rm = TRUE)/eff_sample}  # Average temperature with handling for no effective samples
    avgPressureKPa_avg <- if(eff_sample == 0){mean(press)}else{sum(avgPressureKPa, na.rm = TRUE)/eff_sample}  # Average pressure with handling for no effective samples
    cav_avg <- if(eff_sample == 0){mean(co2)}else{sum(cav, na.rm = TRUE)/eff_sample}  # Average CO2 concentration with handling for no effective samples
    wav_avg <- if(eff_sample == 0){mean(h2o)}else{sum(wav, na.rm = TRUE)/eff_sample}  # Average H2O concentration with handling for no effective samples
    
    r2LinearFitAll_avg <- sum(r2LinearFitAll, na.rm = T)/eff_sample_all
    
    
    if(any(lmSlope > 0, na.rm = T) & any(lmSlope < 0, na.rm = T)) {
      fluxDirectionFlag <- paste0("Weird flux! Both positive and negative segments identified")
      print(fluxDirectionFlag)
    } else {
      fluxDirectionFlag <- paste0("Looks good!")
      print(fluxDirectionFlag)
    }
    
  
    
    # Calculate standard deviations for the parameters based on effective sample sizes
    sd_lm <- sd(param_lm)/eff_sample  # Standard deviation for linear model parameters

      aic_avg <- aic_linear_avg
      r2_avg <- r2LinearFit_avg
      fluxValue <- param_lm_avg
      segSD <- sd_lm
      fluxMethodJustification <- "checked only linear fit"
  
        ### select flux

    # Extract unique plot IDs from the subset data frame using the specified column name
    plotID = unique(dtSub[[plotIDCol]])  
    
    if(!is.na(dayNightCol)){
      dayOrNight = unique(dtSub[[dayNightCol]])  
    }else{dayOrNight = NA}
    if(!is.na(redoCol)){
      redo = unique(dtSub[[redoCol]])  
    }else{redo = NA}
    
    # Print a tibble summarizing various metrics related to the calculated flux
    print(data.table::data.table(
      filename = flux,                          # File name associated with the flux calculations
      avgTemperatureC = avgTemperatureC_avg,   # Average temperature in Celsius
      avgPressureKPa = avgPressureKPa_avg,     # Average pressure in KPa
      fluxValue = fluxValue,                    # Value of the flux being calculated
      fluxType = fluxType,                      # Type of flux (e.g., CO2, H2O)
      plotID = plotID,                          # Unique plot identifier
      segSD = segSD,                            # Standard deviation of segments
      avgCO2 = cav_avg,                         # Average CO2 concentration
      avgH2O = wav_avg,                         # Average H2O concentration
      r2 = r2_avg,                              # Average R-squared value for model fits
      aic = aic_avg,                            # Average Akaike Information Criterion (AIC) for model fits
      param = param,                            # Parameter associated with the flux calculation
      fluxMethodJustification = fluxMethodJustification,  # Justification for the method used to calculate flux
      fluxDirectionFlag = fluxDirectionFlag, 
      redo = redo, 
      dayOrNight = dayOrNight, 
      totalRsq = r2LinearFitAll_avg
    ))
    
    # Create a data table to hold the results of the flux calculations
    tmpRes <- data.table::data.table(
      filename = flux,                          # File name
      avgTemperatureC = avgTemperatureC_avg,   # Average temperature
      avgPressureKPa = avgPressureKPa_avg,     # Average pressure
      fluxValue = fluxValue,                    # Flux value
      fluxType = fluxType,                      # Flux type
      plotID = plotID,                          # Unique plot ID
      segSD = segSD,                            # Segment standard deviation
      avgCO2 = cav_avg,                         # Average CO2 concentration
      avgH2O = wav_avg,                         # Average H2O concentration
      r2 = r2_avg,                              # Average R-squared
      aic = aic_avg,                            # Average AIC
      param = param,                            # Parameter
      fluxMethodJustification = fluxMethodJustification,  # Justification for the flux calculation method
      fluxDirectionFlag = fluxDirectionFlag,
      redo = redo, 
      dayOrNight = dayOrNight, 
      totalRsq = r2LinearFitAll_avg
      
    ) %>%
      # Mutate to create additional flags for quality control
      mutate(
        segSD = ifelse(is.na(segSD), 0, segSD),  # Replace NA segment SDs with 0
        r2Flag = ifelse(r2 < .75 | is.na(r2), "Be careful, R2 is suspiciously low or NA", "Looks good!"),  # Flag for R-squared value
        segFlag = ifelse(segSD > abs(fluxValue/2), "Not good, segments are extremely variable. Suggest to discard", "Looks good!"),  # Flag for segment variability
        fluxFlag = ifelse(r2Flag == "Looks good!" & segFlag == "Looks good!" & fluxDirectionFlag == "Looks good!", "keep", "discard")  # Overall flag for flux quality
      )
    tmpRes
    # Append the new results to the overall results data table
    calculatedFluxes <- rbind(tmpRes, calculatedFluxes)  
  }
  # Return the combined data table with calculated fluxes
  return(calculatedFluxes)  
}

