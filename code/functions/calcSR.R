## Code by Jonas Trepel, Hilary Rose Dawson & Kristine Birkeli

#required packages
library(tidyverse)
#library(tidylog)

#assumptions: read SR files with the readSR or with getFluxDF function

#arguments/required input: 
## data (LI8100 flux data )
## area
## volume

calcSR <- function(data, area = 317.8, volume = 1807.6){
  
  #load dependencies
  library(tidyverse)
  
  #build empty dataframes
  tmp <- data.frame(device = "LI8100")
  
  df.res <- data.frame()
  
  
  area <- 317.8/10000 #most likely in m2
  volume <- 1807.6/1000000
  R <- 8.314472
  
 # unique <- "2000_east_1"
  
  for(unique in unique(data$plotID)){
    
    temp <- mean(as.numeric(data[data$plotID == unique, ]$ChamberTemperature), na.rm = TRUE)
    pressure <- mean(as.numeric(data[data$plotID == unique, ]$PressureKPa), na.rm = TRUE)
    
    #model
    lm.co2 <- lm(Cdry ~ Etime, data = data[data$plotID == unique, ])
    lm.h2o <- lm(ConcH2O ~ Etime, data = data[data$plotID == unique, ])
    
    
    
    #CO2
    tmpCO2 <- tmp %>% mutate(
      co2DrySlope = lm.co2$coefficients[2], #slope in ppm/second
      Rsq = summary(lm.co2)$r.sq,
      fluxType = "SoilResp", 
      #General information
      plot = unique(data[data$plotID == unique, ]$plot),
      site = unique(data[data$plotID == unique, ]$site),
      plotID = unique(data[data$plotID == unique, ]$plotID),
      elevation = unique(data[data$plotID == unique, ]$elevation),
      aspect = unique(data[data$plotID == unique, ]$aspect), 
      DateTime = min(data[data$plotID == unique, ]$DateTime)) %>%
      mutate(
      fluxValue = (volume * pressure * (1000) * co2DrySlope)/(R * area * (temp + 273.15)))  %>% 
      dplyr::select(-co2DrySlope)
    
    tmpH2O <- tmp %>% mutate(
      h2ODrySlope = lm.h2o$coefficients[2], #slope in ppm/second
      Rsq = summary(lm.h2o)$r.sq,
      fluxType = "SoilEvap", 
      #General information
      plot = unique(data[data$plotID == unique, ]$plot),
      site = unique(data[data$plotID == unique, ]$site),
      plotID = unique(data[data$plotID == unique, ]$plotID),
      elevation = unique(data[data$plotID == unique, ]$elevation),
      aspect = unique(data[data$plotID == unique, ]$aspect), 
      DateTime = min(data[data$plotID == unique, ]$DateTime)) %>%
      mutate(
        fluxValue = (volume * pressure * (1000) * h2ODrySlope)/(R * area * (temp + 273.15))) %>% 
      dplyr::select(-h2ODrySlope)
    
    df.res <- rbind(tmpCO2, tmpH2O, df.res)
    
    df.res <- df.res[!is.na(df.res$plotID), ] %>% 
      arrange(fluxType)
    
  }
  
  return(df.res)
  
}
