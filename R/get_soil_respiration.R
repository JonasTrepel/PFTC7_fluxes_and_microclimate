#### Get soil respiration data from LiCOR 8100 ####

#### Assumptions/Settings 
####### naming: SR_site_elevation_aspect (e.g., SR_1_2000_west)
####### Licor model: LI-8100
####### setup/settings: 240 seconds purge time, 180s measurement
####### file-type: .81x
####### start at plot 1, end at 5

library(data.table)
library(tidyverse)

source("R/functions/calcSR.R")
source("R/functions/getFluxDF.R")


filesSR <- list.files("data/rawData/LI8100", recursive = TRUE, full.names = TRUE)

soilResDTRaw <- getFluxDF(files = filesSR,
                          device = "LI8100", 
                          toi = 120:179)

dtSR_raw <- calcSR(data = soilResDTRaw, 
                    area = 317.8, 
                    volume = 1807.6) 

dtSR <- dtSR_raw %>% 
  mutate(year = year(DateTime),
         Date = date(DateTime), 
         day_night = "day") %>% 
  rename(date_time = DateTime, 
         date = Date)  
dtSR[dtSR$year < 2023, ]$date <- NA
dtSR[dtSR$year < 2023, ]$date_time <- NA


fwrite(dtSR, "data/dataFragments/soil_respiration_pftc7.csv")

