#### Example script to read tent fluxes from South Africa 
library(data.table)
library(tidyverse)

### required functions: 

source("R/functions/readFluxFiles.R")
source("R/functions/fixFileNames.R")
source("R/functions/getFluxDF.R")
source("R/functions/readAndAddPAR.R")
source("R/functions/calcTentFluxes.R")
source("R/functions/calcSR.R")



### reguired packages: 


## path with flux files: 
path <- "data/rawData/LI7500"

## this will fix potential weird addons to the file names in the folder directly
fixFileNames(path = path)

# Read flux files 
fluxFiles <- readFluxFiles(path = path,
                           photo = "photo", ## specify the patterns in filenames to categorize fluxes
                           resp = "resp", 
                           ambient = "a",
                           recursive = T)



## get Flux metadata (South Africa specific unfortunately)

fluxMeta <- tibble(Filename = unlist(fluxFiles),
                   file = basename(Filename)) %>%
  mutate(site = unlist(lapply(file, function(x) str_split(x, "_")[[1]][1])),
         elevation = unlist(lapply(file, function(x) str_split(x, "_")[[1]][2])),
         aspect = unlist(lapply(file, function(x) str_split(x, "_")[[1]][3])),
         plot = unlist(lapply(file, function(x) str_split(x, "_")[[1]][4])),
         day_night = unlist(lapply(file, function(x) str_split(x, "_")[[1]][5])),
         measurement = unlist(lapply(file, function(x) gsub(".txt","",tail(str_split(x, "_")[[1]],1)))),
         redo = grepl("redo", file, ignore.case = T), 
         measurement = gsub("\\(1\\)", "", measurement), 
         plotID = paste0(elevation, aspect, plot))

fluxMeta

## read fluxes into a dataframe 
fluxDFRaw <- getFluxDF(files = fluxFiles,
                  skip = 3, #default
                  device = "LI7500" #default
                  )  %>% 
  left_join(fluxMeta) %>% #join metadata
  filter(!grepl("not used", Filename)) #remove unused files 


## Add PAR 

parFiles <- list.files("data/rawData/PAR/", full.names = T)

fluxDT <- readAndAddPAR(parFiles = parFiles, #default = NULL
                        fluxDF = fluxDFRaw, #default = NULL 
                        parTZ = "America/Mexico_City", #default = UTC
                        fluxTZ = "Africa/Johannesburg", #default = UTC
                        fluxDFTimeCol = "DateTime", #default
                        parSkip = 7 #default
                        )

## Calculate CO2 fluxes 

co2FluxDT <- calcTentFluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  sigStrengthThresh = 95.0, #default 
  parThresh = 0, #default 
  fluxDF = fluxDT, #default 
  param = "co2", #default 
  parCol = "PAR", #default 
  dateTimeCol = "DateTime", #default 
  co2Col = "ConcCO2", #default 
  h2oCol = "ConcH2O", #default 
  signalStrengthCol = "SignalStrength", #default 
  tempCol = "AirTemperature", #default 
  pressureCol = "PressureKPa", #default 
  fluxTypeCol = "measurement", #default 
  plotIDCol = "plotID", #default 
  skip = 7, 
  redoCol = "redo",
  dayNightCol = "day_night"
)

## inspect CO2 fluxes 



### calculate H2O fluxes 
h2oFluxDT <- calcTentFluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  sigStrengthThresh = 95.0, #default 
  parThresh = 0, #default 
  fluxDF = fluxDT, #default 
  param = "h2o", #default 
  parCol = "PAR", #default 
  dateTimeCol = "DateTime", #default 
  co2Col = "ConcCO2", #default 
  h2oCol = "ConcH2O", #default 
  signalStrengthCol = "SignalStrength", #default 
  tempCol = "AirTemperature", #default 
  pressureCol = "PressureKPa", #default 
  fluxTypeCol = "measurement", #default 
  plotIDCol = "plotID", #default 
  skip = 7, 
  redoCol = "redo",
  dayNightCol = "day_night"
)


##### Get Soil Respiration #####


filesSR <- list.files("data/rawData/LI8100", recursive = TRUE, full.names = TRUE)

soilResDTRaw <- getFluxDF(files = filesSR,
                  device = "LI8100", 
                  toi = 120:179)

soilResDT <- calcSR(data = soilResDTRaw, 
                    area = 317.8, 
                    volume = 1807.6) 

### combine all data 

soilWide <- soilResDT %>% 
  dplyr::select(-Rsq) %>% 
  pivot_wider(names_from = fluxType, values_from = fluxValue) %>% 
  dplyr::select(-device)


co2TentWide <- co2FluxDT %>% 
  filter(fluxFlag == "keep") %>% 
  mutate(fluxType = ifelse(fluxType == "resp" & dayOrNight == "night", "nightResp", fluxType))%>% 
  group_by(plotID, fluxType) %>% 
  summarize(fluxValue = mean(fluxValue, na.rm = T)) %>% 
  pivot_wider(names_from = fluxType, values_from = fluxValue) %>% 
  rename(NEE = photo, 
         DayReco = resp, 
         NightReco = nightResp) %>% 
  as.data.table()

h2oTentWide <- co2FluxDT %>% 
  filter(fluxFlag == "keep") %>% 
  mutate(fluxType = case_when(
    fluxType == "photo" ~ "ET", #Evapotranspiration 
    fluxType == "resp" & dayOrNight == "day" ~ "DayEvap", 
    fluxType == "resp" & dayOrNight == "night" ~ "NightEvap", 
  ))%>% 
  group_by(plotID, fluxType) %>% 
  summarize(fluxValue = mean(fluxValue, na.rm = T)) %>% 
  pivot_wider(names_from = fluxType, values_from = fluxValue) %>% 
  as.data.table()

fluxComb <- soilWide %>% 
  left_join(co2TentWide) %>% 
  left_join(h2oTentWide) %>% 
  mutate(GPP = NEE - DayReco, #Gross primary productivity 
         NPP = GPP - SoilResp, #Net primary productivity 
         CUE = NPP/GPP, #Carbon use efficiency 
         TRANS = ET - DayEvap, # Transpiration
         WUE = NPP/TRANS) #Water use efficiency
fluxComb

### Plot 

fluxCombLong <- fluxComb %>% 
  pivot_longer(cols = c(NEE, DayReco, NightReco, SoilResp, GPP, NPP, ET, DayEvap, NightEvap, TRANS, CUE, WUE),
               names_to = "fluxType", 
               values_to = "fluxValue")
library(ggridges)
ggplot(data = fluxCombLong) +
  geom_density_ridges(aes(y = as.factor(elevation), x = fluxValue)) +
  facet_wrap(~fluxType, scales = "free")


### Write data

fwrite(fluxCombLong, "builds/processedFluxes/cleanFluxesAllMetrics.csv")
fwrite(co2FluxDT, "builds/processedFluxes/co2Fluxes.csv")
fwrite(h2oFluxDT, "builds/processedFluxes/h2oFluxes.csv")



#### compare with "old" data 

#CO2
co2Man <- fread("data/oldData/licor_nee_flagged.csv") %>%
  filter(flag == "okay") %>% 
  mutate(plotID = paste0(elevation, aspect, plot)) %>% 
  dplyr::select(plotID, measurement, flux_value, flux)
  

compCO2 <- co2Man %>% 
  left_join(co2FluxDT %>% filter(fluxFlag == "keep")) %>% 
  mutate(fluxValue = ifelse(measurement == "photo", fluxValue*-1, fluxValue))

ggplot(data = compCO2) +
  geom_point(aes(x = fluxValue, y = flux_value)) +
  facet_wrap(~measurement, scales = "free") +
  geom_abline() +
  labs(title = "CO2 Fluxes", y = "Manually Processed Fluxes", x = "Fully Automated Fluxes") +
  theme_bw()


#Water
h2oMan <- fread("data/oldData/licor_et_flagged.csv") %>%
  filter(flag == "okay") %>% 
  mutate(plotID = paste0(elevation, aspect, plot)) %>% 
  dplyr::select(plotID, measurement, flux_value, flux)


compH2O <- h2oMan %>% 
  left_join(h2oFluxDT %>% filter(fluxFlag == "keep"))

ggplot(data = compH2O) +
  geom_point(aes(x = fluxValue, y = flux_value)) +
  facet_wrap(~measurement, scales = "free") +
  geom_abline() +
  labs(title = "H2O Fluxes", y = "Manually Processed Fluxes", x = "Fully Automated Fluxes") +
  theme_bw()

### check if the flagged fluxes overlap 


flaggedOld <- fread("data/oldData/licor_nee_flagged.csv") %>% 
  filter(!flag == "okay") %>% 
  mutate(file = gsub("raw_data/LI7500/All_sites/", "", filename)) %>% dplyr::select(file) %>% pull()


flaggedNew <- co2FluxDT %>% 
  filter(!fluxFlag == "keep") %>% 
  mutate(file = gsub("data/rawData/LI7500/LI7500_Site 1/", "", filename), 
         file = gsub("data/rawData/LI7500/LI7500_Site 2/", "", file), 
         file = gsub("data/rawData/LI7500/LI7500_Site 3/", "", file), 
         file = gsub("data/rawData/LI7500/LI7500_Site 4/", "", file), 
         file = gsub("data/rawData/LI7500/LI7500_Site 5/", "", file)) %>% dplyr::select(file) %>% pull()

 
setdiff(flaggedNew, flaggedOld)

setdiff(flaggedOld, flaggedNew)

