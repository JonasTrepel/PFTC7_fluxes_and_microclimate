### get date and time for fluxes 


library(data.table)
library(tidyverse)

source("R/functions/readFluxFiles.R")
source("R/functions/fixFileNames.R")
source("R/functions/getFluxDF.R")

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
  filter(!grepl("not used", Filename)) %>% filter(!grepl("old", Filename)) #remove unused files 

## subset to filenames and date/datetime 


dt_date <- fluxDFRaw %>% 
  dplyr::select(file, DateTime) %>% 
  group_by(file) %>% 
  slice_min(DateTime) %>% 
  mutate(Date = date(DateTime)) %>% 
  rename(date = Date, 
         date_time = DateTime)

fwrite(dt_date, "data/dataFragments/licor7500_datetimes.csv")
