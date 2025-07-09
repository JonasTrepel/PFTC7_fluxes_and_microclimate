### get date and time for fluxes


library(data.table)
library(tidyverse)

source("code/functions/readFluxFiles.R")
source("code/functions/fixFileNames.R")
source("code/functions/getFluxDF.R")

## path with flux files:
path <- "raw_data/LI7500"

## this will fix potential weird addons to the file names in the folder directly
fixFileNames(path = path)

# Read flux files
fluxFiles <- readFluxFiles(path = path,
                           photo = "photo", ## specify the patterns in filenames to categorize fluxes
                           resp = "resp",
                           ambient = "a",
                           recursive = T)

# dt_date <- fread("raw_data/dataFragments/licor7500_datetimes.csv")



### load carbon fluxes
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "licor_nee_flagged.csv",
  # Where do you want the file to go to?
  path = "raw_data/dataFragments",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

dt_c_raw <- fread("raw_data/dataFragments/licor_nee_flagged.csv") %>%
  mutate(file = gsub("raw_data/LI7500/All_sites/", "", filename)) %>%
  left_join(dt_date)

unique(dt_c_raw[is.na(date), ]$filename)

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
  filter(!grepl("not used", Filename)) #%>% filter(!grepl("old", Filename)) #remove unused files

## subset to filenames and date/datetime


dt_date <- fluxDFRaw %>%
  dplyr::select(file, DateTime) %>%
  group_by(file) %>%
  slice_min(DateTime) %>%
  mutate(Date = date(DateTime)) %>%
  rename(date = Date,
         date_time = DateTime)

fwrite(dt_date, "raw_data/dataFragments/licor7500_datetimes.csv")
