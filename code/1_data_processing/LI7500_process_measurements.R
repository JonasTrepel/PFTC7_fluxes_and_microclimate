# Extract temperatures from the LI7500 data
# devtools::install_github("PaulESantos/co2fluxtent")
library(co2fluxtent)
library(tidyverse)
library(data.table)

# Read in all the ambient files ----
# Remember to fix the file names if you've just downloaded from OSF
source("code/functions/fix_file_names.R")
source("code/functions/test_flux_files.R")
fix_file_names(path = "raw_data/LI7500/")

list.files("raw_data/LI7500/", recursive = TRUE)
# Look for flux files in a folder
licor_files <- Map(c, co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 1"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 2"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 3"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 4"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 5"))

## clean file names
# Check if the files are ok
licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 50) ##removed three files

#Only the ambients
licor_files_ambient <- licor_files[["ambient_names"]]
filesAmbient <- licor_files_ambient |>
  # Fixed missing file names thanks to this post https://stackoverflow.com/questions/69357657/purrrmap-dfr-gives-number-of-list-element-as-id-argument-not-value-of-list-e
  purrr::set_names()

#Read them in
tempAmbient <- map_df(set_names(licor_files_ambient), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_delim(file = file, skip = 3, delim = "\t")) #important! reading in American format
}, .id = "File")

# Gather site, plot etc. information from the filenames
t.ambient = tempAmbient |>
  # extract metadata
  mutate(File = basename(File),
         File = str_remove(File, ".txt")
  ) |>
  # Separate into relevant info
  separate(File, into = c("siteID", "elevation", "aspect", "plot", "day.night"), remove = FALSE) |>
  # Get just the relevant data
  select(File:day.night, Time:`Sequence Number`, `Temperature (C)`, `CO2 Signal Strength`) |>
  # Filter to just data with a decent signal strength
  filter(`CO2 Signal Strength` >= 90)

write.csv(t.ambient, "raw_data/LI7500/LI7500_temperature.csv")
