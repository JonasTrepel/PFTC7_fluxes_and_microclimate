# Download data using this script
# Use RStudio's handy script menu to jump to the data you need
# You should only need to download data once, unless you know they have been updated
# If you've been away from the project for awhile, you should download them again
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)


# Tent flux (LI7500) data ----
## LI7500 metadata (plot data) ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "PFTC7_SA_raw_fluxes_2023.csv",
  # Where do you want the file to go to?
  path = "raw_data/LI7500",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI7500")

## Site 1 LI7500 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_Site 1.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI7500",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI7500")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI7500/LI7500_Site 1.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI7500/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI7500/LI7500_Site 1.zip") #let's free some space

## Site 2 LI7500 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_Site 2.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI7500",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI7500")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI7500/LI7500_Site 2.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI7500/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI7500/LI7500_Site 2.zip") #let's free some space

## Site 3 LI7500 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_Site 3.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI7500",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI7500")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI7500/LI7500_Site 3.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI7500/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI7500/LI7500_Site 3.zip") #let's free some space

## Site 4 LI7500 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_Site 4.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI7500",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI7500")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI7500/LI7500_Site 4.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI7500/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI7500/LI7500_Site 4.zip") #let's free some space

## Site 5 LI7500 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_Site 5.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI7500",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI7500")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI7500/LI7500_Site 5.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI7500/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI7500/LI7500_Site 5.zip") #let's free some space

# Soil respiration (LI8100) data ----
## Site 1 LI8100 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI8100_Site 1.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI8100",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI8100")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI8100/LI8100_Site 1.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI8100/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI8100/LI8100_Site 1.zip") #let's free some space

## Site 2 LI8100 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI8100_Site 2.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI8100",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI8100")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI8100/LI8100_Site 2.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI8100/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI8100/LI8100_Site 2.zip") #let's free some space

## Site 3 LI8100 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI8100_Site 3.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI8100",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI8100")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI8100/LI8100_Site 3.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI8100/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI8100/LI8100_Site 3.zip") #let's free some space

## Site 4 LI8100 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI8100_Site 4.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI8100",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI8100")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI8100/LI8100_Site 4.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI8100/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI8100/LI8100_Site 4.zip") #let's free some space

## Site 5 LI8100 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI8100_Site 5.zip",
  # Where do you want the file to go to?
  path = "raw_data/LI8100",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data/LI8100")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/LI8100/LI8100_Site 5.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/LI8100/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/LI8100/LI8100_Site 5.zip") #let's free some space

## PAR data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "PAR.zip",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_flux_data")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/PAR.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/")

# Remove the zip file once you've unzipped it
file.remove("raw_data/PAR.zip") #let's free some space

# Tomst data ----
## Raw Tomst data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "Tomst_logger_raw.zip",
  # Where do you want the file to go to?
  path = "raw_data/microclimate",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_microclimate_data/Tomst_data")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/microclimate/Tomst_logger_raw.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/microclimate")

# Remove the zip file once you've unzipped it
file.remove("raw_data/microclimate/Tomst_logger_raw.zip") #let's free some space

## Raw RangeX Tomst data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "Rangex_data.zip",
  # Where do you want the file to go to?
  path = "raw_data/microclimate",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_microclimate_data/Tomst_data/RangeX")

# Unzip data
unzip(
  # Where is the zipped folder?
  "raw_data/microclimate/Rangex_data.zip",
  # Where do you want the files to go to?
  exdir = "raw_data/microclimate")

# Remove the zip file once you've unzipped it
file.remove("raw_data/microclimate/Rangex_data.zip") #let's free some space

## Tomst metadata ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "Tomst logger IDs.csv",
  # Where do you want the file to go to?
  path = "raw_data/microclimate",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_microclimate_data/Tomst_data")

## Tomst RangeX metadata ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "Tomst logger IDs RangeX.csv",
  # Where do you want the file to go to?
  path = "raw_data/microclimate",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_microclimate_data/Tomst_data")

# FLIR data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "PFTC7_SA_FLIR_2023.csv",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/raw_microclimate_data/FLIR_handheld_data/FLIR_images")

