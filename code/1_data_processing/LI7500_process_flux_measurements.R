library(tidyverse)
library(lubridate)

source("code/functions/flux_calc_seg.R")
source("code/functions/test_flux_files.R")
source("code/functions/neet_calc_wav.R")
source("code/functions/fix_file_names.R")

filesPFTC7 <- dir(path = "raw_data/LI7500/LI7500_Site 1",
                  pattern = ".txt", full.names = TRUE, recursive = TRUE)

#Run only once -- fix_file_names("./raw_data/LI7500/LI7500_Combined")

licor_files <- read_files("raw_data/LI7500/All_sites")
PAR_files <- dir(path = "raw_data/PAR",
                 pattern = ".TXT", full.names = TRUE, recursive = TRUE)

# Test files are good
licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 70)

# Read in data
stuff <- flux_calc_seg(fluxfiles = licor_files,
                       PAR_files=PAR_files,
                       param = "et")
write.csv(stuff, file="segmented_fluxes.csv")

# Troubleshooting
neet_wav(filename = licor_files$photo_names[5],
         fluxfiles = licor_files,
         PAR = PAR_comb)
licor_files$photo_names[5]
nrow(read_delim(licor_files$photo_names[5], delim="\t", skip = 3))
