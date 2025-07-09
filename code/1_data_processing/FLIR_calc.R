########### code for extracting surface temperature from FLIR cam #############


# ###################
# # https://github.com/rasenior/ThermStats
#
# library(devtools)
# install_url('http://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz')
# install_url('https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz')
#
#
# pak::pak("rasenior/ThermStats")
# # devtools::install_github("rasenior/ThermStats")



#######################

# pak::pak("gtatters/Thermimage")
library(Thermimage)
library(dplyr)
library(tidyverse)


dir_exiftool = "C:/Users/e0983474/Desktop/"
dir_FLIRimages = "flir_images/FLIR/"

# load functions modified from rasenior/ThermStats
source("batch_FLIR.r")

# output directory
out_dir <- "outputs/"


# read df of file codes
flir_df <- read.csv("raw_data/PFTC7_SA_FLIR_2023.csv") |>
  rename(file_name = file_code) |>
  # drop_na() |>
  filter(Flag != "missing")
# flir_files <- list.files("raw_data/FLIR/images/")


# get file names of raw FLIR images
flir_df <- getFLIRnames(flir_df)

# Batch extract thermal images included in ThermStats installation
flir_raw <-  batch_extract(in_dir = dir_FLIRimages,
                           file_name = flir_df$file_name,
                           write_results = T,
                           out_dir = out_dir,
                           exiftoolpath= dir_exiftool)

# Define raw data
raw_dat <- flir_raw$raw_dat
# Define camera calibration constants dataframe
camera_params <- flir_raw$camera_params




# Batch convert
flir_converted <- batch_convert(
    raw_dat = raw_dat,
    camera_params = camera_params,
    write_results = T,
    out_dir = out_dir,
    )

row <- dim(flir_converted[[1]])[1]
col <- dim(flir_converted[[1]])[2]

w <- camera_params[[1]][["ImageWidth"]]
h <- camera_params[[1]][["ImageHeight"]]

# batch crop the temp data
flir_cropped <- batch_crop(flir_converted,
                           row_start = 30, row_end = 90,
                           col_start = 30, col_end = 130,
                           out_dir = out_dir)

saveRDS(flir_cropped, paste0(out_dir, "FLIR_cropped.Rds"))

# visualize
plotTherm(flir_converted[[1]], w=w, h=h, minrangeset = 21, maxrangeset = 32, trans="rotate270.matrix")


# calculate statistics
stats <- data.frame()
for(i in 1:length(flir_cropped)){
  if(length(flir_cropped[[i]]) <= 0 ){
    stats[i, "file_number"] = as.double(names(flir_cropped)[[i]])
    stats[i, "meanT"] = NA
    stats[i, "medianT"] = NA
    stats[i, "maxT"] = NA
    stats[i, "minT"] = NA
    stats[i, "sdT"] = NA
    stats[i, "n90T"] = NA
    stats[i, "n10T"] = NA
  } else {
    stats[i, "file_number"] = as.double(names(flir_cropped)[[i]])
    stats[i, "meanT"] = mean(flir_cropped[[i]])
    stats[i, "medianT"] = median(flir_cropped[[i]])
    stats[i, "maxT"] = max(flir_cropped[[i]])
    stats[i, "minT"] = min(flir_cropped[[i]])
    stats[i, "sdT"] = sd(flir_cropped[[i]])
    stats[i, "n90T"] = as.numeric(quantile(flir_cropped[[i]], .90))
    stats[i, "n10T"] = as.numeric(quantile(flir_cropped[[i]], .10))
  }
}

flir_results <- flir_df |> left_join(stats, by = "file_number") |> mutate(id = paste(siteID, plotID, aspect, sep = "_"))

write.csv(flir_results, paste0(out_dir, "flir_stats.csv"))



flir_results <- read.csv(paste0(out_dir, "flir_stats.csv"))
######################## visualize stats ####################

flir_summary <- flir_results %>%
  plyr::ddply(c("day.night", "siteID", "elevation_m_asl","aspect", "plotID"), summarise,
              mean = mean(meanT),
              sd = sqrt(sum(sdT^2)))

flir_summary %>%
  filter(day.night == "day" & siteID != "6") %>%
  ggplot(aes(x = as.factor(aspect), y = mean, fill = aspect, color = aspect)) +
  geom_boxplot(linewidth = 1.2, alpha = 0.2) +
  labs(x = "elevation",
       y = "mean T (C)") +
  geom_jitter() +
  facet_grid(cols = vars(elevation_m_asl))
  # scale_fill_viridis_d(option = "A") +
  # scale_color_viridis_d(option = "D")


flir_summary %>%
  filter(day.night == "night" & siteID != "6") %>%
  ggplot(aes(x = aspect, y = mean, fill = aspect, color = aspect)) +
  geom_boxplot(linewidth = 1.2, alpha = 0.2) +
  labs(x = "elevation",
         y = "mean T (C)") +
  geom_jitter(alpha = 0.4) +
  facet_grid(cols = vars(elevation_m_asl))

