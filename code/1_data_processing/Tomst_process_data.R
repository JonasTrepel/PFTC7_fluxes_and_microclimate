library(tidyverse)
library(data.table)

source("code/functions/read_tomst_data.R")
source("code/functions/moist_calibration.R")

# Read in data from "R/download_raw_data.R"

# Read tomst and site metadata
ids <- read.csv("raw_data/microclimate/Tomst logger IDs.csv") %>%
  bind_rows(read.csv("raw_data/microclimate/Tomst logger IDs RangeX.csv")) |>
  mutate(tomst_id = as.numeric(gsub("TMS","",tomst_id))) %>%
  mutate(site = as.numeric(unlist(lapply(plot_id, function(x){substr(x, 1, 1)}))),
         aspect = unlist(lapply(plot_id, function(x){substr(x, 2, 2)})),
         plot = as.numeric(unlist(lapply(plot_id, function(x){substr(x, 3, 3)})))) |>
  # harmonise aspect
  mutate(aspect = case_when(
    aspect == "E" ~ "east",
    aspect == "W" ~ "west"
  ))

# List Tomst data files in local folder
f <- dir(path = "raw_data/microclimate", pattern = "^data_.*.csv$",
         full.names = TRUE, recursive = TRUE)

# Read and combine Tomst data
d1 <- read_tomst_data(f, tzone = "Etc/GMT-2") %>%
  distinct(tomst_id, datetime, .keep_all = T) %>% # Remove dublicates
  arrange(tomst_id, datetime)

# Join Tomst data and metadata
d <- left_join(ids, d1) %>%
  mutate(moist_vol = cal_fun4(moist)) |> # transform raw moisture counts to volumetric moisture content
  dplyr::rename(temp_soil_C = T1, temp_ground_C = T2, temp_air_C = T3) |> #rename temps by location
  # Remove experimental RangeX loggers
  filter(is.na(RangeX_treatment) | RangeX_treatment %in% c("VN", "VNW")) |>
  filter(datetime >= mind, #filter with set time
         datetime <= maxd)

# Plot temperature timeseries
d %>%
  ggplot(aes(x=datetime, group = tomst_id)) +
  geom_line(aes(y = temp_air_C), col = "cornflowerblue") +
  geom_line(aes(y = temp_ground_C), col = "brown1") +
  geom_line(aes(y = temp_soil_C), col = "darkgoldenrod") +
  theme_minimal() +
  ylab("Temperature") + xlab("Date") +
  facet_grid(rows = vars(site), cols = vars(aspect))

# Plot moisture timeseries
d %>%
  ggplot(aes(x=datetime, color = aspect, group = tomst_id)) +
  geom_line(aes(y = moist_vol)) +
  theme_minimal() +
  ylab("Volumetric moisture content") + xlab("Date") +
  facet_grid(rows = vars(site))

# means for each transect
d %>%
  group_by(site, aspect) %>%
  summarise(across(c(T1,T2,T3,moist_vol), mean))

# Export clean data ----
write.csv(d, "raw_data/PFTC7_Tomst_Data.csv", row.names = FALSE)
