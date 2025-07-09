# Download processed data ----
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

## Processed tomst data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "PFTC7_Tomst_Data.csv",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/xi_raw_microclimate")

## Processed FLIR data ----
# Note that this file is 383MB and may cause R to crash.
# You can also download it from OSF at https://osf.io/gp8u9
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "flir_values.csv",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/xi_raw_microclimate")

## Processed LI7500 temps ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_temperature.csv",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/xi_raw_microclimate")

## Processed IR temps ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "PFTC7_microclimate_IRtemp.csv",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/xi_raw_microclimate")

# Combine and clean separate datasets ----
microclimate <- read.csv("raw_data/flir_values.csv") |>
  add_column(dataset = "FLIR") |>
  rename(plot = plotID) |>
  # Make site 6 aspect west
  mutate(aspect = case_when(
    siteID == 6 ~ "west",
    TRUE ~ aspect
  ),
  # Add in date
  Date = paste0("2023-12-", day..NOT.DATE....)) |>
  # Remove extraneous FLIR readings
  drop_na(aspect) |>
  # Add in infrared temperatures
  bind_rows(read.csv("raw_data/PFTC7_microclimate_IRtemp.csv") |> rename(plot = plotID)) |>
  pivot_longer(temp_C, names_to = "metric", values_to = "value") |>
  # Standardise columns for tomst
  mutate(metric = case_when(
    dataset == "IR Temp" ~ "temperature_leaf",
    dataset == "FLIR" ~ "temperature_ground",
  )) |>
  # Bind in Tomst
  bind_rows(read.csv("raw_data/PFTC7_Tomst_Data.csv") |>
              separate(datetime, into = c("Date", "time"), sep = " ") |>
              mutate(time = replace_na(time, "00:00"),
                     time = str_sub(time, 1, 5)) |>
              pivot_longer(cols = temp_soil_C:moist_vol, names_to = "metric", values_to = "value") |>
              mutate(dataset = "Tomst",
                     PlotID = as.character(str_sub(plot_id, 3, 3)))|>
              rename(siteID = site) |>
              drop_na(metric) |>
              filter(metric != "moist")) |>
  # Convert time to include seconds to match LICOR
  mutate(time = case_when(
    !is.na(time) ~ paste0(time, ":00"),
    TRUE ~ NA
  )) |>
  # Bind in LI7500 temps
  bind_rows(read.csv("raw_data/LI7500_temperature.csv") |>
              pivot_longer('Temperature..C.', names_to = "metric", values_to = "value") |>
              mutate(dataset = "LI-7500") |>
              rename(time = Time) |>
              filter(day.night != "resp")) |>
  # Factor variables
  mutate(metric = case_when(
    metric == "moist_vol" ~ "moisture_soil",
    metric %in% c("temp_air_C", "Temperature..C.") ~ "temperature_air",
    metric == "temp_ground_C" ~ "temperature_near_surface",
    metric == "temp_soil_C" ~ "temperature_soil",
    TRUE ~ metric,
  )) |>
  # Flag outliers
  mutate(flag_all = case_when(
    metric  == "temperature_ground" & value < 0 ~ "negative_ground_temp",
    dataset == "IR Temp" ~ flag,
    TRUE ~ "okay"
  )) |>
  # Handle outliers
  mutate(value = case_when(
    flag_all == "negative_ground_temp" ~ NA,
    flag_all == "IR_soil" ~ NA,
    TRUE ~ value
  )) |>
  # Fill in missing values
  mutate(day.night = str_to_title(day.night)) |>
  mutate(aspect = factor(aspect, levels = c("east", "west")),
         elevation = factor(case_when(
           siteID == 6 ~ 3000,
           siteID == 5 ~ 2800,
           siteID == 4 ~ 2600,
           siteID == 3 ~ 2400,
           siteID == 2 ~ 2200,
           siteID == 1 ~ 2000
         ), levels = c("3000", "2800", "2600", "2400", "2200", "2000"))) |>
  # Remove measurements taken outside sampling period
  mutate(date = ymd(Date)) |>
  filter(date %within% lubridate::interval("2023-12-05", "2023-12-19")) |>
  # Harmonise names
  select(-c(plot_id, elevation_m_asl, Date)) |>
  rename(site_id = siteID, elevation_m_asl = elevation,
         plot_id = plot, device = dataset, climate_variable = metric, day_night = day.night) |>
  select(date, time, site_id, elevation_m_asl,
         #treat_1, treat_2, #strange - somehow these variables don't exist - also not in the description table
         aspect, plot_id, day_night, device,
         climate_variable, value, flag_all) %>%
  relocate(date, time,
           aspect, site_id, elevation_m_asl, plot_id, day_night,
           climate_variable, value, device, flag_all)


write.csv(microclimate |> drop_na(value), "clean_data/xi_PFTC7_clean_microclimate_2023.csv",
          row.names = FALSE)
