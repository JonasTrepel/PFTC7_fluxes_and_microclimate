# Check datasets ----
IR = read.csv("raw_data/PFTC7_microclimate_IRtemp.csv")

IR.check = IR |>
  mutate(ID = paste0(siteID, "_", plotID, "_", aspect)) |>
  group_by(ID, day.night) |>
  summarize(n = length(day.night)) |>
  pivot_wider(names_from = ID, values_from = n, values_fill = 0) |>
  pivot_longer(cols = '1_1_east':'5_5_west', names_to = "ID", values_to = "n") |>
  filter(n != 5)

FLIR = read.csv("raw_data/flir_values.csv") |>
  add_column(dataset = "FLIR") |>
  rename(plot = plotID) |>
  # Make site 6 aspect west
  mutate(aspect = case_when(
    siteID == 6 ~ "west",
    TRUE ~ aspect
  ),
  # Add in date
  Date = paste0("2024-12-", day..NOT.DATE....))

FLIR.check = FLIR |>
  dplyr::select(file_number, day.night, siteID, plot, aspect) |>
  distinct() |>
  filter(siteID != 6) |>
  mutate(ID = paste0(siteID, "_", plot, "_", aspect)) |>
  group_by(ID, day.night) |>
  summarize(n = length(day.night)) |>
  arrange(ID) |>
  pivot_wider(names_from = ID, values_from = n, values_fill = 0) |>
  pivot_longer(cols = '1_1_east':'5_5_west', names_to = "ID", values_to = "n") |>
  filter(n == 0) |>
  arrange(ID, day.night)

tomst = read.csv("raw_data/PFTC7_Tomst_Data.csv") |>
  separate(datetime, into = c("Date", "time"), sep = " ") |>
  mutate(time = replace_na(time, "00:00"),
         time = str_sub(time, 1, 5)) |>
  pivot_longer(cols = temp_soil_C:moist_vol, names_to = "metric", values_to = "value") |>
  mutate(dataset = "Tomst",
         PlotID = as.character(str_sub(plot_id, 3, 3))) |>
  rename(siteID = site) |>
  drop_na(metric) |>
  filter(metric != "moist")

tomst.check = tomst |>
  dplyr::select(tomst_id, siteID, plot, aspect) |>
  distinct() |>
  group_by(siteID, aspect) |>
  summarize(n = length(tomst_id)) |>
  pivot_wider(names_from = siteID, values_from = n, values_fill = 0)

LI7500 = read.csv("raw_data/LI7500_temperature.csv") |>
  pivot_longer('Temperature..C.', names_to = "metric", values_to = "value") |>
  mutate(dataset = "LI-7500",
         day.night = case_when(
           day.night == "resp" ~ "day",
           TRUE ~ day.night))

LI7500.check = LI7500 |>
  select(siteID, aspect, plot, day.night) |>
  distinct() |>
  mutate(ID = paste0(siteID, "_", aspect)) |>
  group_by(ID, day.night) |>
  summarize(n = length(day.night)) |>
  pivot_wider(names_from = ID, values_from = n, values_fill = 0)
