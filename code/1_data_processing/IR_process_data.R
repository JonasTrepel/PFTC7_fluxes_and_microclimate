IRtemp = read.csv("raw_data/LI7500/PFTC7_SA_raw_fluxes_2023.csv") |>
  drop_na(siteID) |>
  # take most recent attempt
  arrange(Attempt) |>
  group_by(day.night, aspect, siteID, plotID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(day.night:plotID, Remarks, IR_temp_1:IR_temp_5) |>
  pivot_longer(cols = IR_temp_1:IR_temp_5, names_to = "delete", values_to = "temp_C") |>
  # flag erroneous reads
  mutate(flag = case_when(
    str_detect(Remarks, "IRt on soil") ~ "IR_soil",
    temp_C > 40 ~ "IR_soil",
    TRUE ~ "okay"
  )) |>
  drop_na(siteID) |>
  mutate(dataset = "IR Temp",
         elevation = case_when(
           siteID == 1 ~ 2000,
           siteID == 2 ~ 2200,
           siteID == 3 ~ 2400,
           siteID == 4 ~ 2600,
           siteID == 5 ~ 2800
         )) |>
  # Add in date
  mutate(Date = paste0("2024-12-", day..NOT.DATE....))

write.csv(IRtemp, "raw_data/LI7500/PFTC7_microclimate_IRtemp.csv", row.names = FALSE)
